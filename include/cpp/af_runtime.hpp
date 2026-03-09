// af_runtime.hpp -- ActorForth C++ coroutine-based actor runtime
//
// Provides lightweight actor concurrency matching BEAM/A4 semantics:
// - Coroutine-based actors (millions possible, ~few KB each)
// - MPSC mailbox per actor with selective receive
// - Cooperative scheduling with reduction-based yielding
// - Cast (async) and call (sync with reply channel) protocols
// - Actor linking and monitoring
// - Supervision with restart policies
// - Timers and receive-with-timeout
//
// Requires C++20 coroutines.

#pragma once

#include <algorithm>
#include <atomic>
#include <cassert>
#include <chrono>
#include <condition_variable>
#include <coroutine>
#include <cstdint>
#include <deque>
#include <functional>
#include <iostream>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace af {

// Forward declarations
struct Value;
class Actor;
class ActorContext;
class Scheduler;

using Stack = std::vector<Value>;
using ProductFields = std::vector<std::pair<std::string, Value>>;
using ActorRef = std::shared_ptr<Actor>;
using ActorWeak = std::weak_ptr<Actor>;

// ===================================================================
// Tagged value type -- mirrors A4's {Type, Value} stack items
// ===================================================================

enum class Type {
    Int, Float, Bool, String, Atom, List, Map, Tuple, Actor, Product, Message
};

struct Value {
    Type type;
    std::variant<
        int64_t,            // Int
        double,             // Float
        bool,               // Bool
        std::string,        // String, Atom
        std::vector<Value>, // List
        ProductFields,      // Product type fields
        ActorRef            // Actor reference
    > data;
    std::string type_name;  // For product types and messages

    // Constructors
    static Value make_int(int64_t v) { return {Type::Int, v, ""}; }
    static Value make_float(double v) { return {Type::Float, v, ""}; }
    static Value make_bool(bool v) { return {Type::Bool, v, ""}; }
    static Value make_string(const std::string& v) { return {Type::String, v, ""}; }
    static Value make_atom(const std::string& v) { return {Type::Atom, v, ""}; }
    static Value make_list(const std::vector<Value>& v) { return {Type::List, v, ""}; }
    static Value make_product(const std::string& name, const ProductFields& fields) {
        return {Type::Product, fields, name};
    }
    static Value make_actor(ActorRef ref) { return {Type::Actor, std::move(ref), ""}; }
    static Value make_message(const std::string& tag, const Value& payload) {
        ProductFields fields = {{"tag", make_string(tag)}, {"data", payload}};
        return {Type::Message, fields, "Message"};
    }

    // Accessors
    int64_t as_int() const { return std::get<int64_t>(data); }
    double as_float() const { return std::get<double>(data); }
    bool as_bool() const { return std::get<bool>(data); }
    const std::string& as_string() const { return std::get<std::string>(data); }
    const std::vector<Value>& as_list() const { return std::get<std::vector<Value>>(data); }
    const ProductFields& as_fields() const { return std::get<ProductFields>(data); }
    ProductFields& as_fields_mut() { return std::get<ProductFields>(data); }
    ActorRef as_actor() const { return std::get<ActorRef>(data); }

    // Field access for product types and messages
    const Value& field(const std::string& name) const {
        for (auto& [k, v] : as_fields()) if (k == name) return v;
        throw std::runtime_error("Unknown field: " + name);
    }
    Value with_field(const std::string& name, const Value& val) const {
        auto fields = as_fields();
        for (auto& [k, v] : fields) if (k == name) { v = val; break; }
        return make_product(type_name, fields);
    }

    // Message helpers
    const std::string& msg_tag() const { return field("tag").as_string(); }
    const Value& msg_data() const { return field("data"); }

    // Equality
    bool operator==(const Value& other) const {
        return type == other.type && data == other.data;
    }
    bool operator!=(const Value& other) const { return !(*this == other); }
};

// Stack operations
inline Value pop(Stack& s) {
    auto v = std::move(s.back()); s.pop_back(); return v;
}
inline void push(Stack& s, Value v) { s.push_back(std::move(v)); }
inline const Value& peek(const Stack& s) { return s.back(); }

// ===================================================================
// Mailbox -- MPSC queue with selective receive
// ===================================================================

class Mailbox {
    std::deque<Value> queue_;
    mutable std::mutex mtx_;

public:
    void send(Value msg) {
        std::lock_guard<std::mutex> lock(mtx_);
        queue_.push_back(std::move(msg));
    }

    // Non-blocking: try to get any message
    std::optional<Value> try_receive() {
        std::lock_guard<std::mutex> lock(mtx_);
        if (queue_.empty()) return std::nullopt;
        auto msg = std::move(queue_.front());
        queue_.pop_front();
        return msg;
    }

    // Non-blocking: selective receive by message tag
    std::optional<Value> try_receive_match(const std::string& tag) {
        std::lock_guard<std::mutex> lock(mtx_);
        for (auto it = queue_.begin(); it != queue_.end(); ++it) {
            if (it->type == Type::Message && it->msg_tag() == tag) {
                auto msg = std::move(*it);
                queue_.erase(it);
                return msg;
            }
            // Also match cast/call protocol messages by op name
            if (it->type == Type::Product && it->type_name == "CastMsg" &&
                it->field("op").as_string() == tag) {
                auto msg = std::move(*it);
                queue_.erase(it);
                return msg;
            }
            if (it->type == Type::Product && it->type_name == "CallMsg" &&
                it->field("op").as_string() == tag) {
                auto msg = std::move(*it);
                queue_.erase(it);
                return msg;
            }
        }
        return std::nullopt;
    }

    bool empty() const {
        std::lock_guard<std::mutex> lock(mtx_);
        return queue_.empty();
    }

    size_t size() const {
        std::lock_guard<std::mutex> lock(mtx_);
        return queue_.size();
    }

    // Peek: check if a matching message exists without consuming it
    bool has_match(const std::string& tag) const {
        std::lock_guard<std::mutex> lock(mtx_);
        for (const auto& msg : queue_) {
            if (msg.type == Type::Message && msg.msg_tag() == tag) return true;
            if (msg.type == Type::Product && msg.type_name == "CastMsg" &&
                msg.field("op").as_string() == tag) return true;
            if (msg.type == Type::Product && msg.type_name == "CallMsg" &&
                msg.field("op").as_string() == tag) return true;
        }
        return false;
    }
};

// ===================================================================
// Actor coroutine task
// ===================================================================

struct Task {
    struct promise_type {
        std::coroutine_handle<> continuation = std::noop_coroutine();
        bool done = false;

        Task get_return_object() {
            return Task{std::coroutine_handle<promise_type>::from_promise(*this)};
        }
        std::suspend_always initial_suspend() noexcept { return {}; }
        std::suspend_always final_suspend() noexcept {
            done = true;
            return {};
        }
        void return_void() {}
        void unhandled_exception() {
            // Store for supervision
            exception = std::current_exception();
        }

        std::exception_ptr exception = nullptr;
    };

    std::coroutine_handle<promise_type> handle;

    Task(std::coroutine_handle<promise_type> h) : handle(h) {}
    Task(Task&& other) noexcept : handle(other.handle) { other.handle = nullptr; }
    Task& operator=(Task&& other) noexcept {
        if (handle) handle.destroy();
        handle = other.handle;
        other.handle = nullptr;
        return *this;
    }
    ~Task() { if (handle) handle.destroy(); }

    Task(const Task&) = delete;
    Task& operator=(const Task&) = delete;

    bool done() const { return !handle || handle.done(); }
    void resume() { if (handle && !handle.done()) handle.resume(); }

    std::exception_ptr exception() const {
        return handle ? handle.promise().exception : nullptr;
    }
};

// ===================================================================
// Actor
// ===================================================================

enum class ActorState { Running, Suspended, Dead };

// Exit reason
struct ExitReason {
    enum Kind { Normal, Error, Killed };
    Kind kind;
    std::string message;
};

class Actor : public std::enable_shared_from_this<Actor> {
public:
    Mailbox mailbox;
    Stack stack;
    Task task;
    ActorState state = ActorState::Suspended;
    std::string type_name;
    int reductions = 0;
    static constexpr int MAX_REDUCTIONS = 100;

    // Linking and monitoring
    std::unordered_set<Actor*> links;
    std::unordered_set<Actor*> monitors;  // actors monitoring this one
    std::mutex link_mtx;

    // Reply channel for synchronous calls
    struct ReplyChannel {
        ActorWeak caller;
        bool pending = false;
    };
    std::optional<ReplyChannel> reply_channel;

    // What this actor is waiting for (empty = any message, set = specific tag)
    std::optional<std::string> waiting_for_tag;

    // Context stored here so it lives as long as the actor
    std::unique_ptr<ActorContext> context;

    // Body function stored here so captured values live as long as coroutine
    std::function<Task(ActorContext&)> body_fn;

    Actor(Task t, std::string tn)
        : task(std::move(t)), type_name(std::move(tn)) {}

    bool is_alive() const { return state != ActorState::Dead; }

    void send(Value msg) {
        mailbox.send(std::move(msg));
        if (state == ActorState::Suspended) {
            state = ActorState::Running;
        }
    }

    // Set up reply channel for a call
    void set_reply_channel(ActorRef caller) {
        reply_channel = ReplyChannel{caller, true};
    }

    // Send reply back through the channel
    void reply(Value val) {
        if (reply_channel && reply_channel->pending) {
            if (auto caller = reply_channel->caller.lock()) {
                // Send reply as a tagged message
                caller->send(Value::make_message("__reply__", std::move(val)));
            }
            reply_channel->pending = false;
        }
    }
};

// ===================================================================
// Awaitable: receive / receive_match / receive_timeout
// ===================================================================

// co_await receive() -- suspend until any message arrives
struct ReceiveAwaitable {
    Actor& actor;

    bool await_ready() { return !actor.mailbox.empty(); }
    void await_suspend(std::coroutine_handle<>) {
        actor.waiting_for_tag = std::nullopt;  // waiting for any
        actor.state = ActorState::Suspended;
    }
    Value await_resume() {
        actor.waiting_for_tag = std::nullopt;
        auto msg = actor.mailbox.try_receive();
        assert(msg.has_value());
        return std::move(*msg);
    }
};

// co_await receive_match(tag) -- selective receive by tag
struct ReceiveMatchAwaitable {
    Actor& actor;
    std::string tag;
    std::optional<Value> cached_;

    bool await_ready() {
        cached_ = actor.mailbox.try_receive_match(tag);
        return cached_.has_value();
    }
    void await_suspend(std::coroutine_handle<>) {
        actor.waiting_for_tag = tag;  // tell scheduler what we need
        actor.state = ActorState::Suspended;
    }
    Value await_resume() {
        actor.waiting_for_tag = std::nullopt;
        if (cached_) return std::move(*cached_);
        auto msg = actor.mailbox.try_receive_match(tag);
        assert(msg.has_value());
        return std::move(*msg);
    }
};

// Receive with timeout -- returns {value, true} or {nil, false}
struct ReceiveTimeoutAwaitable {
    Actor& actor;
    std::chrono::milliseconds timeout;
    std::chrono::steady_clock::time_point deadline;

    ReceiveTimeoutAwaitable(Actor& a, int64_t ms)
        : actor(a), timeout(ms),
          deadline(std::chrono::steady_clock::now() + std::chrono::milliseconds(ms)) {}

    bool await_ready() { return !actor.mailbox.empty(); }
    void await_suspend(std::coroutine_handle<>) {
        actor.state = ActorState::Suspended;
    }
    std::pair<std::optional<Value>, bool> await_resume() {
        auto msg = actor.mailbox.try_receive();
        if (msg) return {std::move(*msg), true};
        if (std::chrono::steady_clock::now() >= deadline) {
            return {std::nullopt, false};  // timed out
        }
        // Not ready yet, will be re-suspended by scheduler
        return {std::nullopt, false};
    }
};

// Selective receive with timeout
struct ReceiveMatchTimeoutAwaitable {
    Actor& actor;
    std::string tag;
    std::chrono::steady_clock::time_point deadline;
    std::optional<Value> cached_;

    ReceiveMatchTimeoutAwaitable(Actor& a, const std::string& t, int64_t ms)
        : actor(a), tag(t),
          deadline(std::chrono::steady_clock::now() + std::chrono::milliseconds(ms)) {}

    bool await_ready() {
        cached_ = actor.mailbox.try_receive_match(tag);
        return cached_.has_value();
    }
    void await_suspend(std::coroutine_handle<>) {
        actor.state = ActorState::Suspended;
    }
    std::pair<std::optional<Value>, bool> await_resume() {
        if (cached_) return {std::move(*cached_), true};
        auto msg = actor.mailbox.try_receive_match(tag);
        if (msg) return {std::move(*msg), true};
        if (std::chrono::steady_clock::now() >= deadline) {
            return {std::nullopt, false};
        }
        return {std::nullopt, false};
    }
};

// Yield point for reduction counting
struct YieldAwaitable {
    Actor& actor;

    bool await_ready() {
        return actor.reductions < Actor::MAX_REDUCTIONS;
    }
    void await_suspend(std::coroutine_handle<>) {
        actor.reductions = 0;
        actor.state = ActorState::Suspended;
    }
    void await_resume() {
        ++actor.reductions;
    }
};

// ===================================================================
// ActorContext -- passed to actor coroutines for runtime access
// ===================================================================

class ActorContext {
    Actor& self_;
    Scheduler& scheduler_;

public:
    ActorContext(Actor& self, Scheduler& sched)
        : self_(self), scheduler_(sched) {}

    // Access own stack
    Stack& stack() { return self_.stack; }

    // Get self reference
    ActorRef self_ref();

    // Spawn a new actor
    ActorRef spawn(std::function<Task(ActorContext&)> body,
                   const std::string& type_name = "");

    // Send (fire and forget)
    void send(ActorRef target, Value msg) {
        target->send(std::move(msg));
    }

    // Cast: async message to server actor
    void cast(ActorRef target, const std::string& op, std::vector<Value> args) {
        ProductFields fields = {
            {"op", Value::make_string(op)},
            {"args", Value::make_list(std::move(args))}
        };
        target->send(Value::make_product("CastMsg", std::move(fields)));
    }

    // Call: sync message, blocks until reply
    void call(ActorRef target, const std::string& op, std::vector<Value> args) {
        target->set_reply_channel(self_ref());
        ProductFields fields = {
            {"op", Value::make_string(op)},
            {"args", Value::make_list(std::move(args))},
            {"caller", Value::make_actor(self_ref())}
        };
        target->send(Value::make_product("CallMsg", std::move(fields)));
    }

    // Reply to a call
    void reply(Value val) {
        self_.reply(std::move(val));
    }

    // Receive any message
    ReceiveAwaitable receive() {
        return ReceiveAwaitable{self_};
    }

    // Selective receive by tag
    ReceiveMatchAwaitable receive_match(const std::string& tag) {
        return ReceiveMatchAwaitable{self_, tag};
    }

    // Receive with timeout (ms)
    ReceiveTimeoutAwaitable receive_timeout(int64_t ms) {
        return ReceiveTimeoutAwaitable{self_, ms};
    }

    // Selective receive with timeout
    ReceiveMatchTimeoutAwaitable receive_match_timeout(const std::string& tag, int64_t ms) {
        return ReceiveMatchTimeoutAwaitable{self_, tag, ms};
    }

    // Yield point (for reduction-based cooperative preemption)
    YieldAwaitable yield() {
        return YieldAwaitable{self_};
    }

    // Link to another actor (bidirectional crash propagation)
    void link(ActorRef other);

    // Monitor another actor (unidirectional death notification)
    void monitor(ActorRef target);

    // Stop this actor
    void stop() {
        self_.state = ActorState::Dead;
    }
};

// ===================================================================
// Scheduler -- round-robin cooperative scheduler
// ===================================================================

class Scheduler {
    std::vector<ActorRef> actors_;
    std::mutex mtx_;
    bool running_ = false;

public:
    ActorRef spawn(std::function<Task(ActorContext&)> body,
                   const std::string& type_name = "") {
        // Create actor with a placeholder task
        auto actor = std::make_shared<Actor>(Task{nullptr}, type_name);

        // Store body and context in actor (must outlive coroutine frame)
        actor->body_fn = std::move(body);
        actor->context = std::make_unique<ActorContext>(*actor, *this);
        actor->task = actor->body_fn(*actor->context);

        actor->state = ActorState::Running;

        std::lock_guard<std::mutex> lock(mtx_);
        actors_.push_back(actor);
        return actor;
    }

    // Run all actors until all are dead or suspended with no messages
    void run() {
        running_ = true;
        while (running_) {
            bool any_progress = false;

            std::vector<ActorRef> snapshot;
            {
                std::lock_guard<std::mutex> lock(mtx_);
                snapshot = actors_;
            }

            for (auto& actor : snapshot) {
                if (actor->state == ActorState::Dead) continue;
                if (actor->state == ActorState::Suspended) continue;

                // Actor is Running -- but verify it can actually proceed
                // If waiting for a specific tag, only resume if matching msg exists
                if (actor->waiting_for_tag.has_value()) {
                    if (!actor->mailbox.has_match(*actor->waiting_for_tag)) {
                        actor->state = ActorState::Suspended;
                        continue;
                    }
                }

                actor->reductions = 0;
                actor->task.resume();
                any_progress = true;

                // Check for unhandled exception
                if (auto ex = actor->task.exception()) {
                    actor->state = ActorState::Dead;
                    handle_actor_exit(*actor, ExitReason{ExitReason::Error, "exception"});
                } else if (actor->task.done()) {
                    actor->state = ActorState::Dead;
                    handle_actor_exit(*actor, ExitReason{ExitReason::Normal, ""});
                }
            }

            // Garbage collect dead actors
            {
                std::lock_guard<std::mutex> lock(mtx_);
                actors_.erase(
                    std::remove_if(actors_.begin(), actors_.end(),
                        [](const ActorRef& a) { return a->state == ActorState::Dead; }),
                    actors_.end());

                if (actors_.empty()) break;
            }

            if (!any_progress) {
                // All actors suspended with no messages -- check for timeouts
                // or just yield the CPU briefly
                std::this_thread::sleep_for(std::chrono::microseconds(100));
            }
        }
    }

    // Run for a limited number of scheduler ticks (for testing)
    void run_ticks(int max_ticks) {
        for (int tick = 0; tick < max_ticks; ++tick) {
            bool any_alive = false;
            std::vector<ActorRef> snapshot;
            {
                std::lock_guard<std::mutex> lock(mtx_);
                snapshot = actors_;
            }

            for (auto& actor : snapshot) {
                if (actor->state == ActorState::Dead) continue;
                any_alive = true;
                if (actor->state == ActorState::Suspended) continue;

                // If waiting for a specific tag, only resume if match exists
                if (actor->waiting_for_tag.has_value()) {
                    if (!actor->mailbox.has_match(*actor->waiting_for_tag)) {
                        actor->state = ActorState::Suspended;
                        continue;
                    }
                }

                if (actor->state == ActorState::Running) {
                    actor->reductions = 0;
                    actor->task.resume();

                    if (auto ex = actor->task.exception()) {
                        actor->state = ActorState::Dead;
                        handle_actor_exit(*actor, ExitReason{ExitReason::Error, "exception"});
                    } else if (actor->task.done()) {
                        actor->state = ActorState::Dead;
                        handle_actor_exit(*actor, ExitReason{ExitReason::Normal, ""});
                    }
                }
            }

            // GC dead actors
            {
                std::lock_guard<std::mutex> lock(mtx_);
                actors_.erase(
                    std::remove_if(actors_.begin(), actors_.end(),
                        [](const ActorRef& a) { return a->state == ActorState::Dead; }),
                    actors_.end());
                if (actors_.empty()) break;
            }

            if (!any_alive) break;
        }
    }

    void stop() { running_ = false; }

    size_t actor_count() const {
        std::lock_guard<std::mutex> lock(const_cast<std::mutex&>(mtx_));
        return actors_.size();
    }

private:
    void handle_actor_exit(Actor& actor, ExitReason reason) {
        std::lock_guard<std::mutex> lock(actor.link_mtx);

        // Notify monitors
        for (auto* monitor : actor.monitors) {
            if (monitor->is_alive()) {
                monitor->send(Value::make_message("__down__",
                    Value::make_string(reason.message)));
            }
        }

        // Propagate to linked actors
        if (reason.kind != ExitReason::Normal) {
            for (auto* linked : actor.links) {
                if (linked->is_alive()) {
                    linked->state = ActorState::Dead;
                }
            }
        }
    }
};

// ===================================================================
// ActorContext method implementations (need Scheduler definition)
// ===================================================================

inline ActorRef ActorContext::self_ref() {
    return self_.shared_from_this();
}

inline ActorRef ActorContext::spawn(std::function<Task(ActorContext&)> body,
                                    const std::string& type_name) {
    return scheduler_.spawn(std::move(body), type_name);
}

inline void ActorContext::link(ActorRef other) {
    {
        std::lock_guard<std::mutex> lock(self_.link_mtx);
        self_.links.insert(other.get());
    }
    {
        std::lock_guard<std::mutex> lock(other->link_mtx);
        other->links.insert(&self_);
    }
}

inline void ActorContext::monitor(ActorRef target) {
    std::lock_guard<std::mutex> lock(target->link_mtx);
    target->monitors.insert(&self_);
}

} // namespace af
