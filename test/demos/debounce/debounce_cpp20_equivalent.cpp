/// debounce_cpp20_equivalent.cpp — C++20 equivalent of debounce_demo.a4
///
/// For side-by-side comparison with the ActorForth demo. Implements the same
/// feature set: product types, EventStore actor, Logger actor, and tests.
///
/// APPROACH
/// ========
/// C++20 has no built-in actor model. We build actors from scratch using
/// std::jthread + std::mutex + std::condition_variable + a message queue.
/// This is the standard approach — there is no widely-adopted actor library
/// in the C++ standard (CAF exists but is not standard).
///
/// WHAT THIS FILE DEMONSTRATES
/// ============================
///   debounce_demo.a4:                ~80 lines of definitions + tests
///   debounce_cpp20_equivalent.cpp:   ~330 lines for the same functionality
///
/// The difference comes from:
///   1. No native actor primitive — must build Actor<T> template with message
///      loop, variant dispatch, mutex, condition variable, and stop protocol.
///   2. Message types must be manually defined as std::variant — adding a new
///      message type requires updating the variant, the visitor, and the
///      handler. Three places to keep in sync.
///   3. std::visit with overloaded lambdas for dispatch — powerful but verbose.
///   4. Explicit memory management considerations (shared_ptr for replies).
///   5. Product types are trivial (structs), but composing them with actors
///      requires template machinery that doesn't exist in simpler languages.
///
/// COMPILE & RUN
/// =============
///   g++ -std=c++20 -pthread -o debounce_demo debounce_cpp20_equivalent.cpp
///   ./debounce_demo
///
/// Or with clang:
///   clang++ -std=c++20 -pthread -o debounce_demo debounce_cpp20_equivalent.cpp

#include <algorithm>
#include <cassert>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <future>
#include <iostream>
#include <mutex>
#include <numeric>
#include <optional>
#include <queue>
#include <string>
#include <thread>
#include <variant>
#include <vector>

using namespace std::chrono_literals;


// ==============================================================
// Product types — equivalent to ActorForth type declarations
// ==============================================================
// ActorForth:  type Entry path String hash String size Int kind String .
// C++: plain structs. Slightly more verbose but straightforward.

struct Entry {
    std::string path;
    std::string hash;
    int size;
    std::string kind;
};

struct Stats {
    int size_diff;
    int count_diff;
};

struct Batch {
    int added;
    int deleted;
    int size_delta;
    int count_delta;
};

// State types for actors
struct Tally {
    int batches = 0;
};

struct Store {
    int events = 0;
};


// ==============================================================
// Actor infrastructure — no equivalent needed in ActorForth
// ==============================================================
// ActorForth: "0 store server" (3 words, zero infrastructure)
// C++20: ~120 lines of template machinery that every actor depends on.
//
// This is the minimum viable actor: a thread, a message queue with
// mutex/condvar, typed message dispatch via std::variant, and a
// synchronous call mechanism via std::promise/future.

// Overloaded lambda helper for std::visit
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

// Message wrapper — separates cast (fire-and-forget) from call (sync reply)
template<typename Msg>
struct CastMessage {
    Msg payload;
};

template<typename Msg, typename Reply>
struct CallMessage {
    Msg payload;
    std::promise<Reply> reply;
};

// Stop sentinel
struct StopMessage {};


// --- Store actor messages ---
// Each operation needs its own message struct + variant entry.
// In ActorForth, the word signature IS the message type — no duplication.

struct RecordMsg { int n; };
struct TotalMsg {};

using StoreMessage = std::variant<
    CastMessage<RecordMsg>,
    CallMessage<TotalMsg, int>,
    StopMessage
>;

// --- Tally actor messages ---

struct LogBatchMsg { Batch batch; };
struct LoggedMsg {};

using TallyMessage = std::variant<
    CastMessage<LogBatchMsg>,
    CallMessage<LoggedMsg, int>,
    StopMessage
>;


// Generic actor template — thread + message queue + dispatch
template<typename State, typename Message>
class Actor {
public:
    explicit Actor(State initial_state)
        : state_(std::move(initial_state)) {}

    virtual ~Actor() { stop(); }

    void start() {
        running_ = true;
        thread_ = std::jthread([this](std::stop_token) { loop(); });
    }

    void stop() {
        if (!running_) return;
        {
            std::lock_guard lock(mutex_);
            queue_.push(StopMessage{});
        }
        cv_.notify_one();
        if (thread_.joinable()) thread_.join();
        running_ = false;
    }

    // Cast: async, no reply — no type checking on whether this actor
    // actually handles this message variant. A wrong variant alternative
    // compiles fine but hits the default visitor branch at runtime.
    template<typename Msg>
    void cast(Msg msg) {
        {
            std::lock_guard lock(mutex_);
            queue_.push(CastMessage<Msg>{std::move(msg)});
        }
        cv_.notify_one();
    }

    // Call: sync, returns reply via promise/future.
    template<typename Msg, typename Reply>
    Reply call(Msg msg) {
        std::promise<Reply> promise;
        auto future = promise.get_future();
        {
            std::lock_guard lock(mutex_);
            queue_.push(CallMessage<Msg, Reply>{std::move(msg), std::move(promise)});
        }
        cv_.notify_one();
        return future.get();
    }

protected:
    State state_;

    // Subclasses implement this to handle messages
    virtual void dispatch(Message& msg) = 0;

private:
    void loop() {
        while (running_) {
            Message msg;
            {
                std::unique_lock lock(mutex_);
                cv_.wait(lock, [this] { return !queue_.empty(); });
                msg = std::move(queue_.front());
                queue_.pop();
            }

            if (std::holds_alternative<StopMessage>(msg)) {
                running_ = false;
                break;
            }

            dispatch(msg);
        }
    }

    std::queue<Message> queue_;
    std::mutex mutex_;
    std::condition_variable cv_;
    std::jthread thread_;
    bool running_ = false;
};


// ==============================================================
// EventStore actor
// ==============================================================
// ActorForth equivalent:
//   : record Store Int -> Store ; swap events rot + events! .
//   : total Store -> Store Int ; dup events .
//   0 store server

class StoreActor : public Actor<Store, StoreMessage> {
public:
    StoreActor() : Actor(Store{0}) {}

protected:
    void dispatch(StoreMessage& msg) override {
        std::visit(overloaded{
            [this](CastMessage<RecordMsg>& m) {
                state_.events += m.payload.n;
            },
            [this](CallMessage<TotalMsg, int>& m) {
                m.reply.set_value(state_.events);
            },
            [](StopMessage&) { /* handled in base */ },
        }, msg);
    }
};


// ==============================================================
// Logger actor
// ==============================================================
// ActorForth equivalent:
//   : log_batch Tally Batch -> Tally ; drop batches 1 + batches! .
//   : logged Tally -> Tally Int ; dup batches .
//   0 tally server

class TallyActor : public Actor<Tally, TallyMessage> {
public:
    TallyActor() : Actor(Tally{0}) {}

protected:
    void dispatch(TallyMessage& msg) override {
        std::visit(overloaded{
            [this](CastMessage<LogBatchMsg>& m) {
                // m.payload.batch is typed — at least the compiler enforces
                // the Batch struct here. But nothing prevents constructing
                // a Batch with garbage values. And the variant dispatch
                // means a new message type requires updating three places:
                // the struct, the variant typedef, and this visitor.
                state_.batches += 1;
            },
            [this](CallMessage<LoggedMsg, int>& m) {
                m.reply.set_value(state_.batches);
            },
            [](StopMessage&) { /* handled in base */ },
        }, msg);
    }
};


// ==============================================================
// Tests
// ==============================================================

void test_product_types() {
    // --- Batch ---
    // ActorForth: 5 3 200 2 batch
    //             added 5 assert-eq
    Batch b{5, 3, 200, 2};
    assert(b.added == 5);
    assert(b.deleted == 3);
    assert(b.size_delta == 200);
    assert(b.count_delta == 2);

    // --- Stats ---
    Stats s{1024, 1};
    assert(s.size_diff == 1024);
    assert(s.count_diff == 1);

    // --- Entry ---
    Entry e{"/docs/report.pdf", "abc123", 4096, "application/pdf"};
    assert(e.path == "/docs/report.pdf");
    assert(e.hash == "abc123");
    assert(e.size == 4096);
    assert(e.kind == "application/pdf");
}

void test_actors() {
    StoreActor store;
    TallyActor tally;
    store.start();
    tally.start();

    // --- Logger tests ---
    // ActorForth: << 5 3 200 2 batch log_batch >>
    tally.cast(LogBatchMsg{Batch{5, 3, 200, 2}});

    // ActorForth: << 10 1 500 9 batch log_batch >>
    tally.cast(LogBatchMsg{Batch{10, 1, 500, 9}});

    // Must sleep because cast is async — no ordering guarantee between
    // the cast processing and the subsequent call without synchronization.
    // ActorForth's server protocol handles this transparently.
    std::this_thread::sleep_for(50ms);

    // ActorForth: << logged >> 2 assert-eq
    int logged = tally.call<LoggedMsg, int>(LoggedMsg{});
    assert(logged == 2);

    // --- EventStore tests ---
    // ActorForth: << 5 record >> << 3 record >>
    store.cast(RecordMsg{5});
    store.cast(RecordMsg{3});

    std::this_thread::sleep_for(50ms);

    // ActorForth: << total >> 8 assert-eq
    int total = store.call<TotalMsg, int>(TotalMsg{});
    assert(total == 8);

    // Cleanup — ActorForth: << stop >> drop
    store.stop();
    tally.stop();
}

void run_once() {
    test_product_types();
    test_actors();
}

void bench(int n) {
    // Warm-up
    run_once();

    std::vector<long long> times;
    times.reserve(n);

    for (int i = 0; i < n; ++i) {
        auto start = std::chrono::steady_clock::now();
        run_once();
        auto end = std::chrono::steady_clock::now();
        auto us = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
        times.push_back(us);
    }

    auto min_t = *std::min_element(times.begin(), times.end());
    auto max_t = *std::max_element(times.begin(), times.end());
    long long sum = 0;
    for (auto t : times) sum += t;
    auto avg_t = sum / n;

    std::cout << "BENCH_RESULT: min=" << min_t << " avg=" << avg_t
              << " max=" << max_t << " iters=" << n << std::endl;
}

int main(int argc, char* argv[]) {
    test_product_types();
    test_actors();
    std::cout << "All C++20 equivalent tests passed." << std::endl;

    if (argc >= 3 && std::string(argv[1]) == "--bench") {
        bench(std::stoi(argv[2]));
    }
    return 0;
}
