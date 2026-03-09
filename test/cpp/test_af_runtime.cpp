// test_af_runtime.cpp -- Tests for the ActorForth C++ coroutine runtime
//
// Tests actor spawn, messaging, cast/call protocol, selective receive,
// timeouts, linking, and supervision.
//
// Compile and run:
//   g++ -std=c++20 -O2 -I include/cpp -o test_af_runtime test/cpp/test_af_runtime.cpp -lpthread
//   ./test_af_runtime

#include "af_runtime.hpp"
#include <cassert>
#include <cstdio>

using namespace af;

// ===================================================================
// Test 1: Basic actor spawn and message passing
// ===================================================================

Task echo_actor(ActorContext& ctx) {
    // Wait for a message and push it onto our stack
    auto msg = co_await ctx.receive();
    push(ctx.stack(), std::move(msg));
}

void test_basic_spawn() {
    printf("  test_basic_spawn...");
    Scheduler sched;
    auto actor = sched.spawn(echo_actor, "Echo");
    actor->send(Value::make_int(42));
    sched.run_ticks(10);
    assert(actor->stack.size() == 1);
    assert(actor->stack[0].as_int() == 42);
    printf(" ok\n");
}

// ===================================================================
// Test 2: Counter actor with cast/call protocol
// ===================================================================

// Simulates A4:
//   type Counter value Int .
//   : increment Counter -> Counter ; value 1 + value! .
//   : count Counter -> Counter Int ; value .

Task counter_actor(ActorContext& ctx) {
    // State: counter value on the stack
    // Stack starts with initial value pushed by spawner
    int64_t count = 0;

    while (true) {
        auto msg = co_await ctx.receive();

        if (msg.type == Type::Product && msg.type_name == "CastMsg") {
            auto op = msg.field("op").as_string();
            if (op == "increment") {
                count += 1;
            } else if (op == "decrement") {
                count -= 1;
            } else if (op == "stop") {
                ctx.stop();
                co_return;
            }
        } else if (msg.type == Type::Product && msg.type_name == "CallMsg") {
            auto op = msg.field("op").as_string();
            if (op == "count") {
                // Reply with current count
                auto caller = msg.field("caller").as_actor();
                caller->send(Value::make_message("__reply__", Value::make_int(count)));
            }
        }
    }
}

void test_counter_actor() {
    printf("  test_counter_actor...");
    Scheduler sched;

    // Track replies
    std::optional<Value> reply_value;

    auto counter = sched.spawn(counter_actor, "Counter");

    // Create a "main" actor that drives the test
    auto main_actor = sched.spawn([counter](ActorContext& ctx) -> Task {
        // Increment 3 times (cast)
        ctx.cast(counter, "increment", {});
        ctx.cast(counter, "increment", {});
        ctx.cast(counter, "increment", {});

        // Decrement once
        ctx.cast(counter, "decrement", {});

        // Call to get count
        ctx.call(counter, "count", {});
        auto reply = co_await ctx.receive_match("__reply__");
        push(ctx.stack(), reply.msg_data());

        // Stop the counter
        ctx.cast(counter, "stop", {});
    }, "Main");

    sched.run();

    assert(main_actor->stack.size() == 1);
    assert(main_actor->stack[0].as_int() == 2);
    printf(" ok\n");
}

// ===================================================================
// Test 3: Selective receive (receive_match)
// ===================================================================

Task selective_actor(ActorContext& ctx) {
    // Receive "b" first, even though "a" arrives first
    auto msg_b = co_await ctx.receive_match("b");
    push(ctx.stack(), msg_b.msg_data());

    // Now receive "a"
    auto msg_a = co_await ctx.receive_match("a");
    push(ctx.stack(), msg_a.msg_data());
}

void test_selective_receive() {
    printf("  test_selective_receive...");
    Scheduler sched;

    auto actor = sched.spawn(selective_actor, "Selective");

    // Send "a" first, then "b"
    actor->send(Value::make_message("a", Value::make_int(1)));
    actor->send(Value::make_message("b", Value::make_int(2)));

    sched.run_ticks(20);

    // Stack should have b's data (2) at bottom, a's data (1) on top
    assert(actor->stack.size() == 2);
    assert(actor->stack[0].as_int() == 2);  // b arrived first in processing
    assert(actor->stack[1].as_int() == 1);  // a arrived second
    printf(" ok\n");
}

// ===================================================================
// Test 4: Multiple actors communicating
// ===================================================================

Task ping_actor(ActorContext& ctx) {
    auto partner = ctx.stack()[0].as_actor();
    ctx.send(partner, Value::make_message("ping", Value::make_int(0)));

    for (int i = 0; i < 3; ++i) {
        auto msg = co_await ctx.receive_match("pong");
        int64_t n = msg.msg_data().as_int();
        ctx.send(partner, Value::make_message("ping", Value::make_int(n + 1)));
    }

    // Final receive
    auto final_msg = co_await ctx.receive_match("pong");
    push(ctx.stack(), final_msg.msg_data());
}

Task pong_actor(ActorContext& ctx) {
    for (int i = 0; i < 4; ++i) {
        auto msg = co_await ctx.receive_match("ping");
        int64_t n = msg.msg_data().as_int();
        // Get sender from context -- we stored it on our stack
        auto partner = ctx.stack()[0].as_actor();
        ctx.send(partner, Value::make_message("pong", Value::make_int(n + 1)));
    }
}

void test_ping_pong() {
    printf("  test_ping_pong...");
    Scheduler sched;

    auto ponger = sched.spawn(pong_actor, "Pong");

    auto pinger = sched.spawn([ponger](ActorContext& ctx) -> Task {
        push(ctx.stack(), Value::make_actor(ponger));
        // Store partner ref on ponger's stack too
        push(ponger->stack, Value::make_actor(ctx.self_ref()));

        ctx.send(ponger, Value::make_message("ping", Value::make_int(0)));

        for (int i = 0; i < 3; ++i) {
            auto msg = co_await ctx.receive_match("pong");
            int64_t n = msg.msg_data().as_int();
            ctx.send(ponger, Value::make_message("ping", Value::make_int(n + 1)));
        }

        auto final_msg = co_await ctx.receive_match("pong");
        push(ctx.stack(), final_msg.msg_data());
    }, "Ping");

    sched.run();

    // After 4 round trips: 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7
    assert(pinger->stack.back().as_int() == 7);
    printf(" ok\n");
}

// ===================================================================
// Test 5: Actor with product type state (like A4 counter)
// ===================================================================

// Simulates:
//   type Counter value Int .
//   0 counter server
//   << increment >> << increment >> << count >> -> 2

Task product_counter_actor(ActorContext& ctx) {
    // State is a product type on the stack
    // Start with: Counter{value: 0}
    ProductFields fields = {{"value", Value::make_int(0)}};
    push(ctx.stack(), Value::make_product("Counter", fields));

    while (true) {
        auto msg = co_await ctx.receive();

        if (msg.type == Type::Product && msg.type_name == "CastMsg") {
            auto op = msg.field("op").as_string();
            if (op == "increment") {
                // value 1 + value!
                auto& instance = ctx.stack().back();
                int64_t val = instance.field("value").as_int();
                ctx.stack().back() = instance.with_field("value", Value::make_int(val + 1));
            } else if (op == "stop") {
                co_return;
            }
        } else if (msg.type == Type::Product && msg.type_name == "CallMsg") {
            auto op = msg.field("op").as_string();
            if (op == "count") {
                auto& instance = ctx.stack().back();
                int64_t val = instance.field("value").as_int();
                auto caller = msg.field("caller").as_actor();
                caller->send(Value::make_message("__reply__", Value::make_int(val)));
            }
        }
    }
}

void test_product_state() {
    printf("  test_product_state...");
    Scheduler sched;

    auto counter = sched.spawn(product_counter_actor, "Counter");

    auto main = sched.spawn([counter](ActorContext& ctx) -> Task {
        ctx.cast(counter, "increment", {});
        ctx.cast(counter, "increment", {});
        ctx.cast(counter, "increment", {});

        ctx.call(counter, "count", {});
        auto reply = co_await ctx.receive_match("__reply__");
        push(ctx.stack(), reply.msg_data());

        ctx.cast(counter, "stop", {});
    }, "Main");

    sched.run();

    assert(main->stack.size() == 1);
    assert(main->stack[0].as_int() == 3);
    printf(" ok\n");
}

// ===================================================================
// Test 6: Many actors (lightweight test)
// ===================================================================

void test_many_actors() {
    printf("  test_many_actors (1000 actors)...");
    Scheduler sched;
    constexpr int N = 1000;

    auto collector = sched.spawn([](ActorContext& ctx) -> Task {
        int64_t sum = 0;
        for (int i = 0; i < N; ++i) {
            auto msg = co_await ctx.receive();
            sum += msg.as_int();
        }
        push(ctx.stack(), Value::make_int(sum));
    }, "Collector");

    // Spawn N actors, each sends its index to the collector
    for (int i = 0; i < N; ++i) {
        sched.spawn([i, collector](ActorContext& ctx) -> Task {
            ctx.send(collector, Value::make_int(i));
            co_return;
        }, "Worker");
    }

    sched.run();

    // Sum of 0..999 = 499500
    assert(collector->stack.size() == 1);
    assert(collector->stack[0].as_int() == 499500);
    printf(" ok\n");
}

// ===================================================================
// Test 7: Actor linking (crash propagation)
// ===================================================================

void test_actor_link() {
    printf("  test_actor_link...");
    Scheduler sched;

    auto victim = sched.spawn([](ActorContext& ctx) -> Task {
        co_await ctx.receive();
        throw std::runtime_error("crash!");
    }, "Victim");

    auto linked = sched.spawn([victim](ActorContext& ctx) -> Task {
        ctx.link(victim);
        // Trigger the crash after establishing the link
        ctx.send(victim, Value::make_int(0));
        // Wait forever -- should be killed when victim crashes
        while (true) {
            co_await ctx.receive();
        }
    }, "Linked");

    sched.run_ticks(20);

    assert(victim->state == ActorState::Dead);
    assert(linked->state == ActorState::Dead);
    printf(" ok\n");
}

// ===================================================================
// Test 8: Actor monitoring (death notification)
// ===================================================================

void test_actor_monitor() {
    printf("  test_actor_monitor...");
    Scheduler sched;

    auto worker = sched.spawn([](ActorContext& ctx) -> Task {
        co_await ctx.receive();  // wait for any message then exit
    }, "Worker");

    auto supervisor = sched.spawn([worker](ActorContext& ctx) -> Task {
        ctx.monitor(worker);
        // Send message to make worker exit normally
        ctx.send(worker, Value::make_int(0));
        // Wait for down notification
        auto down = co_await ctx.receive_match("__down__");
        push(ctx.stack(), Value::make_bool(true));
    }, "Supervisor");

    sched.run();

    assert(supervisor->stack.size() == 1);
    assert(supervisor->stack[0].as_bool() == true);
    printf(" ok\n");
}

// ===================================================================
// Main
// ===================================================================

int main() {
    printf("\n=== ActorForth C++ Runtime Tests ===\n\n");

    test_basic_spawn();
    test_counter_actor();
    test_selective_receive();
    test_ping_pong();
    test_product_state();
    test_many_actors();
    test_actor_link();
    test_actor_monitor();

    printf("\nAll tests passed.\n\n");
    return 0;
}
