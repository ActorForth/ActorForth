/// throughput_cpp20.cpp — Actor message dispatch throughput benchmark
///
/// C++20 equivalent of throughput_demo.a4. A Counter actor receives N bump
/// casts, then a sync get_count call verifies all arrived. No sleep delays —
/// the sync call guarantees FIFO ordering via the message queue.
///
/// COMPILE & RUN
/// =============
///   g++ -std=c++20 -O2 -pthread -o throughput throughput_cpp20.cpp
///   ./throughput
///   ./throughput --bench 100

#include <algorithm>
#include <cassert>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <future>
#include <iostream>
#include <mutex>
#include <numeric>
#include <queue>
#include <string>
#include <thread>
#include <variant>
#include <vector>

// Message types
struct BumpMsg {};
struct GetCountMsg {};
struct StopMsg {};

template<typename Msg>
struct CastMessage { Msg payload; };

template<typename Msg, typename Reply>
struct CallMessage {
    Msg payload;
    std::promise<Reply> reply;
};

using CounterMessage = std::variant<
    CastMessage<BumpMsg>,
    CallMessage<GetCountMsg, int>,
    StopMsg
>;

// Overloaded visitor helper
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

class CounterActor {
public:
    CounterActor() : count_(0), running_(false) {}
    ~CounterActor() { stop(); }

    void start() {
        running_ = true;
        thread_ = std::jthread([this](std::stop_token) { loop(); });
    }

    void stop() {
        if (!running_) return;
        {
            std::lock_guard lock(mutex_);
            queue_.push(StopMsg{});
        }
        cv_.notify_one();
        if (thread_.joinable()) thread_.join();
        running_ = false;
    }

    void cast_bump() {
        {
            std::lock_guard lock(mutex_);
            queue_.push(CastMessage<BumpMsg>{BumpMsg{}});
        }
        cv_.notify_one();
    }

    int call_get_count() {
        std::promise<int> promise;
        auto future = promise.get_future();
        {
            std::lock_guard lock(mutex_);
            queue_.push(CallMessage<GetCountMsg, int>{GetCountMsg{}, std::move(promise)});
        }
        cv_.notify_one();
        return future.get();
    }

private:
    void loop() {
        while (running_) {
            CounterMessage msg;
            {
                std::unique_lock lock(mutex_);
                cv_.wait(lock, [this] { return !queue_.empty(); });
                msg = std::move(queue_.front());
                queue_.pop();
            }
            std::visit(overloaded{
                [this](CastMessage<BumpMsg>&) {
                    count_++;
                },
                [this](CallMessage<GetCountMsg, int>& m) {
                    m.reply.set_value(count_);
                },
                [this](StopMsg&) {
                    running_ = false;
                },
            }, msg);
        }
    }

    int count_;
    bool running_;
    std::queue<CounterMessage> queue_;
    std::mutex mutex_;
    std::condition_variable cv_;
    std::jthread thread_;
};

void run_once(int n = 10000) {
    CounterActor actor;
    actor.start();
    for (int i = 0; i < n; ++i) {
        actor.cast_bump();
    }
    int result = actor.call_get_count();
    assert(result == n);
    actor.stop();
}

void bench(int iters, int n = 10000) {
    // Warm-up
    run_once(n);

    std::vector<long long> times;
    times.reserve(iters);

    for (int i = 0; i < iters; ++i) {
        auto start = std::chrono::steady_clock::now();
        run_once(n);
        auto end = std::chrono::steady_clock::now();
        auto us = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
        times.push_back(us);
    }

    auto min_t = *std::min_element(times.begin(), times.end());
    auto max_t = *std::max_element(times.begin(), times.end());
    long long sum = 0;
    for (auto t : times) sum += t;
    auto avg_t = sum / iters;

    std::cout << "BENCH_RESULT: min=" << min_t << "us avg=" << avg_t
              << "us max=" << max_t << "us (" << iters << " iterations, "
              << n << " msgs each)" << std::endl;
}

int main(int argc, char* argv[]) {
    run_once();
    std::cout << "C++20 throughput test passed (10000 messages)." << std::endl;

    if (argc >= 3 && std::string(argv[1]) == "--bench") {
        bench(std::stoi(argv[2]));
    }
    return 0;
}
