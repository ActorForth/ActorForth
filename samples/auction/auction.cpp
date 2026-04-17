// auction.cpp — C++20 reference implementation of the a4 auction demo.
//
// Idiomatic C++20: class with std::mutex for concurrent bid serialisation.
// Build and run with:
//
//   g++ -std=c++20 -O2 -pthread -o auction auction.cpp && ./auction

#include <cassert>
#include <iostream>
#include <mutex>
#include <string>

enum class Status { Open, Closed };

class Auction {
public:
    Auction(std::string item, int start_bid, std::string start_bidder)
        : item_(std::move(item)),
          current_bid_(start_bid),
          highest_bidder_(std::move(start_bidder)) {}

    bool bid(int amount, const std::string& bidder) {
        std::lock_guard lock(mu_);
        if (status_ == Status::Closed) return false;
        if (amount <= current_bid_) return false;
        current_bid_ = amount;
        highest_bidder_ = bidder;
        return true;
    }

    void close() {
        std::lock_guard lock(mu_);
        status_ = Status::Closed;
    }

    std::string leader() {
        std::lock_guard lock(mu_);
        return highest_bidder_;
    }

    int leading_bid() {
        std::lock_guard lock(mu_);
        return current_bid_;
    }

    bool is_open() {
        std::lock_guard lock(mu_);
        return status_ == Status::Open;
    }

private:
    std::string item_;
    int current_bid_;
    std::string highest_bidder_;
    Status status_ = Status::Open;
    std::mutex mu_;
};

int main() {
    Auction a("laptop", 100, "");

    assert(a.bid(150, "alice"));
    assert(a.leader() == "alice");
    assert(a.leading_bid() == 150);

    assert(!a.bid(120, "bob"));                    // too low
    assert(a.leader() == "alice");
    assert(a.leading_bid() == 150);

    assert(a.bid(200, "bob"));
    assert(a.leader() == "bob");
    assert(a.leading_bid() == 200);

    a.close();
    assert(!a.is_open());

    assert(!a.bid(500, "carol"));                  // closed
    assert(a.leader() == "bob");
    assert(a.leading_bid() == 200);

    std::cout << "auction.cpp: all assertions passed\n";
    return 0;
}
