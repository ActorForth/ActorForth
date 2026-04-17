// escrow.cpp — C++20 reference implementation of the a4 escrow contract.
//   g++ -std=c++20 -O2 -o escrow escrow.cpp && ./escrow

#include <cassert>
#include <iostream>
#include <string>

enum class Status {
    Pending,
    Funded,
    Verified,
    Disputed,
    Released,
    Refunded,
};

class Escrow {
public:
    Escrow(std::string buyer, std::string seller,
           std::string verifier, long amount)
        : buyer_(std::move(buyer)),
          seller_(std::move(seller)),
          verifier_(std::move(verifier)),
          amount_(amount) {}

    void deposit(long amount, const std::string& caller) {
        if (status_ != Status::Pending) return;
        if (amount != amount_ || caller != buyer_) return;
        status_ = Status::Funded;
    }

    void verify(const std::string& caller) {
        if (status_ != Status::Funded) return;
        if (caller != verifier_) return;
        status_ = Status::Verified;
    }

    void release() {
        if (status_ != Status::Verified) return;
        status_ = Status::Released;
    }

    void dispute(const std::string& caller) {
        if (status_ != Status::Funded) return;
        if (caller != buyer_ && caller != seller_) return;
        status_ = Status::Refunded;
    }

    Status state() const { return status_; }
    bool is_released() const { return status_ == Status::Released; }
    bool is_refunded() const { return status_ == Status::Refunded; }

private:
    std::string buyer_, seller_, verifier_;
    long amount_;
    Status status_ = Status::Pending;
};

int main() {
    // Happy path
    {
        Escrow e("alice", "bob", "oracle", 1000);
        assert(e.state() == Status::Pending);
        e.deposit(1000, "alice");
        assert(e.state() == Status::Funded);
        e.verify("oracle");
        assert(e.state() == Status::Verified);
        e.release();
        assert(e.is_released());
        std::cout << "escrow.cpp happy path: passed\n";
    }

    // State guards
    {
        Escrow e("alice", "bob", "oracle", 500);
        e.verify("oracle");
        assert(e.state() == Status::Pending);
        e.release();
        assert(e.state() == Status::Pending);
        e.deposit(500, "alice");
        e.release();
        assert(e.state() == Status::Funded);
        e.verify("oracle");
        e.deposit(999, "alice");
        assert(e.state() == Status::Verified);
        e.release();
        e.release();
        assert(e.is_released());
        std::cout << "escrow.cpp state guards: passed\n";
    }

    // Dispute path
    {
        Escrow e("alice", "bob", "oracle", 750);
        e.deposit(750, "alice");
        e.dispute("alice");
        assert(e.is_refunded());
        e.release();
        assert(e.is_refunded());
        std::cout << "escrow.cpp dispute path: passed\n";
    }

    return 0;
}
