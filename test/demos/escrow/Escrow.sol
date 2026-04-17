// SPDX-License-Identifier: MIT
// Escrow.sol — same contract as escrow.a4, written in Solidity
// for side-by-side comparison.
pragma solidity ^0.8.20;

/// @notice Three-party escrow: buyer deposits, verifier confirms,
///         funds release to seller. Either party can dispute before
///         verification, refunding the buyer.
contract Escrow {
    enum Status { Pending, Funded, Verified, Disputed, Released, Refunded }

    address public buyer;
    address public seller;
    address public verifier;
    uint256 public amount;
    Status  public status;

    event Deposited(address from, uint256 amount);
    event Verified(address by);
    event Released(address to, uint256 amount);
    event Disputed(address by);
    event Refunded(address to, uint256 amount);

    error InvalidState(Status actual, Status expected);
    error NotAuthorised(address caller, address expected);
    error WrongAmount(uint256 provided, uint256 expected);

    constructor(address _buyer, address _seller, address _verifier, uint256 _amount) {
        buyer    = _buyer;
        seller   = _seller;
        verifier = _verifier;
        amount   = _amount;
        status   = Status.Pending;
    }

    modifier only(address who) {
        if (msg.sender != who) revert NotAuthorised(msg.sender, who);
        _;
    }

    modifier inStatus(Status s) {
        if (status != s) revert InvalidState(status, s);
        _;
    }

    function deposit() external payable only(buyer) inStatus(Status.Pending) {
        if (msg.value != amount) revert WrongAmount(msg.value, amount);
        status = Status.Funded;
        emit Deposited(msg.sender, msg.value);
    }

    function verify() external only(verifier) inStatus(Status.Funded) {
        status = Status.Verified;
        emit Verified(msg.sender);
    }

    function release() external inStatus(Status.Verified) {
        status = Status.Released;
        (bool ok, ) = payable(seller).call{value: amount}("");
        require(ok, "transfer failed");
        emit Released(seller, amount);
    }

    function dispute() external inStatus(Status.Funded) {
        if (msg.sender != buyer && msg.sender != seller) {
            revert NotAuthorised(msg.sender, buyer);
        }
        status = Status.Refunded;
        (bool ok, ) = payable(buyer).call{value: amount}("");
        require(ok, "transfer failed");
        emit Disputed(msg.sender);
        emit Refunded(buyer, amount);
    }
}
