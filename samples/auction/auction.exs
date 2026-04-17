# auction.exs — Elixir reference implementation of the a4 auction demo.
#
# Idiomatic Elixir: GenServer per auction, struct-based state,
# pattern-matched handle_call clauses. Run with:
#
#   elixir auction.exs

defmodule Auction do
  use GenServer

  defstruct [:item, :current_bid, :highest_bidder, status: :open]

  # --- public API ---

  def start_link(item, start_bid, start_bidder) do
    GenServer.start_link(__MODULE__,
      %Auction{item: item, current_bid: start_bid, highest_bidder: start_bidder})
  end

  def bid(pid, amount, bidder), do: GenServer.call(pid, {:bid, amount, bidder})
  def close(pid),               do: GenServer.call(pid, :close)
  def leader(pid),              do: GenServer.call(pid, :leader)
  def leading_bid(pid),         do: GenServer.call(pid, :leading_bid)
  def open?(pid),               do: GenServer.call(pid, :is_open)

  # --- callbacks ---

  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:bid, _amount, _bidder}, _from, %{status: :closed} = s),
    do: {:reply, false, s}

  def handle_call({:bid, amount, _bidder}, _from, %{current_bid: c} = s)
      when amount <= c,
    do: {:reply, false, s}

  def handle_call({:bid, amount, bidder}, _from, s),
    do: {:reply, true, %{s | current_bid: amount, highest_bidder: bidder}}

  def handle_call(:close, _from, s),
    do: {:reply, :ok, %{s | status: :closed}}

  def handle_call(:leader, _from, s),
    do: {:reply, s.highest_bidder, s}

  def handle_call(:leading_bid, _from, s),
    do: {:reply, s.current_bid, s}

  def handle_call(:is_open, _from, s),
    do: {:reply, s.status == :open, s}
end

# --- test driver ---

{:ok, a} = Auction.start_link("laptop", 100, "")

true  = Auction.bid(a, 150, "alice")
"alice" = Auction.leader(a)
150   = Auction.leading_bid(a)

false = Auction.bid(a, 120, "bob")      # too low
"alice" = Auction.leader(a)
150   = Auction.leading_bid(a)

true  = Auction.bid(a, 200, "bob")
"bob" = Auction.leader(a)
200   = Auction.leading_bid(a)

Auction.close(a)
false = Auction.open?(a)

false = Auction.bid(a, 500, "carol")    # closed
"bob" = Auction.leader(a)
200   = Auction.leading_bid(a)

IO.puts("auction.exs: all assertions passed")
