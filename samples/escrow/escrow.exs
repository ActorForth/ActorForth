# escrow.exs — Elixir reference implementation of the a4 escrow contract.
#
#   elixir escrow.exs

defmodule Escrow do
  use GenServer

  defstruct [:buyer, :seller, :verifier, :amount, status: :pending]

  # --- API ---

  def start_link(buyer, seller, verifier, amount) do
    GenServer.start_link(__MODULE__,
      %Escrow{buyer: buyer, seller: seller, verifier: verifier, amount: amount})
  end

  def deposit(pid, amount, caller), do: GenServer.call(pid, {:deposit, amount, caller})
  def verify(pid, caller),          do: GenServer.call(pid, {:verify, caller})
  def release(pid),                  do: GenServer.call(pid, :release)
  def dispute(pid, caller),          do: GenServer.call(pid, {:dispute, caller})
  def state(pid),                    do: GenServer.call(pid, :state)
  def released?(pid),                do: GenServer.call(pid, :is_released)
  def refunded?(pid),                do: GenServer.call(pid, :is_refunded)

  # --- callbacks ---

  @impl true
  def init(s), do: {:ok, s}

  @impl true
  def handle_call({:deposit, amount, caller}, _from,
                  %{status: :pending, amount: amount, buyer: caller} = s),
    do: {:reply, :ok, %{s | status: :funded}}
  def handle_call({:deposit, _, _}, _from, s), do: {:reply, :ok, s}

  def handle_call({:verify, caller}, _from,
                  %{status: :funded, verifier: caller} = s),
    do: {:reply, :ok, %{s | status: :verified}}
  def handle_call({:verify, _}, _from, s), do: {:reply, :ok, s}

  def handle_call(:release, _from, %{status: :verified} = s),
    do: {:reply, :ok, %{s | status: :released}}
  def handle_call(:release, _from, s), do: {:reply, :ok, s}

  def handle_call({:dispute, caller}, _from,
                  %{status: :funded, buyer: b, seller: se} = s)
      when caller == b or caller == se,
    do: {:reply, :ok, %{s | status: :refunded}}
  def handle_call({:dispute, _}, _from, s), do: {:reply, :ok, s}

  def handle_call(:state, _from, s),         do: {:reply, s.status, s}
  def handle_call(:is_released, _from, s),   do: {:reply, s.status == :released, s}
  def handle_call(:is_refunded, _from, s),   do: {:reply, s.status == :refunded, s}
end

# --- test driver ---

{:ok, e1} = Escrow.start_link("alice", "bob", "oracle", 1000)
:pending = Escrow.state(e1)
Escrow.deposit(e1, 1000, "alice")
:funded = Escrow.state(e1)
Escrow.verify(e1, "oracle")
:verified = Escrow.state(e1)
Escrow.release(e1)
true = Escrow.released?(e1)
IO.puts("escrow.exs happy path: passed")

{:ok, e2} = Escrow.start_link("alice", "bob", "oracle", 500)
Escrow.verify(e2, "oracle")
:pending = Escrow.state(e2)
Escrow.release(e2)
:pending = Escrow.state(e2)
Escrow.deposit(e2, 500, "alice")
Escrow.release(e2)
:funded = Escrow.state(e2)
Escrow.verify(e2, "oracle")
Escrow.deposit(e2, 999, "alice")
:verified = Escrow.state(e2)
Escrow.release(e2)
Escrow.release(e2)
true = Escrow.released?(e2)
IO.puts("escrow.exs state guards: passed")

{:ok, e3} = Escrow.start_link("alice", "bob", "oracle", 750)
Escrow.deposit(e3, 750, "alice")
Escrow.dispute(e3, "alice")
true = Escrow.refunded?(e3)
Escrow.release(e3)
true = Escrow.refunded?(e3)
IO.puts("escrow.exs dispute path: passed")
