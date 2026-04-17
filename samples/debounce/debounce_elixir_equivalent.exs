# debounce_elixir_equivalent.exs — Elixir equivalent of debounce_demo.a4
#
# Idiomatic Elixir: GenServer per actor, structs for typed messages.
# Run with:  elixir debounce_elixir_equivalent.exs
#
# Comparison with a4:
#   debounce_demo.a4:                 ~80 lines of core definitions
#   debounce_elixir_equivalent.exs:   ~120 lines
#
# What the extra lines are:
#   - `use GenServer` + impl/behaviour boilerplate
#   - Separate handle_cast/handle_call per message kind
#   - No native actor-as-type; actor state = struct + module
#   - Reply values require explicit {:reply, Value, State} tuples
#
# Elixir is still substantially shorter than Python or Erlang because of
# pipelined struct updates and the tolerable GenServer shape, but each
# operation costs more ceremony than a4's `word Actor -> Actor` form.

# ============================================================
# Structs (product types)
# ============================================================

defmodule Entry do
  defstruct [:path, :hash, :size, :kind]
end

defmodule Stats do
  defstruct [:size_diff, :count_diff]
end

defmodule Batch do
  defstruct [:added, :deleted, :size_delta, :count_delta]
end

# ============================================================
# StoreActor — counts events recorded
# ============================================================

defmodule StoreActor do
  use GenServer

  defstruct events: 0

  def start_link, do: GenServer.start_link(__MODULE__, %StoreActor{})

  def record(pid, n), do: GenServer.cast(pid, {:record, n})
  def total(pid),     do: GenServer.call(pid, :total)

  @impl true
  def init(s), do: {:ok, s}

  @impl true
  def handle_cast({:record, n}, s), do: {:noreply, %{s | events: s.events + n}}

  @impl true
  def handle_call(:total, _from, s), do: {:reply, s.events, s}
end

# ============================================================
# TallyActor — counts logged batches
# ============================================================

defmodule TallyActor do
  use GenServer

  defstruct batches: 0

  def start_link, do: GenServer.start_link(__MODULE__, %TallyActor{})

  def log_batch(pid, %Batch{} = _b), do: GenServer.cast(pid, :log_batch)
  def logged(pid),                    do: GenServer.call(pid, :logged)

  @impl true
  def init(s), do: {:ok, s}

  @impl true
  def handle_cast(:log_batch, s), do: {:noreply, %{s | batches: s.batches + 1}}

  @impl true
  def handle_call(:logged, _from, s), do: {:reply, s.batches, s}
end

# ============================================================
# Tests
# ============================================================

defmodule Main do
  def run do
    # Struct construction and access
    b = %Batch{added: 5, deleted: 3, size_delta: 200, count_delta: 2}
    5   = b.added
    3   = b.deleted
    200 = b.size_delta
    2   = b.count_delta

    st = %Stats{size_diff: 1024, count_diff: 1}
    1024 = st.size_diff
    1    = st.count_diff

    e = %Entry{path: "/docs/report.pdf", hash: "abc123",
               size: 4096, kind: "application/pdf"}
    "/docs/report.pdf"  = e.path
    "abc123"            = e.hash
    4096                = e.size
    "application/pdf"   = e.kind

    # Actors
    {:ok, store} = StoreActor.start_link()
    {:ok, tally} = TallyActor.start_link()

    TallyActor.log_batch(tally, %Batch{added: 5, deleted: 3, size_delta: 200, count_delta: 2})
    TallyActor.log_batch(tally, %Batch{added: 10, deleted: 1, size_delta: 500, count_delta: 9})

    # Cast is async; the subsequent call acts as a sync barrier.
    2 = TallyActor.logged(tally)

    StoreActor.record(store, 3)
    StoreActor.record(store, 7)
    10 = StoreActor.total(store)

    IO.puts("debounce.exs: all assertions passed")
  end
end

Main.run()
