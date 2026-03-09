# bench_ops_elixir.exs — Comprehensive operations benchmark (native Elixir)
#
# Performs the same computational work as the A4 benchmark using native Elixir,
# for cross-language performance comparison.
#
# Reports microseconds per operation for direct cross-language comparison.
#
# Usage:
#   elixir bench_ops_elixir.exs

defmodule BenchOps do
  @n 100_000

  def n, do: @n

  # --- Arithmetic operations ---

  def square(x), do: x * x
  def cube(x), do: x * x * x
  def arith_chain(x), do: abs(square(x) + cube(x))

  # --- List operations ---

  def list_ops do
    l = [1, 2, 3, 4, 5]
    l = Enum.reverse(l)
    l = l ++ [10, 20]
    length(l)
  end

  # --- String operations ---

  def str_work(s), do: s |> String.trim() |> String.upcase()

  def string_ops do
    str_work("  hello world  ")
  end

  # --- Map operations ---

  def map_ops do
    m = %{}
    m = Map.put(m, "a", 1)
    m = Map.put(m, "b", 2)
    m = Map.put(m, "c", 3)
    m |> Map.keys() |> length()
  end

  # --- Product type (Vec3) operations ---

  defmodule Vec3 do
    defstruct [:x, :y, :z]
  end

  def vec_mag2(%Vec3{x: x, y: y, z: z}) do
    square(x) + square(y) + square(z)
  end

  # --- Composed word chain ---

  def compose_test(x), do: abs(max(square(x), cube(x)))

  # --- Benchmark runners ---

  def run_arith(n) do
    Enum.each(1..n, fn i -> arith_chain(i) end)
  end

  def run_list_ops(n) do
    Enum.each(1..n, fn _ -> list_ops() end)
  end

  def run_string_ops(n) do
    Enum.each(1..n, fn _ -> string_ops() end)
  end

  def run_map_ops(n) do
    Enum.each(1..n, fn _ -> map_ops() end)
  end

  def run_product_ops(n) do
    Enum.each(1..n, fn i -> vec_mag2(%Vec3{x: i, y: i + 1, z: i + 2}) end)
  end

  def run_compiled_words(n) do
    Enum.each(1..n, fn i -> compose_test(i) end)
  end

  # --- Benchmark harness ---

  def bench(label, func, n_iters) do
    # Warmup
    func.()

    times =
      for _ <- 1..n_iters do
        {t, _} = :timer.tc(func)
        t
      end

    sorted = Enum.sort(times)
    median = Enum.at(sorted, div(n_iters, 2))
    per_op = median / @n
    :io.format("~s: ~8.1f ms  (~6.3f us/op)~n", [label, median / 1000.0, per_op])
    median
  end
end

n = BenchOps.n()
n_iters = 5

IO.puts("\n=== Elixir Native Operations Benchmark ===\n")
IO.puts("--- Native Elixir (#{n} iterations, #{n_iters} timed runs, median) ---")

t_arith = BenchOps.bench("  Arithmetic chain", fn -> BenchOps.run_arith(n) end, n_iters)
t_list  = BenchOps.bench("  List ops",         fn -> BenchOps.run_list_ops(n) end, n_iters)
t_str   = BenchOps.bench("  String ops",       fn -> BenchOps.run_string_ops(n) end, n_iters)
t_map   = BenchOps.bench("  Map ops",          fn -> BenchOps.run_map_ops(n) end, n_iters)
t_prod  = BenchOps.bench("  Product type ops", fn -> BenchOps.run_product_ops(n) end, n_iters)
t_comp  = BenchOps.bench("  Compiled word chain", fn -> BenchOps.run_compiled_words(n) end, n_iters)

total = t_arith + t_list + t_str + t_map + t_prod + t_comp
per_op = total / n
:io.format("~n  TOTAL: ~8.1f ms  (~6.3f us/op)~n~n", [total / 1000.0, per_op])

# Machine-readable summary for cross-language comparison
:io.format("BENCH_DATA: arith=~.3f list=~.3f string=~.3f map=~.3f product=~.3f wordchain=~.3f~n",
  [t_arith/n, t_list/n, t_str/n, t_map/n, t_prod/n, t_comp/n])
