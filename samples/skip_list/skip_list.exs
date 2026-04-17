# skip_list.exs — Elixir reference implementation of skip_list.a4.
#
# Functional skip list using an Elixir map + list of level lists.
# Run with:  elixir skip_list.exs

defmodule SkipList do
  @max_level 4

  defstruct data: %{}, levels: [[], [], [], []]

  def new, do: %SkipList{}

  def insert(%SkipList{data: d, levels: ls} = s, key, value) do
    ls =
      if Map.has_key?(d, key) do
        ls
      else
        lvl = random_level()
        promote(key, 0, lvl, ls)
      end

    %{s | data: Map.put(d, key, value), levels: ls}
  end

  def delete(%SkipList{data: d, levels: ls} = s, key) do
    if Map.has_key?(d, key) do
      %{s | data: Map.delete(d, key), levels: Enum.map(ls, &List.delete(&1, key))}
    else
      s
    end
  end

  def has?(%SkipList{data: d}, key), do: Map.has_key?(d, key)
  def get(%SkipList{data: d}, key),  do: Map.get(d, key)
  def size(%SkipList{data: d}),      do: map_size(d)
  def keys(%SkipList{levels: [l0 | _]}), do: l0

  # --- internal ---

  defp random_level, do: random_level(0)
  defp random_level(n) when n >= @max_level - 1, do: n
  defp random_level(n) do
    if :rand.uniform(2) == 1, do: random_level(n + 1), else: n
  end

  defp promote(_key, lvl, max_lvl, ls) when lvl > max_lvl, do: ls
  defp promote(key, lvl, max_lvl, ls) do
    l = Enum.at(ls, lvl)
    new_l = sorted_insert(l, key)
    promote(key, lvl + 1, max_lvl, List.replace_at(ls, lvl, new_l))
  end

  def sorted_insert([], x), do: [x]
  def sorted_insert([h | t] = lst, x) do
    if x <= h, do: [x | lst], else: [h | sorted_insert(t, x)]
  end
end

defmodule Main do
  def run do
    :rand.seed(:exsplus, {42, 42, 42})

    # sorted-insert
    lst = Enum.reduce([3, 1, 5, 2], [], &SkipList.sorted_insert(&2, &1))
    [1 | _] = lst
    4 = length(lst)
    IO.puts("sorted-insert: ok")

    # empty
    s0 = SkipList.new()
    0 = SkipList.size(s0)
    IO.puts("sl-new: ok")

    # single
    s1 = SkipList.insert(s0, 10, 100)
    true = SkipList.has?(s1, 10)
    100  = SkipList.get(s1, 10)
    1    = SkipList.size(s1)
    IO.puts("single insert: ok")

    # multi
    s2 = Enum.reduce([{5,50},{3,30},{8,80},{1,10}], SkipList.new(),
                     fn {k, v}, acc -> SkipList.insert(acc, k, v) end)
    4  = SkipList.size(s2)
    10 = SkipList.get(s2, 1)
    30 = SkipList.get(s2, 3)
    50 = SkipList.get(s2, 5)
    80 = SkipList.get(s2, 8)
    [1 | _] = SkipList.keys(s2)
    IO.puts("multi insert: ok")

    # delete
    s3 = SkipList.delete(s2, 3)
    3 = SkipList.size(s3)
    false = SkipList.has?(s3, 3)
    IO.puts("delete: ok")

    # delete non-existent
    s4 = SkipList.delete(s3, 99)
    3 = SkipList.size(s4)

    # update
    s5 = SkipList.insert(s4, 5, 999)
    999 = SkipList.get(s5, 5)
    IO.puts("update: ok")

    # larger
    s6 = Enum.reduce(
      [{20,200},{10,100},{30,300},{15,150},{25,250},{5,50},{35,350}],
      SkipList.new(),
      fn {k, v}, acc -> SkipList.insert(acc, k, v) end)
    7 = SkipList.size(s6)
    50  = SkipList.get(s6, 5)
    350 = SkipList.get(s6, 35)

    IO.puts("Skip list with 7 entries:")
    s6.levels
    |> Enum.with_index()
    |> Enum.each(fn {l, i} -> IO.puts("  Level #{i}: #{length(l)} keys") end)
    IO.puts("large dataset: ok")
  end
end

Main.run()
