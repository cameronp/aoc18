defmodule Labels do
  def count_letters(s), do: _count_letters(s, %{})
  def _count_letters("", m), do: m
  def _count_letters(<<c::utf8, tail::binary>>, m) do
    _count_letters(tail, Map.put(m,c, Map.get(m,c,0) + 1))
  end

  def has_two_matching?(s) do
    s
    |> count_letters
    |> Map.values
    |> Enum.member?(2)
  end

  def has_three_matching?(s) do
    s
    |> count_letters
    |> Map.values
    |> Enum.member?(3)
  end

  def load(file \\ "input.txt") do
    file
    |> File.stream!()
    |> Stream.map(&String.trim/1)
  end

  def checksum(labels) do
    twos =
      labels
      |> Stream.filter(&has_two_matching?/1)
      |> Enum.count
    threes =
      labels
      |> Stream.filter(&has_three_matching?/1)
      |> Enum.count
    twos * threes
  end

  def close_match(a,b), do: _close_match(a,b,0)
  def _close_match("", _, diffs), do: diffs == 1
  def _close_match(<<h::utf8, t::binary>>, <<h::utf8, t1::binary>>, diffs),
    do: _close_match(t,t1,diffs)
  def _close_match(<<_::utf8, t::binary>>, <<_::utf8, t1::binary>>, diffs),
    do: _close_match(t,t1,diffs + 1)

  def find_close_pair(labels) do
    for a <- labels, b <- labels -- [a], close_match(a,b) do
      {a,b}
    end
    |> hd()
  end

  def common_letters({a,b}), do: common_letters(a,b)
  def common_letters("",""), do: ""
  def common_letters(<<h::utf8, t::binary>>, <<h::utf8, t1::binary>>),
    do: <<h::utf8>> <> common_letters(t,t1)
  def common_letters(<<_::utf8, t::binary>>, <<_::utf8, t1::binary>>),
    do: common_letters(t,t1)

  def part1() do
    load()
    |> checksum()
    |> IO.inspect
  end

  def part2() do
    load()
    |> Enum.to_list()
    |> find_close_pair()
    |> common_letters()
    |> IO.inspect()
  end

end


Labels.part1()
Labels.part2()


