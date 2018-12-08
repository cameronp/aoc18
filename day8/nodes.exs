defmodule Nodes do
  def load() do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split(" ")
    |> Enum.map(&String.to_integer/1)
  end

  def parse_node([children, meta_count | tail]) do
    {ks, tail2} =
      (for i <- 0..children, i > 0, do: {})
      |> Enum.reduce({[], tail},
          fn _, {kids, rest} ->
            {kid, rest2} = parse_node(rest)
            {[kid | kids], rest2}
          end )

    metadata = Enum.take(tail2, meta_count)
    {{metadata, Enum.reverse(ks)}, Enum.drop(tail2, meta_count)}
  end

  def node_sum({meta, kids}) do
    kids_sum =
      kids
      |> Enum.map(&node_sum/1)
      |> Enum.sum
    Enum.sum(meta) + kids_sum
  end

  def value({meta, []}), do: Enum.sum(meta)
  def value({meta, kids}) do
    meta
      |> Enum.map(fn m -> get_meta_value(m, Enum.count(kids), kids) end)
      |> Enum.sum()
  end

  def kid_values({meta, kids}) do
    meta
    |> Enum.map(fn m -> get_meta_value(m, Enum.count(kids), kids) end)
  end

  def get_meta_value(m, count, _) when m > count, do: 0
  def get_meta_value(m, _, kids) do
    # IO.puts("Getting #{m}th from #{inspect(kids)}")
    kids
    |> Enum.at(m - 1)
    |> value()
  end

  def part1() do
    {root, []} =
      load()
      |> parse_node()
    node_sum(root)
    |> IO.inspect()
  end

  def part2() do
    {root, []} =
      load()
      |> parse_node()
    value(root)
    |> IO.inspect()
  end

end

Nodes.part1()
Nodes.part2()

