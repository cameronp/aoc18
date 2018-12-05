defmodule Reactor do
  def react(s) when is_binary(s), do: s |> String.to_charlist() |> react() |> to_string()

  def react([a,b | tail]) when a == b - 32, do: react(tail)
  def react([a,b | tail]) when b == a - 32, do: react(tail)
  def react([a,b | tail]), do: [a | react([b|tail])]
  def react(l), do: l

  def fullyreact(s) do
    once = react(s)
    if once == s do
      s
    else
      fullyreact(once)
    end
  end

  def remove(s,c) when is_binary(s), do: String.replace(s, <<c>>, "") |> String.replace(<<c-32>>, "")

  def load() do
    File.read!("input.txt")
    |> String.trim()
  end

  def find_problem_element(s) do
    (for el <- ?a..?z, do: {el, s |> remove(el) |> fullyreact() |> String.length})
    |> Enum.sort_by(fn {_, l} -> l end)
    |> hd
  end

  def part1() do
    load()
    |> fullyreact
    |> String.length()
    |> IO.puts
  end

  def part2() do
    load()
    |> find_problem_element()
    |> IO.inspect()
  end

end


Reactor.part1()
Reactor.part2()

