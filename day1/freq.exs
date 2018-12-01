defmodule Freq do


  def load(file \\ "input.txt") do
    file
    |> File.stream!()
    |> Stream.map(&String.trim/1)
  end


  def part1() do
    load()
    |> Enum.reduce(0, &handle_change/2)
    |> IO.inspect()
  end

  def part2() do
    load()
    |> find_dupe(%{freqs: %{}, f: 0})
    |> IO.inspect()
  end

  def find_dupe(commands, %{dupe: f}), do: f
  def find_dupe(commands, state) do
    new_state = run_once(commands, state)
    find_dupe(commands, new_state)
  end

  def run_once(commands, state) do
    Enum.reduce(commands, state, &detect_dupe/2)
  end

  def detect_dupe(_, %{dupe: f}), do: %{dupe: f}
  def detect_dupe(change, %{freqs: fs, f: f}) do
    new_f = handle_change(change, f)
    case fs[new_f] do
      nil -> %{freqs: Map.put(fs, new_f, 1), f: new_f }
      n -> %{dupe: new_f}
    end
  end

  def handle_change("+" <> amt, freq), do: freq + String.to_integer(amt)
  def handle_change("-" <> amt, freq), do: freq - String.to_integer(amt)



end

Freq.part1()
Freq.part2()
