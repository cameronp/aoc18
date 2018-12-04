defmodule Guard do
  def load(file \\ "input.txt") do
    file
    |> File.stream!()
    |> Stream.map(&String.trim/1)
  end

  def parse_command([_, "Guard" <> rest]) do
    re = ~r/#(\d+)/
    [_,id] = Regex.run(re, rest)
    {:guard, String.to_integer(id)}
  end

  def parse_command([time, "falls" <> _]) do
    re = ~r/:(\d+)/
    [_,minute] = Regex.run(re, time)
    {:sleeps, String.to_integer(minute)}
  end

  def parse_command([time, "wakes" <> _]) do
    re = ~r/:(\d+)/
    [_,minute] = Regex.run(re, time)
    {:wakes, String.to_integer(minute)}
  end

  def split_time(command) do
    ~r/\[(.+)\] (.+)/
    |> Regex.run(command)
    |> tl
  end

  def parse(commands) do
    commands
    |> Enum.map(&split_time/1)
    |> Enum.map(&parse_command/1)
  end

  def init, do: %{on_duty: nil, guards: %{}}

  def on_duty(m,id), do: m |> Map.put(:on_duty, id)

  def sleep(m, minute), do: %{m | guards: fall_asleep(m.on_duty, m.guards, minute)}

  def wake(m, minute), do: %{m | guards: wake_up(m.on_duty, m.guards, minute)}

  def fall_asleep(id, guards, minute) do
    history = Map.get(guards, id, [])
    Map.put(guards, id, [{minute} | history])
  end

  def wake_up(id, guards, minute) do
    history = Map.get(guards, id)
    [{went_to_sleep} | tail] = history
    Map.put(guards, id, [{went_to_sleep, minute} | tail])
  end

  def exec_cmd({:guard, id}, m), do: on_duty(m,id)
  def exec_cmd({:sleeps, minute}, m), do: sleep(m,minute)
  def exec_cmd({:wakes, minute}, m), do: wake(m,minute)

  def process(commands) do
    commands
    |> Enum.reduce(init(), &exec_cmd/2)
  end

  def total_minutes(naps) do
    naps
    |> Enum.reduce(0, fn {s,f}, sum -> sum + (f - s) end)
  end

  def guard_totals(%{guards: guards}) do
    guards
    |> Map.keys()
    |> Enum.reduce(guards, fn key, acc -> Map.update!(acc, key, &total_minutes/1) end)
  end

  def inc_minute(i,m) do
    curr = Map.get(m, i, 0)
    Map.put(m,i,curr+1)
  end

  def build_minute_map(naps) do
    naps
    |> Enum.reduce(%{}, fn {s, f},acc -> s..(f-1) |> Enum.reduce(acc, &inc_minute/2) end)
    |> Map.to_list()
    |> Enum.sort_by(fn {k,v} -> v end)
    |> Enum.reverse()
  end

  def best_minute(naps) do
    naps
    |> build_minute_map()
    |> hd
  end

  def prime_time(naps) do
    naps
    |> best_minute()
    |> elem(0)
  end

  def compute_best_minutes(guards) do
    guards
    |> Map.to_list()
    |> Enum.map(fn {id, naps} -> {id, best_minute(naps)} end)
    |> Enum.sort_by(fn {_id, {_minute, freq}} -> freq end)
    |> Enum.reverse
    |> hd
  end

  def part1() do
    sleeps =
      load()
      |> Enum.sort()
      |> parse()
      |> process()

    totals =
      sleeps
      |> guard_totals()

    sleepiest =
      totals
      |> Map.to_list()
      |> Enum.sort_by(fn {_k, v} -> v end)
      |> Enum.reverse()
      |> hd
      |> elem(0)

    best_minute =
      prime_time(sleeps.guards[sleepiest])

    IO.puts(sleepiest * best_minute)
  end

  def part2() do
    sleeps =
      load()
      |> Enum.sort()
      |> parse()
      |> process()

    {id, {min, _}} =
      sleeps.guards
      |> compute_best_minutes
      |> IO.inspect()
    IO.puts(id * min)
  end

end

Guard.part1()
Guard.part2()
