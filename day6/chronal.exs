defmodule Chronal do
  def load(file \\ "input.txt") do
    file
    |> File.stream!()
    |> Stream.map(&String.trim/1)
  end

  def parse(line) do
    [x,y] =
      line
      |> String.split(",")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)
    {x,y}
  end

  def manhattan({x,y}, {x1,y1}) do
    abs(x - x1) + abs(y - y1)
  end

  def compute_furthest(direction, points) do
    sorted =
      case direction do
        :left -> points |> Enum.sort_by(fn {x,_} -> x end)
        :up -> points |> Enum.sort_by(fn {_,y} -> y end)
        :right -> points |> Enum.sort_by(fn {x,_} -> x end, &>=/2)
        :down -> points |> Enum.sort_by(fn {_,y} -> y end, &>=/2)
    end
    hd(sorted)
  end

  def shortest(point, points) do
    points
      |> Enum.map(&manhattan(&1, point))
      |> Enum.sort
      |> hd
  end

  def bounded?(points, furthest, point) do
    if point in Map.values(furthest) do
      false
    else
      [:left,:right,:up,:down]
      |> Enum.all?(fn dir -> bounded?(points, furthest, point, dir) end)
    end
  end

  def bounded?(points, %{up: {_, upy}}, point = {x,y}, :up) do
    test_points =
      for test_y <- y-1..upy-2 do
        {x,test_y}
      end
    Enum.any?(test_points, fn p -> closest_point(points, p) != point end)
  end

  def bounded?(points, %{down: {_, downy}}, point = {x,y}, :down) do
    test_points =
      for test_y <- y+1..downy+2 do
        {x,test_y}
      end
    Enum.any?(test_points, fn p -> closest_point(points, p) != point end)
  end

  def bounded?(points, %{left: {leftx, _}}, point = {x,y}, :left) do
    test_points =
      for test_x <- x-1..leftx-2 do
        {test_x,y}
      end
    Enum.any?(test_points, fn p -> closest_point(points, p) != point end)
  end

  def bounded?(points, %{right: {rightx, _}}, point = {x,y}, :right) do
    test_points =
      for test_x <- x+1..rightx+2 do
        {test_x,y}
      end
    Enum.any?(test_points, fn p -> closest_point(points, p) != point end)
  end

  def range(%{up: {_, top}, down: {_, bottom}, left: {left, _}, right: {right, _}}),
    do: {{left,top}, {right, bottom}}

  def closest_point(points, point) do
    sorted =
      points
      |> Enum.map(fn p -> {p, manhattan(p, point)} end)
      |> Enum.sort_by(fn {_, d} -> d end)
    select_owner(sorted)
  end

  def inc_point(p,m) do
    curr = Map.get(m, p, 0)
    Map.put(m,p,curr+1)
  end

  def map_regions(furthest, points) do
    {{x0,y0},{x1,y1}} = range(furthest)
    (for x <- x0..x1, y <- y0..y1 do
      closest_point(points, {x,y})
    end)
    |> Enum.reduce(%{}, &inc_point/2)
  end

  def select_owner([{_, d}, {_, d} | _]), do: :tie
  def select_owner([{p, _} | _]), do: p

  def in_safe_region(points, point) do
    total =
      points
      |> Enum.reduce(0, fn p,acc -> acc + manhattan(p,point) end )
    total < 32
  end

  def load_points() do
    load()
    |> Stream.map(&parse/1)
    |> Enum.to_list()
  end

  def build_furthest(points) do
    [:left,:right,:up,:down] |> Enum.reduce(%{}, fn dir, acc -> Map.put(acc, dir, compute_furthest(dir,points)) end)
  end

  def part1() do
    points = load_points
    furthest = build_furthest(points)

    {_bounded, unbounded} = Enum.split_with(points, fn p -> bounded?(points, furthest, p) end)

    regions = map_regions(furthest,points)

    candidates =
      unbounded
      |> Enum.reduce(regions, fn p,acc -> Map.delete(acc, p) end)
      |> Map.delete(:tie)

    candidates
      |> Map.values()
      |> Enum.sort()
      |> Enum.reverse()
      |> hd()
      |> IO.puts()

  end

  def in_safe_region?(points, point) do
    total =
      points
      |> Enum.reduce(0, fn p,acc -> acc + manhattan(p,point) end )
    total < 10000
  end


  def part2() do
    points = load_points()
    furthest = build_furthest(points)
    {{x0,y0},{x1,y1}} = range(furthest)
    (for x <- x0..x1, y <- y0..y1 do
      {x,y}
    end)
    |> Enum.filter(fn p -> in_safe_region?(points, p) end)
    |> Enum.count()
    |> IO.puts()
  end
end

Chronal.part1()
Chronal.part2()
