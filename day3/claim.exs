defmodule Claim do
  def empty(), do: %{}

  def claim_rect(m, _id, _x, _y, 0, _w, _f), do: m
  def claim_rect(m, id, x, y, h, w, f), do: m |> claim_row(id, x,y,w,f) |> claim_rect(id, x, y+1, h-1, w,f)

  def claim_row(m,_id, _x,_y,0,_f), do: m
  def claim_row(m, id, x,y,l,f), do: m |> f.(id, x,y) |> claim_row(id, x+1,y,l-1,f)

  def inc_cell(m, _id, x,y) do
    v = Map.get(m, {x,y}, 0)
    Map.put(m,{x,y}, v + 1)
  end

  def claim_cell(m, id, x,y) do
    cs = Map.get(m, {x,y}, [])
    Map.put(m, {x,y}, [id | cs])
  end



  def load(file \\ "input.txt") do
    file
    |> File.stream!()
    |> Stream.map(&String.trim/1)
  end

  def parse(s) do
    r = ~r/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/
    [_ | vals] = Regex.run(r, s)

    [id, x, y, w, h] = vals |> Enum.map(&String.to_integer/1)
    {id,x,y,w,h}
  end

  def claim({id, x,y,w,h}, m), do: m |> claim_rect(id, x,y,h,w, &inc_cell/4)

  def claim_with_id({id, x,y,w,h}, m), do: m |> claim_rect(id, x,y,h,w, &claim_cell/4)

  def detect_overlap([], ids), do: ids
  def detect_overlap([_id], ids), do: ids
  def detect_overlap(multiple_ids, ids), do: ids -- multiple_ids

  def part1() do
    load()
    |> Enum.map(&parse/1)
    |> Enum.reduce(%{}, &claim/2)
    |> Map.values()
    |> Enum.filter(fn v -> v > 1 end)
    |> Enum.count()
    |> IO.inspect()
  end

  def part2() do
    claims =
      load()
      |> Enum.map(&parse/1)

    map =
      claims
      |> Enum.reduce(%{}, &claim_with_id/2)

    ids =
      claims
      |> Enum.map(fn {id, _, _, _ ,_} -> id end)

    map
    |> Map.values()
    |> Enum.reduce(ids, &detect_overlap/2)
    |> IO.inspect()
  end
end

Claim.part1()
Claim.part2()
