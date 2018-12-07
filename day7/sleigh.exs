defmodule Sleigh do
  def load() do
    "test.txt"
    |> File.stream!()
    |> Stream.map(&String.trim/1)
  end

  def parse(line) do
    re = ~r/Step (\w).+step (\w)/
    [dependency, dependent] =
      Regex.run(re, line)
      |> tl()
      |> Enum.map(&String.downcase/1)
      |> Enum.map(&String.to_atom/1)

    {dependent, dependency}
  end

  def add_dependency({subj, dep}, m) do
    list = [dep | Map.get(m, subj, [])]
    Map.put(m,subj,list)
  end



  def build_tree(dependencies) do
    init = Enum.reduce(dependencies, %{}, fn {_, k},acc -> Map.put(acc,k,[]) end)
    dependencies
    |> Enum.reduce(init, &add_dependency/2)
  end

  def select_doable(tree) do
    tree
    |> Map.to_list()
    |> Enum.filter(fn {_, v} -> v == [] end)
    |> Enum.sort_by(fn {k, _} -> k end)
    |> hd
    |> elem(0)
  end

  def remove_dep(tree, k, job) do
    if tree[k] == :in_progress do
      tree
    else
      tree
      |> Map.update!(k, fn deps -> deps -- [job] end)
    end
  end

  def remove_deps(tree, job) do
    tree
    |> Map.keys
    |> Enum.reduce(tree, fn k,acc -> remove_dep(acc, k, job) end)
  end

  def complete(tree, todo) do
    tree
    |> remove_deps(todo)
    |> Map.delete(todo)
  end

  def execute_tree(tree) do
    if Map.keys(tree) |> length() > 0 do
      todo = select_doable(tree)
      rest =
        tree
        |> complete(todo)
        |> execute_tree()
      [todo | rest]
    else
      []
    end
  end

  def part1() do
    load()
    |> Enum.map(&parse/1)
    |> build_tree()
    |> execute_tree()
    |> Enum.map(&Atom.to_string/1)
    |> Enum.join()
    |> IO.inspect()
  end

  def duration(atom) do
    c = atom
      |> Atom.to_charlist()
      |> hd
    c - ?a + 1
  end

  def init_workers() do
    # [:wa, :wb, :wc, :wd, :we]
    [:wa, :wb]
    |> Enum.reduce(%{}, fn w, acc -> Map.put(acc, w, %{job: nil, time: 0}) end)
  end

  def assign_job(worker, workers, job) do
    Map.put(workers, worker, %{job: job, time: duration(job)})
  end

  def mark_in_progress(tree, j), do: Map.put(tree, j, :working)


  def available_work(tree) do
    tree
    |> Map.to_list()
    |> Enum.filter(fn {_, v} -> v == [] end)
    |> Enum.map(fn {k, _} -> k end)
  end

  def detect_complete_jobs(ws) do
    ws
    |> Map.to_list()
    |> Enum.filter(fn {_, %{job: j, time: n}} -> j != nil && n == 0 end)
    |> Enum.map(fn {_, %{job: j}} -> j end)
  end

  def reset_if_available(%{time: 0}), do: %{job: nil, time: 0}
  def reset_if_available(w), do: w

  def reset_available_workers(ws) do
    ws
    |> Enum.map(fn {k, v} -> {k, reset_if_available(v)} end)
    |> Enum.into(%{})
  end

  def mark_complete(tree, complete_jobs) do
    complete_jobs
    |> Enum.reduce(tree, fn j, acc -> complete(acc, j) end)
  end

  def assign_work(ws,tree) do
    jobs = available_work(tree)
    workers =
      ws
      |> Enum.filter(fn {_id, %{job: j}} -> j == nil end)
      |> Enum.map(fn {id, _} -> id end)

    assignments = Enum.zip(workers,jobs)
    tree2 =
      assignments
      |> Enum.map(fn {_, job} -> job end)
      |> Enum.reduce(tree, fn job,acc -> Map.put(acc, job, :in_progress) end)
    ws2 =
      assignments
      |> Enum.reduce(ws, fn {worker, job}, acc -> assign_job(worker, acc, job) end)
    {ws2, tree2}
  end

  def tick(state = %{workers: ws, tree: tree}) do
    ws2 = tick(ws)
    complete_jobs = detect_complete_jobs(ws2)
    ws3 = reset_available_workers(ws2)
    tree2 = mark_complete(tree, complete_jobs)
    {ws4, tree3} = assign_work(ws3, tree2)
    %{workers: ws4, tree: tree3, complete: state.complete ++ complete_jobs, elapsed: state.elapsed + 1}
  end

  def tick(%{job: nil} = w), do: w
  def tick(%{time: n} = w), do: w |> Map.put(:time, n - 1)

  def tick(ws) do
    ws
    |> Map.keys()
    |> Enum.reduce(ws, fn k, acc -> Map.put(acc, k, tick(ws[k])) end)
  end

  def completed?(tree), do: (Map.keys(tree) |> length()) == 0

  def run(state) do
    next = tick(state)
    if completed?(next.tree) do
      next
    else
      run(next)
    end
  end


  def part2() do
    workers = init_workers()
    tree =
      load()
      |> Enum.map(&parse/1)
      |> build_tree()

    %{workers: workers, tree: tree, complete: [], elapsed: 0}
    |> run()
    |> IO.inspect()
  end
end

Sleigh.part2()
