defmodule Mix.Local do
  @moduledoc """
  Module responsible to manage local .mix installation.
  """

  @doc """
  The path for local tasks.
  """
  def tasks_path do
    Path.join Mix.Utils.mix_home, "tasks"
  end

  @doc """
  Append local tasks path into Erlang code path.
  """
  def append_tasks do
    Code.append_path tasks_path
  end

  @doc """
  Append mix paths into Erlang code path.
  """
  def append_paths do
    Enum.each Mix.Utils.mix_path, Code.append_path(&1)
  end

  @doc """
  Returns all tasks modules in .mix/tasks.
  """
  def all_tasks do
    query   = Path.join(tasks_path, "Elixir.Mix.Tasks.*.beam")
    files   = Path.wildcard(query)
    modules = Enum.map files, &1 |> Path.basename |> Path.rootname(".beam") |> binary_to_atom
    Enum.filter(modules, fn(mod) ->
      match? { :module, _ }, Code.ensure_loaded(mod)
    end)
  end
end
