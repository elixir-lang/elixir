defmodule Mix.Tasks do
  @moduledoc """
  Utilities for finding tasks and returning them as modules.
  """

  @doc """
  Takes a raw task name (totally lower case) and
  tries to load that task's associated module.
  If the task does not exist or cannot be loaded,
  a tuple of { :error, what } is returned. If the
  task module is successfully loaded, a tuple of
  { :module, module } is returned.
  """
  def get_module(s) when is_binary(s) do
    name = Module.concat(Mix.Tasks, task_to_module(s))
    Code.ensure_loaded(name)
  end

  @doc """
  Run a task with arguments. It returns `{ :ok, result }`
  if the task could be run with success, `{ :error, reason }`
  otherwise. `reason` may be:

  * :invalid_task - the task exists but does not confirm with Mix.Task behavior;
  * :no_task - the task does not exist

  """
  def run(name, args // []) do
    case get_module(name) do
      { :module, module } ->
        if :erlang.function_exported(module, :run, 1) do
          { :ok, module.run(args) }
        else
          { :error, :invalid_task }
        end
      { :error, _ } -> { :error, :no_task }
    end
  end

  @doc """
  Similar to `run/2` but raises an exception in case of failure.
  """
  def run!(name, args // []) do
    case run(name, args) do
      { :ok, result }           -> result
      { :error, :no_task }      -> raise Mix.NoTaskError, task: name
      { :error, :invalid_task } -> raise Mix.InvalidTaskError, task: name
    end
  end

  @doc """
  Takes a module and extracts the last portion of it,
  lower-cases the first letter, and returns the name.
  """
  def module_to_task(module) do
    [_,_|t] = Regex.split(%r/\./, to_binary(module))
    t /> Enum.map(to_lower(&1)) /> Enum.join(".")
  end

  @doc """
  Takes a task as a string like "foo" or "foo.bar"
  and capitalizes each segment to form a module name.
  """
  def task_to_module(s) do
    Regex.split(%r/\./, s) />
      Enum.map(to_upper(&1)) />
      Enum.join(".")
  end

  defp to_upper(<<s, t|:binary>>), do: <<:string.to_upper(s)>> <> t
  defp to_upper(<<>>), do: <<>>

  defp to_lower(<<s, t|:binary>>), do: <<:string.to_lower(s)>> <> t
  defp to_lower(<<>>), do: <<>>
end
