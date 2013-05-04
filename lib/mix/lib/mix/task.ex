defmodule Mix.Task do
  use Behaviour
  alias :ordsets, as: Ordset

  @moduledoc """
  A simple module that provides conveniences for creating tasks.
  """

  @doc """
  A task needs to implement run which receives
  a list of command line args.
  """
  defcallback run([binary]) :: any

  @doc false
  defmacro __using__(_opts) do
    Enum.each [:shortdoc, :hidden, :recursive],
      Module.register_attribute __CALLER__.module, &1, accumulate: false

    quote do
      @behaviour Mix.Task
    end
  end

  @doc """
  Loads all tasks in all code paths.
  """
  def load_all do
    Enum.each :code.get_path, fn(codepath) ->
      files = Path.wildcard(codepath ++ '/Elixir-Mix-Tasks-*.beam')
      Enum.each files, &1 |> Path.basename |> Path.rootname('.beam') |> list_to_atom |> Code.ensure_loaded
    end
  end

  @doc """
  Returns all loaded modules. Modules that were not yet loaded
  won't show up. Check `load_all/0` if you want to preload all tasks.
  """
  def all_modules do
    Enum.reduce :code.all_loaded, [], fn({ module, _ }, acc) ->
      case atom_to_list(module) do
        'Elixir-Mix-Tasks-' ++ _ ->
          if is_task?(module), do: [module|acc], else: acc
        _ ->
          acc
      end
    end
  end

  @doc """
  Gets the moduledoc for the given module.
  Returns the moduledoc or `nil`.
  """
  def moduledoc(module) when is_atom(module) do
    case module.__info__(:moduledoc) do
      { _line, moduledoc } -> moduledoc
      nil -> nil
    end
  end

  @doc """
  Gets the shortdoc for the given module.
  Returns the shortdoc or `nil`.
  """
  def shortdoc(module) when is_atom(module) do
    case List.keyfind module.__info__(:attributes), :shortdoc, 0 do
      { :shortdoc, [shortdoc] } -> shortdoc
      _ -> nil
    end
  end

  @doc """
  Checks if the task is hidden or not. Returns a boolean.
  """
  def hidden?(module) when is_atom(module) do
    case List.keyfind module.__info__(:attributes), :hidden, 0 do
      { :hidden, [bool] } -> bool
      _ -> false
    end
  end

  @doc """
  Checks if the task is defined for umbrella projects.
  """
  def recursive?(module) when is_atom(module) do
    case List.keyfind module.__info__(:attributes), :recursive, 0 do
      { :recursive, [bool] } -> bool
      _ -> false
    end
  end

  @doc """
  Returns the task name for the given module.
  """
  def task_name(module) do
    Mix.Utils.module_name_to_command(module, 2)
  end

  @doc """
  Receives a task name and retrives the task module.

  ## Exceptions

  * `Mix.NoTaskError` - raised if the task could not be found;
  * `Mix.InvalidTaskError` - raised if the task is not a valid `Mix.Task`
  """
  def get(task) do
    case Mix.Utils.command_to_module(task, Mix.Tasks) do
      { :module, module } ->
        if is_task?(module) do
          module
        else
          raise Mix.InvalidTaskError, task: task
        end
      { :error, _ } ->
        raise Mix.NoTaskError, task: task
    end
  end

  @doc """
  Runs a `task` with the given `args`.

  If the task was not yet invoked, it returns `:ok`.

  If the task was already invoked, it does not run the task
  again and simply aborts with `:noop`.

  It may raise an exception if the task was not found
  or it is invalid. Check `get/2` for more information.
  """
  def run(task, args // []) do
    task = to_binary(task)

    if Mix.Server.call({ :has_task?, task }) do
      :noop
    else
      module = get(task)
      Mix.Server.cast({ :add_task, task })

      if Mix.Project.umbrella? && recursive?(module) do
        Mix.Project.recursive(fn (_, _) -> module.run(args) end)
      else
        module.run(args)
      end
    end
  end

  @doc """
  Clears all invoked tasks, allowing them to be reinvoked.
  Returns an ordset with all the tasks invoked thus far.
  """
  def clear do
    Mix.Server.call(:clear_tasks)
  end

  @doc """
  Reenables a given task so it can be executed again down the stack.
  """
  def reenable(task) do
    Mix.Server.cast({ :delete_task, to_binary(task) })
  end

  # Used internally by Mix to swap tasks in and out when
  # moving in between projects.
  @doc false
  def set_tasks(tasks) do
    Mix.Server.cast({ :set_tasks, tasks })
  end

  defp is_task?(module) do
    function_exported?(module, :run, 1)
  end
end
