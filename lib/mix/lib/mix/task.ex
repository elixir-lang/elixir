defmodule Mix.Task do
  use Behaviour
  alias :ordsets, as: Ordset

  @moduledoc """
  A simple module that provides conveniences for creating,
  loading and manipulating tasks.
  """

  @doc """
  A task needs to implement `run` which receives
  a list of command line args.
  """
  defcallback run([binary]) :: any

  @doc false
  defmacro __using__(_opts) do
    quote do
      Enum.each [:shortdoc, :hidden, :recursive],
        Module.register_attribute(__MODULE__, &1, persist: true)

      @behaviour Mix.Task
    end
  end

  @doc """
  Loads all tasks in all code paths.
  """
  def load_all, do: load_tasks(:code.get_path)

  @doc """
  Loads all tasks in the given `paths`.
  """
  def load_tasks(paths) do
    Enum.reduce(paths, [], fn(path, matches) ->
      { :ok, files } = :erl_prim_loader.list_dir(path |> :unicode.characters_to_list)
      Enum.reduce(files, matches, match_tasks(&1, &2))
    end)
  end

  defp match_tasks(file_name, modules) do
    if Regex.match?(%r/Elixir\.Mix\.Tasks\..*\.beam/, file_name) do
      mod = Path.rootname(file_name, '.beam') |> list_to_atom
      if Code.ensure_loaded?(mod), do: [mod | modules], else: modules
    else
      modules
    end
  end

  @doc """
  Returns all loaded tasks. Modules that are not yet loaded
  won't show up. Check `load_all/0` if you want to preload all tasks.
  """
  def all_modules do
    Enum.reduce :code.all_loaded, [], fn({ module, _ }, acc) ->
      case atom_to_list(module) do
        'Elixir.Mix.Tasks.' ++ _ ->
          if is_task?(module), do: [module|acc], else: acc
        _ ->
          acc
      end
    end
  end

  @doc """
  Gets the moduledoc for the given task `module`.
  Returns the moduledoc or `nil`.
  """
  def moduledoc(module) when is_atom(module) do
    case module.__info__(:moduledoc) do
      { _line, moduledoc } -> moduledoc
      nil -> nil
    end
  end

  @doc """
  Gets the shortdoc for the given task `module`.
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
  Checks if the task should be run recursively for all sub-apps in
  umbrella projects. Returns `true`, `false` or `:both`.
  """
  def recursive(module) when is_atom(module) do
    case List.keyfind module.__info__(:attributes), :recursive, 0 do
      { :recursive, [setting] } -> setting
      _ -> false
    end
  end

  @doc """
  Returns the task name for the given `module`.
  """
  def task_name(module) do
    Mix.Utils.module_name_to_command(module, 2)
  end

  @doc """
  Receives a task name and retrieves the task module.

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

  If the task was not yet invoked, it runs the task and
  returns the result.

  If the task was already invoked, it does not run the task
  again and simply aborts with `:noop`.

  It may raise an exception if the task was not found
  or it is invalid. Check `get/1` for more information.
  """
  def run(task, args // []) do
    task = to_binary(task)
    app = Mix.project[:app]

    if Mix.Server.call({ :has_task?, task, app }) do
      :noop
    else
      module = get(task)
      Mix.Server.cast({ :add_task, task, app })

      recursive = recursive(module)

      if recursive && Mix.Server.call(:recursive_enabled?) do
        Mix.Server.cast({ :recursive_enabled?, false })
        res = if Mix.Project.umbrella? and recursive == :both do
          [module.run(args)]
        else
          []
        end
        res = res ++ Mix.Project.recur(fn _ -> module.run(args) end)
        Mix.Server.cast({ :recursive_enabled?, true })
        res
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
  Reenables a given task so it can be executed again down the stack. If
  an umbrella project reenables a task it is reenabled for all sub projects.
  """
  def reenable(task) do
    if Mix.Project.umbrella? do
      Mix.Server.cast({ :delete_task, to_binary(task) })
    else
      Mix.Server.cast({ :delete_task, to_binary(task), Mix.project[:app] })
    end
  end

  defp is_task?(module) do
    function_exported?(module, :run, 1)
  end
end
