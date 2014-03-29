defmodule Mix.Task do
  use Behaviour

  @moduledoc """
  A simple module that provides conveniences for creating,
  loading and manipulating tasks.

  A Mix task can be defined by simply using `Mix.Task`
  in a module starting with `Mix.Tasks.` and defining
  the `run/1` function:

      defmodule Mix.Tasks.Hello do
        use Mix.Task

        def run(_) do
          IO.puts "hello"
        end
      end

  The `run/1` function will receive all arguments passed
  to the command line.

  ## Attributes

  There are a couple attributes available in Mix tasks to
  configure them in Mix:

  * `@shortdoc` - makes the task public with a short description that appears on `mix help`
  * `@recursive` - run the task recursively in umbrella projects

  """

  @doc """
  A task needs to implement `run` which receives
  a list of command line args.
  """
  defcallback run([binary]) :: any

  @doc false
  defmacro __using__(_opts) do
    quote do
      Enum.each [:shortdoc, :recursive],
        &Module.register_attribute(__MODULE__, &1, persist: true)

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
      { :ok, files } = :erl_prim_loader.list_dir(path |> to_char_list)
      Enum.reduce(files, matches, &match_tasks/2)
    end)
  end

  @re_pattern Regex.re_pattern(~r/Elixir\.Mix\.Tasks\..*\.beam$/)

  defp match_tasks(filename, modules) do
    if :re.run(filename, @re_pattern, [capture: :none]) == :match do
      mod = Path.rootname(filename, '.beam') |> list_to_atom
      if Code.ensure_loaded?(mod), do: [mod | modules], else: modules
    else
      modules
    end
  end

  @doc """
  Returns all loaded tasks.

  Modules that are not yet loaded won't show up.
  Check `load_all/0` if you want to preload all tasks.
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
  Returns nil if the task cannot be found.
  """
  def get(task) do
    case Mix.Utils.command_to_module(task, Mix.Tasks) do
      { :module, module } -> module
      { :error, _ } -> nil
    end
  end

  @doc """
  Receives a task name and retrieves the task module.

  ## Exceptions

  * `Mix.NoTaskError` - raised if the task could not be found;
  * `Mix.InvalidTaskError` - raised if the task is not a valid `Mix.Task`

  """
  def get!(task) do
    if module = get(task) do
      if is_task?(module) do
        module
      else
        raise Mix.InvalidTaskError, task: task
      end
    else
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
  or it is invalid. Check `get!/1` for more information.
  """
  def run(task, args \\ []) do
    task = to_string(task)

    if Mix.TasksServer.call({ :run_task, task, Mix.Project.get }) do
      module = get!(task)

      recur module, fn proj ->
        Mix.TasksServer.cast({ :put_task, task, proj })
        module.run(args)
      end
    else
      :noop
    end
  end

  @doc """
  Clears all invoked tasks, allowing them to be reinvoked.
  """
  def clear do
    Mix.TasksServer.call(:clear_tasks)
  end

  @doc """
  Reenables a given task so it can be executed again down the stack. If
  an umbrella project reenables a task it is reenabled for all sub projects.
  """
  def reenable(task) do
    task   = to_string(task)
    module = get!(task)

    recur module, fn project ->
      Mix.TasksServer.cast({ :delete_task, task, project })
    end
  end

  defp recur(module, fun) do
    umbrella? = Mix.Project.umbrella?
    recursive = recursive(module)

    if umbrella? && recursive && Mix.ProjectStack.enable_recursion do
      config = [build_path: Mix.Project.build_path]
      res = for %Mix.Dep{app: app, opts: opts} <- Mix.Dep.Umbrella.loaded do
        Mix.Project.in_project(app, opts[:path], config, fun)
      end
      Mix.ProjectStack.disable_recursion
      res
    else
      fun.(Mix.Project.get)
    end
  end

  @doc """
  Returns `true` if given module is a task.
  """
  def is_task?(module) do
    function_exported?(module, :run, 1)
  end
end
