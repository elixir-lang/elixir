defmodule Mix.Task do
  @moduledoc """
  Provides conveniences for creating, loading, and manipulating Mix tasks.

  A Mix task can be defined by using `Mix.Task` in a module whose name
  begins with `Mix.Tasks.` and which defines the `run/1` function.
  Typically, task modules live inside the `lib/mix/tasks/` directory,
  and their file names use dot separators instead of underscores
  (e.g. `deps.clean.ex`) - although ultimately the file name is not
  relevant.

  For example:

      # lib/mix/tasks/echo.ex
      defmodule Mix.Tasks.Echo do
        @moduledoc "Printed when the user requests `mix help echo`"
        @shortdoc "Echoes arguments"

        use Mix.Task

        @impl Mix.Task
        def run(args) do
          Mix.shell().info(Enum.join(args, " "))
        end
      end

  The command name will correspond to the portion of the module
  name following `Mix.Tasks.`. For example, a module name of
  `Mix.Tasks.Deps.Clean` corresponds to a task name of `deps.clean`.

  The `run/1` function will receive a list of all command line
  arguments passed, according to the user's terminal.

  For example, if the `args` in the above `echo` task were
  inspected, you might see something like this:

      mix echo 'A and B' C --test
      ["A and B", "C", "--test"]

  Define the `@shortdoc` attribute if you wish to make the task
  publicly visible on `mix help`. Omit this attribute if you do
  not want your task to be listed via `mix help`.

  If a task has requirements, they can be listed using the
  `@requirements` attribute. For example:

      @requirements ["app.config"]

  Tasks typically depend on the `"app.config"` task, when they
  need to access code from the current project with all apps
  already configured, or the "app.start" task, when they also
  need those apps to be already started:

      @requirements ["app.start"]

  You can also run tasks directly with `run/2`.

  ## Attributes

  There are a few attributes available in Mix tasks to
  configure them in Mix:

    * `@shortdoc`  - makes the task public with a short description that appears
      on `mix help`
    * `@recursive` - runs the task recursively in umbrella projects
    * `@requirements` - list of required tasks to be run before the task
    * `@preferred_cli_env` - recommends an environment in which to run the task.
      It is used only if `MIX_ENV` is not yet set. Note `@preferred_cli_env` is
      not loaded from dependencies as we need to know the environment in order to
      load the dependencies themselves. In those cases, you can set the
      `preferred_cli_env` configuration under `def project` in your `mix.exs`

  ## Documentation

  Users can read the documentation for public Mix tasks by running `mix help my_task`.
  The documentation that will be shown is the `@moduledoc` of the task's module.
  """

  @type task_name :: String.t() | atom
  @type task_module :: atom

  @doc """
  A task needs to implement `run` which receives
  a list of command line args.
  """
  @callback run(command_line_args :: [binary]) :: any

  @doc false
  defmacro __using__(_opts) do
    quote do
      Enum.each(
        Mix.Task.supported_attributes(),
        &Module.register_attribute(__MODULE__, &1, persist: true)
      )

      @behaviour Mix.Task
    end
  end

  @doc false
  def supported_attributes do
    [:shortdoc, :recursive, :preferred_cli_env, :requirements]
  end

  @doc """
  Loads all tasks in all code paths.
  """
  @spec load_all() :: [task_module]
  def load_all, do: load_tasks(:code.get_path())

  @doc """
  Loads all tasks in the given `paths`.
  """
  @spec load_tasks([List.Chars.t()]) :: [task_module]
  def load_tasks(dirs) do
    # We may get duplicate modules because we look through the
    # entire load path so make sure we only return unique modules.
    for dir <- dirs,
        file <- safe_list_dir(to_charlist(dir)),
        mod = task_from_path(file),
        uniq: true,
        do: mod
  end

  defp safe_list_dir(path) do
    case File.ls(path) do
      {:ok, paths} -> paths
      {:error, _} -> []
    end
  end

  @prefix_size byte_size("Elixir.Mix.Tasks.")
  @suffix_size byte_size(".beam")

  defp task_from_path(filename) do
    base = Path.basename(filename)
    part = byte_size(base) - @prefix_size - @suffix_size

    case base do
      <<"Elixir.Mix.Tasks.", rest::binary-size(part), ".beam">> ->
        mod = :"Elixir.Mix.Tasks.#{rest}"
        ensure_task?(mod) && mod

      _ ->
        nil
    end
  end

  @doc """
  Returns all loaded task modules.

  Modules that are not yet loaded won't show up.
  Check `load_all/0` if you want to preload all tasks.
  """
  @spec all_modules() :: [task_module]
  def all_modules do
    for {module, _} <- :code.all_loaded(), task?(module), do: module
  end

  @doc """
  Gets the moduledoc for the given task `module`.

  Returns the moduledoc or `nil`.
  """
  @spec moduledoc(task_module) :: String.t() | nil | false
  def moduledoc(module) when is_atom(module) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, _, %{"en" => moduledoc}, _, _} -> moduledoc
      {:docs_v1, _, _, _, :none, _, _} -> nil
      _ -> false
    end
  end

  @doc """
  Gets the shortdoc for the given task `module`.

  Returns the shortdoc or `nil`.
  """
  @spec shortdoc(task_module) :: String.t() | nil
  def shortdoc(module) when is_atom(module) do
    case List.keyfind(module.__info__(:attributes), :shortdoc, 0) do
      {:shortdoc, [shortdoc]} -> shortdoc
      _ -> nil
    end
  end

  @doc """
  Checks if the task should be run recursively for all sub-apps in
  umbrella projects.

  Returns `true` or `false`.
  """
  @spec recursive(task_module) :: boolean
  def recursive(module) when is_atom(module) do
    case List.keyfind(module.__info__(:attributes), :recursive, 0) do
      {:recursive, [setting]} -> setting
      _ -> false
    end
  end

  @doc """
  Indicates if the current task is recursing.

  This returns true if a task is marked as recursive
  and it is being executed inside an umbrella project.
  """
  @doc since: "1.8.0"
  @spec recursing?() :: boolean
  def recursing?() do
    Mix.ProjectStack.recursing() != nil
  end

  @doc """
  Gets preferred CLI environment for the task.

  Returns environment (for example, `:test`, or `:prod`), or `nil`.
  """
  @spec preferred_cli_env(task_name) :: atom | nil
  def preferred_cli_env(task) when is_atom(task) or is_binary(task) do
    case get(task) do
      nil ->
        nil

      module ->
        case List.keyfind(module.__info__(:attributes), :preferred_cli_env, 0) do
          {:preferred_cli_env, [setting]} -> setting
          _ -> nil
        end
    end
  end

  @doc """
  Gets the list of requirements for the given task.

  Returns a list of strings, where the string is expected
  to be a task optionally followed by its arguments.
  """
  @doc since: "1.11.0"
  @spec requirements(task_module) :: []
  def requirements(module) when is_atom(module) do
    {:requirements, requirements} =
      List.keyfind(module.__info__(:attributes), :requirements, 0, {:requirements, []})

    requirements
  end

  @doc """
  Returns the task name for the given `module`.

  ## Examples

      iex> Mix.Task.task_name(Mix.Tasks.Test)
      "test"

  """
  @spec task_name(task_module) :: task_name
  def task_name(module) when is_atom(module) do
    Mix.Utils.module_name_to_command(module, 2)
  end

  @doc """
  Checks if the given `task` name is an alias.

  Returns false if the given name is not an alias or if it is not a task.

  For more information about task aliasing, take a look at the
  ["Aliases"](https://hexdocs.pm/mix/Mix.html#module-aliases) section in the
  docs for `Mix`.
  """
  @spec alias?(task_name) :: boolean
  def alias?(task) when is_binary(task) do
    alias?(String.to_atom(task))
  end

  def alias?(task) when is_atom(task) do
    Keyword.has_key?(Mix.Project.config()[:aliases], task)
  end

  @doc """
  Receives a task name and returns the corresponding task module if one exists.

  Returns `nil` if the module cannot be found, if it is an alias, or if it is
  not a valid `Mix.Task`.
  """
  @spec get(task_name) :: task_module | nil
  def get(task) do
    case fetch(task) do
      {:ok, module} -> module
      {:error, _} -> nil
    end
  end

  @doc """
  Receives a task name and retrieves the corresponding task module.

  ## Exceptions

    * `Mix.NoTaskError`      - raised if the task could not be found
    * `Mix.InvalidTaskError` - raised if the task is not a valid `Mix.Task`

  """
  @spec get!(task_name) :: task_module
  def get!(task) do
    case fetch(task) do
      {:ok, module} ->
        module

      {:error, :invalid} ->
        raise Mix.InvalidTaskError, task: task

      {:error, :not_found} ->
        raise Mix.NoTaskError, task: task
    end
  end

  defp fetch(task) when is_binary(task) or is_atom(task) do
    case Mix.Utils.command_to_module(to_string(task), Mix.Tasks) do
      {:module, module} ->
        if task?(module), do: {:ok, module}, else: {:error, :invalid}

      {:error, _} ->
        {:error, :not_found}
    end
  end

  @doc """
  Conditionally runs the task (or alias) with the given `args`.

  If there exists a task matching the given task name and it has not yet been
  invoked, this will run the task with the given `args` and return the result.

  If there is an [alias](https://hexdocs.pm/mix/Mix.html#module-aliases) defined
  for the given task name, the alias will be invoked instead of the original
  task.

  If the task or alias has already been invoked, subsequent calls to `run/2`
  will _abort_ without executing and return `:noop`.

  Remember: by default, tasks will only run _once_, even when called repeatedly!
  If you need to run a task multiple times, you need to re-enable it via
  `reenable/1` or call it using `rerun/2`.

  `run/2` raises an exception if an alias or a task cannot be found or if the
  task is invalid. See `get!/1` for more information.
  """
  @spec run(task_name, [any]) :: any
  def run(task, args \\ [])

  def run(task, args) when is_atom(task) do
    run(Atom.to_string(task), args)
  end

  def run(task, args) when is_binary(task) do
    proj = Mix.Project.get()
    alias = Mix.Project.config()[:aliases][String.to_atom(task)]

    cond do
      alias && Mix.TasksServer.run({:alias, task, proj}) ->
        run_alias(List.wrap(alias), args, proj, task, :ok)

      !Mix.TasksServer.get({:task, task, proj}) ->
        run_task(proj, task, args)

      true ->
        :noop
    end
  end

  defp run_task(proj, task, args) do
    # 1. If the task is available, we run it.
    # 2. Otherwise we compile and load dependencies
    # 3. Finally, we compile the current project in hope it is available.
    module =
      get_task_or_run(proj, task, fn -> run("deps.loadpaths") end) ||
        get_task_or_run(proj, task, fn -> run("compile", []) end) ||
        get!(task)

    recursive = recursive(module)

    cond do
      recursive && Mix.Project.umbrella?() ->
        Mix.ProjectStack.recur(fn ->
          recur(fn _ -> run(task, args) end)
        end)

      not recursive && Mix.ProjectStack.recursing() ->
        Mix.ProjectStack.on_recursing_root(fn -> run(task, args) end)

      Mix.TasksServer.run({:task, task, proj}) ->
        run_requirements(module)

        with_debug(task, args, proj, fn ->
          try do
            module.run(args)
          rescue
            e in OptionParser.ParseError ->
              Mix.raise("Could not invoke task #{inspect(task)}: " <> Exception.message(e))
          end
        end)

      true ->
        :noop
    end
  end

  defp run_requirements(module) do
    Enum.each(requirements(module), fn requirement ->
      [task | args] = OptionParser.split(requirement)
      run(task, args)
    end)
  end

  defp with_debug(task, args, proj, fun) do
    if Mix.debug?() do
      shell = Mix.shell()
      shell.info(["-> Running mix ", task_to_string(task, args), project_to_string(proj)])
      {time, res} = :timer.tc(fun)
      shell.info(["<- Ran mix ", task, " in ", Integer.to_string(div(time, 1000)), "ms"])
      res
    else
      fun.()
    end
  end

  defp project_to_string(nil), do: ""
  defp project_to_string(proj), do: " (inside #{inspect(proj)})"

  defp task_to_string(task, []), do: task
  defp task_to_string(task, args), do: task <> " " <> Enum.join(args, " ")

  defp get_task_or_run(proj, task, fun) do
    cond do
      module = get(task) ->
        module

      proj ->
        fun.()
        nil

      true ->
        nil
    end
  end

  defp run_alias([h | t], alias_args, proj, original_task, _res) when is_binary(h) do
    case OptionParser.split(h) do
      [^original_task | args] ->
        res = run_task(proj, original_task, args ++ alias_args)
        run_alias(t, [], proj, original_task, res)

      [task | args] ->
        res = run(task, join_args(args, alias_args, t))
        run_alias(t, alias_args, proj, original_task, res)
    end
  end

  defp run_alias([h | t], alias_args, proj, original_task, _res) when is_function(h, 1) do
    res = h.(join_args([], alias_args, t))
    run_alias(t, alias_args, proj, original_task, res)
  end

  defp run_alias([], _alias_args, proj, original_task, res) do
    Mix.TasksServer.put({:task, original_task, proj})
    res
  end

  defp join_args(args, alias_args, []), do: args ++ alias_args
  defp join_args(args, _alias_args, _), do: args

  @doc """
  Clears all invoked tasks, allowing them to be reinvoked.

  This operation is not recursive.
  """
  @spec clear :: :ok
  def clear do
    Mix.TasksServer.clear()
  end

  @doc """
  Reenables a given task so it can be executed again down the stack.

  Both alias and the regular stack are reenabled when this function
  is called.

  If an umbrella project reenables a task, it is reenabled for all
  child projects.
  """
  @spec reenable(task_name) :: :ok
  def reenable(task) when is_binary(task) or is_atom(task) do
    task = to_string(task)
    proj = Mix.Project.get()
    recursive = (module = get(task)) && recursive(module)

    Mix.TasksServer.delete_many([{:task, task, proj}, {:alias, task, proj}])

    cond do
      recursive && Mix.Project.umbrella?() ->
        recur(fn proj ->
          Mix.TasksServer.delete_many([{:task, task, proj}, {:alias, task, proj}])
        end)

      proj = !recursive && Mix.ProjectStack.recursing() ->
        Mix.TasksServer.delete_many([{:task, task, proj}, {:alias, task, proj}])

      true ->
        :ok
    end

    :ok
  end

  defp recur(fun) do
    # Get all dependency configuration but not the deps path
    # as we leave the control of the deps path still to the
    # umbrella child.
    config = Mix.Project.deps_config() |> Keyword.delete(:deps_path)

    for %Mix.Dep{app: app, opts: opts} <- Mix.Dep.Umbrella.cached() do
      Mix.Project.in_project(app, opts[:path], [inherit_parent_config_files: true] ++ config, fun)
    end
  end

  @doc """
  Reruns `task` with the given arguments.

  This function reruns the given task; to do that, it first re-enables the task
  and then runs it as normal.
  """
  @spec rerun(task_name, [any]) :: any
  def rerun(task, args \\ []) do
    reenable(task)
    run(task, args)
  end

  @doc """
  Returns `true` if given module is a task.
  """
  @spec task?(task_module) :: boolean
  def task?(module) when is_atom(module) do
    match?('Elixir.Mix.Tasks.' ++ _, Atom.to_charlist(module)) and ensure_task?(module)
  end

  defp ensure_task?(module) do
    Code.ensure_loaded?(module) and function_exported?(module, :run, 1)
  end
end
