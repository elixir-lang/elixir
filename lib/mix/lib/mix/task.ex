defmodule Mix.Task do
  alias :ordsets, as: Ordset

  @moduledoc """
  A simple module that provides conveniences for creating tasks.
  """

  @doc false
  def behaviour_info(:callbacks) do
    [run: 1]
  end

  @doc false
  defmacro __using__(_opts) do
    Module.register_attribute __CALLER__.module, :shortdoc, accumulate: false

    quote do
      @behavior Mix.Task
    end
  end

  # Starts up project related environment variables.
  @doc false
  def start do
    :application.set_env(:mix, :invoked_tasks, Ordset.new)
  end

  @doc """
  Loads all tasks in all code paths.
  """
  def load_all do
    Enum.each :code.get_path, fn(codepath) ->
      files = File.wildcard(codepath ++ '/__MAIN__-Mix-Tasks-*.beam')
      Enum.each files, &1 /> File.basename /> File.rootname('.beam') /> list_to_atom /> Code.ensure_loaded
    end
  end

  @doc """
  Returns all loaded modules. Modules that were not yet loaded
  won't show up. Check `load_all/0` if you want to preload all tasks.
  """
  def all_modules do
    Enum.reduce :code.all_loaded, [], fn({ module, _ }, acc) ->
      case atom_to_list(module) do
        '__MAIN__-Mix-Tasks-' ++ _ ->
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
    case List.keyfind module.__info__(:attributes), :shortdoc, 1 do
      { :shortdoc, [shortdoc] } -> shortdoc
      _ -> nil
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
    case Mix.Utils.get_module(task, Mix.Tasks) do
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
    { :ok, ordset } = :application.get_env(:mix, :invoked_tasks)

    if Ordset.is_element(task, ordset) do
      :noop
    else
      do_run(task, args, ordset)
    end
  end

  @doc """
  Run a `task` with the given `args` regardless if
  it was executed previously or not. It returns `:ok`
  if the task ran with success, otherwise raises an
  exception.

  It may raise an exception if the task was not found
  or it is invalid. Check `get/2` for more information.
  """
  def run!(task, args // []) do
    { :ok, ordset } = :application.get_env(:mix, :invoked_tasks)
    do_run(to_binary(task), args, ordset)
  end

  @doc """
  Clears all invoked tasks, allowing them to be reinvoked.
  Returns an ordset with all the tasks invoked thus far.
  """
  def clear do
    { :ok, ordset } = :application.get_env(:mix, :invoked_tasks)
    :application.set_env(:mix, :invoked_tasks, Ordset.new)
    ordset
  end

  @doc """
  Reenables a given task so it can be executed again down the stack.
  """
  def reenable(task) do
    task = to_binary(task)
    { :ok, ordset } = :application.get_env(:mix, :invoked_tasks)
    :application.set_env(:mix, :invoked_tasks, Ordset.del_element(task, ordset))
  end

  defp do_run(task, args, ordset) do
    module = get(task)

    # This is not free of race conditions, but this is
    # only a problem if we are running tasks in parallel.
    # A possible solution would be to provide a server
    # for mix to handle this atomically.
    :application.set_env(:mix, :invoked_tasks, Ordset.add_element(task, ordset))

    module.run(args)
    :ok
  end

  defp is_task?(module) do
    function_exported?(module, :run, 1)
  end
end