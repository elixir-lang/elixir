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
    Enum.each [:shortdoc, :hidden],
      Module.register_attribute __CALLER__.module, &1, accumulate: false

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
  Run a `task` with the given `args`.

  If the task was not yet invoked, it returns `:ok`.

  If the task was already invoked, it does not run the task
  again and simply aborts with `:noop`.

  It may raise an exception if the task was not found
  or it is invalid. Check `run!/2` for more information.
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

  ## Exceptions

  * `Mix.NoTaskError` - raised if the task could not be found;
  * `Mix.InvalidTaskError` - raised if the task is not a valid `Mix.Task`;

  """
  def run!(task, args // []) do
    { :ok, ordset } = :application.get_env(:mix, :invoked_tasks)
    do_run(to_binary(task), args, ordset)
  end

  @doc """
  Clears all invoked tasks, allowing them to be reinvoked.
  Returns an ordsets with all the tasks invoked thus far.
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
    case Mix.Utils.get_module(task, Mix.Tasks) do
      { :module, module } ->
        if :erlang.function_exported(module, :run, 1) do
          # This is not free of race conditions, but this is
          # only a problem if we are running tasks in parallel.
          # A possible solution would be to provide a server
          # for mix to handle this atomically.
          :application.set_env(:mix, :invoked_tasks, Ordset.add_element(task, ordset))
          module.run(args)
          :ok
        else
          raise Mix.InvalidTaskError, task: task
        end
      { :error, _ } ->
        raise Mix.NoTaskError, task: task
    end
  end
end