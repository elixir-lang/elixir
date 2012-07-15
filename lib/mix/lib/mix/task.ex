defmodule Mix.Task do
  @moduledoc """
  A simple module that provides conveniences for creating tasks.
  """

  @doc false
  def behaviour_info(:callbacks) do
    [run: 1]
  end

  @doc false
  defmacro __using__(_opts) do
    quote do
      @behavior Mix.Task
    end
  end

  # Starts up project related environment variables.
  @doc false
  def start do
    :application.set_env(:mix, :invoked_tasks, [])
  end

  @doc """
  Run a task with arguments. It returns `:ok` if the task
  could be ran with success. It raises an exception if
  the task does not exist or it is invalid (i.e. it does
  not implement the expected API).
  """
  def run(name, args // []) do
    case Mix.Utils.get_module(name, Mix.Tasks) do
      { :module, module } ->
        if :erlang.function_exported(module, :run, 1) do
          module.run(args)
          :ok
        else
          raise Mix.InvalidTaskError, task: name
        end
      { :error, _ } ->
        raise Mix.NoTaskError, task: name
    end
  end
end