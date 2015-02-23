defmodule Mix.NoTaskError do
  defexception [:task, :message, :mix]

  def exception(opts) do
    task = opts[:task]
    %Mix.NoTaskError{task: task, message: msg(task)}
  end

  defp msg(task) do
    msg = "The task #{task} could not be found"
    case did_you_mean(task) do
      nil -> msg
      similar -> msg <> ". Did you mean '#{similar}'?"
    end
  end

  defp did_you_mean(task) do
    Mix.Task.load_all # Ensure all tasks are loaded
    Mix.Task.all_modules
    |> Enum.map(&Mix.Task.task_name/1)
    |> Enum.find(&similar?(&1, task))
  end

  defp similar?(source, target) do
    String.jaro_distance(source, target) > 0.77
  end
end

defmodule Mix.InvalidTaskError do
  defexception [:task, :message, :mix]

  def exception(opts) do
    task = opts[:task]
    %Mix.InvalidTaskError{task: task, message: "The task #{task} does not export run/1"}
  end
end

defmodule Mix.ElixirVersionError do
  defexception [:target, :expected, :actual, :message, :mix]

  def exception(opts) do
    target   = opts[:target]
    actual   = opts[:actual]
    expected = opts[:expected]
    message  = "You're trying to run #{inspect target} on Elixir v#{actual} but it " <>
               "has declared in its mix.exs file it supports only Elixir #{expected}"
    %Mix.ElixirVersionError{target: target, expected: expected, actual: actual, message: message}
  end
end

defmodule Mix.NoProjectError do
  defexception message: "Could not find a Mix.Project, please ensure a mix.exs file is available",
               mix: nil
end

defmodule Mix.Error do
  defexception [:mix, :message]
end
