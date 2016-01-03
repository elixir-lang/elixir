defmodule Mix.NoTaskError do
  defexception [:task, :message, :mix]

  def exception(opts) do
    task = opts[:task]
    %Mix.NoTaskError{task: task, message: msg(task)}
  end

  defp msg(task) do
    msg = "The task #{inspect task} could not be found"
    case did_you_mean(task) do
      {mod, ^task, _score} ->
        msg <> " because the module is named #{inspect mod} instead of " <>
               "#{expected_mod_name(task)} as expected. " <>
               "Please rename it and try again"

      {_mod, similar, score} when score > 0.8 ->
        msg <> ". Did you mean #{inspect similar}?"

      _otherwise -> msg
    end
  end

  defp did_you_mean(task) do
    Mix.Task.load_all # Ensure all tasks are loaded
    Mix.Task.all_modules
    |> Enum.map(&{&1, Mix.Task.task_name(&1)})
    |> Enum.reduce({nil, nil, 0}, &max_similar(&1, task, &2))
  end

  defp max_similar({mod, source}, target, {_, _, current} = best) do
    score = String.jaro_distance(source, target)
    if score < current, do: best, else: {mod, source, score}
  end

  defp expected_mod_name(task) do
    "Mix.Tasks." <> Mix.Utils.command_to_module_name(task)
  end
end

defmodule Mix.InvalidTaskError do
  defexception [:task, :message, :mix]

  def exception(opts) do
    task = opts[:task]
    %Mix.InvalidTaskError{task: task, message: "The task #{inspect task} does not export run/1"}
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
