defmodule Mix.NoTaskError do
  defexception task: nil, mix_error: true, message: nil

  def exception(opts) do
    task = opts[:task]
    %Mix.NoTaskError{task: task, message: "The task #{task} could not be found"}
  end
end

defmodule Mix.InvalidTaskError do
  defexception task: nil, mix_error: true, message: nil

  def exception(opts) do
    task = opts[:task]
    %Mix.InvalidTaskError{task: task, message: "The task #{task} does not export run/1"}
  end
end

defmodule Mix.NoProjectError do
  defexception mix_error: true,
               message: "Could not find a Mix.Project, please ensure a mix.exs file is available"
end

defmodule Mix.ElixirVersionError do
  defexception mix_error: true, target: nil, expected: nil,
               actual: nil, message: nil

  def exception(opts) do
    target   = opts[:target]
    actual   = opts[:actual]
    expected = opts[:expected]
    message  = "You're trying to run #{inspect target} on Elixir v#{actual} but it " <>
               "has declared in its mix.exs file it supports only Elixir #{expected}"
    %Mix.ElixirVersionError{target: target, expected: expected, actual: actual, message: message}
  end
end

defmodule Mix.Error do
  defexception mix_error: true, message: nil
end

