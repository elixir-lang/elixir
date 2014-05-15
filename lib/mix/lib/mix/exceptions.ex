defexception Mix.NoTaskError, task: nil, mix_error: true, message: nil do
  def exception(opts) do
    task = opts[:task]
    %Mix.NoTaskError{task: task, message: "The task #{task} could not be found"}
  end
end

defexception Mix.InvalidTaskError, task: nil, mix_error: true, message: nil do
  def exception(opts) do
    task = opts[:task]
    %Mix.InvalidTaskError{task: task, message: "The task #{task} does not export run/1"}
  end
end

defexception Mix.NoProjectError, mix_error: true,
  message: "Could not find a Mix.Project, please ensure a mix.exs file is available"

defexception Mix.ElixirVersionError, mix_error: true, target: nil, expected: nil, actual: nil, message: nil do
  def exception(opts) do
    target   = opts[:target]
    actual   = opts[:actual]
    expected = opts[:expected]
    message  = "You're trying to run #{inspect target} on Elixir v#{actual} but it " <>
               "has declared in its mix.exs file it supports only Elixir #{expected}"
    %Mix.ElixirVersionError{target: target, expected: expected, actual: actual, message: message}
  end
end

defexception Mix.Error, mix_error: true, message: nil
