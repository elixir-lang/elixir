defexception Mix.NoTaskError, task: nil, mix_error: true do
  def message(exception) do
    "The task #{task(exception)} could not be found"
  end
end

defexception Mix.InvalidTaskError, task: nil, mix_error: true do
  def message(exception) do
    "The task #{task(exception)} does not respond to run/1"
  end
end

defexception Mix.NoProjectError, mix_error: true,
  message: "Could not find a Mix.Project"

defexception Mix.Error, mix_error: true, message: nil

defexception Mix.SystemVersionError, mix_error: true, expected: nil, actual: nil do
  def message(exception) do
    "You're trying to run the project on Elixir v#{exception.actual} " <>
    "but it's supposed to run on Elixir #{exception.expected}"
  end
end
