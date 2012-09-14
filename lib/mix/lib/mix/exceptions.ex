defexception Mix.NoTaskError, task: nil do
  def message(exception) do
    "The task #{task(exception)} could not be found"
  end
end

defexception Mix.InvalidTaskError, task: nil do
  def message(exception) do
    "The task #{task(exception)} does not respond to run/1"
  end
end

defexception Mix.NoProjectError,
  message: "Could not find a Mix.Project"

defexception Mix.Error,
  message: nil

defexception Mix.OutOfDateDepsError,
  message: "Some dependencies are out of date, please run `mix deps.get` to proceed"

defexception Mix.OutOfDateNestedDepsError, app: nil do
  def message(exception) do
    app = exception.app
    "Missing dependencies for #{app}. For now, Mix cannot handle nested dependencies " <>
      "so please move #{app}'s dependencies above to your application"
  end
end