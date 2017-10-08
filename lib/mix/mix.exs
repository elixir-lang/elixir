defmodule Mix.MixProject do
  use Mix.Project

  def project do
    [
      app: :mix,
      build_per_environment: false,
      version: System.version(),
      escript: [main_module: Mix.CLI]
    ]
  end

  def application do
    [
      registered: [Mix.State, Mix.TasksServer, Mix.ProjectStack],
      mod: {Mix, []},
      env: [colors: []]
    ]
  end
end
