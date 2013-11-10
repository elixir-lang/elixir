defmodule Mix.Mixfile do
  use Mix.Project

  def project do
    [ app: :mix,
      version: System.version,
      escript_main_module: Mix.CLI ]
  end

  def application do
    [ registered: [Mix.TasksServer, Mix.ProjectStack],
      mod: { Mix, [] },
      env: [shell: Mix.Shell.IO,
            env: :dev,
            scm: [Mix.SCM.Git, Mix.SCM.Path]] ]
  end
end
