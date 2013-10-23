defmodule Mix.Mixfile do
  use Mix.Project

  def project do
    [ app: :mix,
      version: System.version,
      escript_main_module: Mix.CLI ]
  end

  def application do
    [ registered: [Mix.Server],
      mod: { Mix, [] } ]
  end
end
