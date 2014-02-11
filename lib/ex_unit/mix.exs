defmodule ExUnit.Mixfile do
  use Mix.Project

  def project do
    [app: :ex_unit, version: System.version, build_per_environment: false]
  end

  def application do
    [ registered: [ExUnit.Server],
      mod: { ExUnit, [] },
      env: [
        autorun: true,
        trace: false,
        formatters: [ExUnit.CLIFormatter],
        include: [],
        exclude: [] ] ]
  end
end
