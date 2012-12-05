defmodule ExUnit.Mixfile do
  use Mix.Project

  def project do
    [app: :ex_unit, version: System.version]
  end

  def application do
    [ registered: [ExUnit.Server],
      mod: { ExUnit, [] } ]
  end
end