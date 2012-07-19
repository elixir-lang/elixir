defmodule ExUnit.MixFile do
  use Mix.Project

  def project do
    [app: "ex_unit", version: System.version]
  end

  def application do
    [
      registered: [ExUnit.Server]
    ]
  end
end