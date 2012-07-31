defmodule Mix.Mixfile do
  use Mix.Project

  def project do
    [app: :mix, version: System.version]
  end

  def application do
    [
      registered: [Mix.Server]
    ]
  end
end