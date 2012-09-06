defmodule IEx.Mixfile do
  use Mix.Project

  def project do
    [app: :iex, version: System.version]
  end

  def application do
    []
  end
end