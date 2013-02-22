defmodule ExUnit.Mixfile do
  use Mix.Project

  def project do
    [ app: :graphviz, version: System.version ]
  end
end
