defmodule EEx.MixFile do
  use Mix.Project

  def project do
    [app: :eex, version: System.version]
  end
end