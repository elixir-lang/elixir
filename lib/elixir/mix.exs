defmodule Elixir.Mixfile do
  use Mix.Project

  def project do
    [app: :elixir,
     version: System.version,
     build_per_environment: false,
     deps: deps,
     elixirc_paths: ["lib", "unicode"]]
  end
  
  defp deps do
    [{:benchfella, "~> 0.3.0"}]
  end
end
