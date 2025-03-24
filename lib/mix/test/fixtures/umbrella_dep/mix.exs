defmodule UmbrellaDep.MixProject do
  use Mix.Project

  def project do
    [
      app: :umbrella_dep,
      version: "0.1.0",
      deps: deps()
    ]
  end

  defp deps do
    [
      {:umbrella, path: "deps/umbrella"}
    ]
  end
end
