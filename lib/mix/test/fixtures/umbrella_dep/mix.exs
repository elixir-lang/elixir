defmodule UmbrellaDep.MixProject do
  use Mix.Project

  def project do
    [
      app: :umbrella_dep,
      deps: deps()
    ]
  end

  defp deps do
    [
      {:umbrella, path: "deps/umbrella"}
    ]
  end
end
