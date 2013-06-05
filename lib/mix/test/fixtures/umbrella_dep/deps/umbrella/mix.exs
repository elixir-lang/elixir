defmodule Umbrella.Mixfile do
  use Mix.Project

  def project do
    [ apps_path: "apps",
      deps: deps ]
  end

  defp deps do
    [ { :some_dep, path: "deps/some_dep" } ]
  end
end
