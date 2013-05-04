defmodule Umbrella.Mixfile do
  use Mix.Project

  def project do
    [ app: :umbrella,
      version: "0.1.0",
      apps_path: "apps" ]
  end
end
