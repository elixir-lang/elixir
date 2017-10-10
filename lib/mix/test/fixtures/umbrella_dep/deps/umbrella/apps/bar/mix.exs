defmodule Bar.MixProject do
  use Mix.Project

  def project do
    Mix.shell().info(":bar env is #{Mix.env()}")

    [
      app: :bar,
      version: "0.1.0",
      deps: [{:foo, in_umbrella: true}]
    ]
  end
end
