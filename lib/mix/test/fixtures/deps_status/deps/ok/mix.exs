defmodule Ok.MixProject do
  use Mix.Project

  def project do
    [
      app: :ok,
      version: "0.1.0"
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end
end
