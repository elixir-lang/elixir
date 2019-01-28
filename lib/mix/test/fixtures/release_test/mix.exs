defmodule ReleaseTest.MixProject do
  use Mix.Project

  def project do
    [
      app: :release_test,
      version: "0.1.0"
    ]
  end

  def application do
    [
      extra_applications: [:logger, :runtime_tools],
      mod: {ReleaseTest, []}
    ]
  end
end
