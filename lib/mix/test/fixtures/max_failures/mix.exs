defmodule MaxFailures.MixProject do
  use Mix.Project

  def project do
    [
      app: :max_failures,
      version: "0.1.0",
      test_pattern: "*_test!.exs"
    ]
  end
end
