defmodule TestFailed.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_only_failures,
      version: "0.0.1",
      test_pattern: "*_test_failed.exs"
    ]
  end
end
