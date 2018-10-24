defmodule TestPassed.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_passed,
      version: "0.1.0",
      test_pattern: "*_test_passed.exs"
    ]
  end
end
