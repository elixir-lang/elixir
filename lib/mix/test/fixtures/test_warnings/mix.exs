defmodule TestWarnings.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_warnings,
      version: "0.0.1",
      test_pattern: "*_test_warnings.exs"
    ]
  end
end
