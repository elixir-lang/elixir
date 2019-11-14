defmodule TestWarning.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_warning,
      version: "0.0.1",
      test_pattern: "*_test_warning.exs"
    ]
  end
end
