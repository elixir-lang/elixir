defmodule TestFailedParameterize.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_failed_parameterize,
      version: "0.0.1",
      test_load_filters: [~r/.*_test_failed\.exs/]
    ]
  end
end
