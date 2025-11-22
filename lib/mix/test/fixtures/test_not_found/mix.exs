defmodule TestFailed.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_not_found,
      version: "0.0.1",
      test_load_filters: [~r/.*_test_failed\.exs/]
    ]
  end
end
