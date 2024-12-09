defmodule TestStale.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_stale,
      version: "0.0.1",
      test_load_pattern: ~r/.*_test_stale\.exs/
    ]
  end
end
