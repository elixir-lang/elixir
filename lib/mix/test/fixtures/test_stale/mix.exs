defmodule TestStale.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_stale,
      version: "0.0.1",
      test_load_filters: [fn file -> String.ends_with?(file, "_test_stale.exs") end]
    ]
  end
end
