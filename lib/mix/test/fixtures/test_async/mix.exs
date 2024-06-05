defmodule TestAsync.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_async,
      version: "0.0.1",
      test_pattern: "*_test_async.exs"
    ]
  end
end
