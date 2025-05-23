defmodule Foo.MixProject do
  use Mix.Project

  def project do
    [
      app: :foo,
      version: "0.1.0",
      # Choose something besides *_test.exs so that these test files don't
      # get accidentally swept up into the actual Mix test suite.
      test_load_filters: [~r/.*_tests\.exs/],
      aliases: [mytask: fn _ -> Mix.shell().info("foo_running") end]
    ]
  end
end
