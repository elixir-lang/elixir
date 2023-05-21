defmodule Bar.MixProject do
  use Mix.Project

  def project do
    [
      app: :bar,
      version: "0.1.0",
      # Choose something besides *_test.exs so that these test files don't
      # get accidentally swept up into the actual Mix test suite.
      test_pattern: "*_tests.exs",
      test_coverage: [ignore_modules: [Bar, ~r/Ignore/]],
      aliases: [mytask: fn _ -> Mix.shell().info("bar_running") end]
    ]
  end
end
