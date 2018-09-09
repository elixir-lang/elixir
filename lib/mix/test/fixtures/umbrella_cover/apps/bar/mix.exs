defmodule Bar.MixProject do
  use Mix.Project

  def project do
    [
      app: :bar,
      version: "0.1.0",
      # Choose something besides *_test.exs so that these test files don't
      # get accidentally swept up into the actual Mix test suite.
      test_pattern: "*_tests.exs"
    ]
  end
end
