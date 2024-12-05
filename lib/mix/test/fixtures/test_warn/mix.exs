defmodule TestWarn.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_warn,
      version: "0.0.1",
      test_load_pattern: ~r/.*_tests\.exs/,
      test_warn_pattern: ~r/^(?!.*_helper\.exs$)(?:.*_tests\.ex|.*\.exs)$/
    ]
  end
end
