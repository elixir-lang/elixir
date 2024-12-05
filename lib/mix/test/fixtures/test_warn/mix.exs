defmodule TestWarn.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_warn,
      version: "0.0.1",
      test_load_filters: [~r/.*_tests\.exs/],
      test_warn_filters: [
        "test/test_helper.exs",
        ~r/ignored_regex/,
        fn file -> file == "test/ignored_file.exs" end
      ]
    ]
  end
end
