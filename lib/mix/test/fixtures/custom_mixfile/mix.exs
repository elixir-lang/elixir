defmodule MyProject do
  use Mix.Project

  def project do
    [
      app: :my_project,
      version: "0.1.0",
      test_pattern: "test_*.exs",
      test_helper: "test/my_helper.exs"
    ]
  end
end