defmodule MyProject do
  use Mix.Project

  def project do
    [
      app: :my_project,
      version: "0.1.0",
      test_pattern: "test/**/test_*.exs",
      test_helper: "test/my_helper.exs",
      compile_first: ["lib/c.ex"]
    ]
  end
end