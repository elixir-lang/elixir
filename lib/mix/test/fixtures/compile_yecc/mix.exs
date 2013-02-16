defmodule MyProject do
  use Mix.Project

  def project do
    [
      app: :my_project,
      version: "0.1.0",
      compilers: [:yecc],
    ]
  end
end
