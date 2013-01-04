defmodule MyProject do
  use Mix.Project

  def project do
    [
      app: :my_project,
      version: "0.1.0",
      compilers: [:erlang],
      source_paths: ["src"],
      erlangrc_options: [{:i, "include"}]
    ]
  end
end