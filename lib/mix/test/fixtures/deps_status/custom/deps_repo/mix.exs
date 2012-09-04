defmodule DepsRepo do
  use Mix.Project

  def project do
    [
      app: :deps_repo,
      version: "0.1.0",
      deps: [
        { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo") }
      ]
    ]
  end
end