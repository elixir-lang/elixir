defmodule OnlyDeps.Mix do
  use Mix.Project

  def project do
    [app: :only_deps, version: "0.1.0",
     deps: [{ :git_repo, git: MixTest.Case.fixture_path("git_repo"), only: :other_env }] ]
  end
end
