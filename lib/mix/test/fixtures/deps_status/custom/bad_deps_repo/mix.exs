defmodule BadDepsRepo do
  use Mix.Project

  def project do
    [
      app: :bad_deps_repo,
      version: "0.1.0",
      deps: [
        {:git_repo, "0.0.0", git: MixTest.Case.fixture_path("bad_git_repo")}
      ]
    ]
  end
end
