defmodule DepsRepo do
  use Mix.Project

  def project do
    opts = Process.get(:custom_deps_git_repo_opts) || []

    [
      app: :deps_repo,
      version: "0.1.0",
      deps: [{:git_repo, "0.1.0", [git: MixTest.Case.fixture_path("git_repo")] ++ opts}]
    ]
  end
end
