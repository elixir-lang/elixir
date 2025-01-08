defmodule OverrideForRepo do
  use Mix.Project

  def project do
    [
      app: :override_for_repo,
      version: "0.1.0",
      deps: [{:git_repo, "0.3.0", [git: MixTest.Case.fixture_path("git_repo")]}]
    ]
  end
end
