defmodule RawRepo.Mix do
  use Mix.Project

  def project do
    Process.put(:raw_repo_env, Mix.env)
    [app: :raw_repo, version: "0.1.0"]
  end
end
