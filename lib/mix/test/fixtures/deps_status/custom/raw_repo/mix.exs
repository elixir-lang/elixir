defmodule RawRepo.Mix do
  use Mix.Project

  def project do
    Mix.shell.info ":raw_repo env is #{Mix.env}"
    [app: :raw_repo, version: "0.1.0"]
  end
end
