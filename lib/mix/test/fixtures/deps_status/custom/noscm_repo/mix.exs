defmodule NoSCMRepo do
  use Mix.Project

  def project do
    [
      app: :noscm_repo,
      version: "0.1.0",
      deps: [
        {:git_repo, "0.1.0"}
      ]
    ]
  end
end
