defmodule App1 do
  use Mix.Project

  def project do
    [
      app: :app1,
      version: "0.1.0",
      deps: [
        {:app2, "0.1.0", in_umbrella: true}
      ]
    ]
  end
end
