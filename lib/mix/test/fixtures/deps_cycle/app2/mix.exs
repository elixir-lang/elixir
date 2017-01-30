defmodule App2 do
  use Mix.Project

  def project do
    [
      app: :app2,
      version: "0.1.0",
      deps: [
        {:app1, "0.1.0", in_umbrella: true}
      ]
    ]
  end
end
