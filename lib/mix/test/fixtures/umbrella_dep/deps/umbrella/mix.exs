defmodule Umbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      aliases: [
        test_cmd: [
          "cmd echo hello",
          "cmd echo hola"
        ]
      ]
    ]
  end
end
