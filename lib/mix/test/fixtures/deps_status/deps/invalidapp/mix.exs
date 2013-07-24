defmodule InvalidApp.Mixfile do
  use Mix.Project

  def project do
    [ app: :invalidapp,
      version: "1.0",
      # We are setting a custom ebin and the app
      # file will still be found and be invalid.
      compile_path: "custom_ebin" ]
  end
end