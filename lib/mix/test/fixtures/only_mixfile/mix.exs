defmodule MyProject do
  use Mix.Project

  def project do
    [ app: :no_mixfile,
      version: "0.1.0" ]
  end

  def hello_world do
    "Hello from MyProject!"
  end
end
