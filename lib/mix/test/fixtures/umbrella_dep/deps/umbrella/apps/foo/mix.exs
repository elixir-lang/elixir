defmodule Foo.Mix do
  use Mix.Project

  def project do
    Mix.shell.info ":foo env is #{Mix.env}"
    [ app: :foo,
      version: "0.1.0" ]
  end
end
