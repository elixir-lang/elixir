defmodule Bar.Mix do
  use Mix.Project

  def project do
    [ app: :bar,
      version: "0.1.0",
      deps: [ { :foo, path: "../foo" } ] ]
  end
end
