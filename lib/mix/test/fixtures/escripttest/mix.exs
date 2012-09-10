defmodule Escripttest.Mixfile do
  use Mix.Project

  def project do
    [ app: :escripttest,
      version: "0.0.1",
      escript_embed_elixir: true ]
  end
end
