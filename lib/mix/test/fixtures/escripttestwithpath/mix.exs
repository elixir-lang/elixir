defmodule Escripttestwithpath.Mixfile do
  use Mix.Project

  def project do
    [ app: :escripttestwithpath,
      version: "0.0.1",
      escript_embed_elixir: true,
      escript_name: :escripttestwithpath,
      escript_path: Path.join("ebin", "escripttestwithpath") ]
  end
end
