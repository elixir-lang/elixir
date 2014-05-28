defmodule Elixir.Mixfile do
  use Mix.Project

  def project do
    [app: :elixir,
     version: System.version,
     build_per_environment: false,
     escript_embed_elixir: false,
     escript_main_module: :elixir,
     escript_emu_args: "%%! -noshell\n"]
  end
end
