defmodule Elixir.Mixfile do
  use Mix.Project

  def project do
    [app: :elixir,
     version: System.version,
     build_per_environment: false,
     escript: [
      embed_elixir: false,
      main_module: :elixir,
      emu_args: "%%! -noshell\n"
     ]]
  end
end
