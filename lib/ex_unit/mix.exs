defmodule ExUnit.Mixfile do
  use Mix.Project

  def project do
    [app: :ex_unit,
     version: System.version,
     build_per_environment: false]
  end

  def application do
    [registered: [ExUnit.Server],
     mod: {ExUnit, []},
     env: [
       # Calculated on demand
       # max_cases: :erlang.system_info(:schedulers_online),
       # color: IO.ANSI.terminal?,
       # seed: rand(),

       autorun: true,
       trace: false,
       formatters: [ExUnit.CLIFormatter],
       include: [],
       exclude: []]]
  end
end
