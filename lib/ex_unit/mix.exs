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
       # seed: rand(),
       # timeout: 30_000

       autorun: true,
       colors: [],
       exclude: [],
       include: [],
       formatters: [ExUnit.CLIFormatter],
       trace: false]]
  end
end
