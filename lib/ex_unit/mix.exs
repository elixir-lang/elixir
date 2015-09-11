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

       assert_receive_timeout: 100,
       autorun: true,
       capture_log: false,
       colors: [],
       exclude: [],
       include: [],
       formatters: [ExUnit.CLIFormatter],
       refute_receive_timeout: 100,
       stacktrace_depth: 20,
       timeout: 60_000,
       trace: false]]
  end
end
