defmodule ExUnit.MixProject do
  use Mix.Project

  def project do
    [
      app: :ex_unit,
      version: System.version(),
      build_per_environment: false
    ]
  end

  def application do
    [
      registered: [ExUnit.CaptureServer, ExUnit.OnExitHandler, ExUnit.Server, ExUnit.Supervisor],
      mod: {ExUnit, []},
      env: [
        # Calculated on demand
        # max_cases: System.schedulers_online * 2,
        # seed: rand(),

        assert_receive_timeout: 100,
        autorun: true,
        capture_log: false,
        module_load_timeout: 60000,
        colors: [],
        exclude: [],
        formatters: [ExUnit.CLIFormatter],
        include: [],
        max_failures: :infinity,
        refute_receive_timeout: 100,
        slowest: 0,
        stacktrace_depth: 20,
        timeout: 60000,
        trace: false,
        after_suite: []
      ]
    ]
  end
end
