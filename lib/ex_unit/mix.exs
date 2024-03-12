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
        colors: [],
        exclude: [],
        exit_status: 2,
        formatters: [ExUnit.CLIFormatter],
        include: [],
        max_failures: :infinity,
        rand_algorithm: :exsss,
        refute_receive_timeout: 100,
        slowest: 0,
        slowest_modules: 0,
        stacktrace_depth: 20,
        timeout: 60000,
        trace: false,
        after_suite: [],
        repeat_until_failure: 0
      ]
    ]
  end
end
