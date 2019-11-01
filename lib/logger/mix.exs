defmodule Logger.MixProject do
  use Mix.Project

  def project do
    [
      app: :logger,
      version: System.version(),
      build_per_environment: false
    ]
  end

  def application do
    [
      registered: [Logger, Logger.BackendSupervisor, Logger.Supervisor, Logger.Watcher],
      mod: {Logger.App, []},
      env: [
        utc_log: false,
        truncate: 8096,
        backends: [:console],
        translators: [{Logger.Translator, :translate}],
        sync_threshold: 20,
        discard_threshold: 500,
        handle_otp_reports: true,
        handle_sasl_reports: false,
        discard_threshold_periodic_check: 30_000,
        discard_threshold_for_error_logger: 500,
        compile_time_purge_matching: [],
        compile_time_application: nil,
        translator_inspect_opts: [],
        console: [],
        start_options: []
      ]
    ]
  end
end
