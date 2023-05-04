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
      registered: [Logger, Logger.Supervisor],
      mod: {Logger.App, []},
      env: [
        utc_log: false,
        truncate: 8096,
        translators: [{Logger.Translator, :translate}],
        handle_otp_reports: true,
        handle_sasl_reports: false,
        always_evaluate_messages: false,
        compile_time_purge_matching: [],
        compile_time_application: nil,
        translator_inspect_opts: [],
        start_options: [],
        sync_threshold: 20,
        discard_threshold: 500,
        discard_threshold_periodic_check: 30_000,
        metadata: []
      ]
    ]
  end
end
