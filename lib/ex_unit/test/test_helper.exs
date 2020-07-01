Logger.configure_backend(:console, colors: [enabled: false])
ExUnit.start(trace: "--trace" in System.argv())
