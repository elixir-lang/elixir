defmodule Logger.CLIFormatterTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  setup_all do
    ExUnit.configure(formatters: [Logger.CLIFormatter])
    on_exit(fn -> ExUnit.configure(formatters: [ExUnit.CLIFormatter]) end)
  end

  test "opportunistic logging" do
    defmodule TestOpportunisticLogging do
      use ExUnit.Case

      require Logger

      test "prints output" do
        Logger.info "one"
        assert 1 == 2
      end

      test "no output" do
        Logger.error "two"
        assert 1 == 1
      end
    end

    output = capture_io(&ExUnit.run/0)
    assert output =~ "The following output was logged:"
    assert output =~ "[info]  one"
    refute output =~ "[error] two"
  end
end
