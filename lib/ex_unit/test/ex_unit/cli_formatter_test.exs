Code.require_file "../../test_helper.exs", __FILE__

defmodule ExUnit.CLIFormatterTest do
  use ExUnit.Case, sync: false

  defp exec_test(file) do
    cmd("#{elixir_path} #{file}")
  end

  defp elixir_path do
    File.expand_path("../../../../../bin/elixir", __FILE__)
  end

  defp cmd(command) do
    port = Port.open({ :spawn, to_char_list(command) },
      [:stream, :binary, :exit_status, :hide])
    do_cmd(port, "")
  end

  defp do_cmd(port, acc) do
    receive do
      { ^port, { :data, data } } ->
        do_cmd(port, acc <> data)
      { ^port, { :exit_status, status } } ->
        acc
    end
  end

  test :ptint_stacktrace_when_raising_not_at_assertion do
    File.write "raising_test.exs", """
    ExUnit.start
    defmodule RaisingTest do
      use ExUnit.Case
      ExUnit.configure formatter: ExUnit.CLIFormatter

      test :raise do
        assert raise("raise")
      end
    end
    """

    out_put = exec_test("raising_test.exs")
    assert out_put =~ %r/stacktrace:/
  after
    File.rm("raising_test.exs")
  end

  test :hide_stacktrace_when_raising_at_assertion do
    File.write "failure_test.exs", """
    ExUnit.start
    defmodule FailureTest do
      use ExUnit.Case
      ExUnit.configure formatter: ExUnit.CLIFormatter

      test :fail do
        assert false
      end
    end
    """

    out_put = exec_test("failure_test.exs")
    assert out_put =~ %r/at\ #{File.expand_path("failure_test.exs")}:7/
  after
    File.rm("failure_test.exs")
  end
end
