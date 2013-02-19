Code.require_file "../../test_helper.exs", __FILE__

defmodule ExUnit.CLIFormatterTest do
  use ExUnit.Case, sync: false

  defp exec_test(file) do
    System.cmd("#{elixir_path} #{file}")
  end

  defp elixir_path do
    Path.expand("../../../../../bin/elixir", __FILE__)
  end

  test :print_stacktrace_when_raising_not_at_assertion do
    File.write "raising_test.exs", """
    ExUnit.start formatter: ExUnit.CLIFormatter
    defmodule RaisingTest do
      use ExUnit.Case

      test :raise do
        Enum.each { :will_fail }
        assert true
      end
    end
    """

    out_put = exec_test("raising_test.exs")
    assert out_put =~ %r/stacktrace:/
    refute out_put =~ %r/ExUnit\.Runner/
  after
    File.rm("raising_test.exs")
  end

  test :hide_stacktrace_when_raising_at_assertion do
    File.write "failure_test.exs", """
    ExUnit.start formatter: ExUnit.CLIFormatter
    defmodule FailureTest do
      use ExUnit.Case

      test :fail do
        assert false
      end
    end
    """

    out_put = exec_test("failure_test.exs")
    assert out_put =~ %r/at failure_test.exs:6/
  after
    File.rm("failure_test.exs")
  end
end
