Code.require_file "test_helper.exs", __DIR__

defmodule ExUnitTest do
  use ExUnit.Case, async: false

  setup do
    ExUnit.configure(formatters: [])
    :ok
  end

  teardown do
    ExUnit.configure(formatters: [ExUnit.CLIFormatter])
    :ok
  end

  import ExUnit.CaptureIO

  test "it supports many runs" do
    defmodule SampleTest do
      use ExUnit.Case, async: false

      test "true" do
        assert false
      end

      test "false" do
        assert false
      end
    end

    assert ExUnit.run == %{ failures: 2, total: 2 }
  end

  test "it doesn't hang on exists" do
    defmodule EventServerTest do
      use ExUnit.Case, async: false

      test "spawn and crash" do
        Process.spawn_link(fn ->
          exit :foo
        end)
        receive after: (1000 -> :ok)
      end
    end

    assert ExUnit.run == %{ failures: 1, total: 1 }
  end

  test "it doesn't choke on setup_all/teardown_all errors" do
    defmodule SetupAllTest do
      use ExUnit.Case, async: false

      setup_all _ do
        :ok = error
      end

      test "ok" do
        :ok
      end

      defp error, do: :error
    end

    ExUnit.configure(formatters: [ExUnit.CLIFormatter])

    assert capture_io(fn -> ExUnit.run end) =~
           "** (MatchError) no match of right hand side value: :error"
  end

  test "filtering cases with tags" do
    defmodule ParityTest do
      use ExUnit.Case

      test "zero", do: :ok

      @tag even: false
      test "one", do: :ok

      @tag even: true
      test "two", do: assert 1 == 2

      @tag even: false
      test "three", do: :ok
    end

    test_cases = ExUnit.Server.start_run

    assert run_with_filter([], test_cases) ==
           %{ failures: 1, total: 4 }

    assert run_with_filter([exclude: [even: true]], test_cases) ==
           %{ failures: 0, total: 3 }

    assert run_with_filter([exclude: :even], test_cases) ==
           %{ failures: 0, total: 1 }

    assert run_with_filter([exclude: :even, include: [even: true]], test_cases) ==
           %{ failures: 1, total: 2 }

    assert run_with_filter([exclude: :test, include: [even: true]], test_cases) ==
           %{ failures: 1, total: 1 }
  end

  defp run_with_filter(filters, { async, sync, load_us }) do
    opts = Keyword.merge(ExUnit.configuration, filters)
    ExUnit.Runner.run(async, sync, opts, load_us)
  end
end
