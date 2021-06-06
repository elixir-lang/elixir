Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  def start_counter(_) do
    [counter: []]
  end

  test "callbacks run custom code with context" do
    defmodule CallbacksTest do
      use ExUnit.Case

      setup_all do
        [context: :setup_all]
      end

      setup do
        %{initial_setup: true}
      end

      setup context do
        assert context[:initial_setup]
        assert context[:context] == :setup_all
        {:ok, [context: :setup]}
      end

      test "callbacks", context do
        assert context[:context] == :setup
      end
    end

    assert capture_io(fn -> ExUnit.run() end) =~ "1 test, 0 failures"
  end

  test "named callbacks run custom code in order" do
    defmodule NamedCallbacksTest do
      use ExUnit.Case

      import ExUnit.CallbacksTest
      setup_all :start_counter

      setup :store_1
      setup [:store_2, :store_3]

      setup context do
        [counter: [4 | context.counter]]
      end

      setup :store_5

      test "callbacks", context do
        assert context[:counter] == [5, 4, 3, 2, 1]
      end

      defp store(context, number), do: [counter: [number | context.counter]]
      defp store_1(context), do: store(context, 1)
      defp store_2(context), do: store(context, 2)
      defp store_3(context), do: store(context, 3)
      defp store_5(context), do: store(context, 5)
    end

    assert capture_io(fn -> ExUnit.run() end) =~ "1 test, 0 failures"
  end

  test "doesn't choke on setup errors" do
    defmodule SetupTest do
      use ExUnit.Case

      setup _ do
        :ok = error()
      end

      test "ok" do
        :ok
      end

      defp error, do: :error
    end

    assert capture_io(fn -> ExUnit.run() end) =~
             "** (MatchError) no match of right hand side value: :error"
  end

  test "doesn't choke on setup_all errors" do
    defmodule SetupAllTest do
      use ExUnit.Case

      setup_all _ do
        :ok = error()
      end

      test "ok" do
        :ok
      end

      defp error, do: :error
    end

    assert capture_io(fn -> ExUnit.run() end) =~
             "** (MatchError) no match of right hand side value: :error"
  end

  test "doesn't choke on dead supervisor" do
    defmodule StartSupervisedErrorTest do
      use ExUnit.Case

      @tag timeout: 500
      test "ok" do
        fun = fn ->
          Process.flag(:trap_exit, true)
          Process.sleep(:infinity)
        end

        start_supervised({Task, fun})
      end
    end

    assert capture_io(fn -> ExUnit.run() end) =~ "supervisor shutdown timed out after 500ms"
  end

  test "doesn't choke on on_exit errors" do
    defmodule OnExitErrorTest do
      use ExUnit.Case

      test "ok" do
        on_exit(fn -> :ok = error() end)
        :ok
      end

      defp error, do: :error
    end

    assert capture_io(fn -> ExUnit.run() end) =~
             "** (MatchError) no match of right hand side value: :error"
  end

  test "doesn't choke when on_exit exits" do
    defmodule OnExitExitTest do
      use ExUnit.Case, async: false

      test "ok" do
        on_exit(fn -> Process.exit(self(), :kill) end)
        :ok
      end
    end

    assert capture_io(fn -> ExUnit.run() end) =~ ">) killed"
  end

  test "invalidates all tests when on_exit errors within setup_all" do
    defmodule InvalidatesTestsOnExitErrorTest do
      use ExUnit.Case

      setup_all do
        on_exit(fn -> raise "boom" end)
        :ok
      end

      test "succeeds" do
        assert true
      end

      test "fails" do
        assert false
      end
    end

    assert capture_io(fn -> ExUnit.run() end) =~ "2 tests, 2 failures"
  end

  defp no_formatters! do
    ExUnit.configure(formatters: [])
    on_exit(fn -> ExUnit.configure(formatters: [ExUnit.CLIFormatter]) end)
  end

  test "exits with shutdown reason" do
    defmodule OnExitAliveTest do
      use ExUnit.Case

      setup do
        parent = self()

        pid =
          spawn_link(fn ->
            Process.flag(:trap_exit, true)
            send(parent, :ready)

            receive do
              {:EXIT, ^parent, :shutdown} ->
                receive do: ({:on_exit, pid} -> send(pid, :done))
            end
          end)

        receive do: (:ready -> :ok)

        on_exit(fn ->
          send(pid, {:on_exit, self()})
          assert_receive :done
          IO.puts("on_exit run")
        end)

        :ok
      end

      test "ok" do
        :ok
      end
    end

    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ "on_exit run"
    assert output =~ "1 test, 0 failures"
  end

  test "runs multiple on_exit exits and overrides by ref" do
    defmodule OnExitSuccessTest do
      use ExUnit.Case

      setup do
        on_exit(fn ->
          IO.puts("on_exit setup run")
        end)

        on_exit({:overridden, 1}, fn ->
          IO.puts("on_exit 1 overridden -> not run")
        end)

        :ok
      end

      setup_all do
        on_exit(fn ->
          IO.puts("on_exit setup_all run")
        end)

        :ok
      end

      test "ok" do
        on_exit(fn ->
          IO.puts("simple on_exit run")
        end)

        on_exit({:overridden, 2}, fn ->
          IO.puts("on_exit 2 overridden -> not run")
        end)

        on_exit({:overridden, 2}, fn ->
          IO.puts("on_exit 2 overrides -> run")
        end)

        on_exit({:overridden, 1}, fn ->
          IO.puts("on_exit 1 overrides -> run")
        end)

        :ok
      end
    end

    no_formatters!()
    output = capture_io(fn -> ExUnit.run() end)

    assert output =~ """
           on_exit 2 overrides -> run
           simple on_exit run
           on_exit 1 overrides -> run
           on_exit setup run
           on_exit setup_all run
           """

    refute output =~ "not run"
  end

  test "runs multiple on_exit on failure" do
    defmodule OnExitFailureTest do
      use ExUnit.Case

      setup do
        on_exit(fn ->
          IO.puts("on_exit setup run")
        end)

        :ok
      end

      setup_all do
        on_exit(fn ->
          IO.puts("on_exit setup_all run")
        end)

        :ok
      end

      test "ok" do
        on_exit(fn ->
          IO.puts("simple on_exit run")
        end)

        flunk("oops")
      end
    end

    no_formatters!()
    output = capture_io(fn -> ExUnit.run() end)

    assert output =~ """
           simple on_exit run
           on_exit setup run
           on_exit setup_all run
           """
  end

  test "raises an error when setting an invalid callback in setup" do
    defmodule SetupErrorTest do
      use ExUnit.Case

      setup do
        {:ok, "foo"}
      end

      test "ok" do
        :ok
      end
    end

    assert capture_io(fn -> ExUnit.run() end) =~
             "** (RuntimeError) expected ExUnit callback in " <>
               "ExUnit.CallbacksTest.SetupErrorTest to return " <>
               ":ok | keyword | map, got {:ok, \"foo\"} instead"
  end

  test "raises an error when overriding a reserved callback key in setup" do
    defmodule SetupReservedTest do
      use ExUnit.Case

      setup do
        {:ok, file: "foo"}
      end

      test "ok" do
        :ok
      end
    end

    assert capture_io(fn -> ExUnit.run() end) =~
             "** (RuntimeError) ExUnit callback in " <>
               "ExUnit.CallbacksTest.SetupReservedTest is " <>
               "trying to set reserved field :file to \"foo\""
  end
end

defmodule ExUnit.CallbacksNoTests do
  use ExUnit.Case, async: true

  setup_all do
    raise "never run"
  end

  setup do
    raise "never run"
  end
end
