Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "callbacks run custom code with context" do
    defmodule CallbacksTest do
      use ExUnit.Case

      setup_all do
        {:ok, [context: :setup_all]}
      end

      setup do
        {:ok, [initial_setup: true]}
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

    assert capture_io(fn -> ExUnit.run end) =~
           "1 test, 0 failures"
  end

  test "doesn't choke on setup errors" do
    defmodule SetupTest do
      use ExUnit.Case

      setup _ do
        :ok = error
      end

      test "ok" do
        :ok
      end

      defp error, do: :error
    end

    assert capture_io(fn -> ExUnit.run end) =~
           "** (MatchError) no match of right hand side value: :error"
  end

  test "doesn't choke on setup_all errors" do
    defmodule SetupAllTest do
      use ExUnit.Case

      setup_all _ do
        :ok = error
      end

      test "ok" do
        :ok
      end

      defp error, do: :error
    end

    assert capture_io(fn -> ExUnit.run end) =~
           "** (MatchError) no match of right hand side value: :error"
  end

  test "doesn't choke on on_exit errors" do
    defmodule OnExitErrorTest do
      use ExUnit.Case

      test "ok" do
        on_exit fn -> :ok = error end
        :ok
      end

      defp error, do: :error
    end

    assert capture_io(fn -> ExUnit.run end) =~
           "** (MatchError) no match of right hand side value: :error"
  end

  test "doesn't choke when on_exit exits" do
    defmodule OnExitExitTest do
      use ExUnit.Case, async: false

      test "ok" do
        on_exit fn -> Process.exit(self(), :kill) end
        :ok
      end
    end

    assert capture_io(fn -> ExUnit.run end) =~
           ">) killed"
  end

  defp no_formatters! do
    ExUnit.configure(formatters: [])
    on_exit fn -> ExUnit.configure(formatters: [ExUnit.CLIFormatter]) end
  end

  test "exits with shutdown reason" do
    defmodule OnExitAliveTest do
      use ExUnit.Case

      setup do
        parent = self()

        pid = spawn_link fn ->
          Process.flag(:trap_exit, true)
          send parent, :ready
          receive do
            {:EXIT, ^parent, :shutdown} ->
              receive do: ({:on_exit, pid} -> send pid, :done)
          end
        end

        receive do: (:ready -> :ok)

        on_exit fn ->
          send pid, {:on_exit, self}
          assert_receive :done
          IO.puts "on_exit run"
        end

        :ok
      end

      test "ok" do
        :ok
      end
    end

    output = capture_io(fn -> ExUnit.run end)
    assert output =~ "on_exit run"
    assert output =~ "1 test, 0 failures"
  end

  test "runs multiple on_exit exits and overrides by ref" do
    defmodule OnExitSuccessTest do
      use ExUnit.Case

      setup do
        on_exit fn ->
          IO.puts "on_exit setup run"
        end

        on_exit {:overridden, 1}, fn ->
          IO.puts "on_exit 1 overridden -> not run"
        end

        :ok
      end

      setup_all do
        on_exit fn ->
          IO.puts "on_exit setup_all run"
        end

        :ok
      end

      test "ok" do
        on_exit fn ->
          IO.puts "simple on_exit run"
        end

        on_exit {:overridden, 2}, fn ->
          IO.puts "on_exit 2 overridden -> not run"
        end

        on_exit {:overridden, 2}, fn ->
          IO.puts "on_exit 2 overrides -> run"
        end

        on_exit {:overridden, 1}, fn ->
          IO.puts "on_exit 1 overrides -> run"
        end

        :ok
      end
    end

    no_formatters!
    output = capture_io(fn -> ExUnit.run end)

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
        on_exit fn ->
          IO.puts "on_exit setup run"
        end

        :ok
      end

      setup_all do
        on_exit fn ->
          IO.puts "on_exit setup_all run"
        end

        :ok
      end

      test "ok" do
        on_exit fn ->
          IO.puts "simple on_exit run"
        end

        flunk "oops"
      end
    end

    no_formatters!
    output = capture_io(fn -> ExUnit.run end)

    assert output =~ """
    simple on_exit run
    on_exit setup run
    on_exit setup_all run
    """
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
