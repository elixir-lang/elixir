Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case, async: true

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

  setup context do
    if Process.get(:ex_unit_callback) do
      raise "ex_unit_callback was not cleaned"
    else
      Process.put(:ex_unit_callback, context[:test])
    end
    :ok
  end

  teardown context do
    assert context[:context] == :setup
    {:ok, context: :teardown}
  end

  teardown context do
    assert Process.get(:ex_unit_callback) == context[:test]
    Process.delete(:ex_unit_callback)
    assert context[:context] == :teardown
    :ok
  end

  teardown_all context do
    assert context[:context] == :setup_all
    :ok
  end

  import ExUnit.CaptureIO

  test "callbacks can run custom code" do
    assert Process.get(:ex_unit_callback) == :"test callbacks can run custom code"
  end

  test "receives context from callback", context do
    assert context[:context] == :setup
  end

  test "doesn't choke on setup errors" do
    defmodule SetupTest do
      use ExUnit.Case, async: false

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

  test "doesn't choke on teardown errors" do
    defmodule TeardownTest do
      use ExUnit.Case, async: false

      teardown _ do
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

  test "doesn't choke on teardown_all errors" do
    defmodule TeardownAllTest do
      use ExUnit.Case, async: false

      teardown_all _ do
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
end

defmodule ExUnit.CallbacksNoTests do
  use ExUnit.Case, async: true

  setup_all do
    raise "Never run"
  end

  setup do
    raise "Never run"
  end

  teardown do
    raise "Never run"
  end

  teardown_all do
    raise "Never run"
  end
end
