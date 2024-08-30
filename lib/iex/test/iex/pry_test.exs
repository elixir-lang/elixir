Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.PryTest do
  use ExUnit.Case

  setup do
    on_exit(fn -> IEx.Pry.remove_breaks() end)
  end

  describe "whereami" do
    test "shows lines with radius" do
      Application.put_env(:elixir, :ansi_enabled, true)

      {:ok, contents} = IEx.Pry.whereami(__ENV__.file, 3, 2)

      assert IO.iodata_to_binary(contents) == """
                 1: Code.require_file("../test_helper.exs", __DIR__)
                 2:\s
             \e[1m    3: defmodule IEx.PryTest do
             \e[22m    4:   use ExUnit.Case
                 5:\s
             """

      {:ok, contents} = IEx.Pry.whereami(__ENV__.file, 1, 4)

      assert IO.iodata_to_binary(contents) == """
             \e[1m    1: Code.require_file("../test_helper.exs", __DIR__)
             \e[22m    2:\s
                 3: defmodule IEx.PryTest do
                 4:   use ExUnit.Case
                 5:\s
             """
    after
      Application.delete_env(:elixir, :ansi_enabled)
    end

    test "returns error for unknown files" do
      assert IEx.Pry.whereami("unknown", 3, 2) == :error
    end

    test "returns error for out of range lines" do
      assert IEx.Pry.whereami(__ENV__.file, 1000, 2) == :error
    end
  end

  describe "break" do
    test "sets up a breakpoint on the given module" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert instrumented?(URI)
      assert [_] = IEx.Pry.breaks()
    end

    test "sets up multiple breakpoints in the same module" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert instrumented?(URI)
      assert IEx.Pry.break(URI, :parse, 1) == {:ok, 2}
      assert instrumented?(URI)
      assert [_, _] = IEx.Pry.breaks()
    end

    test "reinstruments if module has been reloaded" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert instrumented?(URI)
      deinstrument!(URI)
      refute instrumented?(URI)
      assert IEx.Pry.break(URI, :parse, 1) == {:ok, 2}
      assert instrumented?(URI)
      assert [_, _] = IEx.Pry.breaks()
    end

    test "returns ID when breakpoint is already set" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert [_] = IEx.Pry.breaks()
    end

    test "returns ID even when breakpoint is already set on deinstrument" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      deinstrument!(URI)
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert [_] = IEx.Pry.breaks()
    end

    test "errors when setting up a break with no beam" do
      assert IEx.Pry.break(__MODULE__, :setup, 2) == {:error, :no_beam_file}
    end

    test "errors when setting up a break for unknown function" do
      assert IEx.Pry.break(URI, :unknown, 2) == {:error, :unknown_function_arity}
    end

    test "errors for non-Elixir modules" do
      assert IEx.Pry.break(:elixir, :unknown, 2) == {:error, :non_elixir_module}
    end
  end

  describe "breaks" do
    test "returns all breaks" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]

      assert IEx.Pry.break(URI, :decode_query, 2, 10) == {:ok, 1}
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 10}]

      assert IEx.Pry.break(URI, :parse, 1, 1) == {:ok, 2}
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 10}, {2, URI, {:parse, 1}, 1}]
    end

    test "sets negative break to 0" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      :ets.insert(IEx.Pry, {1, URI, {:decode_query, 2}, {[], true}, -1})
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 0}]
    end

    test "do not return break points for deinstrumented modules" do
      assert IEx.Pry.break(URI, :parse, 1) == {:ok, 1}
      assert IEx.Pry.breaks() == [{1, URI, {:parse, 1}, 1}]
      deinstrument!(URI)
      assert IEx.Pry.breaks() == []
    end
  end

  describe "reset_break" do
    test "resets break for given ID" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert IEx.Pry.reset_break(1) == :ok
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 0}]
    end

    test "resets break for given mfa" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert IEx.Pry.reset_break(URI, :decode_query, 2) == :ok
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 0}]
    end

    test "returns not_found if module is deinstrumented" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      deinstrument!(URI)
      assert IEx.Pry.reset_break(URI, :decode_query, 2) == :not_found
      assert IEx.Pry.breaks() == []
    end

    test "returns not_found if mfa has no break" do
      assert IEx.Pry.reset_break(URI, :decode_query, 2) == :not_found
    end

    test "returns not_found if ID is deinstrumented" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      deinstrument!(URI)
      assert IEx.Pry.reset_break(1) == :not_found
      assert IEx.Pry.breaks() == []
    end

    test "returns not_found if ID has no break" do
      assert IEx.Pry.reset_break(1) == :not_found
    end
  end

  describe "remove_breaks" do
    test "removes all breaks" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert IEx.Pry.remove_breaks() == :ok
      assert IEx.Pry.breaks() == []
    end

    test "removes all breaks even if module is deinstrumented" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      deinstrument!(URI)
      assert IEx.Pry.remove_breaks() == :ok
      assert IEx.Pry.breaks() == []
    end

    test "remove breaks in a given module" do
      assert IEx.Pry.remove_breaks(Date.Range) == :ok
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      assert IEx.Pry.break(Date.Range, :__struct__, 1) == {:ok, 2}
      assert IEx.Pry.remove_breaks(Date.Range) == :ok
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "remove breaks in a given module even if deinstrumented" do
      assert IEx.Pry.break(URI, :decode_query, 2) == {:ok, 1}
      deinstrument!(URI)
      assert IEx.Pry.breaks() == []
    end
  end

  describe "annotate_quoted" do
    def annotate_quoted(block, condition \\ true) do
      {:__block__, meta, [{:=, _, [{:env, _, _}, _]} | tail]} =
        block
        |> Code.string_to_quoted!()
        |> IEx.Pry.annotate_quoted(condition, %{__ENV__ | line: 1})

      Macro.to_string({:__block__, meta, tail})
    end

    test "one expression, one line" do
      assert annotate_quoted("""
             x = 123
             y = 456
             x + y
             """) == """
             next? = true
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 1})
             x = 123
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 2})
             y = 456
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 3})
             x + y\
             """
    end

    test "one expression, multiple lines" do
      assert annotate_quoted("""
             (x = 123) &&
              (y = 456)
             x + y
             """) == """
             next? = true
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 1})
             (x = 123) && (y = 456)
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 3})
             x + y\
             """
    end

    test "one line, multiple expressions" do
      assert annotate_quoted("""
             x = 123; y = 456
             x + y
             """) == """
             next? = true
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 1})
             x = 123
             y = 456
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 2})
             x + y\
             """
    end

    test "multiple line, multiple expressions" do
      assert annotate_quoted("""
             x = 123; y =
              456
             x + y
             """) == """
             next? = true
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 1})
             x = 123
             y = 456
             next? = IEx.Pry.__next__(next?, binding(), %{env | line: 3})
             x + y\
             """
    end
  end

  defp instrumented?(module) do
    module.module_info(:attributes)[:iex_pry] == [true]
  end

  defp deinstrument!(module) do
    beam = :code.which(module)
    :code.purge(module)
    {:module, _} = :code.load_binary(module, beam, File.read!(beam))
    :ok
  end
end
