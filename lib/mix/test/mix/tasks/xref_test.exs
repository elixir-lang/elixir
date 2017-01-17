Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.XrefTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  setup_all do
    previous = Application.get_env(:elixir, :ansi_enabled, false)
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit fn -> Application.put_env(:elixir, :ansi_enabled, previous) end
  end

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  ## Warnings

  test "warnings: reports nothing with no references" do
    assert_no_warnings "defmodule A do end"
  end

  test "warnings: reports missing functions" do
    assert_warnings """
    defmodule A do
      def a, do: A.no_func
      def b, do: A.a()
    end
    """, """
    warning: function A.no_func/0 is undefined or private
      lib/a.ex:2

    """
  end

  test "warnings: reports missing functions respecting arity" do
    assert_warnings """
    defmodule A do
      def a, do: :ok
      def b, do: A.a(1)
    end
    """, """
    warning: function A.a/1 is undefined or private. Did you mean one of:

          * a/0

      lib/a.ex:3

    """
  end

  test "warnings: reports missing modules" do
    assert_warnings """
    defmodule A do
      def a, do: D.no_module
    end
    """, """
    warning: function D.no_module/0 is undefined (module D is not available)
      lib/a.ex:2

    """
  end

  test "warnings: reports module case mismatch" do
    assert_warnings """
    defmodule A do
      def a, do: Genserver.start_link(A, [], [])
    end
    """, """
    warning: function Genserver.start_link/3 is undefined (module Genserver is not available). Did you mean:

          * GenServer.start_link/3

      lib/a.ex:2

    """
  end

  test "warnings: reports missing captures" do
    assert_warnings """
    defmodule A do
      def a, do: &A.no_func/0
    end
    """, """
    warning: function A.no_func/0 is undefined or private
      lib/a.ex:2

    """
  end

  test "warnings: doesn't report missing funcs at compile time" do
    assert_no_warnings """
      Enum.map([], fn _ -> BadReferencer.no_func4() end)

      if function_exported?(List, :flatten, 1) do
        List.flatten([1, 2, 3])
      else
        List.old_flatten([1, 2, 3])
      end
    """
  end

  test "warnings: protocols are checked, ignoring missing built-in impls" do
    assert_warnings """
    defprotocol AProtocol do
      def func(arg)
    end

    defmodule AImplementation do
      defimpl AProtocol do
        def func(_), do: B.no_func
      end
    end
    """, """
    warning: function B.no_func/0 is undefined or private
      lib/a.ex:7

    """
  end

  test "warnings: handles Erlang ops" do
    assert_no_warnings """
    defmodule A do
      def a(a, b), do: a and b
      def b(a, b), do: a or b
    end
    """
  end

  test "warnings: handles Erlang modules" do
    assert_warnings """
    defmodule A do
      def a, do: :not_a_module.no_module
      def b, do: :lists.no_func
    end
    """, """
    warning: function :not_a_module.no_module/0 is undefined (module :not_a_module is not available)
      lib/a.ex:2

    warning: function :lists.no_func/0 is undefined or private
      lib/a.ex:3

    """
  end

  test "warnings: handles multiple modules in one file" do
    assert_warnings """
    defmodule A1 do
      def a, do: A2.no_func
      def b, do: A2.a
    end

    defmodule A2 do
      def a, do: A1.no_func
      def b, do: A1.b
    end
    """, """
    warning: function A2.no_func/0 is undefined or private
      lib/a.ex:2

    warning: function A1.no_func/0 is undefined or private
      lib/a.ex:7

    """
  end

  test "warnings: doesn't load unloaded modules" do
    assert_warnings """
    defmodule A1 do
      @compile {:autoload, false}
      @on_load :init
      def init do
        raise "oops"
      end
    end

    defmodule A2 do
      def a, do: A1.no_func
      def b, do: A1.init
    end
    """, """
    warning: function A1.no_func/0 is undefined or private
      lib/a.ex:10

    """
  end

  test "warnings: groups multiple warnings in one file" do
    assert_warnings """
    defmodule A do
      def a, do: A.no_func
      def b, do: A2.no_func
      def c, do: A.no_func
      def d, do: A2.no_func
    end
    """, """
    warning: function A.no_func/0 is undefined or private
    Found at 2 locations:
      lib/a.ex:2
      lib/a.ex:4

    warning: function A2.no_func/0 is undefined (module A2 is not available)
    Found at 2 locations:
      lib/a.ex:3
      lib/a.ex:5

    """
  end

  test "warnings: handles module body conditionals" do
    assert_warnings """
    defmodule A do
      if function_exported?(List, :flatten, 1) do
        List.flatten([1, 2, 3])
      else
        List.old_flatten([1, 2, 3])
      end

      if function_exported?(List, :flatten, 1) do
        def flatten(arg), do: List.flatten(arg)
      else
        def flatten(arg), do: List.old_flatten(arg)
      end

      if function_exported?(List, :flatten, 1) do
        def flatten2(arg), do: List.old_flatten(arg)
      else
        def flatten2(arg), do: List.flatten(arg)
      end
    end
    """, """
    warning: function List.old_flatten/1 is undefined or private. Did you mean one of:

          * flatten/1
          * flatten/2

      lib/a.ex:15

    """
  end

  test "warnings: imports" do
    assert_no_warnings """
    defmodule A do
      import Record

      def a(a, b), do: extract(a, b)
      def b(arg), do: is_record(arg)
    end
    """
  end

  test "warnings: aliases" do
    assert_warnings """
    defmodule A do
      alias Enum, as: E

      def a(a, b), do: E.map2(a, b)
      def b, do: &E.map2/2
    end
    """, """
    warning: function Enum.map2/2 is undefined or private. Did you mean one of:

          * map/2

    Found at 2 locations:
      lib/a.ex:4
      lib/a.ex:5

    """
  end

  test "warnings: requires" do
    assert_no_warnings """
    defmodule A do
      require Integer

      def a(a), do: Integer.is_even(a)
    end
    """
  end

  defp assert_warnings(contents, expected) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      assert capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", ["warnings"]) == :error
      end) == expected
    end
  end

  defp assert_no_warnings(contents) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      assert capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", ["warnings"]) == :ok
      end) == ""
    end
  end

  ## Unreachable

  test "unreachable: reports missing functions" do
    assert_unreachable """
    defmodule A do
      def a, do: A.no_func
      def b, do: A.a()
    end
    """, """
    lib/a.ex:2: A.no_func/0
    """
  end

  defp assert_unreachable(contents, expected) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      assert capture_io(fn ->
        assert Mix.Task.run("xref", ["unreachable"]) == :error
      end) == expected
    end
  end

  ## Exclude

  test "exclude: excludes specified modules and MFAs" do
    defmodule ExcludeSample do
      def project do
        [app: :sample,
         version: "0.1.0",
         xref: [exclude: [MissingModule, {MissingModule2, :no_func, 2}]]]
      end
    end

    Mix.Project.push ExcludeSample

    assert_warnings """
    defmodule A do
      def a, do: MissingModule.no_func(1)
      def b, do: MissingModule2.no_func(1, 2)
      def c, do: MissingModule2.no_func(1)
      def d, do: MissingModule3.no_func(1, 2)
    end
    """, """
    warning: function MissingModule2.no_func/1 is undefined (module MissingModule2 is not available)
      lib/a.ex:4

    warning: function MissingModule3.no_func/2 is undefined (module MissingModule3 is not available)
      lib/a.ex:5

    """
  end

  ## Callers

  test "callers: prints callers of specified Module" do
    assert_callers "A", """
    defmodule A do
      def a, do: A.a()
      def a(arg), do: A.a(arg)
      def b, do: A.b()
      def c, do: B.a()
    end
    """,
    """
    lib/a.ex:2: A.a/0
    lib/a.ex:3: A.a/1
    lib/a.ex:4: A.b/0
    """
  end

  test "callers: prints callers of specified Module.func" do
    assert_callers "A.a", """
    defmodule A do
      def a, do: A.a()
      def a(arg), do: A.a(arg)
      def b, do: A.b()
      def c, do: B.a()
    end
    """,
    """
    lib/a.ex:2: A.a/0
    lib/a.ex:3: A.a/1
    """
  end

  test "callers: prints callers of specified Module.func/arity" do
    assert_callers "A.a/0", """
    defmodule A do
      def a, do: A.a()
      def a(arg), do: A.a(arg)
      def b, do: A.b()
      def c, do: B.a()
    end
    """,
    """
    lib/a.ex:2: A.a/0
    """
  end

  test "callers: lists compile calls and macros" do
    assert_callers "A", """
    defmodule A do
      defmacro a_macro, do: :ok
      def a, do: :ok
    end
    """, """
    defmodule B do
      require A

      A.a_macro()
      A.a()
    end
    """,
    """
    lib/b.ex:5: A.a/0
    lib/b.ex:4: A.a_macro/0
    """
  end

  test "callers: handles aliases" do
    assert_callers "Enum", """
    defmodule A do
      alias Enum, as: E

      E.map([], &E.flatten/1)

      def a(a, b), do: E.map(a, b)
    end
    """, """
    lib/a.ex:4: Enum.flatten/1
    lib/a.ex:4: Enum.map/2
    lib/a.ex:6: Enum.map/2
    """
  end

  test "callers: handles imports" do
    assert_callers "Integer", ~S"""
    defmodule A do
      import Integer

      &is_even/1
      &parse/1

      _ = is_even(Enum.random([1]))
      _ = parse("2")

      def a(a), do: is_even(a)
      def b(a), do: parse(a)
      _ = is_even(Enum.random([1])); def c(a), do: is_even(a)
    end
    """, """
    lib/a.ex:4: Integer.is_even/1
    lib/a.ex:7: Integer.is_even/1
    lib/a.ex:10: Integer.is_even/1
    lib/a.ex:12: Integer.is_even/1
    lib/a.ex:5: Integer.parse/1
    lib/a.ex:8: Integer.parse/1
    lib/a.ex:11: Integer.parse/1
    """
  end

  test "callers: no argument gives error" do
    in_fixture "no_mixfile", fn ->
      message = "xref doesn't support this command, see \"mix help xref\" for more information"

      assert_raise Mix.Error, message, fn ->
        assert Mix.Task.run("xref", ["callers"]) == :error
      end
    end
  end

  test "callers: gives nice error for quotable but invalid callers spec" do
    in_fixture "no_mixfile", fn ->
      message =
        "xref callers CALLEE expects Module, Module.function, or Module.function/arity, got: Module.func(arg)"

      assert_raise Mix.Error, message, fn ->
        Mix.Task.run("xref", ["callers", "Module.func(arg)"])
      end
    end
  end

  test "callers: gives nice error for unquotable callers spec" do
    in_fixture "no_mixfile", fn ->
      message =
        "xref callers CALLEE expects Module, Module.function, or Module.function/arity, got: %"

      assert_raise Mix.Error, message, fn ->
        Mix.Task.run("xref", ["callers", "%"])
      end
    end
  end

  defp assert_callers(callee, contents_a, contents_b \\ "", expected) do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", contents_a)
      File.write!("lib/b.ex", contents_b)

      assert capture_io(fn ->
        assert Mix.Task.run("xref", ["callers", callee]) == :ok
      end) == expected
    end
  end

  ## Graph

  test "graph: basic usage" do
    assert_graph """
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    lib/b.ex
    lib/c.ex
    lib/d.ex
    └── lib/a.ex (compile)
    """
  end

  test "graph: exclude" do
    assert_graph ~w[--exclude lib/c.ex --exclude lib/b.ex], """
    lib/a.ex
    lib/d.ex
    └── lib/a.ex (compile)
    """
  end

  test "graph: exclude 1" do
    assert_graph ~w[--exclude lib/d.ex], """
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    lib/b.ex
    lib/c.ex
    """
  end

  test "graph: dot format" do
    assert_graph ~w[--format dot], true, """
    digraph "xref graph" {
      "lib/a.ex"
      "lib/a.ex" -> "lib/b.ex"
      "lib/b.ex" -> "lib/a.ex"
      "lib/b.ex"
      "lib/c.ex"
      "lib/d.ex"
      "lib/d.ex" -> "lib/a.ex" [label="(compile)"]
    }
    """
  end

  test "graph: source" do
    assert_graph ~w[--source lib/a.ex], """
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    """
  end

  test "graph: invalid source" do
    assert_raise Mix.Error, "Source could not be found: lib/a2.ex", fn ->
      assert_graph ~w[--source lib/a2.ex], ""
    end
  end

  test "graph: sink" do
    assert_graph ~w[--sink lib/b.ex], """
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    lib/d.ex
    └── lib/a.ex (compile)
    """
  end

  test "graph: invalid sink" do
    assert_raise Mix.Error, "Sink could not be found: lib/b2.ex", fn ->
      assert_graph ~w[--sink lib/b2.ex], ""
    end
  end

  test "graph: sink and source is error" do
    assert_raise Mix.Error, "mix xref graph expects only one of --source and --sink", fn ->
      assert_graph ~w[--source lib/a.ex --sink lib/b.ex], ""
    end
  end

  test "graph: with dynamic module" do
    in_fixture "no_mixfile", fn ->
      File.write! "lib/a.ex", """
      B.define()
      """

      File.write! "lib/b.ex", """
      defmodule B do
        def define do
          defmodule A do
          end
        end
      end
      """

      assert Mix.Task.run("xref", ["graph"]) == :ok

      assert """
      Compiling 2 files (.ex)
      Generated sample app
      lib/a.ex
      lib/b.ex
      """ = receive_until_no_messages([])
    end
  end

  defp assert_graph(opts \\ [], dot \\ false, expected) do
    in_fixture "no_mixfile", fn ->
      File.write! "lib/a.ex", """
      defmodule A do
        def a do
          B.a
        end

        def b, do: :ok
      end
      """

      File.write! "lib/b.ex", """
      defmodule B do
        def a do
          A.a
          B.a
        end
      end
      """

      File.write! "lib/c.ex", """
      defmodule C do
      end
      """

      File.write! "lib/d.ex", """
      defmodule :d do
        A.b
      end
      """

      assert Mix.Task.run("xref", opts ++ ["graph"]) == :ok

      result =
        if dot do
          File.read!("xref_graph.dot")
        else
          assert "Compiling 4 files (.ex)\nGenerated sample app\n" <> result =
            receive_until_no_messages([])

          result
        end

      assert result == expected
    end
  end

  defp receive_until_no_messages(acc) do
    receive do
      {:mix_shell, :info, [line]} -> receive_until_no_messages([acc, line | "\n"])
    after
      0 -> IO.iodata_to_binary(acc)
    end
  end
end
