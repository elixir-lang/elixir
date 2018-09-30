Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.XrefTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  setup_all do
    previous = Application.get_env(:elixir, :ansi_enabled, false)
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit(fn -> Application.put_env(:elixir, :ansi_enabled, previous) end)
  end

  setup do
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  test "calls: returns all function calls" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: A.a()
        def a(arg), do: A.a(arg)
        def c, do: B.a()
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        def a, do: nil
      end
      """
    }

    output = [
      %{callee: {A, :a, 0}, caller_module: A, file: "lib/a.ex", line: 2},
      %{callee: {A, :a, 1}, caller_module: A, file: "lib/a.ex", line: 3},
      %{callee: {B, :a, 0}, caller_module: A, file: "lib/a.ex", line: 4},
      %{callee: {Kernel, :def, 2}, caller_module: A, file: "lib/a.ex", line: 4},
      %{callee: {Kernel, :def, 2}, caller_module: A, file: "lib/a.ex", line: 3},
      %{callee: {Kernel, :def, 2}, caller_module: A, file: "lib/a.ex", line: 2},
      %{callee: {Kernel, :defmodule, 2}, caller_module: A, file: "lib/a.ex", line: 1},
      %{
        callee: {Kernel.LexicalTracker, :read_cache, 2},
        caller_module: A,
        file: "lib/a.ex",
        line: 1
      },
      %{callee: {Kernel, :def, 2}, caller_module: B, file: "lib/b.ex", line: 2},
      %{callee: {Kernel, :defmodule, 2}, caller_module: B, file: "lib/b.ex", line: 1},
      %{
        callee: {Kernel.LexicalTracker, :read_cache, 2},
        caller_module: B,
        file: "lib/b.ex",
        line: 1
      }
    ]

    assert_all_calls(files, output)
  end

  test "calls: returns macro call" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        defmacro a_macro, do: :ok
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        require A
        def a, do: A.a_macro()
      end
      """
    }

    output = [
      %{callee: {A, :a_macro, 0}, caller_module: B, file: "lib/b.ex", line: 3},
      %{callee: {Kernel, :def, 2}, caller_module: B, file: "lib/b.ex", line: 3},
      %{callee: {Kernel, :defmodule, 2}, caller_module: B, file: "lib/b.ex", line: 1},
      %{
        callee: {Kernel.LexicalTracker, :read_cache, 2},
        line: 1,
        caller_module: B,
        file: "lib/b.ex"
      },
      %{callee: {Kernel, :defmacro, 2}, caller_module: A, file: "lib/a.ex", line: 2},
      %{callee: {Kernel, :defmodule, 2}, caller_module: A, file: "lib/a.ex", line: 1},
      %{
        callee: {Kernel.LexicalTracker, :read_cache, 2},
        line: 1,
        caller_module: A,
        file: "lib/a.ex"
      }
    ]

    assert_all_calls(files, output)
  end

  test "calls: returns function call inside macro" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        defmacro a_macro(x) do
          quote do
            A.b(unquote(x))
          end
        end

        def b(x), do: x
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        require A
        def a, do: A.a_macro(1)
      end
      """
    }

    output = [
      %{callee: {A, :b, 1}, caller_module: B, file: "lib/b.ex", line: 3},
      %{callee: {A, :a_macro, 1}, caller_module: B, file: "lib/b.ex", line: 3},
      %{callee: {Kernel, :def, 2}, caller_module: B, file: "lib/b.ex", line: 3},
      %{callee: {Kernel, :defmodule, 2}, caller_module: B, file: "lib/b.ex", line: 1},
      %{
        callee: {Kernel.LexicalTracker, :read_cache, 2},
        caller_module: B,
        file: "lib/b.ex",
        line: 1
      },
      %{callee: {Kernel, :def, 2}, caller_module: A, file: "lib/a.ex", line: 8},
      %{callee: {Kernel, :defmacro, 2}, caller_module: A, file: "lib/a.ex", line: 2},
      %{callee: {Kernel, :defmodule, 2}, caller_module: A, file: "lib/a.ex", line: 1},
      %{
        callee: {Kernel.LexicalTracker, :read_cache, 2},
        caller_module: A,
        file: "lib/a.ex",
        line: 1
      }
    ]

    assert_all_calls(files, output)
  end

  defp assert_all_calls(files, expected) do
    in_fixture("no_mixfile", fn ->
      generate_files(files)

      Mix.Task.run("compile")
      assert Enum.sort(Mix.Tasks.Xref.calls()) == Enum.sort(expected)
    end)
  end

  ## Warnings

  test "warnings: reports nothing with no references" do
    files = %{"lib/a.ex" => "defmodule A do end"}

    assert_no_warnings(files)
  end

  test "warnings: reports missing functions" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: A.no_func
        def b, do: A.a()

        @file "lib/external_source.ex"
        def c, do: &A.no_func/1
      end
      """
    }

    warning = """
    warning: function A.no_func/0 is undefined or private
      lib/a.ex:2

    warning: function A.no_func/1 is undefined or private
      lib/external_source.ex:6

    """

    assert_warnings(files, warning)
  end

  test "warnings: reports missing functions respecting arity" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: :ok
        def b, do: A.a(1)

        @file "lib/external_source.ex"
        def c, do: A.b(1)
      end
      """
    }

    warning = """
    warning: function A.a/1 is undefined or private. Did you mean one of:

          * a/0

      lib/a.ex:3

    warning: function A.b/1 is undefined or private. Did you mean one of:

          * b/0

      lib/external_source.ex:6

    """

    assert_warnings(files, warning)
  end

  test "warnings: reports missing modules" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: D.no_module

        @file "lib/external_source.ex"
        def c, do: E.no_module
      end
      """
    }

    warning = """
    warning: function D.no_module/0 is undefined (module D is not available)
      lib/a.ex:2

    warning: function E.no_module/0 is undefined (module E is not available)
      lib/external_source.ex:5

    """

    assert_warnings(files, warning)
  end

  test "warnings: reports missing captures" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: &A.no_func/0

        @file "lib/external_source.ex"
        def c, do: &A.no_func/1
      end
      """
    }

    warning = """
    warning: function A.no_func/0 is undefined or private
      lib/a.ex:2

    warning: function A.no_func/1 is undefined or private
      lib/external_source.ex:5

    """

    assert_warnings(files, warning)
  end

  test "warnings: doesn't report missing funcs at compile time" do
    files = %{
      "lib/a.ex" => """
        Enum.map([], fn _ -> BadReferencer.no_func4() end)

        if function_exported?(List, :flatten, 1) do
          List.flatten([1, 2, 3])
        else
          List.old_flatten([1, 2, 3])
        end
      """
    }

    assert_no_warnings(files)
  end

  test "warnings: reports deprecated calls" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        @deprecated "Use A.c/0 instead"
        def a, do: c()

        def b, do: a()
        def c, do: :c
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        def a, do: A.a()
      end
      """
    }

    warning = """
    warning: A.a/0 is deprecated. Use A.c/0 instead
      lib/b.ex:2

    """

    assert_warnings(files, warning)
  end

  test "warnings: protocols are checked, ignoring missing built-in impls" do
    files = %{
      "lib/a.ex" => """
      defprotocol AProtocol do
        def func(arg)
      end

      defmodule AImplementation do
        defimpl AProtocol do
          def func(_), do: B.no_func
        end
      end
      """
    }

    warning = """
    warning: function B.no_func/0 is undefined or private
      lib/a.ex:7

    """

    assert_warnings(files, warning)
  end

  test "warnings: handles Erlang ops" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a(a, b), do: a and b
        def b(a, b), do: a or b
      end
      """
    }

    assert_no_warnings(files)
  end

  test "warnings: handles Erlang modules" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: :not_a_module.no_module
        def b, do: :lists.no_func
      end
      """
    }

    warning = """
    warning: function :lists.no_func/0 is undefined or private
      lib/a.ex:3

    warning: function :not_a_module.no_module/0 is undefined (module :not_a_module is not available)
      lib/a.ex:2

    """

    assert_warnings(files, warning)
  end

  test "warnings: handles multiple modules in one file" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: B.no_func
        def b, do: B.a
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        def a, do: A.no_func
        def b, do: A.b
      end
      """
    }

    warning = """
    warning: function A.no_func/0 is undefined or private
      lib/b.ex:2

    warning: function B.no_func/0 is undefined or private
      lib/a.ex:2

    """

    assert_warnings(files, warning)
  end

  test "warnings: doesn't load unloaded modules" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        @compile {:autoload, false}
        @on_load :init
        def init do
          raise "oops"
        end
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        def a, do: A.no_func
        def b, do: A.init
      end
      """
    }

    warning = """
    warning: function A.no_func/0 is undefined or private
      lib/b.ex:2

    """

    assert_warnings(files, warning)
  end

  test "warnings: groups multiple warnings in one file" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: A.no_func

        @file "lib/external_source.ex"
        def b, do: A2.no_func

        def c, do: A.no_func
        def d, do: A2.no_func
      end
      """
    }

    warning = """
    warning: function A.no_func/0 is undefined or private
    Found at 2 locations:
      lib/a.ex:2
      lib/a.ex:7

    warning: function A2.no_func/0 is undefined (module A2 is not available)
    Found at 2 locations:
      lib/a.ex:8
      lib/external_source.ex:5

    """

    assert_warnings(files, warning)
  end

  test "warnings: handles module body conditionals" do
    files = %{
      "lib/a.ex" => """
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
      """
    }

    warning = """
    warning: function List.old_flatten/1 is undefined or private. Did you mean one of:

          * flatten/1
          * flatten/2

      lib/a.ex:15

    """

    assert_warnings(files, warning)
  end

  test "warnings: hints exclude deprecated functions" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def to_charlist(a), do: a

        @deprecated "Use String.to_charlist/1 instead"
        def to_char_list(a), do: a

        def c(a), do: A.to_list(a)
      end
      """
    }

    warning = """
    warning: function A.to_list/1 is undefined or private. Did you mean one of:

          * to_charlist/1

      lib/a.ex:7

    """

    assert_warnings(files, warning)
  end

  test "warnings: imports" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        import Record

        def a(a, b), do: extract(a, b)
        def b(arg), do: is_record(arg)
      end
      """
    }

    assert_no_warnings(files)
  end

  test "warnings: aliases" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        alias Enum, as: E

        def a(a, b), do: E.map2(a, b)
        def b, do: &E.map2/2

        @file "lib/external_source.ex"
        def c do
          alias Enum, as: EE
          &EE.map2/2
        end
      end
      """
    }

    warning = """
    warning: function Enum.map2/2 is undefined or private. Did you mean one of:

          * map/2

    Found at 3 locations:
      lib/a.ex:4
      lib/a.ex:5
      lib/external_source.ex:10

    """

    assert_warnings(files, warning)
  end

  test "warnings: requires" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        require Integer

        def a(a), do: Integer.is_even(a)
      end
      """
    }

    assert_no_warnings(files)
  end

  defp assert_warnings(files, expected) do
    in_fixture("no_mixfile", fn ->
      generate_files(files)
      output = capture_io(:stderr, fn -> Mix.Task.run("compile.xref") end)
      assert output == expected
    end)
  end

  defp assert_no_warnings(files) do
    in_fixture("no_mixfile", fn ->
      generate_files(files)
      output = capture_io(:stderr, fn -> Mix.Task.run("compile.xref") end)
      assert output == ""
    end)
  end

  ## Unreachable

  test "unreachable: reports nothing with no references" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A do end")

      assert Mix.Task.run("xref", ["unreachable"]) == :ok
    end)
  end

  test "unreachable: reports missing functions" do
    code = """
    defmodule A do
      def a, do: A.no_func
      def b, do: A.a()

      @file "lib/external_source.ex"
      def c, do: A.no_func
    end
    """

    warning = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:2: A.no_func/0
    lib/external_source.ex:6: A.no_func/0
    """

    assert_unreachable(code, warning)
  end

  test "unreachable: aborts if any" do
    code = """
    defmodule A do
      def a, do: A.no_func
    end
    """

    assert_raise Mix.Error, "mix xref unreachable failed: unreachable calls were found", fn ->
      assert_unreachable(code, "NOT USED", ~w(--abort-if-any))
    end
  end

  defp assert_unreachable(contents, expected, args \\ []) do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", ["unreachable" | args]) == :error
      end)

      assert ^expected = receive_until_no_messages([])
    end)
  end

  ## Deprecated

  test "deprecated: reports nothing with no references" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A do end")

      assert Mix.Task.run("xref", ["deprecated"]) == :ok
    end)
  end

  test "deprecated: reports deprecated functions" do
    code = """
    defmodule A do
      @deprecated "oops"
      def a, do: A.a
    end
    """

    warning = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:3: A.a/0
    """

    assert_deprecated(code, warning)
  end

  test "deprecated: reports deprecated structs" do
    code = """
    defmodule A do
      @deprecated "oops"
      defstruct [:x, :y]
      def match(%A{}), do: :ok
      def build(:ok), do: %A{}
    end
    """

    warning = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:4: A.__struct__/0
    lib/a.ex:5: A.__struct__/0
    """

    assert_deprecated(code, warning)
  end

  test "deprecated: aborts if any" do
    code = """
    defmodule A do
      @deprecated "oops"
      def a, do: A.a
    end
    """

    assert_raise Mix.Error, "mix xref deprecated failed: deprecated calls were found", fn ->
      assert_deprecated(code, "NOT USED", ~w(--abort-if-any))
    end
  end

  defp assert_deprecated(contents, expected, args \\ []) do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", contents)

      capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", ["deprecated" | args]) == :error
      end)

      assert ^expected = receive_until_no_messages([])
    end)
  end

  ## Exclude

  test "exclude: excludes specified modules and MFAs" do
    defmodule ExcludeSample do
      def project do
        [
          app: :sample,
          version: "0.1.0",
          xref: [exclude: [MissingModule, {MissingModule2, :no_func, 2}]]
        ]
      end
    end

    Mix.Project.pop()
    Mix.Project.push(ExcludeSample)

    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: MissingModule.no_func(1)
        def b, do: MissingModule2.no_func(1, 2)
        def c, do: MissingModule2.no_func(1)
        def d, do: MissingModule3.no_func(1, 2)
      end
      """
    }

    warning = """
    warning: function MissingModule2.no_func/1 is undefined (module MissingModule2 is not available)
      lib/a.ex:4

    warning: function MissingModule3.no_func/2 is undefined (module MissingModule3 is not available)
      lib/a.ex:5

    """

    assert_warnings(files, warning)
  end

  ## Callers

  test "callers: prints callers of specified Module" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: A.a()
        def a(arg), do: A.a(arg)
        def b, do: A.b()
        def c, do: B.a()

        @file "lib/external_source.ex"
        def d, do: A.a()
      end
      """
    }

    output = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:2: A.a/0
    lib/external_source.ex:8: A.a/0
    lib/a.ex:3: A.a/1
    lib/a.ex:4: A.b/0
    """

    assert_callers("A", files, output)
  end

  test "callers: prints callers of specified Module.func" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: A.a()
        def a(arg), do: A.a(arg)
        def b, do: A.b()
        def c, do: B.a()

        @file "lib/external_source.ex"
        def d, do: A.a()
      end
      """
    }

    output = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:2: A.a/0
    lib/external_source.ex:8: A.a/0
    lib/a.ex:3: A.a/1
    """

    assert_callers("A.a", files, output)
  end

  test "callers: prints callers of specified Module.func/arity" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: A.a()
        def a(arg), do: A.a(arg)
        def b, do: A.b()
        def c, do: B.a()

        @file "lib/external_source.ex"
        def d, do: A.a()
      end
      """
    }

    output = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:2: A.a/0
    lib/external_source.ex:8: A.a/0
    """

    assert_callers("A.a/0", files, output)
  end

  test "callers: lists compile calls and macros" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        defmacro a_macro, do: :ok
        def a, do: :ok
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        require A

        A.a_macro()
        A.a()

        @file "lib/external_source.ex"
        def b do
          A.a_macro()
          A.a()
        end
      end
      """
    }

    output = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/b.ex:5: A.a/0
    lib/external_source.ex:10: A.a/0
    lib/b.ex:4: A.a_macro/0
    lib/external_source.ex:9: A.a_macro/0
    """

    assert_callers("A", files, output)
  end

  test "callers: handles aliases" do
    files = %{
      "lib/a.ex" => """
      defmodule A do
        alias Enum, as: E

        E.map([], &E.flatten/1)

        def a(a, b), do: E.map(a, b)

        @file "lib/external_source.ex"
        def b() do
          alias Enum, as: EE
          EE.map([], &EE.flatten/1)
        end
      end
      """
    }

    output = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:4: Enum.flatten/1
    lib/external_source.ex:11: Enum.flatten/1
    lib/a.ex:4: Enum.map/2
    lib/a.ex:6: Enum.map/2
    lib/external_source.ex:11: Enum.map/2
    """

    assert_callers("Enum", files, output)
  end

  test "callers: handles imports" do
    files = %{
      "lib/a.ex" => ~S"""
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
      """,
      "lib/b.ex" => ~S"""
      defmodule B do
        &Integer.parse/1

        @file "lib/external_source.ex"
        def a(a) do
          import Integer
          parse(1)
          is_even(a)
        end
      end
      """
    }

    output = """
    Compiling 2 files (.ex)
    Generated sample app
    lib/a.ex:4: Integer.is_even/1
    lib/a.ex:7: Integer.is_even/1
    lib/a.ex:10: Integer.is_even/1
    lib/a.ex:12: Integer.is_even/1
    lib/external_source.ex:8: Integer.is_even/1
    lib/a.ex:5: Integer.parse/1
    lib/a.ex:8: Integer.parse/1
    lib/a.ex:11: Integer.parse/1
    lib/b.ex:2: Integer.parse/1
    lib/external_source.ex:7: Integer.parse/1
    """

    assert_callers("Integer", files, output)
  end

  test "callers: no argument gives error" do
    in_fixture("no_mixfile", fn ->
      message = "xref doesn't support this command. For more information run \"mix help xref\""

      assert_raise Mix.Error, message, fn ->
        assert Mix.Task.run("xref", ["callers"]) == :error
      end
    end)
  end

  test "callers: gives nice error for quotable but invalid callers spec" do
    in_fixture("no_mixfile", fn ->
      message =
        "xref callers CALLEE expects Module, Module.function, or Module.function/arity, got: Module.func(arg)"

      assert_raise Mix.Error, message, fn ->
        Mix.Task.run("xref", ["callers", "Module.func(arg)"])
      end
    end)
  end

  test "callers: gives nice error for unquotable callers spec" do
    in_fixture("no_mixfile", fn ->
      message =
        "xref callers CALLEE expects Module, Module.function, or Module.function/arity, got: %"

      assert_raise Mix.Error, message, fn ->
        Mix.Task.run("xref", ["callers", "%"])
      end
    end)
  end

  defp assert_callers(callee, files, expected) do
    in_fixture("no_mixfile", fn ->
      for {file, contents} <- files do
        File.write!(file, contents)
      end

      capture_io(:stderr, fn ->
        assert Mix.Task.run("xref", ["callers", callee]) == :ok
      end)

      assert ^expected = receive_until_no_messages([])
    end)
  end

  ## Graph

  test "graph: basic usage" do
    assert_graph("""
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    lib/b.ex
    lib/c.ex
    lib/d.ex
    └── lib/a.ex (compile)
    """)
  end

  test "graph: stats" do
    assert_graph(["--format", "stats"], """
    Tracked files: 4 (nodes)
    Compile dependencies: 1 (edges)
    Structs dependencies: 0 (edges)
    Runtime dependencies: 2 (edges)

    Top 4 files with most outgoing dependencies:
      * lib/d.ex (1)
      * lib/b.ex (1)
      * lib/a.ex (1)
      * lib/c.ex (0)

    Top 2 files with most incoming dependencies:
      * lib/a.ex (2)
      * lib/b.ex (1)
    """)
  end

  test "graph: exclude many" do
    assert_graph(~w[--exclude lib/c.ex --exclude lib/b.ex], """
    lib/a.ex
    lib/d.ex
    └── lib/a.ex (compile)
    """)
  end

  test "graph: exclude one" do
    assert_graph(~w[--exclude lib/d.ex], """
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    lib/b.ex
    lib/c.ex
    """)
  end

  test "graph: source" do
    assert_graph(~w[--source lib/a.ex], """
    lib/b.ex
    └── lib/a.ex
        └── lib/b.ex
    """)
  end

  test "graph: only nodes" do
    assert_graph(~w[--only-nodes], """
    lib/a.ex
    lib/b.ex
    lib/c.ex
    lib/d.ex
    """)
  end

  test "graph: filter by compile label" do
    assert_graph(~w[--label compile], """
    lib/a.ex
    lib/b.ex
    lib/c.ex
    lib/d.ex
    └── lib/a.ex (compile)
    """)
  end

  test "graph: filter by runtime label" do
    assert_graph(~w[--label runtime], """
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    lib/b.ex
    lib/c.ex
    lib/d.ex
    """)
  end

  test "graph: invalid source" do
    assert_raise Mix.Error, "Source could not be found: lib/a2.ex", fn ->
      assert_graph(~w[--source lib/a2.ex], "")
    end
  end

  test "graph: sink" do
    assert_graph(~w[--sink lib/b.ex], """
    lib/a.ex
    └── lib/b.ex
        └── lib/a.ex
    lib/d.ex
    └── lib/a.ex (compile)
    """)
  end

  test "graph: invalid sink" do
    assert_raise Mix.Error, "Sink could not be found: lib/b2.ex", fn ->
      assert_graph(~w[--sink lib/b2.ex], "")
    end
  end

  test "graph: sink and source is error" do
    assert_raise Mix.Error, "mix xref graph expects only one of --source and --sink", fn ->
      assert_graph(~w[--source lib/a.ex --sink lib/b.ex], "")
    end
  end

  test "graph: with dynamic module" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      B.define()
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def define do
          defmodule A do
          end
        end
      end
      """)

      assert Mix.Task.run("xref", ["graph", "--format", "dot"]) == :ok

      assert File.read!("xref_graph.dot") === """
             digraph "xref graph" {
               "lib/a.ex"
               "lib/b.ex"
             }
             """
    end)
  end

  test "graph: with struct" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def fun do
          %B{}
        end
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        defstruct []
      end
      """)

      assert Mix.Task.run("xref", ["graph", "--format", "dot"]) == :ok

      assert File.read!("xref_graph.dot") === """
             digraph "xref graph" {
               "lib/a.ex"
               "lib/a.ex" -> "lib/b.ex" [label="(struct)"]
               "lib/b.ex"
             }
             """
    end)
  end

  test "graph: with mixed cyclic dependencies" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A.Behaviour do
        @callback foo :: :foo
      end

      defmodule A do
        B

        def foo do
          :foo
        end
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        # Let's also test that we track literal atom behaviours
        @behaviour :"Elixir.A.Behaviour"

        def foo do
          A.foo
        end
      end
      """)

      assert Mix.Task.run("xref", ["graph", "--format", "dot"]) == :ok

      assert File.read!("xref_graph.dot") === """
             digraph "xref graph" {
               "lib/a.ex"
               "lib/a.ex" -> "lib/b.ex" [label="(compile)"]
               "lib/b.ex" -> "lib/a.ex" [label="(compile)"]
               "lib/b.ex"
             }
             """
    end)
  end

  defp assert_graph(opts \\ [], expected) do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def a do
          B.a
        end

        def b, do: :ok
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def a do
          A.a
          B.a
        end
      end
      """)

      File.write!("lib/c.ex", """
      defmodule C do
      end
      """)

      File.write!("lib/d.ex", """
      defmodule :d do
        A.b
      end
      """)

      assert Mix.Task.run("xref", opts ++ ["graph"]) == :ok

      assert "Compiling 4 files (.ex)\nGenerated sample app\n" <> result =
               receive_until_no_messages([])

      assert normalize_graph_output(result) == normalize_graph_output(expected)
    end)
  end

  defp normalize_graph_output(graph) do
    String.replace(graph, "└──", "`--")
  end

  describe "inside umbrellas" do
    test "generates reports considering siblings" do
      Mix.Project.pop()

      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:bar, "apps/bar", fn _ ->
          File.write!("lib/bar.ex", """
          defmodule Bar do
            def bar do
              Foo.foo
            end
          end
          """)

          Mix.Task.run("compile")
          Mix.shell().flush()

          Mix.Tasks.Xref.run(["graph", "--format", "stats", "--include-siblings"])

          assert receive_until_no_messages([]) == """
                 Tracked files: 2 (nodes)
                 Compile dependencies: 0 (edges)
                 Structs dependencies: 0 (edges)
                 Runtime dependencies: 1 (edges)

                 Top 2 files with most outgoing dependencies:
                   * lib/bar.ex (1)
                   * lib/foo.ex (0)

                 Top 1 files with most incoming dependencies:
                   * lib/foo.ex (1)
                 """

          Mix.Tasks.Xref.run(["callers", "Foo.foo"])

          assert receive_until_no_messages([]) == """
                 lib/bar.ex:3: Foo.foo/0
                 """
        end)
      end)
    end
  end

  ## Helpers

  defp receive_until_no_messages(acc) do
    receive do
      {:mix_shell, :info, [line]} -> receive_until_no_messages([acc, line | "\n"])
    after
      0 -> IO.iodata_to_binary(acc)
    end
  end

  defp generate_files(files) do
    for {file, contents} <- files do
      File.write!(file, contents)
    end
  end
end
