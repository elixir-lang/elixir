Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.LexicalTrackerTest do
  use ExUnit.Case, async: true

  alias Kernel.LexicalTracker, as: D

  setup do
    {:ok, pid} = D.start_link()
    {:ok, [pid: pid]}
  end

  test "can add remote dispatch", config do
    D.remote_dispatch(config[:pid], String, :runtime)
    assert D.references(config[:pid]) == {[], [], [String], []}

    D.remote_dispatch(config[:pid], String, :compile)
    assert D.references(config[:pid]) == {[String], [], [], []}

    D.remote_dispatch(config[:pid], String, :runtime)
    assert D.references(config[:pid]) == {[String], [], [], []}
  end

  test "can add requires", config do
    D.add_require(config[:pid], URI)
    assert D.references(config[:pid]) == {[], [URI], [], []}

    D.remote_dispatch(config[:pid], URI, :runtime)
    assert D.references(config[:pid]) == {[], [URI], [URI], []}

    D.remote_dispatch(config[:pid], URI, :compile)
    assert D.references(config[:pid]) == {[URI], [URI], [], []}
  end

  test "can add module imports", config do
    D.add_require(config[:pid], String)
    D.add_import(config[:pid], String, [], 1, true)

    D.import_dispatch(config[:pid], String, {:upcase, 1}, :runtime)
    assert D.references(config[:pid]) == {[], [String], [String], []}

    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.references(config[:pid]) == {[String], [String], [], []}
  end

  test "can add module with {function, arity} imports", config do
    D.add_require(config[:pid], String)
    D.add_import(config[:pid], String, [upcase: 1], 1, true)

    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.references(config[:pid]) == {[String], [String], [], []}
  end

  test "can add aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    D.alias_dispatch(config[:pid], String)
    assert D.references(config[:pid]) == {[], [], [], []}
  end

  test "unused module imports", config do
    D.add_import(config[:pid], String, [], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, 1}]
  end

  test "used module imports are not unused", config do
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "unused {module, function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, 1}, {{String, :upcase, 1}, 1}]
  end

  test "used {module, function, arity} imports are not unused", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [downcase: 1], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1}, :compile)
    assert D.collect_unused_imports(config[:pid]) == [{{String, :downcase, 1}, 1}]
  end

  test "overwriting {module, function, arity} import with module import", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:downcase, 1}, :compile)
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "imports with no warn are not unused", config do
    D.add_import(config[:pid], String, [], 1, false)
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "unused aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    assert D.collect_unused_aliases(config[:pid]) == [{String, 1}]
  end

  test "used aliases are not unused", config do
    D.add_alias(config[:pid], String, 1, true)
    D.alias_dispatch(config[:pid], String)
    assert D.collect_unused_aliases(config[:pid]) == []
  end

  test "aliases with no warn are not unused", config do
    D.add_alias(config[:pid], String, 1, false)
    assert D.collect_unused_aliases(config[:pid]) == []
  end

  test "does not tag aliases nor types" do
    Code.eval_string("""
    defmodule Kernel.LexicalTrackerTest.AliasTypespecs do
      alias Foo.Bar, as: Bar, warn: false
      @type bar :: Foo.Bar | Foo.Bar.t
      @opaque bar2 :: Foo.Bar.t
      @typep bar3 :: Foo.Bar.t
      @callback foo :: Foo.Bar.t
      @macrocallback foo2(Foo.Bar.t) :: Foo.Bar.t
      @spec foo(bar3) :: Foo.Bar.t
      def foo(_), do: :ok

      # References from specs are processed only late
      @after_compile __MODULE__
      def __after_compile__(env, _) do
        send(self(), {:references, Kernel.LexicalTracker.references(env.lexical_tracker)})
      end
    end
    """)

    assert_received {:references, {compile, _exports, runtime, _}}

    refute Elixir.Bar in runtime
    refute Elixir.Bar in compile

    refute Foo.Bar in runtime
    refute Foo.Bar in compile
  end

  test "does not tag imports" do
    {{compile, exports, runtime, _}, _binding} =
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.Imports do
        import String, warn: false
        Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
      end |> elem(3)
      """)

    refute String in compile
    assert String in exports
    refute String in runtime
  end

  test "defdelegate with literal does not add compile dependency" do
    {{compile, _exports, _runtime, _}, _binding} =
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.Defdelegate do
        defdelegate a, to: A

        opts = [to: B]
        defdelegate b, opts

        Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
      end |> elem(3)
      """)

    refute A in compile
    assert B in compile
  end

  test "Macro.struct! adds an export dependency" do
    {{compile, exports, runtime, _}, _binding} =
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.MacroStruct do
        Macro.struct!(:"Elixir.URI", __ENV__)
        Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
      end |> elem(3)
      """)

    refute URI in compile
    assert URI in exports
    refute URI in runtime
  end
end
