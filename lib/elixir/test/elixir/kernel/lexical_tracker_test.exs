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
    assert D.remote_references(config[:pid]) == {[], [], [String]}

    D.remote_dispatch(config[:pid], String, :compile)
    assert D.remote_references(config[:pid]) == {[String], [], []}

    D.remote_dispatch(config[:pid], String, :runtime)
    assert D.remote_references(config[:pid]) == {[String], [], []}
  end

  test "can add remote structs", config do
    D.remote_struct(config[:pid], URI)
    assert D.remote_references(config[:pid]) == {[], [URI], []}

    D.remote_dispatch(config[:pid], URI, :runtime)
    assert D.remote_references(config[:pid]) == {[], [URI], [URI]}

    D.remote_dispatch(config[:pid], URI, :compile)
    assert D.remote_references(config[:pid]) == {[URI], [URI], []}
  end

  test "can add module imports", config do
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1})
    assert D.remote_references(config[:pid]) == {[String], [], []}

    D.import_dispatch(config[:pid], String, {:upcase, 1})
    assert D.remote_references(config[:pid]) == {[String], [], []}
  end

  test "can add module with {function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1})
    assert D.remote_references(config[:pid]) == {[String], [], []}
  end

  test "can add aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    D.alias_dispatch(config[:pid], String)
    assert D.remote_references(config[:pid]) == {[], [], []}
  end

  test "unused module imports", config do
    D.add_import(config[:pid], String, [], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, 1}]
  end

  test "used module imports are not unused", config do
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1})
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "unused {module, function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, 1}, {{String, :upcase, 1}, 1}]
  end

  test "used {module, function, arity} imports are not unused", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [downcase: 1], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1})
    assert D.collect_unused_imports(config[:pid]) == [{{String, :downcase, 1}, 1}]
  end

  test "overwriting {module, function, arity} import with module import", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:downcase, 1})
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
    {{compile, _structs, runtime}, _binding} =
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.Typespecs do
        alias Foo.Bar, as: Bar, warn: false
        @type bar :: Foo.Bar.t
        @opaque bar2 :: Foo.Bar.t
        @typep bar3 :: Foo.Bar.t
        @callback foo :: Foo.Bar.t
        @macrocallback foo2(Foo.Bar.t) :: Foo.Bar.t
        @spec foo(bar3) :: Foo.Bar.t
        def foo(_), do: :bar
        Kernel.LexicalTracker.remote_references(__ENV__.lexical_tracker)
      end |> elem(3)
      """)

    refute Elixir.Bar in runtime
    refute Elixir.Bar in compile

    refute Foo.Bar in runtime
    refute Foo.Bar in compile
  end
end
