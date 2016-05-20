Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.LexicalTrackerTest do
  use ExUnit.Case, async: true

  alias Kernel.LexicalTracker, as: D

  setup do
    {:ok, pid} = D.start_link("dest")
    {:ok, [pid: pid]}
  end

  test "gets the destination", config do
    assert D.dest(config[:pid]) == "dest"
  end

  test "can add remote references", config do
    D.remote_reference(config[:pid], String, :runtime)
    assert D.remote_references(config[:pid]) == {[], [String]}

    D.remote_reference(config[:pid], String, :compile)
    assert D.remote_references(config[:pid]) == {[String], []}

    D.remote_reference(config[:pid], String, :runtime)
    assert D.remote_references(config[:pid]) == {[String], []}
  end

  test "can add remote dispatches with {function, arity} and line", config do
    D.remote_dispatch(config[:pid], String, {:upcase, 1}, 1, :runtime)
    assert D.remote_dispatches(config[:pid]) == {[], [{String, {:upcase, 1}, 1}]}
    assert D.remote_references(config[:pid]) == {[], [String]}

    D.remote_dispatch(config[:pid], String, {:upcase, 1}, 1, :compile)
    assert D.remote_dispatches(config[:pid]) == {[{String, {:upcase, 1}, 1}], []}
    assert D.remote_references(config[:pid]) == {[String], []}

    D.remote_dispatch(config[:pid], String, {:upcase, 1}, 1, :runtime)
    assert D.remote_dispatches(config[:pid]) == {[{String, {:upcase, 1}, 1}], []}
    assert D.remote_references(config[:pid]) == {[String], []}

    D.remote_dispatch(config[:pid], String, {:upcase, 1}, 2, :runtime)
    assert D.remote_dispatches(config[:pid]) == {[{String, {:upcase, 1}, 1}], [{String, {:upcase, 1}, 2}]}
    assert D.remote_references(config[:pid]) == {[String], []}
  end

  test "can add module imports", config do
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], {String, :upcase, 1})
    assert D.remote_references(config[:pid]) == {[String], []}
  end

  test "can add module with {function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.import_dispatch(config[:pid], {String, :upcase, 1})
    assert D.remote_references(config[:pid]) == {[String], []}
  end

  test "can add aliases", config do
    D.add_alias(config[:pid], String, 1, true)
    D.alias_dispatch(config[:pid], String)
    assert D.remote_references(config[:pid]) == {[], []}
  end

  test "unused module imports", config do
    D.add_import(config[:pid], String, [], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, 1}]
  end

  test "used module imports are not unused", config do
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], {String, :upcase, 1})
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "unused {module, function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, 1}, {{String, :upcase, 1}, 1}]
  end

  test "used {module, function, arity} imports are not unused", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [downcase: 1], 1, true)
    D.import_dispatch(config[:pid], {String, :upcase, 1})
    assert D.collect_unused_imports(config[:pid]) == [{{String, :downcase, 1}, 1}]
  end

  test "overwriting {module, function, arity} import with module import", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], {String, :downcase, 1})
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

  test "does not tag aliases nor types as compile time" do
    {{compile, runtime}, _binding} =
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.Sample do
        alias Foo.Bar, as: Bar, warn: false
        @spec foo :: Foo.Bar.t
        def foo, do: Bar.t
        Kernel.LexicalTracker.remote_references(__ENV__.module)
      end |> elem(3)
      """)

    refute Elixir.Bar in runtime
    refute Elixir.Bar in compile

    assert Foo.Bar in runtime
    refute Foo.Bar in compile
  end

  test "call graph" do
  remotes =
    Code.eval_string("""
    defmodule CallGraphTesterModule do
      import Record
      require Integer

      def reference do
        _ = extract(1, 2)
        _ = is_record({:record})
        _ = Integer.is_even(2)

        NotAModule
        MissingModuleReferencer.no_func()
      end

      references = Kernel.LexicalTracker.remote_references(__ENV__.module)
      dispatches = Kernel.LexicalTracker.remote_dispatches(__ENV__.module)

      {references, dispatches}
    end |> elem(3)
    """)

  {{compile, runtime}, {compile_remote_calls, runtime_remote_calls}} =
    elem(remotes, 0)

  assert Enum.sort(compile) == [Integer, Kernel, Kernel.LexicalTracker, Record, :elixir_def]
  assert Enum.sort(runtime) == [Bitwise, MissingModuleReferencer, NotAModule, :erlang]

  assert Enum.sort(compile_remote_calls) == [
    {Kernel.LexicalTracker, {:remote_dispatches, 1}, 15},
    {Kernel.LexicalTracker, {:remote_references, 1}, 14},
    {:elixir_def, {:store_definition, 6}, 5}
  ]

  assert Enum.sort(runtime_remote_calls) == [
    {MissingModuleReferencer, {:no_func, 0}, 11},
    {Record, {:extract, 2}, 6},
    {:erlang, {:==, 2}, 8},
    {:erlang, {:>, 2}, 7},
    {:erlang, {:andalso, 2}, 7},
    {:erlang, {:band, 2}, 8},
    {:erlang, {:element, 2}, 7},
    {:erlang, {:is_atom, 1}, 7},
    {:erlang, {:is_tuple, 1}, 7},
    {:erlang, {:tuple_size, 1}, 7}
  ]
end
end
