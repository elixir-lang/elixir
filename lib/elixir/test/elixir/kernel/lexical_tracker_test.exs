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
    assert D.remote_dispatches(config[:pid]) == {%{}, %{String => %{{:upcase, 1} => [1]}}}
    assert D.remote_references(config[:pid]) == {[], [String]}

    D.remote_dispatch(config[:pid], String, {:upcase, 1}, 1, :compile)
    assert D.remote_dispatches(config[:pid]) ==
      {%{String => %{{:upcase, 1} => [1]}}, %{String => %{{:upcase, 1} => [1]}}}
    assert D.remote_references(config[:pid]) == {[String], []}

    D.remote_dispatch(config[:pid], String, {:upcase, 1}, 1, :runtime)
    assert D.remote_dispatches(config[:pid]) ==
      {%{String => %{{:upcase, 1} => [1]}}, %{String => %{{:upcase, 1} => [1]}}}
    assert D.remote_references(config[:pid]) == {[String], []}

    D.remote_dispatch(config[:pid], String, {:upcase, 1}, 2, :runtime)
    assert D.remote_dispatches(config[:pid]) ==
      {%{String => %{{:upcase, 1} => [1]}}, %{String => %{{:upcase, 1} => [2, 1]}}}
    assert D.remote_references(config[:pid]) == {[String], []}
  end

  test "can add module imports", config do
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1}, 1, :compile)
    assert D.remote_references(config[:pid]) == {[String], []}
    assert D.remote_dispatches(config[:pid]) ==
      {%{String => %{{:upcase, 1} => [1]}}, %{}}

    D.import_dispatch(config[:pid], String, {:upcase, 1}, 1, :runtime)
    assert D.remote_references(config[:pid]) == {[String], []}
    assert D.remote_dispatches(config[:pid]) ==
      {%{String => %{{:upcase, 1} => [1]}}, %{String => %{{:upcase, 1} => [1]}}}
  end

  test "can add module with {function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1}, 1, :compile)
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
    D.import_dispatch(config[:pid], String, {:upcase, 1}, 1, :compile)
    assert D.collect_unused_imports(config[:pid]) == []
  end

  test "unused {module, function, arity} imports", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    assert D.collect_unused_imports(config[:pid]) == [{String, 1}, {{String, :upcase, 1}, 1}]
  end

  test "used {module, function, arity} imports are not unused", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [downcase: 1], 1, true)
    D.import_dispatch(config[:pid], String, {:upcase, 1}, 1, :compile)
    assert D.collect_unused_imports(config[:pid]) == [{{String, :downcase, 1}, 1}]
  end

  test "overwriting {module, function, arity} import with module import", config do
    D.add_import(config[:pid], String, [upcase: 1], 1, true)
    D.add_import(config[:pid], String, [], 1, true)
    D.import_dispatch(config[:pid], String, {:downcase, 1}, 1, :compile)
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
    {{{compile, runtime}, {compile_dispatches, runtime_dispatches}}, _binding} =
      Code.eval_string("""
      defmodule Kernel.LexicalTrackerTest.Sample do
        alias Foo.Bar, as: Bar, warn: false
        @type bar :: Foo.Bar.t
        @opaque bar2 :: Foo.Bar.t
        @typep bar3 :: Foo.Bar.t
        @callback foo :: Foo.Bar.t
        @macrocallback foo2(Foo.Bar.t) :: Foo.Bar.t
        @spec foo(bar3) :: Foo.Bar.t
        def foo(_), do: :bar
        refs = Kernel.LexicalTracker.remote_references(__ENV__.module)
        dispatches = Kernel.LexicalTracker.remote_dispatches(__ENV__.module)
        {refs, dispatches}
      end |> elem(3)
      """)

    refute Elixir.Bar in runtime
    refute Map.has_key?(runtime_dispatches, Elixir.Bar)
    refute Elixir.Bar in compile
    refute Map.has_key?(compile_dispatches, Elixir.Bar)

    refute Foo.Bar in runtime
    refute Map.has_key?(runtime_dispatches, Foo.Bar)
    refute Foo.Bar in compile
    refute Map.has_key?(compile_dispatches, Foo.Bar)
  end

  test "remote dispatches" do
    {{compile_remote_calls, runtime_remote_calls}, []} =
      Code.eval_string("""
      defmodule RemoteDispatches do
        import Record
        require Integer
        alias Remote, as: R

        def a do
          _ = extract(1, 2)
          _ = is_record(1)
          _ = Integer.is_even(2)

          NotAModule
          Remote.func()
          R.func()
          &extract/2
          &is_record/1
          &R.func/0
          &Remote.func/0
          &Integer.is_even/1
          %Macro.Env{}
        end

        &extract/2
        &is_record/1
        &R.func/0
        &Remote.func/0
        &Integer.is_even/1

        &is_record/1; def b(a), do: is_record(a)

        %Macro.Env{}

        Kernel.LexicalTracker.remote_dispatches(__ENV__.module)
      end |> elem(3)
      """)

    compile_remote_calls = unroll_dispatches(compile_remote_calls)
    assert {6, Kernel, :def, 2} in compile_remote_calls
    assert {8, Record, :is_record, 1} in compile_remote_calls
    assert {9, Integer, :is_even, 1} in compile_remote_calls
    assert {15, Record, :is_record, 1} in compile_remote_calls
    assert {18, Integer, :is_even, 1} in compile_remote_calls
    assert {19, Macro.Env, :__struct__, 1} in compile_remote_calls
    assert {22, Record, :extract, 2} in compile_remote_calls
    assert {23, Record, :is_record, 1} in compile_remote_calls
    assert {24, Remote, :func, 0} in compile_remote_calls
    assert {25, Remote, :func, 0} in compile_remote_calls
    assert {26, Integer, :is_even, 1} in compile_remote_calls
    assert {28, Kernel, :def, 2} in compile_remote_calls
    assert {28, Record, :is_record, 1} in compile_remote_calls
    assert {30, Macro.Env, :__struct__, 1} in compile_remote_calls
    assert {32, Kernel.LexicalTracker, :remote_dispatches, 1} in compile_remote_calls

    runtime_remote_calls = unroll_dispatches(runtime_remote_calls)
    assert {7, Record, :extract, 2} in runtime_remote_calls
    assert {8, :erlang, :is_tuple, 1} in runtime_remote_calls
    assert {12, Remote, :func, 0} in runtime_remote_calls
    assert {13, Remote, :func, 0} in runtime_remote_calls
    assert {14, Record, :extract, 2} in runtime_remote_calls
    assert {15, :erlang, :is_tuple, 1} in runtime_remote_calls
    assert {16, Remote, :func, 0} in runtime_remote_calls
    assert {17, Remote, :func, 0} in runtime_remote_calls
    assert {18, :erlang, :==, 2} in runtime_remote_calls
    assert {28, :erlang, :is_tuple, 1} in runtime_remote_calls
  end

  defp unroll_dispatches(dispatches) do
    for {module, fals} <- dispatches,
        {{func, arity}, lines} <- fals,
        line <- lines,
        do: {line, module, func, arity}
  end
end
