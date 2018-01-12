Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DocsTest do
  use ExUnit.Case

  import PathHelpers

  test "attributes format" do
    defmodule DocAttributes do
      @moduledoc "Module doc"
      assert @moduledoc == "Module doc"
      assert Module.get_attribute(__MODULE__, :moduledoc) == {__ENV__.line - 2, "Module doc"}

      @typedoc "Type doc"
      assert @typedoc == "Type doc"
      assert Module.get_attribute(__MODULE__, :typedoc) == {__ENV__.line - 2, "Type doc"}
      @type foobar :: any

      @doc "Function doc"
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {__ENV__.line - 2, "Function doc"}

      def foobar() do
        :ok
      end
    end
  end

  test "compiled without docs" do
    Code.compiler_options(docs: false)

    write_beam(
      defmodule WithoutDocs do
        @moduledoc "Module doc"

        @doc "Some doc"
        def foobar(arg), do: arg
      end
    )

    assert Code.get_docs(WithoutDocs, :docs) == nil
    assert Code.get_docs(WithoutDocs, :moduledoc) == nil
    assert Code.get_docs(WithoutDocs, :type_docs) == nil
    assert Code.get_docs(WithoutDocs, :callback_docs) == nil
  after
    Code.compiler_options(docs: true)
  end

  test "compiled in memory does not have accessible docs" do
    defmodule InMemoryDocs do
      @moduledoc "Module doc"

      @doc "Some doc"
      def foobar(arg), do: arg
    end

    assert Code.get_docs(InMemoryDocs, :docs) == nil
    assert Code.get_docs(InMemoryDocs, :moduledoc) == nil
    assert Code.get_docs(InMemoryDocs, :callback_docs) == nil
  end

  test "raises on invalid @since" do
    assert_raise ArgumentError, ~r"should be a string representing the version", fn ->
      defmodule InvalidSince do
        @since 1.2
        def foo, do: :bar
      end
    end
  end

  test "raises on invalid @doc" do
    assert_raise ArgumentError, ~r/When set dynamically, it should be {line, doc}/, fn ->
      defmodule DocAttributesFormat do
        Module.put_attribute(__MODULE__, :moduledoc, "Other")
      end
    end

    assert_raise ArgumentError, ~r/should be a binary, a boolean, or nil/, fn ->
      defmodule AtSyntaxDocAttributesFormat do
        @moduledoc :not_a_binary
      end
    end
  end

  describe "compiled with docs" do
    test "infers signatures" do
      write_beam(
        defmodule SignatureDocs do
          def arg_names([], [], %{}, [], %{}), do: false

          @year 2015
          def with_defaults(@year, arg \\ 0, year \\ @year, fun \\ &>=/2) do
            {fun, arg + year}
          end

          def with_struct(%URI{}), do: :ok

          def with_underscore({_, _} = _two_tuple), do: :ok
          def with_underscore(_), do: :error

          def only_underscore(_), do: :ok

          def two_good_names(first, :ok), do: first
          def two_good_names(second, :error), do: second
        end
      )

      assert [
               arg_names,
               only_underscore,
               two_good_names,
               with_defaults,
               with_struct,
               with_underscore
             ] = Code.get_docs(SignatureDocs, :docs)

      # arg_names/5
      assert {{:arg_names, 5}, _, :def, args, nil} = arg_names

      assert [
               {:list1, _, Elixir},
               {:list2, _, Elixir},
               {:map1, _, Elixir},
               {:list3, _, Elixir},
               {:map2, _, Elixir}
             ] = args

      # only_underscore/1
      assert {{:only_underscore, 1}, _, :def, [{:_, _, Elixir}], nil} = only_underscore

      # two_good_names/2
      assert {{:two_good_names, 2}, _, :def, args, nil} = two_good_names
      assert [{:first, _, nil}, {:atom, _, Elixir}] = args

      # with_defaults/4
      assert {{:with_defaults, 4}, _, :def, args, nil} = with_defaults

      assert [
               {:int, _, Elixir},
               {:\\, _, [{:arg, _, nil}, 0]},
               {:\\, _, [{:year, _, nil}, 2015]},
               {:\\, _, [{:fun, _, nil}, {:&, _, [{:/, _, [{:>=, _, nil}, 2]}]}]}
             ] = args

      # with_struct/1
      assert {{:with_struct, 1}, _, :def, [{:uri, _, Elixir}], nil} = with_struct

      # with_underscore/1
      assert {{:with_underscore, 1}, _, :def, [{:two_tuple, _, nil}], nil} = with_underscore
    end

    test "includes docs for functions, modules, types and callbacks" do
      write_beam(
        defmodule SampleDocs do
          @moduledoc "Module doc"

          @typedoc "Type doc"
          @since "1.2.3"
          @type foo(any) :: any

          @typedoc "Opaque type doc"
          @opaque bar(any) :: any

          @doc "Callback doc"
          @since "1.2.3"
          @callback foo(any) :: any

          @doc false
          @callback bar() :: term
          @callback baz(any, term) :: any

          @doc "Macrocallback doc"
          @macrocallback qux(any) :: any

          @doc "Function doc"
          @since "1.2.3"
          def foo(arg), do: arg + 1

          @doc "Multiple bodiless clause doc"
          @since "1.2.3"
          def bar(_arg)
          def bar(_arg)
          def bar(arg), do: arg + 1

          @doc "Wrong doc"
          @since "1.2"
          def baz(_arg)
          def baz(arg), do: arg + 1
          @doc "Multiple bodiless clause and docs"
          @since "1.2.3"
          def baz(_arg)

          @doc false
          def qux(true), do: false
        end
      )

      docs = Code.get_docs(SampleDocs, :all)
      assert Code.get_docs(SampleDocs, :docs) == docs[:docs]
      assert Code.get_docs(SampleDocs, :moduledoc) == docs[:moduledoc]
      assert Code.get_docs(SampleDocs, :type_docs) == docs[:type_docs]
      assert Code.get_docs(SampleDocs, :callback_docs) == docs[:callback_docs]

      assert [
               {{:bar, 1}, _, :def, [{:arg, _, nil}], "Multiple bodiless clause doc"},
               {{:baz, 1}, _, :def, [{:arg, _, nil}], "Multiple bodiless clause and docs"},
               {{:foo, 1}, _, :def, [{:arg, _, nil}], "Function doc"},
               {{:qux, 1}, _, :def, [{:bool, _, Elixir}], false}
             ] = docs[:docs]

      assert {_, "Module doc"} = docs[:moduledoc]

      assert [{{:bar, 1}, _, :opaque, "Opaque type doc"}, {{:foo, 1}, _, :type, "Type doc"}] =
               docs[:type_docs]

      assert [
               {{:bar, 0}, _, :callback, false},
               {{:baz, 2}, _, :callback, nil},
               {{:foo, 1}, _, :callback, "Callback doc"},
               {{:qux, 1}, _, :macrocallback, "Macrocallback doc"}
             ] = docs[:callback_docs]
    end
  end
end
