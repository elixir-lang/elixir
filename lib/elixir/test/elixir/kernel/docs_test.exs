Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.DocsTest do
  use ExUnit.Case

  test "attributes format" do
    defmodule DocAttributes do
      @moduledoc "Module doc"
      assert @moduledoc == "Module doc"
      assert Module.get_attribute(__MODULE__, :moduledoc) == {8, "Module doc"}

      @typedoc "Type doc"
      assert @typedoc == "Type doc"
      assert Module.get_attribute(__MODULE__, :typedoc) == {12, "Type doc"}
      @type foobar :: any

      @doc "Function doc"
      assert @doc == "Function doc"
      assert Module.get_attribute(__MODULE__, :doc) == {17, "Function doc"}
      def foobar() do
        :ok
      end
    end
  end

  test "compiled with docs" do
    deftestmodule(SampleDocs)

    docs = Code.get_docs(SampleDocs, :all)
    assert Code.get_docs(SampleDocs, :docs) == docs[:docs]
    assert Code.get_docs(SampleDocs, :moduledoc) == docs[:moduledoc]
    assert Code.get_docs(SampleDocs, :type_docs) == docs[:type_docs]
    assert Code.get_docs(SampleDocs, :callback_docs) == docs[:callback_docs]

    assert [{{:arg_names, 5}, _, :def,
             [{:list1, [], Elixir},
              {:list2, [], Elixir},
              {:map1, [], Elixir},
              {:list3, [], Elixir},
              {:map2, [], Elixir}], nil},
            {{:foo, 1}, _, :def, [{:arg, [], nil}], "Function doc"},
            {{:foobar, 0}, _, :def, [], nil},
            {{:qux, 1}, _, :def, [{:bool, [], Elixir}], false},
            {{:with_defaults, 4}, _, :def,
             [{:int, [], Elixir},
              {:\\, [], [{:arg, [], nil}, 0]},
              {:\\, [], [{:year, [], nil}, 2015]},
              {:\\, [], [{:fun, [], nil}, {:&, _, [{:/, _, [{:>=, _, nil}, 2]}]}]}], nil}] = docs[:docs]

    assert {_, "Module doc"} = docs[:moduledoc]

    assert [{{:bar, 1}, _, :opaque, "Opaque type doc"},
            {{:foo, 1}, _, :type, "Type doc"}] = docs[:type_docs]

    assert [{{:bar, 0}, _, :callback, false},
            {{:baz, 2}, _, :callback, nil},
            {{:foo, 1}, _, :callback, "Callback doc"},
            {{:qux, 1}, _, :macrocallback, "Macrocallback doc"}] = docs[:callback_docs]
  end

  test "compiled without docs" do
    Code.compiler_options(docs: false)

    deftestmodule(WithoutSampleDocs)

    assert Code.get_docs(WithoutSampleDocs, :docs) == nil
    assert Code.get_docs(WithoutSampleDocs, :moduledoc) == nil
    assert Code.get_docs(WithoutSampleDocs, :type_docs) == nil
    assert Code.get_docs(WithoutSampleDocs, :callback_docs) == nil
  after
    Code.compiler_options(docs: true)
  end

  test "compiled in memory does not have accessible docs" do
    defmodule WithoutDocs do
      @moduledoc "Module doc"

      @doc "Some doc"
      def foobar(arg), do: arg
    end

    assert Code.get_docs(NoDocs, :docs) == nil
    assert Code.get_docs(NoDocs, :moduledoc) == nil
    assert Code.get_docs(NoDocs, :callback_docs) == nil
  end

  defp deftestmodule(name) do
    import PathHelpers

    write_beam(defmodule name do
      @moduledoc "Module doc"

      @typedoc "Type doc"
      @type foo(any) :: any

      @typedoc "Opaque type doc"
      @opaque bar(any) :: any

      @doc "Callback doc"
      @callback foo(any) :: any

      @doc false
      @callback bar() :: term

      @callback baz(any, term) :: any

      @doc "Macrocallback doc"
      @macrocallback qux(any) :: any

      @doc "Function doc"
      def foo(arg) do
        arg + 1
      end

      @doc false
      def qux(true), do: false

      def foobar(), do: nil

      def arg_names([], [], %{}, [], %{}), do: false

      @year 2015
      def with_defaults(@year, arg \\ 0, year \\ @year, fun \\ &>=/2) do
        {fun, arg + year}
      end
    end)
  end
end
