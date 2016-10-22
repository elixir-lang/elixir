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

    assert [{{:argnames, 5}, _, :def, [
              {:list1, [], Elixir},
              {:list2, [], Elixir},
              {:map1, [], Elixir},
              {:list3, [], Elixir},
              {:map2, [], Elixir}], nil},
            {{:fun, 2}, _, :def, [{:x, [], nil}, {:y, [], nil}], "This is fun!\n"},
            {{:nofun, 0}, _, :def, [], nil},
            {{:sneaky, 1}, _, :def, [{:bool, [], Elixir}], false},
            {{:with_defaults, 4}, _, :def,
             [{:int, [], Elixir},
              {:\\, [], [{:x, [], nil}, 0]},
              {:\\, [], [{:y, [], nil}, 2015]},
              {:\\, [], [{:f, [], nil}, {:&, _, [{:/, _, [{:>=, _, _}, 2]}]}]}], nil}] = docs[:docs]

    assert {_, "Hello, I am a module"} = docs[:moduledoc]

    assert [{{:bar, 1}, _, :opaque, "Me too."},
            {{:foo, 1}, _, :type, "I am a type."}] = docs[:type_docs]

    assert [{{:bar, 1}, _, :callback, false},
            {{:baz, 2}, _, :callback, nil},
            {{:first, 0}, _, :callback, "I should be first."},
            {{:foo, 1}, _, :callback, "Foo"},
            {{:last, 1}, _, :macrocallback, "I should be last."}] = docs[:callback_docs]
  end

  test "compiled without docs" do
    Code.compiler_options(docs: false)

    deftestmodule(SampleNoDocs)

    assert Code.get_docs(SampleNoDocs, :docs) == nil
    assert Code.get_docs(SampleNoDocs, :moduledoc) == nil
    assert Code.get_docs(SampleNoDocs, :type_docs) == nil
    assert Code.get_docs(SampleNoDocs, :callback_docs) == nil
  after
    Code.compiler_options(docs: true)
  end

  test "compiled in memory does not have accessible docs" do
    defmodule NoDocs do
      @moduledoc "moduledoc"

      @doc "Some example"
      def example(var), do: var
    end

    assert Code.get_docs(NoDocs, :docs) == nil
    assert Code.get_docs(NoDocs, :moduledoc) == nil
    assert Code.get_docs(NoDocs, :callback_docs) == nil
  end

  defp deftestmodule(name) do
    import PathHelpers

    write_beam(defmodule name do
      @moduledoc "Hello, I am a module"

      @typedoc "I am a type."
      @type foo(any) :: any

      @typedoc "Me too."
      @opaque bar(any) :: any

      @doc "I should be first."
      @callback first :: term

      @doc "Foo"
      @callback foo(any) :: any

      @doc false
      @callback bar(true) :: false

      @callback baz(1, binary) :: binary

      @doc "I should be last."
      @macrocallback last(integer) :: Macro.t

      @doc """
      This is fun!
      """
      def fun(x, y) do
        {x, y}
      end

      @doc false
      def sneaky(true), do: false

      def nofun() do
        'not fun at all'
      end

      def argnames([], [], %{}, [], %{}), do: false

      @year 2015
      def with_defaults(@year, x \\ 0, y \\ @year, f \\ &>=/2) do
        {f, x + y}
      end
    end)
  end
end
