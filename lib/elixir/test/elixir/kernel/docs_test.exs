Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.DocsTest do
  use ExUnit.Case

  test "compiled with docs" do
    deftestmodule(SampleDocs)
    docs = Code.get_docs(SampleDocs, :all)

    assert [{{:__behaviour__, 1}, _, :def, [{:atom1, [], Elixir}], false},
            {{:fun, 2}, _, :def, [{:x, [], nil}, {:y, [], nil}], "This is fun!\n"},
            {{:nofun, 0}, _, :def, [], nil},
            {{:sneaky, 1}, _, :def, [{:bool1, [], Elixir}], false},
            {{:with_defaults, 4}, _, :def,
             [{:int1, [], Elixir},
              {:\\, [], [{:x, [], nil}, 0]},
              {:\\, [], [{:y, [], nil}, 2015]},
              {:\\, [], [{:f, [], nil}, {:&, _, [{:/, _, [{:>=, _, _}, 2]}]}]}], nil}] = docs[:docs]
    assert {_, "Hello, I am a module"} = docs[:moduledoc]
    assert [{{:bar, 1}, _, :def, false}, {{:baz, 2}, _, :def, nil},
            {{:first, 0}, _, :def, "I should be first."},
            {{:foo, 1}, _, :def, "Foo"},
            {{:last, 1}, _, :defmacro, "I should be last."}] = docs[:behaviour_docs]
  end

  test "compiled without docs" do
    Code.compiler_options(docs: false)

    deftestmodule(SampleNoDocs)

    assert Code.get_docs(SampleNoDocs, :docs) == nil
    assert Code.get_docs(SampleNoDocs, :moduledoc) == nil
    assert Code.get_docs(SampleNoDocs, :behaviour_docs) == nil
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
    assert Code.get_docs(NoDocs, :behaviour_docs) == nil
  end

  defp deftestmodule(name) do
    import PathHelpers

    write_beam(defmodule name do
      @moduledoc "Hello, I am a module"

      use Behaviour

      @doc "I should be first."
      defcallback first :: term

      @doc "Foo"
      defcallback foo(any) :: any

      @doc false
      defcallback bar(true) :: false

      defcallback baz(1, binary) :: binary

      @doc "I should be last."
      defmacrocallback last(integer) :: Macro.t

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

      @year 2015
      def with_defaults(@year, x \\ 0, y \\ @year, f \\ &>=/2) do
        {f, x + y}
      end
    end)
  end
end
