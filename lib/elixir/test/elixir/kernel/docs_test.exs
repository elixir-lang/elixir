Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.DocsTest do
  use ExUnit.Case

  test "compiled with docs" do
    deftestmodule(SampleDocs)
    docs = Code.get_docs(SampleDocs, :all)

    assert [{{:fun, 2}, _, :def, [{:x, [], nil}, {:y, [], nil}], "This is fun!\n"},
            {{:nofun, 0}, _, :def, [], nil},
            {{:sneaky, 1}, _, :def, [{:bool1, [], Elixir}], false}] = docs[:docs]
    assert {_, "Hello, I am a module"} = docs[:moduledoc]
  end

  test "compiled without docs" do
    Code.compiler_options(docs: false)

    deftestmodule(SampleNoDocs)

    assert Code.get_docs(SampleNoDocs, :docs) == nil
    assert Code.get_docs(SampleNoDocs, :moduledoc) == nil
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
  end

  defp deftestmodule(name) do
    import PathHelpers

    write_beam(defmodule name do
      @moduledoc "Hello, I am a module"

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
    end)
  end
end
