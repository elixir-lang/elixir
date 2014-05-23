Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.DocsTest do
  use ExUnit.Case

  setup_all do
    CodeHelpers.enter_fixture_dir()
  end

  teardown_all do
    CodeHelpers.leave_fixture_dir()
  end

  test "compiled with docs" do
    expected = [
      {{:fun, 2}, 60, :def, [{:x, [], nil}, {:y, [], nil}], "This is fun!\n"},
      {{:nofun, 0}, 67, :def, [], nil},
      {{:sneaky, 1}, 65, :def, [{:bool1, [], Elixir}], false},
    ]

    deftestmodule(SampleDocs)

    docs = Code.get_docs(SampleDocs, :all)
    assert docs[:docs] == expected
    assert docs[:moduledoc] == {54, "Hello, I am a module"}
  end

  test "compiled without docs" do
    Code.compiler_options(docs: false)

    deftestmodule(SampleNoDocs)

    assert Code.get_docs(SampleNoDocs, :docs) == nil
    assert Code.get_docs(SampleNoDocs, :moduledoc) == nil
  after
    Code.compiler_options(docs: true)
  end

  test "compiled in memory" do
    defmodule NoDocs do
      @moduledoc "moduledoc"

      @doc "Some example"
      def example(var), do: var
    end

    assert Code.get_docs(NoDocs, :docs) == nil
    assert Code.get_docs(NoDocs, :moduledoc) == nil
  end

  defp deftestmodule(name) do
    import CodeHelpers, only: [defbeam: 2]

    defbeam name do
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
    end
  end
end
