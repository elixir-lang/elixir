Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.DocsTest do
  use ExUnit.Case, async: false

  test "compiled with docs" do
    expected = [
      {{:fun, 2}, 0, :def, [{:x, [], nil}, {:y, [], nil}], "This is fun!\n"},
      {{:nofun, 0}, 0, :def, [], nil},
      {{:sneaky, 1}, 0, :def, [{:bool1, [], Elixir}], false},
    ]

    with_test_module(SampleDocsModule, fn ->
      docs = Code.get_docs(SampleDocsModule, :all)
      assert docs[:docs] == expected
      assert docs[:moduledoc] == {1, "Hello, I am a module"}
    end)
  end

  test "compiled without docs" do
    Code.compiler_options(docs: false)

    with_test_module(SampleDocsModule, fn ->
      assert Code.get_docs(SampleDocsModule, :docs) == nil
      assert Code.get_docs(SampleDocsModule, :moduledoc) == nil
    end)
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

  defp with_test_module(name, f) do
    code_path = :code.get_path()

    tmp_dir = System.tmp_dir
    Code.prepend_path(tmp_dir)

    beam_path = Path.join(tmp_dir, atom_to_binary(name) <> ".beam")
    File.rm(beam_path)

    bin = deftestmodule(name)
    try do
      File.write!(beam_path, bin)
      f.()
    after
      :code.set_path(code_path)
      true = :code.delete(name)
      false = :code.purge(name)
    end
  end

  def deftestmodule(name) do
    {:module, ^name, bin, _} = Module.create(name, quote do
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
    bin
  end
end
