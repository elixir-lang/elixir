Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.DocsTest do
  use ExUnit.Case

  test :compiled_docs do
    path = Path.join(__DIR__, "../fixtures/compiled_with_docs.ex")

    try do
      Code.load_file path

      expected = [
        {{:example, 1}, 5, :def, [{:var, [line: 6], nil}], "Some example"},
        {{:nodoc, 1}, 8, :def, [{:"//", [line: 8], [{:var, [line: 8], nil}, 0]}], nil}
      ]

      assert CompiledWithDocs.__info__(:docs) == expected
      assert CompiledWithDocs.__info__(:moduledoc) == { 1, "moduledoc" }
    after
      :code.delete CompiledWithDocs
      :code.purge CompiledWithDocs
    end
  end

  test :compiled_without_docs do
    path = Path.join(__DIR__, "../fixtures/compiled_with_docs.ex")

    try do
      Code.compiler_options(docs: false)
      Code.load_file path

      assert CompiledWithDocs.__info__(:docs) == nil
      assert CompiledWithDocs.__info__(:moduledoc) == nil
    after
      Code.compiler_options(docs: true)
      :code.delete CompiledWithDocs
      :code.purge CompiledWithDocs
    end
  end
end
