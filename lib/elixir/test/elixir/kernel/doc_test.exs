Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.DocTest do
  use ExUnit.Case

  test :compiled_docs do
    path = File.expand_path("../../fixtures/compiled_with_docs.ex", __FILE__)

    try do
      Code.load_file path

      expected = [
        {{:example,1},5,:def,[{:var,6,nil}],"Some example"},
        {{:nodoc,1},8,:def,[{:"//",8,[{:var,8,nil},0]}],nil}
      ]

      assert CompiledWithDocs.__info__(:docs) == expected
      assert CompiledWithDocs.__info__(:moduledoc) == { 1, "moduledoc" }
    after
      :code.purge CompiledWithDocs
      :code.delete CompiledWithDocs
    end
  end

  test :compiled_without_docs do
    path = File.expand_path("../../fixtures/compiled_with_docs.ex", __FILE__)

    try do
      Code.compiler_options(docs: false)
      Code.load_file path

      assert CompiledWithDocs.__info__(:docs) == nil
      assert CompiledWithDocs.__info__(:moduledoc) == nil
    after
      Code.compiler_options(docs: true)
      :code.purge CompiledWithDocs
      :code.delete CompiledWithDocs
    end
  end
end
