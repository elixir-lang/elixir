Code.require_file "../../test_helper", __FILE__

defmodule Kernel.DocTest do
  # Since this module is changing the code
  # server state, we need to run it in sync.
  use ExUnit.Case, sync: true

  test :compiled_docs do
    tmp  = File.expand_path("../../tmp", __FILE__)
    path = File.expand_path("../../fixtures/compiled_with_docs.ex", __FILE__)

    try do
      File.mkdir(tmp)
      Code.compiler_options(docs: true)
      Elixir.ParallelCompiler.files_to_path([path], tmp)
      Code.prepend_path(tmp)

      expected = [
        {{:example,1},5,:def,[{:var,6,nil}],"Some example"},
        {{:nodoc,1},8,:def,[{:"//",8,[{:var,8,nil},0]}],nil}
      ]

      assert CompiledWithDocs.__info__(:docs) == expected
      assert CompiledWithDocs.__info__(:moduledoc) == { 1, "moduledoc" }
    after
      Code.compiler_options(docs: false)
      :os.cmd('rm -rf #{tmp}')
    end
  end

  test :compiled_no_docs do
    assert __MODULE__.__info__(:docs) == nil
    assert __MODULE__.__info__(:moduledoc) == nil
  end
end
