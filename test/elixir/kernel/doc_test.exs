Code.require_file "../../test_helper", __FILE__

defmodule Kernel.DocTest do
  # Since this module is changing the code
  # server state, we need to run it in sync.
  use ExUnit.Case, sync: true

  test :compiled_docs do
    tmp  = File.expand_path("../../tmp", __FILE__)
    path = File.expand_path("../../fixtures/compiled_with_docs.ex", __FILE__)

    try do
      :file.make_dir(tmp)
      Code.compiler_options(docs: true)
      Elixir.ParallelCompiler.files_to_path([path], tmp)
      Code.prepend_path(tmp)

      assert_equal [], CompiledWithDocs.__info__(:data)
      expected = [{{:example,1},5,:def,"Some example"},{{:nodoc,0},8,:def,nil}]
      assert_equal expected, CompiledWithDocs.__info__(:docs)
      assert_equal { 1, "moduledoc" }, CompiledWithDocs.__info__(:moduledoc)
    after:
      Code.compiler_options(docs: false)
      :os.cmd('rm -rf #{tmp}')
    end
  end

  test :compiled_no_docs do
    assert_equal nil, __MODULE__.__info__(:docs)
    assert_equal nil, __MODULE__.__info__(:moduledoc)
  end
end
