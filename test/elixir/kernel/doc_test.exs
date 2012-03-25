Code.require_file "../../test_helper", __FILE__

defmodule Kernel.DocTest do
  use ExUnit.Case

  test :compiled_docs do
    tmp  = File.expand_path("../../tmp", __FILE__)
    path = File.expand_path("../../fixtures/compiled_with_docs.ex", __FILE__)

    try do
      :file.make_dir(tmp)
      Code.compile_file_to_dir(path, tmp, docs: true)
      Code.prepend_path(tmp)

      assert_equal [], CompiledWithDocs.__info__(:data)
      assert_equal [{{:example,0},5,:def,"Some example"}], CompiledWithDocs.__info__(:docs)
      assert_equal { 1, "moduledoc" }, CompiledWithDocs.__info__(:moduledoc)
    after:
      :os.cmd('rm -rf #{tmp}')
    end
  end

  test :compiled_no_docs do
    assert_equal nil, __MODULE__.__info__(:docs)
    assert_equal nil, __MODULE__.__info__(:moduledoc)
  end
end
