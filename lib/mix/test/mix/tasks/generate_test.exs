Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.GenerateTest do
  use MixTest.Case

  test "generate module: creates new module file" do
    in_tmp "generate_module", fn ->
      Mix.Tasks.Generate.run ["module", "ModName"]

      assert_file "lib/mod_name.ex"

      assert_file "lib/mod_name.ex", fn (file) ->
        assert file =~ "defmodule ModName do"
      end
    end
  end
end
