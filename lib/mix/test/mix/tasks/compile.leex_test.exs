Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.LeexTest do
  use MixTest.Case

  test "tries to compile src/test_fail.xrl" do
    in_fixture "compile_leex", fn ->
      output = mix "compile"

      assert output =~ "src/test_fail.xrl:15: bad rule"
    end
  end

  test "compiles src/test_ok.xrl" do
    in_fixture "compile_leex", fn ->
      output = mix "compile"

      assert output =~ "Compiled src/test_ok.xrl"
      assert File.regular?("src/test_ok.erl")
    end
  end

  test "compiles with --force src/test_ok.xrl" do
    in_fixture "compile_leex", fn ->
      mix "compile"
      output = mix "compile --force"

      assert output =~ "Compiled src/test_ok.xrl"
      assert File.regular?("src/test_ok.erl")
    end
  end

end
