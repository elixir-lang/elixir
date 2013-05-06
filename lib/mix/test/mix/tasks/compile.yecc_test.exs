Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.YeccTest do
  use MixTest.Case

  test "tries to compile src/test_fail.yrl" do
    in_fixture "compile_yecc", fn ->
      output = mix "compile"

      assert output =~ %r"src/test.yrl.+grammar rules are missing"

    end
  end

  test "compiles src/test_ok.yrl" do
    in_fixture "compile_yecc", fn ->
      output = mix "compile"

      assert output =~ %r"Compiled .+test_ok\.yrl"
      assert File.regular?("src/test_ok.erl")
    end
  end

  test "compiles with --force src/test_ok.yrl" do
    in_fixture "compile_yecc", fn ->
      mix "compile"
      output = mix "compile --force"

      assert output =~ %r"Compiled .+test_ok\.yrl"
      assert File.regular?("src/test_ok.erl")
    end
  end

end
