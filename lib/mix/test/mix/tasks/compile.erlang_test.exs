Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.Compile.ErlangTest do
  use MixTest.Case

  test "tries to compile src/a.erl" do
    in_fixture "compile_erlang", fn ->
      output = mix "compile"

      assert output =~ %r"src/a.erl.+syntax error"
    end
  end

  test "compiles src/b.erl and src/c.erl" do
    in_fixture "compile_erlang", fn ->
      output = mix "compile"

      assert output =~ %r"Compiled .+c\.erl"
      assert output =~ %r"Compiled .+b\.erl"

      assert File.regular?("ebin/b.beam")
      assert File.regular?("ebin/c.beam")
    end
  end

  test "compiles with --force src/b.erl and src/c.erl" do
    in_fixture "compile_erlang", fn ->
      mix "compile"
      output = mix "compile --force"

      assert output =~ %r"Compiled .+c\.erl"
      assert output =~ %r"Compiled .+b\.erl"

      assert File.regular?("ebin/b.beam")
      assert File.regular?("ebin/c.beam")
    end
  end

end
