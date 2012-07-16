Code.require_file "../../test_helper", __FILE__

defmodule Mix.CLITest do
  use MixTest.Case

  test "invoke simple task from CLI" do
    in_fixture "only_mixfile", fn ->
      assert mix("hello") == "Hello from MyProject!\n"
    end
  end

  test "compile smoke test" do
    in_fixture "no_mixfile", fn ->
      output = mix "compile"

      assert File.regular?("ebin/__MAIN__-A.beam")
      assert File.regular?("ebin/__MAIN__-B.beam")
      assert File.regular?("ebin/__MAIN__-C.beam")

      assert "Compiled lib/a.ex" in Regex.split(%r/\n/, output)
    end
  end

  test "test smoke test" do
    in_fixture "custom_mixfile", fn ->
      output = mix "test"
      assert File.regular?("ebin/__MAIN__-A.beam")
      assert output =~ %r"1 tests, 0 failures"
    end
  end

  test "help smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "help"
      assert output =~ %r"mix compile\s+# Compile Elixir"
      assert output =~ %r"mix hello\s+# Hello"
      refute output =~ %r"mix invalid"
    end
  end

  test "help TASK smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "help compile"
      assert output =~ %r"mix help compile"
      assert output =~ %r"## Command line options"
    end
  end

end