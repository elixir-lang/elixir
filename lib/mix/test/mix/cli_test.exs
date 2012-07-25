Code.require_file "../../test_helper", __FILE__

defmodule Mix.CLITest do
  use MixTest.Case

  test "default task" do
    in_fixture "custom_mixfile", fn ->
      output = mix ""
      assert File.regular?("ebin/Elixir-A.beam")
      assert output =~ %r"1 tests, 0 failures"
    end
  end

  test "invoke simple task from CLI" do
    in_fixture "only_mixfile", fn ->
      assert mix("hello") == "Hello from MyProject!\n"
    end
  end

  test "compile smoke test" do
    in_fixture "no_mixfile", fn ->
      output = mix "compile"

      assert File.regular?("ebin/Elixir-A.beam")
      assert File.regular?("ebin/Elixir-B.beam")
      assert File.regular?("ebin/Elixir-C.beam")

      assert output =~ %r"Compiled lib/a\.ex"
    end
  end

  test "test smoke test" do
    in_fixture "custom_mixfile", fn ->
      output = mix "test"
      assert File.regular?("ebin/Elixir-A.beam")
      assert output =~ %r"1 tests, 0 failures"
    end
  end

  test "help smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "help"
      assert output =~ %r"mix compile\s+# Compile source files"
      assert output =~ %r"mix hello\s+# Hello"
      refute output =~ %r"mix invalid"
    end
  end

  test "help TASK smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "help compile"
      assert output =~ %r"# mix help compile"
      assert output =~ %r"## Command line options"
      assert output =~ %r"^Source:"m
    end
  end

  test "do smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "do compile --list, help compile"
      assert output =~ %r"# mix help compile"
      assert output =~ %r"mix compile.elixir #"
    end
  end

  test "new with tests smoke test" do
    in_tmp "new_with_tests", fn ->
      output = mix "new ."
      assert output =~ %r(\* creating lib/new_with_tests.ex)

      output = mix "test"
      assert File.regular?("ebin/Elixir-NewWithTests.beam")
      assert output =~ %r"1 tests, 0 failures"
    end
  end
end