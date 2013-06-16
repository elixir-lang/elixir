Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.CLITest do
  use MixTest.Case

  test "env" do
    in_fixture "custom_mixfile", fn ->
      env = System.cmd %b(MIX_ENV=prod #{elixir_executable} #{mix_executable} run "IO.puts Mix.env")
      assert env =~ "prod"
    end
  end

  test "default task" do
    in_fixture "custom_mixfile", fn ->
      output = mix ""
      assert File.regular?("ebin/Elixir.A.beam")
      assert output =~ "Compiled lib/a.ex"
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

      assert File.regular?("ebin/Elixir.A.beam")
      assert File.regular?("ebin/Elixir.B.beam")
      assert File.regular?("ebin/Elixir.C.beam")

      assert output =~ "Compiled lib/a.ex"
    end
  end

  test "test smoke test" do
    in_fixture "custom_mixfile", fn ->
      output = mix "test"
      assert File.regular?("ebin/Elixir.A.beam")
      assert output =~ "1 tests, 0 failures"

      output = mix "test test/hidden.ex --cover cover"
      assert output =~ "1 tests, 1 failures"
      assert output =~ "Generating cover results ... ok"
      assert File.regular?("cover/Elixir.A.html")
    end
  end

  test "help smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "help"
      assert output =~ %r/mix compile\s+# Compile source files/
      assert output =~ %r/mix hello\s+# Hello/
      refute output =~ "mix invalid"
    end
  end

  test "--help smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "--help"
      assert output =~ %r/mix compile\s+# Compile source files/
      refute output =~ "mix invalid"
    end
  end

  test "--version smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "--version"
      assert output =~ %r/Elixir [0-9\.a-z]+/
      refute output =~ "Something silly"
    end
  end

  test "help TASK smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "help compile"
      assert output =~ "# mix help compile"
      assert output =~ "## Command line options"
      assert output =~ %r/^Location:/m
    end
  end

  test "do smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "do compile --list, help compile"
      assert output =~ "# mix help compile"
      assert output =~ "mix compile.elixir #"
    end
  end

  test "new with tests smoke test" do
    in_tmp "new_with_tests", fn ->
      output = mix "new ."
      assert output =~ "* creating lib/new_with_tests.ex"

      output = mix "test"
      assert File.regular?("ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 tests, 0 failures"
    end
  end

  test "new --sup with tests smoke test" do
    in_tmp "new_with_tests", fn ->
      output = mix "new . --sup"
      assert output =~ "* creating lib/new_with_tests.ex"
      assert output =~ "* creating lib/new_with_tests/supervisor.ex"

      output = mix "test"
      assert File.regular?("ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 tests, 0 failures"
    end
  end

end
