Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.CLITest do
  use MixTest.Case

  test "env" do
    in_fixture "only_mixfile", fn ->
      if match? { :win32, _ }, :os.type do
        temp_env = "set MIX_ENV=prod &"
      else
        temp_env = "MIX_ENV=prod"
      end

      env = System.cmd %b(#{temp_env} #{elixir_executable} #{mix_executable} run -e "IO.inspect { Mix.env, System.argv }" -- 1 2 3)
      assert env =~ %b({:prod, ["1", "2", "3"]})
    end
  end

  test "default task" do
    in_fixture "no_mixfile", fn ->
      File.write! "mix.exs", """
      defmodule P do
        use Mix.Project
        def project, do: []
      end
      """
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

      output = mix "test test/new_with_tests_test.exs --cover"
      assert File.regular?("ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 tests, 0 failures"
      assert output =~ "Generating cover results ... ok"
      assert File.regular?("cover/Elixir.NewWithTests.html")
    end
  end

  defp mix(args) do
    System.cmd "#{elixir_executable} #{mix_executable} #{args}"
  end

  defp mix_executable do
    Path.expand("../../../../bin/mix", __DIR__)
  end

  defp elixir_executable do
    Path.expand("../../../../bin/elixir", __DIR__)
  end
end
