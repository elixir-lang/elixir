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

      env = System.cmd ~s(#{temp_env} #{elixir_executable} #{mix_executable} run -e "IO.inspect { Mix.env, System.argv }" -- 1 2 3)
      assert env =~ ~s({:prod, ["1", "2", "3"]})
    end
  end

  test "default task" do
    in_fixture "no_mixfile", fn ->
      File.write! "mix.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p]
      end
      """
      output = mix ""
      assert File.regular?("_build/dev/lib/p/ebin/Elixir.A.beam")
      assert output =~ "Compiled lib/a.ex"
    end
  end

  test "compiles and invokes simple task from CLI" do
    in_fixture "only_mixfile", fn ->
      File.mkdir_p!("lib")
      File.write! "lib/hello.ex", """
      defmodule Mix.Tasks.Hello do
        use Mix.Task

        @shortdoc "Hello"

        def run(_) do
          IO.puts Mix.Project.get!.hello_world
        end
      end
      """

      contents = mix("hello")
      assert contents =~ "Hello from MyProject!\n"
      assert contents =~ "Compiled lib/hello.ex\n"
    end
  end

  test "no task error" do
    in_fixture "no_mixfile", fn ->
      contents = mix("no_task")
      assert contents =~ "** (Mix) The task no_task could not be found\n"
    end
  end

  test "--help smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "--help"
      assert output =~ ~r/mix compile\s+# Compile source files/
      refute output =~ "mix invalid"
    end
  end

  test "--version smoke test" do
    in_fixture "only_mixfile", fn ->
      output = mix "--version"
      assert output =~ ~r/Elixir [0-9\.a-z]+/
    end
  end

  test "new with tests" do
    in_tmp "new_with_tests", fn ->
      output = mix "new ."
      assert output =~ "* creating lib/new_with_tests.ex"
      assert output =~ "* creating lib/new_with_tests/supervisor.ex"

      output = mix "test test/new_with_tests_test.exs --cover"
      assert File.regular?("_build/test/lib/new_with_tests/ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 tests, 0 failures"
      assert output =~ "Generating cover results ..."
      assert File.regular?("cover/Elixir.NewWithTests.html")
    end
  end

  test "new --bare with tests" do
    in_tmp "new_with_tests", fn ->
      output = mix "new --bare ."
      assert output =~ "* creating lib/new_with_tests.ex"

      output = mix "test"
      assert File.regular?("_build/test/lib/new_with_tests/ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 tests, 0 failures"
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
