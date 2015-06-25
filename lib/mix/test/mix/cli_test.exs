Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.CLITest do
  use MixTest.Case

  test "default task" do
    in_fixture "no_mixfile", fn ->
      File.write! "mix.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
      end
      """
      output = mix ~w[]
      assert File.regular?("_build/dev/lib/p/ebin/Elixir.A.beam")
      assert output =~ "Compiled lib/a.ex"
    end
  end

  test "compiles and invokes simple task from CLI" do
    in_fixture "no_mixfile", fn ->
      File.mkdir_p!("lib")

      File.write! "mix.exs", """
      defmodule MyProject do
        use Mix.Project

        def project do
          [app: :my_project, version: "0.0.1"]
        end

        def hello_world do
          "Hello from MyProject!"
        end
      end
      """

      File.write! "lib/hello.ex", """
      defmodule Mix.Tasks.Hello do
        use Mix.Task

        @shortdoc "Hello"

        def run(_) do
          IO.puts Mix.Project.get!.hello_world
        end
      end
      """

      contents = mix ~w[hello]
      assert contents =~ "Hello from MyProject!\n"
      assert contents =~ "Compiled lib/hello.ex\n"
    end
  end

  test "no task error" do
    in_fixture "no_mixfile", fn ->
      contents = mix ~w[no_task]
      assert contents =~ "** (Mix) The task no_task could not be found"
    end
  end

  test "--help smoke test" do
    in_fixture "no_mixfile", fn ->
      output = mix ~w[--help]
      assert output =~ ~r/mix compile\s+# Compile source files/
      refute output =~ "mix invalid"
    end
  end

  test "--version smoke test" do
    in_fixture "no_mixfile", fn ->
      output = mix ~w[--version]
      assert output =~ ~r/Mix [0-9\.a-z]+/
    end
  end

  test "env config" do
    in_fixture "no_mixfile", fn ->
      File.write! "custom.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
      end
      """

      System.put_env("MIX_ENV", "prod")
      System.put_env("MIX_EXS", "custom.exs")

      output = mix ["run", "-e", "IO.inspect {Mix.env, System.argv}",
                    "--", "1", "2", "3"]

      assert output =~ "Compiled lib/a.ex"
      assert output =~ ~s({:prod, ["1", "2", "3"]})
    end
  after
    System.delete_env("MIX_ENV")
    System.delete_env("MIX_EXS")
  end

  test "new with tests" do
    in_tmp "new_with_tests", fn ->
      output = mix ~w[new .]
      assert output =~ "* creating lib/new_with_tests.ex"

      output = mix ~w[test test/new_with_tests_test.exs --cover]
      assert File.regular?("_build/test/lib/new_with_tests/ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 test, 0 failures"
      assert output =~ "Generating cover results ..."
      assert File.regular?("cover/Elixir.NewWithTests.html")
    end
  end

  test "new --sup with tests" do
    in_tmp "sup_with_tests", fn ->
      output = mix ~w[new --sup .]
      assert output =~ "* creating lib/sup_with_tests.ex"

      output = mix ~w[test]
      assert File.regular?("_build/test/lib/sup_with_tests/ebin/Elixir.SupWithTests.beam")
      assert output =~ "1 test, 0 failures"
    end
  end

  defp mix(args) when is_list(args) do
    System.cmd(elixir_executable,
               ["-r", mix_executable, "--"|args],
               [stderr_to_stdout: true]) |> elem(0)
  end

  defp mix_executable do
    Path.expand("../../../../bin/mix", __DIR__)
  end

  defp elixir_executable do
    Path.expand("../../../../bin/elixir", __DIR__)
  end
end
