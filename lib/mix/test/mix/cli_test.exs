Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.CLITest do
  use MixTest.Case

  @moduletag :tmp_dir

  test "default task" do
    in_fixture("no_mixfile", fn ->
      File.write!("mix.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
      end
      """)

      mix(~w[])
      assert File.regular?("_build/dev/lib/p/ebin/Elixir.A.beam")
    end)
  end

  test "custom default task" do
    in_fixture("no_mixfile", fn ->
      File.write!("mix.exs", """
      defmodule P do
        use Mix.Project
        def cli, do: [default_task: "oops"]
        def project, do: [app: :p, version: "0.1.0"]
      end
      """)

      assert mix(~w[]) =~ "The task \"oops\" could not be found"
    end)
  end

  test "Mix.raise/2 can set exit status", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.mkdir_p!("lib")

      File.write!("mix.exs", """
      defmodule MyProject do
        use Mix.Project

        def project do
          [app: :my_project, version: "0.0.1", aliases: aliases()]
        end

        defp aliases do
          [
            custom: &error(&1, exit_status: 99),
          ]
        end

        defp error(_args, opts), do: Mix.raise("oops", opts)
      end
      """)

      assert {_, 99} = mix_code(~w[custom])
    end)
  end

  test "compiles and invokes simple task from CLI", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.mkdir_p!("lib")

      File.write!("mix.exs", """
      defmodule MyProject do
        use Mix.Project

        def project do
          [app: :my_project, version: "0.0.1"]
        end

        def hello_world do
          "Hello from MyProject!"
        end
      end
      """)

      File.write!("lib/hello.ex", """
      defmodule Mix.Tasks.MyHello do
        use Mix.Task

        @shortdoc "Says hello"

        def run(_) do
          IO.puts(Mix.Project.get!().hello_world())
          Mix.shell().info("This won't appear")
          Mix.raise("oops")
        end
      end
      """)

      contents = mix(~w[my_hello], [{"MIX_QUIET", "1"}])
      assert contents =~ "Hello from MyProject!\n"
      refute contents =~ "This won't appear"

      contents = mix(~w[my_hello], [{"MIX_QUIET", "0"}])
      assert contents =~ "Hello from MyProject!\n"
      assert contents =~ "This won't appear"

      contents = mix(~w[my_hello], [{"MIX_DEBUG", "1"}])
      assert contents =~ "-> Running mix my_hello (inside MyProject)"
      assert contents =~ "** (Mix.Error) oops"

      contents = mix(~w[my_hello], [{"MIX_DEBUG", "0"}])
      refute contents =~ "-> Running mix my_hello (inside MyProject)"
      refute contents =~ "** (Mix.Error) oops"
    end)
  end

  test "no task error", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      contents = mix(~w[no_task])
      assert contents =~ "** (Mix) The task \"no_task\" could not be found"
    end)
  end

  test "tasks with slashes raise NoTaskError", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      contents = mix(~w[my/task])
      assert contents =~ "** (Mix) The task \"my/task\" could not be found"
    end)
  end

  test "--help smoke test", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      output = mix(~w[--help])
      assert output =~ "Mix is a build tool for Elixir"
      assert output =~ "mix help TASK"
    end)
  end

  test "--version smoke test", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      output = mix(~w[--version])
      assert output =~ ~r/Erlang.+\n\nMix [0-9\.a-z]+/
    end)
  end

  test "env var config", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("custom.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
      end
      """)

      System.put_env("MIX_ENV", "prod")
      System.put_env("MIX_TARGET", "rpi")
      System.put_env("MIX_EXS", "custom.exs")

      inspect = "IO.inspect {Mix.env(), Mix.target(), System.argv()}"
      output = mix(["run", "-e", inspect, "--", "1", "2", "3"])
      assert output =~ ~s({:prod, :rpi, ["1", "2", "3"]})
    end)
  after
    System.delete_env("MIX_ENV")
    System.delete_env("MIX_TARGET")
    System.delete_env("MIX_EXS")
  end

  test "cli config", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("custom.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
        def cli, do: [default_env: :prod, default_target: :rpi]
      end
      """)

      System.put_env("MIX_EXS", "custom.exs")

      inspect = "IO.inspect {Mix.env(), Mix.target(), System.argv()}"
      output = mix(["run", "-e", inspect, "--", "1", "2", "3"])
      assert output =~ ~s({:prod, :rpi, ["1", "2", "3"]})
    end)
  after
    System.delete_env("MIX_EXS")
  end

  test "preferred cli config", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("custom.exs", """
      defmodule P do
        use Mix.Project

        def cli do
          [
            default_env: :not_prod,
            default_target: :not_rpi,
            preferred_envs: [test_task: :prod],
            preferred_targets: [test_task: :rpi]
          ]
        end

        def project, do: [app: :p, version: "0.1.0"]
      end

      defmodule Mix.Tasks.TestTask do
        use Mix.Task

        def run(args) do
          IO.inspect {Mix.env(), Mix.target(), args}
        end
      end
      """)

      System.put_env("MIX_EXS", "custom.exs")

      output = mix(["test_task", "a", "b", "c"])
      assert output =~ ~s({:prod, :rpi, ["a", "b", "c"]})
    end)
  after
    System.delete_env("MIX_EXS")
  end

  test "env config and target use defaults when empty", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("custom.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
      end
      """)

      System.put_env("MIX_ENV", "")
      System.put_env("MIX_TARGET", "")
      System.put_env("MIX_EXS", "custom.exs")

      inspect = "IO.inspect {Mix.env(), Mix.target(), System.argv()}"
      output = mix(["run", "-e", inspect, "--", "a", "b"])
      assert output =~ ~s({:dev, :host, ["a", "b"]})
    end)
  after
    System.delete_env("MIX_ENV")
    System.delete_env("MIX_TARGET")
    System.delete_env("MIX_EXS")
  end

  @tag tmp_dir: "new_with_tests"
  test "new with tests and cover", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      output = mix(~w[new .])
      assert output =~ "* creating lib/new_with_tests.ex"

      output = mix(~w[test test/new_with_tests_test.exs --cover])
      assert File.regular?("_build/test/lib/new_with_tests/ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 doctest, 1 test, 0 failures"
      assert output =~ "Generating cover results ..."
      assert File.regular?("cover/Elixir.NewWithTests.html")
    end)
  end

  @tag tmp_dir: "sup_with_tests"
  test "new --sup with tests", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      output = mix(~w[new --sup .])
      assert output =~ "* creating lib/sup_with_tests.ex"

      output = mix(~w[test])
      assert File.regular?("_build/test/lib/sup_with_tests/ebin/Elixir.SupWithTests.beam")
      assert output =~ "1 doctest, 1 test, 0 failures"
    end)
  end
end
