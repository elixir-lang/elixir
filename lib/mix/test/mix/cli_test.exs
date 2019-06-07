Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.CLITest do
  use MixTest.Case

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

  test "compiles and invokes simple task from CLI", context do
    in_tmp(context.test, fn ->
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
          Mix.shell.info("This won't appear")
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
      assert contents =~ "** Running mix my_hello (inside MyProject)"
      assert contents =~ "** (Mix.Error) oops"

      contents = mix(~w[my_hello], [{"MIX_DEBUG", "0"}])
      refute contents =~ "** Running mix my_hello (inside MyProject)"
      refute contents =~ "** (Mix.Error) oops"
    end)
  end

  test "no task error", context do
    in_tmp(context.test, fn ->
      contents = mix(~w[no_task])
      assert contents =~ "** (Mix) The task \"no_task\" could not be found"
    end)
  end

  test "tasks with slashes in them raise a NoTaskError right away", context do
    in_tmp(context.test, fn ->
      contents = mix(~w[my/task])
      assert contents =~ "** (Mix) The task \"my/task\" could not be found"
    end)
  end

  test "--help smoke test", context do
    in_tmp(context.test, fn ->
      output = mix(~w[--help])
      assert output =~ "Mix is a build tool for Elixir"
      assert output =~ "mix help TASK"
    end)
  end

  test "--version smoke test", context do
    in_tmp(context.test, fn ->
      output = mix(~w[--version])
      assert output =~ ~r/Erlang.+\n\nMix [0-9\.a-z]+/
    end)
  end

  test "env config", context do
    in_tmp(context.test, fn ->
      File.write!("custom.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
      end
      """)

      System.put_env("MIX_ENV", "prod")
      System.put_env("MIX_EXS", "custom.exs")

      output = mix(["run", "-e", "IO.inspect {Mix.env, System.argv}", "--", "1", "2", "3"])
      assert output =~ ~s({:prod, ["1", "2", "3"]})
    end)
  after
    System.delete_env("MIX_ENV")
    System.delete_env("MIX_EXS")
  end

  test "env config defaults to the tasks's preferred cli environment", context do
    in_tmp(context.test, fn ->
      File.write!("custom.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0"]
      end

      defmodule Mix.Tasks.TestTask do
        use Mix.Task
        @preferred_cli_env :prod

        def run(args) do
          IO.inspect {Mix.env, args}
        end
      end
      """)

      System.put_env("MIX_EXS", "custom.exs")

      output = mix(["test_task", "a", "b", "c"])
      assert output =~ ~s({:prod, ["a", "b", "c"]})
    end)
  after
    System.delete_env("MIX_EXS")
  end

  test "target config defaults to the user's preferred cli target", context do
    in_tmp(context.test, fn ->
      File.write!("custom.exs", """
      defmodule P do
        use Mix.Project
        def project, do: [app: :p, version: "0.1.0", preferred_cli_target: [test_task: :other]]
      end

      defmodule Mix.Tasks.TestTask do
        use Mix.Task

        def run(args) do
          IO.inspect {Mix.target, args}
        end
      end
      """)

      System.put_env("MIX_EXS", "custom.exs")

      output = mix(["test_task", "a", "b", "c"])
      assert output =~ ~s({:other, ["a", "b", "c"]})
    end)
  after
    System.delete_env("MIX_EXS")
  end

  test "new with tests and cover" do
    in_tmp("new_with_tests", fn ->
      output = mix(~w[new .])
      assert output =~ "* creating lib/new_with_tests.ex"

      output = mix(~w[test test/new_with_tests_test.exs --cover])
      assert File.regular?("_build/test/lib/new_with_tests/ebin/Elixir.NewWithTests.beam")
      assert output =~ "1 doctest, 1 test, 0 failures"
      assert output =~ "Generating cover results ..."
      assert File.regular?("cover/Elixir.NewWithTests.html")
    end)
  end

  test "new --sup with tests" do
    in_tmp("sup_with_tests", fn ->
      output = mix(~w[new --sup .])
      assert output =~ "* creating lib/sup_with_tests.ex"

      output = mix(~w[test])
      assert File.regular?("_build/test/lib/sup_with_tests/ebin/Elixir.SupWithTests.beam")
      assert output =~ "1 doctest, 1 test, 0 failures"
    end)
  end
end
