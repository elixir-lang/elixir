Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.NewTest do
  use MixTest.Case

  test "new" do
    in_tmp("new", fn ->
      Mix.Tasks.New.run(["hello_world"])

      assert_file("hello_world/mix.exs", fn file ->
        assert file =~ "app: :hello_world"
        assert file =~ "version: \"0.1.0\""
      end)

      assert_file("hello_world/README.md", ~r/# HelloWorld\n/)
      assert_file("hello_world/.gitignore")

      assert_file("hello_world/lib/hello_world.ex", ~r/defmodule HelloWorld do/)

      assert_file("hello_world/test/test_helper.exs", ~r/ExUnit.start()/)

      assert_file("hello_world/test/hello_world_test.exs", fn file ->
        assert file =~ ~r/defmodule HelloWorldTest do/
        assert file =~ "assert HelloWorld.hello() == :world"
      end)

      assert_received {:mix_shell, :info, ["* creating mix.exs"]}
      assert_received {:mix_shell, :info, ["* creating lib/hello_world.ex"]}

      # Ensure formatting is setup and consistent.
      File.cd!("hello_world", fn ->
        Mix.Tasks.Format.run(["--check-formatted"])
      end)
    end)
  end

  test "new with --sup" do
    in_tmp("new sup", fn ->
      Mix.Tasks.New.run(["hello_world", "--sup"])

      assert_file("hello_world/mix.exs", fn file ->
        assert file =~ "app: :hello_world"
        assert file =~ "version: \"0.1.0\""
        assert file =~ "mod: {HelloWorld.Application, []}"
      end)

      assert_file("hello_world/README.md", ~r/# HelloWorld\n/)
      assert_file("hello_world/.gitignore")

      assert_file("hello_world/lib/hello_world.ex", fn file ->
        assert file =~ "defmodule HelloWorld do"
        assert file =~ "def hello do"
      end)

      assert_file("hello_world/lib/hello_world/application.ex", fn file ->
        assert file =~ "defmodule HelloWorld.Application do"
        assert file =~ "use Application"
        assert file =~ "Supervisor.start_link(children, opts)"
      end)

      assert_file("hello_world/test/test_helper.exs", ~r/ExUnit.start()/)
      assert_file("hello_world/test/hello_world_test.exs", ~r/defmodule HelloWorldTest do/)

      assert_received {:mix_shell, :info, ["* creating mix.exs"]}
      assert_received {:mix_shell, :info, ["* creating lib/hello_world.ex"]}

      # Ensure formatting is setup and consistent.
      File.cd!("hello_world", fn ->
        Mix.Tasks.Format.run(["--check-formatted"])
      end)
    end)
  end

  test "new with --module uses the module name also for naming the files in lib and test" do
    in_tmp("new_with_module", fn ->
      Mix.Tasks.New.run([".", "--sup", "--module", "MyTestModule"])
      assert_file("lib/my_test_module.ex", ~r/defmodule MyTestModule do/)
      assert_file("lib/my_test_module/application.ex", ~r/defmodule MyTestModule.Application do/)
      assert_file("test/my_test_module_test.exs", ~r/defmodule MyTestModuleTest do/)
    end)
  end

  test "new with --app" do
    in_tmp("new app", fn ->
      Mix.Tasks.New.run(["HELLO_WORLD", "--app", "hello_world"])

      assert_file("HELLO_WORLD/mix.exs", fn file ->
        assert file =~ "app: :hello_world"
        assert file =~ "version: \"0.1.0\""
      end)

      assert_file("HELLO_WORLD/README.md", ~r/# HelloWorld\n/)
      assert_file("HELLO_WORLD/.gitignore")

      assert_file("HELLO_WORLD/lib/hello_world.ex", ~r/defmodule HelloWorld do/)

      assert_file("HELLO_WORLD/test/test_helper.exs", ~r/ExUnit.start()/)
      assert_file("HELLO_WORLD/test/hello_world_test.exs", ~r/defmodule HelloWorldTest do/)

      assert_received {:mix_shell, :info, ["* creating mix.exs"]}
      assert_received {:mix_shell, :info, ["* creating lib/hello_world.ex"]}

      # Ensure formatting is setup and consistent.
      File.cd!("HELLO_WORLD", fn ->
        Mix.Tasks.Format.run(["--check-formatted"])
      end)
    end)
  end

  test "new with --umbrella" do
    in_tmp("new umbrella", fn ->
      Mix.Tasks.New.run(["hello_world", "--umbrella"])

      assert_file("hello_world/mix.exs", fn file ->
        assert file =~ "apps_path: \"apps\""
      end)

      assert_file("hello_world/README.md", ~r/# HelloWorld\n/)
      assert_file("hello_world/.gitignore")

      assert_received {:mix_shell, :info, ["* creating mix.exs"]}

      # Ensure formatting is setup and consistent.
      File.cd!("hello_world", fn ->
        Mix.Tasks.Format.run(["--check-formatted"])
      end)
    end)
  end

  test "new inside umbrella" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      File.cd!("apps", fn ->
        Mix.Tasks.New.run(["hello_world"])

        assert_file("hello_world/mix.exs", fn file ->
          assert file =~ "deps_path: \"../../deps\""
          assert file =~ "lockfile: \"../../mix.lock\""
        end)

        # Ensure formatting is setup and consistent.
        File.cd!("hello_world", fn ->
          Mix.Tasks.Format.run(["--check-formatted"])
        end)
      end)
    end)
  end

  test "new with dot" do
    in_tmp("new_with_dot", fn ->
      Mix.Tasks.New.run(["."])
      assert_file("lib/new_with_dot.ex", ~r/defmodule NewWithDot do/)
    end)
  end

  test "new with invalid args" do
    in_tmp("new with an invalid application name", fn ->
      assert_raise Mix.Error,
                   ~r"Application name must start with a lowercase ASCII letter",
                   fn ->
                     Mix.Tasks.New.run(["007invalid"])
                   end

      assert_raise Mix.Error,
                   ~r"followed by lowercase ASCII letters, numbers, or underscores",
                   fn ->
                     Mix.Tasks.New.run(["invAlid"])
                   end

      assert_raise Mix.Error,
                   ~r"followed by lowercase ASCII letters, numbers, or underscores",
                   fn ->
                     Mix.Tasks.New.run(["inválido"])
                   end
    end)

    in_tmp("new with an invalid application name from the app option", fn ->
      assert_raise Mix.Error, ~r"Application name must start with a lowercase ASCII letter", fn ->
        Mix.Tasks.New.run(["valid", "--app", "007invalid"])
      end
    end)

    in_tmp("new with an invalid module name from the module options", fn ->
      assert_raise Mix.Error, ~r"Module name must be a valid Elixir alias", fn ->
        Mix.Tasks.New.run(["valid", "--module", "not.valid"])
      end
    end)

    in_tmp("new with an already taken application name", fn ->
      assert_raise Mix.Error, ~r"Module name \w+ is already taken", fn ->
        Mix.Tasks.New.run(["mix"])
      end
    end)

    in_tmp("new with an already taken application name from the app option", fn ->
      assert_raise Mix.Error, ~r"Module name \w+ is already taken", fn ->
        Mix.Tasks.New.run(["valid", "--app", "mix"])
      end
    end)

    in_tmp("new with an already taken module name from the module options", fn ->
      assert_raise Mix.Error, ~r"Module name \w+ is already taken", fn ->
        Mix.Tasks.New.run(["valid", "--module", "Mix"])
      end
    end)

    in_tmp("new without a specified path", fn ->
      assert_raise Mix.Error, "Expected PATH to be given, please use \"mix new PATH\"", fn ->
        Mix.Tasks.New.run([])
      end
    end)
  end

  test "new with existent directory" do
    in_tmp("new_with_existent_directory", fn ->
      File.mkdir_p!("my_app")
      send(self(), {:mix_shell_input, :yes?, false})

      assert_raise Mix.Error, "Please select another directory for installation", fn ->
        Mix.Tasks.New.run(["my_app"])
      end
    end)
  end

  defp assert_file(file) do
    assert File.regular?(file), "Expected #{file} to exist, but does not"
  end

  defp assert_file(file, match) do
    cond do
      Regex.regex?(match) ->
        assert_file(file, &assert(&1 =~ match))

      is_function(match, 1) ->
        assert_file(file)
        match.(File.read!(file))
    end
  end
end
