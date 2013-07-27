Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.NewTest do
  use MixTest.Case

  test "new with underscore" do
    in_tmp "new with underscore", fn ->
      Mix.Tasks.New.run ["hello_world"]

      assert_file "hello_world/mix.exs", fn(file) ->
        assert file =~ "app: :hello_world"
        assert file =~ "version: \"0.0.1\""
      end

      assert_file "hello_world/README.md", %r/# HelloWorld/
      assert_file "hello_world/.gitignore"

      assert_file "hello_world/lib/hello_world.ex",  %r/defmodule HelloWorld do/

      assert_file "hello_world/test/test_helper.exs", %r/HelloWorld.start/
      assert_file "hello_world/test/hello_world_test.exs", %r/defmodule HelloWorldTest do/

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating lib/hello_world.ex"] }
    end
  end

  test "new with --sup" do
    in_tmp "new with underscore", fn ->
      Mix.Tasks.New.run ["hello_world", "--sup"]

      assert_file "hello_world/mix.exs", fn(file) ->
        assert file =~ "app: :hello_world"
        assert file =~ "version: \"0.0.1\""
        assert file =~ "[mod: { HelloWorld, [] }]"
      end

      assert_file "hello_world/README.md", %r/# HelloWorld/
      assert_file "hello_world/.gitignore"

      assert_file "hello_world/lib/hello_world.ex", fn(file) ->
        assert file =~ "defmodule HelloWorld do"
        assert file =~ "use Application.Behaviour"
        assert file =~ "HelloWorld.Supervisor.start_link"
      end

      assert_file "hello_world/lib/hello_world/supervisor.ex", fn(file) ->
        assert file =~ "defmodule HelloWorld.Supervisor do"
        assert file =~ "supervise(children, strategy: :one_for_one)"
      end

      assert_file "hello_world/test/test_helper.exs", %r/HelloWorld.start/
      assert_file "hello_world/test/hello_world_test.exs", %r/defmodule HelloWorldTest do/

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating lib/hello_world.ex"] }
    end
  end

  test "new with dot" do
    in_tmp "new_with_dot", fn ->
      Mix.Tasks.New.run ["."]
      assert_file "lib/new_with_dot.ex", %r/defmodule NewWithDot do/
    end
  end

  test "new with invalid args" do
    in_tmp "new with invalid args", fn ->
      assert_raise Mix.Error, "Project path must start with a letter and have only lowercase letters, numbers and underscore", fn ->
        Mix.Tasks.New.run ["007invalid"]
      end

      assert_raise Mix.Error, "Expected PATH to be given, please use `mix new PATH`", fn ->
        Mix.Tasks.New.run []
      end
    end
  end

  defp assert_file(file) do
    assert File.regular?(file), "Expected #{file} to exist, but does not"
  end

  defp assert_file(file, match) when is_regex(match) do
    assert_file file, &1 =~ match
  end

  defp assert_file(file, callback) when is_function(callback, 1) do
    assert_file(file)
    callback.(File.read!(file))
  end
end
