Code.require_file "../../../test_helper", __FILE__

defmodule Mix.Tasks.NewTest do
  use MixTest.Case

  test "new with underscore" do
    in_tmp "new with underscore", fn ->
      Mix.Tasks.New.run ["hello_world"]

      assert_file "hello_world/mix.exs", fn(file) ->
        assert file =~ %r/app: :hello_world/
        assert file =~ %r/version: "0.0.1"/
      end

      assert_file "hello_world/README.md", %r/# HelloWorld/
      assert_file "hello_world/.gitignore"

      assert_file "hello_world/lib/hello_world.ex", fn(file) ->
        assert file =~ %r/defmodule HelloWorld do/
        assert file =~ %r/:ok = :application.start\(:hello_world\)/
      end

      assert_file "hello_world/test/test_helper.exs", %r/HelloWorld.start/
      assert_file "hello_world/test/hello_world_test.exs", %r/defmodule HelloWorldTest do/

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating lib/hello_world.ex"] }
    end
  end

  test "new with camelize" do
    in_tmp "new with camelize", fn ->
      Mix.Tasks.New.run ["HelloWorld"]

      assert_file "HelloWorld/mix.exs", fn(file) ->
        assert file =~ %r/app: :hello_world/
        assert file =~ %r/version: "0.0.1"/
      end

      assert_file "HelloWorld/README.md", %r/# HelloWorld/
      assert_file "HelloWorld/.gitignore"

      assert_file "HelloWorld/lib/hello_world.ex", fn(file) ->
        assert file =~ %r/defmodule HelloWorld do/
        assert file =~ %r/:ok = :application.start\(:hello_world\)/
      end

      assert_file "HelloWorld/test/test_helper.exs", %r/HelloWorld.start/
      assert_file "HelloWorld/test/hello_world_test.exs", %r/defmodule HelloWorldTest do/

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating lib/hello_world.ex"] }
    end
  end

  test "new with custom name" do
    in_tmp "new with camelize", fn ->
      Mix.Tasks.New.run ["sample", "--module", "HelloWorld", "--app", "helloworld"]

      assert_file "sample/mix.exs", fn(file) ->
        assert file =~ %r/app: :helloworld/
        assert file =~ %r/version: "0.0.1"/
      end

      assert_file "sample/README.md", %r/# HelloWorld/
      assert_file "sample/.gitignore"

      assert_file "sample/lib/helloworld.ex", fn(file) ->
        assert file =~ %r/defmodule HelloWorld do/
        assert file =~ %r/:ok = :application.start\(:helloworld\)/
      end

      assert_file "sample/test/test_helper.exs", %r/HelloWorld.start/
      assert_file "sample/test/helloworld_test.exs", %r/defmodule HelloWorldTest do/

      assert_received { :mix_shell, :info, ["* creating mix.exs"] }
      assert_received { :mix_shell, :info, ["* creating lib/helloworld.ex"] }
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
      assert_raise Mix.Error, "project path must start with a letter and have only letters, numbers and underscore", fn ->
        Mix.Tasks.New.run ["007invalid"]
      end

      assert_raise Mix.Error, "expected PATH to be given, please use `mix new PATH`", fn ->
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