Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.UmbrellaTest do
  use MixTest.Case

  test "compile umbrella" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "compile"
        assert_received { :mix_shell, :info, ["==> bar"] }
        assert_received { :mix_shell, :info, ["Compiled lib/bar.ex"] }
        assert_received { :mix_shell, :info, ["Generated bar.app"] }
        assert_received { :mix_shell, :info, ["==> foo"] }
        assert_received { :mix_shell, :info, ["Compiled lib/foo.ex"] }
        assert_received { :mix_shell, :info, ["Generated foo.app"] }

        # Ensure foo was loaded and in the same env as Mix.env
        assert_received { :mix_shell, :info, [":foo env is dev"] }
      end)
    end
  end

  test "dependency in umbrella" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "deps"
        assert_received { :mix_shell, :info, ["==> bar"] }
        assert_received { :mix_shell, :info, ["* foo [path: \"../foo\"]"] }
      end)
    end
  end

  test "list deps for umbrella as dependency" do
    in_fixture("umbrella_dep", fn ->
      Mix.Project.in_project(:umbrella_dep, ".", fn _ ->
        Mix.Task.run "deps"
        assert_received { :mix_shell, :info, ["* umbrella [path: \"deps/umbrella\"]"] }
        assert_received { :mix_shell, :info, ["* foo [path: \"../foo\"]"] }

        # Don't include the umbrella projects top-level dependencies
        refute_received { :mix_shell, :info, ["* some_dep [path: \"deps/some_dep\"]"] }
      end)
    end)
  end

  test "compile for umbrella as dependency" do
    in_fixture "umbrella_dep", fn ->
      Mix.Project.in_project(:umbrella_dep, ".", fn _ ->
        Mix.Task.run "deps.compile"
        assert "hello world" == Bar.bar
      end)
    end
  end
end
