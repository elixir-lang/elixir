Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.UmbrellaTest do
  use MixTest.Case

  test "compile umbrella" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "deps.get"
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
        assert_received { :mix_shell, :info, ["* bar (apps/bar)"] }
        assert_received { :mix_shell, :info, ["* foo (apps/foo)"] }
      end)
    end
  end

  defmodule UmbrellaDeps do
    def project do
      [ apps_path: "apps",
        deps: [{ :some_dep, path: "deps/some_dep" }] ]
    end
  end

  test "loads umbrella dependencies" do
    Mix.Project.push UmbrellaDeps

    in_fixture "umbrella_dep/deps/umbrella", fn ->
      File.mkdir_p!("deps/some_dep/ebin")
      Mix.Task.run "loadpaths", ["--no-deps-check", "--no-elixir-version-check"]
      assert Path.expand('deps/some_dep/ebin') in :code.get_path
    end
  after
    Mix.Project.pop
  end

  defmodule CycleDeps do
    def project do
      [ app: :umbrella_dep,
        deps: [
          { :bar, path: "deps/umbrella/apps/bar" },
          { :umbrella, path: "deps/umbrella" }
        ] ]
    end
  end

  test "handles dependencies with cycles" do
    Mix.Project.push CycleDeps

    in_fixture "umbrella_dep", fn ->
      assert Enum.map(Mix.Deps.all, & &1.app) == [:foo, :bar, :umbrella]
    end
  after
    Mix.Project.pop
  end

  test "list deps for umbrella as dependency" do
    in_fixture("umbrella_dep", fn ->
      Mix.Project.in_project(:umbrella_dep, ".", fn _ ->
        Mix.Task.run "deps"
        assert_received { :mix_shell, :info, ["* umbrella (deps/umbrella)"] }
        assert_received { :mix_shell, :info, ["* foo (apps/foo)"] }
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

  test "recompiles after path dependency changed" do
    in_fixture("umbrella_dep/deps/umbrella/apps", fn ->
      Mix.Project.in_project(:bar, "bar", fn _ ->
        Mix.Tasks.Deps.Compile.run []

        assert Mix.Tasks.Compile.Elixir.run([]) == :ok
        assert Mix.Tasks.Compile.Elixir.run([]) == :noop
        assert_received { :mix_shell, :info, ["Compiled lib/foo.ex"] }
        purge [Bar]

        future = { { 2020, 4, 17 }, { 14, 0, 0 } }
        File.touch!("../foo/ebin/.compile.elixir", future)
        assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      end)
    end)
  end

  defmodule Selective do
    def project do
      [ apps_path: "apps",
        apps: [:foo, :bar] ]
    end
  end

  test "can select which apps to use" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.push Selective

      File.mkdir_p! "apps/errors/lib"
      File.write! "apps/errors/lib/always_fail.ex", "raise %s[oops]"

      assert Mix.Task.run("compile.elixir") == [:ok, :ok]
      assert_received { :mix_shell, :info, ["Compiled lib/bar.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/foo.ex"] }
    end)
  after
    Mix.Project.pop
  end
end
