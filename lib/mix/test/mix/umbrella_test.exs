Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.UmbrellaTest do
  use MixTest.Case

  @moduletag apps: [:foo, :bar]

  test "compiles umbrella" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "deps"
        assert_received {:mix_shell, :info, ["* bar (apps/bar) (mix)"]}
        assert_received {:mix_shell, :info, ["* foo (apps/foo) (mix)"]}

        # Ensure we can compile and run checks
        Mix.Task.run "deps.compile"
        Mix.Task.run "deps.check"
        Mix.Task.run "compile"

        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :info, ["Compiled lib/bar.ex"]}
        assert_received {:mix_shell, :info, ["Generated bar app"]}
        assert_received {:mix_shell, :info, ["==> foo"]}
        assert_received {:mix_shell, :info, ["Compiled lib/foo.ex"]}
        assert_received {:mix_shell, :info, ["Generated foo app"]}

        # Ensure foo was loaded and in the same env as Mix.env
        assert_received {:mix_shell, :info, [":foo env is dev"]}
        assert_received {:mix_shell, :info, [":bar env is dev"]}

        Mix.Task.clear
        Mix.Task.run "app.start", ["--no-compile"]
      end)
    end
  end

  test "compiles umbrella with protocol consolidation (via build embedded)" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", [build_embedded: true], fn _ ->
        assert_raise Mix.Error, ~r"Cannot execute task because the project was not yet compiled", fn ->
          Mix.Tasks.App.Start.run []
        end

        Mix.Task.run "compile"

        assert_received {:mix_shell, :info, ["Generated bar app"]}
        assert_received {:mix_shell, :info, ["Generated foo app"]}
        assert File.regular? "_build/dev/consolidated/Elixir.Enumerable.beam"
        purge [Enumerable]

        assert Mix.Tasks.App.Start.run []
        assert Protocol.consolidated?(Enumerable)
      end)
    end
  end

  test "recursive compiles umbrella with protocol consolidation" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", [build_embedded: true], fn _ ->
        defmodule Elixir.Mix.Tasks.Umbrella.Recur do
          use Mix.Task
          @recursive true
          def run(_), do: Mix.Task.run "compile"
        end

        Mix.Task.run "umbrella.recur"
        assert_received {:mix_shell, :info, ["Generated bar app"]}
        assert_received {:mix_shell, :info, ["Generated foo app"]}
        assert File.regular? "_build/dev/consolidated/Elixir.Enumerable.beam"
        purge [Enumerable]

        assert Mix.Tasks.App.Start.run []
        assert Protocol.consolidated?(Enumerable)
      end)
    end
  end

  defmodule UmbrellaDeps do
    def project do
      [apps_path: "apps",
       deps: [{:some_dep, path: "deps/some_dep"}]]
    end
  end

  test "loads umbrella dependencies" do
    Mix.Project.push UmbrellaDeps

    in_fixture "umbrella_dep/deps/umbrella", fn ->
      File.mkdir_p!("deps/some_dep/ebin")
      File.mkdir_p!("_build/dev/lib/some_dep/ebin")
      File.mkdir_p!("_build/dev/lib/foo/ebin")
      File.mkdir_p!("_build/dev/lib/bar/ebin")

      Mix.Task.run "loadpaths", ["--no-deps-check", "--no-elixir-version-check"]
      assert to_char_list(Path.expand("_build/dev/lib/some_dep/ebin")) in :code.get_path
      assert to_char_list(Path.expand("_build/dev/lib/foo/ebin")) in :code.get_path
      assert to_char_list(Path.expand("_build/dev/lib/bar/ebin")) in :code.get_path
    end
  end

  test "loads umbrella child dependencies in all environments" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project :umbrella, ".", fn _ ->
        File.write! "apps/bar/mix.exs", """
        defmodule Bar.Mixfile do
          use Mix.Project

          def project do
            [app: :bar,
             version: "0.1.0",
             deps: [{:git_repo, git: MixTest.Case.fixture_path("git_repo"), only: :other}]]
          end
        end
        """

        # Does not fetch when filtered
        Mix.Tasks.Deps.Get.run ["--only", "dev"]
        refute_received {:mix_shell, :info, ["* Getting git_repo" <> _]}

        # But works across all environments
        Mix.Tasks.Deps.Get.run []
        assert_received {:mix_shell, :info, ["* Getting git_repo" <> _]}

        # Does not show by default
        Mix.Tasks.Deps.run []
        refute_received {:mix_shell, :info, ["* git_repo" <> _]}

        # But shows on proper environment
        Mix.env(:other)
        Mix.Tasks.Deps.run []
        assert_received {:mix_shell, :info, ["* git_repo " <> _]}
      end
    end
  after
    Mix.env(:test)
  end

  test "loads umbrella child dependencies in umbrellas" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project :umbrella, ".", fn _ ->
        File.write! "apps/bar/mix.exs", """
        defmodule Bar.Mixfile do
          use Mix.Project

          def project do
            [app: :bar,
             version: "0.1.0",
             deps: [{:foo, in_umbrella: true}]]
          end
        end
        """

        # Running from umbrella should not cause conflicts
        Mix.Tasks.Deps.Get.run []
        Mix.Tasks.Run.run []
      end
    end
  end

  ## Umbrellas as a dependency

  test "list deps for umbrella as dependency" do
    in_fixture("umbrella_dep", fn ->
      Mix.Project.in_project(:umbrella_dep, ".", fn _ ->
        Mix.Task.run "deps"
        assert_received {:mix_shell, :info, ["* umbrella (deps/umbrella) (mix)"]}
        assert_received {:mix_shell, :info, ["* foo (apps/foo) (mix)"]}
      end)
    end)
  end

  test "compile for umbrella as dependency" do
    in_fixture "umbrella_dep", fn ->
      Mix.Project.in_project(:umbrella_dep, ".", fn _ ->
        Mix.Task.run "deps.compile"
        assert Bar.bar == "hello world"
      end)
    end
  end

  defmodule CycleDeps do
    def project do
      [app: :umbrella_dep,
       deps: [
         {:bar, path: "deps/umbrella/apps/bar"},
         {:umbrella, path: "deps/umbrella"}
       ]]
    end
  end

  test "handles dependencies with cycles" do
    Mix.Project.push CycleDeps

    in_fixture "umbrella_dep", fn ->
      assert Enum.map(Mix.Dep.loaded([]), & &1.app) == [:foo, :bar, :umbrella]
    end
  end

  test "handles dependencies with cycles and overridden deps" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project :umbrella, ".", fn _ ->
        File.write! "apps/foo/mix.exs", """
        defmodule Foo.Mixfile do
          use Mix.Project

          def project do
            # Ensure we have the proper environment
            :dev = Mix.env

            [app: :foo,
             version: "0.1.0",
             deps: [{:bar, in_umbrella: true}]]
          end
        end
        """

        File.write! "apps/bar/mix.exs", """
        defmodule Bar.Mixfile do
          use Mix.Project

          def project do
            # Ensure we have the proper environment
            :dev = Mix.env

            [app: :bar,
             version: "0.1.0",
             deps: [{:a, path: "deps/a"},
                    {:b, path: "deps/b"}]]
          end
        end
        """

        assert Enum.map(Mix.Dep.loaded([]), & &1.app) == [:a, :b, :bar, :foo]
      end
    end
  end

  test "uses dependency aliases" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project :umbrella, ".", fn _ ->
        File.write! "apps/bar/mix.exs", """
        defmodule Bar.Mixfile do
          use Mix.Project

          def project do
            [app: :bar,
             version: "0.1.0",
             aliases: ["compile.all": fn _ -> Mix.shell.info "no compile bar" end]]
          end
        end
        """

        Mix.Task.run "compile"
        assert_receive {:mix_shell, :info, ["no compile bar"]}
        refute_receive {:mix_shell, :info, ["Compiled lib/bar.ex"]}
      end
    end
  end

  test "recompiles after path dependency changes" do
    in_fixture("umbrella_dep/deps/umbrella/apps", fn ->
      Mix.Project.in_project(:bar, "bar", fn _ ->
        Mix.Task.run "compile"
        assert Mix.Tasks.Compile.Elixir.run([]) == :noop
        assert_receive {:mix_shell, :info, ["Compiled lib/foo.ex"]}
        assert_receive {:mix_shell, :info, ["Compiled lib/bar.ex"]}
        assert File.regular?("_build/dev/consolidated/Elixir.Enumerable.beam")

        # Ensure we can measure a timestamp difference
        ensure_touched("_build/dev/lib/foo/.compile.elixir",
                       File.stat!("_build/dev/lib/bar/.compile.lock").mtime)
        ensure_touched("../foo/mix.exs",
                       File.stat!("_build/dev/lib/foo/.compile.elixir").mtime)

        # Mark locks and protocols as outdated
        File.touch!("_build/dev/lib/foo/.compile.elixir",
                    {{2010, 1, 1}, {0, 0, 0}})
        File.touch!("_build/dev/consolidated/Elixir.Enumerable.beam",
                    {{2010, 1, 1}, {0, 0, 0}})

        purge [Foo, Bar]
        Mix.Task.clear
        Mix.shell.flush

        assert Mix.Task.run("compile") == :ok
        assert Mix.Tasks.Compile.Elixir.run([]) == :noop
        assert_receive {:mix_shell, :info, ["Compiled lib/foo.ex"]}
        assert_receive {:mix_shell, :info, ["Compiled lib/bar.ex"]}
        assert File.stat!("_build/dev/consolidated/Elixir.Enumerable.beam").mtime >
               {{2010, 1, 1}, {0, 0, 0}}
        purge [Foo, Bar]
      end)
    end)
  end

  defmodule Selective do
    def project do
      [apps_path: "apps",
       apps: [:foo, :bar]]
    end
  end

  test "can select which apps to use" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.push Selective

      File.mkdir_p! "apps/errors/lib"
      File.write! "apps/errors/lib/always_fail.ex", "raise ~s[oops]"

      assert Mix.Task.run("compile.elixir") == [:ok, :ok]
      assert_received {:mix_shell, :info, ["Compiled lib/bar.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/foo.ex"]}
    end)
  end
end
