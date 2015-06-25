Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.UmbrellaTest do
  use MixTest.Case

  test "compiles umbrella" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "deps"
        assert_received {:mix_shell, :info, ["* bar (apps/bar)"]}
        assert_received {:mix_shell, :info, ["* foo (apps/foo)"]}

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

  test "compiles umbrella with protocol consolidation" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", [consolidate_protocols: true], fn _ ->
        Mix.Task.run "compile"

        assert_received {:mix_shell, :info, ["Generated bar app"]}
        assert_received {:mix_shell, :info, ["Generated foo app"]}
        assert_received {:mix_shell, :info, ["Consolidated Enumerable"]}
        assert File.regular? "_build/dev/consolidated/Elixir.Enumerable.beam"

        assert Mix.Task.run "app.start"
        assert Protocol.consolidated?(Enumerable)
      end)
    end
  after
    purge [Enumerable]
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

  test "list deps for umbrella as dependency" do
    in_fixture("umbrella_dep", fn ->
      Mix.Project.in_project(:umbrella_dep, ".", fn _ ->
        Mix.Task.run "deps"
        assert_received {:mix_shell, :info, ["* umbrella (deps/umbrella)"]}
        assert_received {:mix_shell, :info, ["* foo (apps/foo)"]}
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
        defmodule Foo.Mix do
          use Mix.Project

          def project do
            # Ensure we have the proper environment
            :dev = Mix.env

            [ app: :foo,
              version: "0.1.0",
              deps: [{:bar, in_umbrella: true}] ]
          end
        end
        """

        File.write! "apps/bar/mix.exs", """
        defmodule Bar.Mix do
          use Mix.Project

          def project do
            # Ensure we have the proper environment
            :dev = Mix.env

            [ app: :bar,
              version: "0.1.0",
              deps: [{:a, path: "deps/a"},
                     {:b, path: "deps/b"}] ]
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
        defmodule Bar.Mix do
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
        purge [Foo, Bar]
        Mix.Task.clear

        # Ensure we can measure a timestamp difference
        ensure_touched("../foo/lib/foo.ex",
                       File.stat!("_build/dev/lib/bar/.compile.lock").mtime)

        Mix.Task.run "compile"
        assert Mix.Tasks.Compile.Elixir.run([]) == :noop
        assert_receive {:mix_shell, :info, ["Compiled lib/foo.ex"]}
        assert_receive {:mix_shell, :info, ["Compiled lib/bar.ex"]}
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
