Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.RebarTest do
  use MixTest.Case

  defmodule RebarAsDep do
    def project do
      [
        app: :rebar_as_dep,
        version: "0.1.0",
        deps: [
          {:rebar_dep, path: MixTest.Case.tmp_path("rebar_dep"), app: false}
        ]
      ]
    end
  end

  defmodule RebarAsDepWithEnv do
    def project do
      [
        app: :rebar_as_dep,
        version: "0.1.0",
        deps: [
          {
            :rebar_dep,
            path: MixTest.Case.tmp_path("rebar_dep"),
            app: false,
            manager: :rebar,
            system_env: [{"FILE_FROM_ENV", "rebar-test-rebar"}, {"CONTENTS_FROM_ENV", "rebar"}]
          }
        ]
      ]
    end
  end

  defmodule Rebar3AsDep do
    def project do
      [
        app: :rebar_as_dep,
        version: "0.1.0",
        deps: [
          {
            :rebar_dep,
            path: MixTest.Case.tmp_path("rebar_dep"),
            app: false,
            manager: :rebar3,
            system_env: [{"FILE_FROM_ENV", "rebar-test-rebar3"}, {"CONTENTS_FROM_ENV", "rebar3"}]
          }
        ]
      ]
    end
  end

  defmodule RebarOverrideAsDep do
    def project do
      [
        app: :rebar_as_dep,
        version: "0.1.0",
        deps: [
          {
            :rebar_override,
            path: MixTest.Case.tmp_path("rebar_override"), app: false, manager: :rebar3
          }
        ]
      ]
    end
  end

  describe "load_config/1" do
    test "loads rebar.config" do
      path = MixTest.Case.fixture_path("rebar_dep")
      config = Mix.Rebar.load_config(path)
      assert config[:sub_dirs] == ['apps/*']
      assert config[:SCRIPT] == 'rebar.config.script'
    end

    test "loads rebar.config.script on dependency directory" do
      path = MixTest.Case.fixture_path("rebar_dep_script")
      config = Mix.Rebar.load_config(path)
      assert config[:dir] == {:ok, String.to_charlist(path)}
    end
  end

  @git_rebar_charlist '../../test/fixtures/git_rebar'
  @git_rebar_string "../../test/fixtures/git_rebar"

  describe "deps/1" do
    defp parse_dep(dep) do
      [deps: [dep]] |> Mix.Rebar.deps() |> hd()
    end

    test "parses Rebar dependencies" do
      assert parse_dep({:git_rebar, '~> 1.0'}) == {:git_rebar, "~> 1.0", override: true}

      assert parse_dep({:git_rebar, '~> 1.0', {:pkg, :rebar_fork}}) ==
               {:git_rebar, "~> 1.0", override: true, hex: :rebar_fork}

      assert parse_dep({:git_rebar, {:pkg, :rebar_fork}}) ==
               {:git_rebar, ">= 0.0.0", override: true, hex: :rebar_fork}

      assert parse_dep({:git_rebar, '0.1..*', {:git, @git_rebar_charlist, :master}}) ==
               {:git_rebar, ~r"0.1..*", override: true, git: @git_rebar_string, ref: "master"}

      assert parse_dep({:git_rebar, {:git, @git_rebar_charlist, :master}}) ==
               {:git_rebar, ">= 0.0.0", override: true, git: @git_rebar_string, ref: "master"}

      assert parse_dep({:git_rebar, '0.1..*', {:git, @git_rebar_charlist}, [:raw]}) ==
               {:git_rebar, ~r"0.1..*", override: true, git: @git_rebar_string, compile: false}

      assert parse_dep({:git_rebar, '', {:git, @git_rebar_charlist, {:ref, '64691eb'}}}) ==
               {:git_rebar, ~r"", override: true, git: @git_rebar_string, ref: "64691eb"}
    end
  end

  describe "apply_overrides/3" do
    test "applies overrides" do
      config = [deps: {:git_rebar, '~> 2.0'}]
      overrides = [{:override, [deps: [{:git_rebar, '~> 1.0'}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, '~> 1.0'}], overrides: overrides]

      config = [deps: [{:git_rebar, '~> 2.0'}]]
      overrides = [{:override, :bar, [deps: [{:git_rebar, '~> 1.0'}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, '~> 2.0'}], overrides: overrides]

      config = [deps: [{:git_rebar, '~> 2.0'}]]
      overrides = [{:override, :foo, [deps: [{:git_rebar, '~> 1.0'}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, '~> 1.0'}], overrides: overrides]

      config = [deps: [{:git_rebar, '~> 1.0'}]]
      overrides = [{:add, :foo, [deps: [{:git_rebar2, '~> 2.0'}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar2, '~> 2.0'}, {:git_rebar, '~> 1.0'}], overrides: overrides]
    end

    test "concatenates overrides" do
      config = [deps: {:git_rebar, '~> 2.0'}, overrides: [{:add, :bar, []}]]
      overrides = [{:override, [deps: [{:git_rebar, '~> 1.0'}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, '~> 1.0'}], overrides: overrides ++ [{:add, :bar, []}]]
    end
  end

  describe "dependency_config/1" do
    test "converts Rebar config to dependency config" do
      config = Mix.Rebar.load_config(fixture_path("rebar_dep"))
      dep_config = Mix.Rebar.dependency_config(config)

      assert config[:erl_opts] == [:warnings_as_errors]
      assert dep_config[:erl_opts] == []
    end
  end

  describe "recur/1" do
    test "recurs over sub dirs" do
      path = MixTest.Case.fixture_path("rebar_dep")

      File.cd!(path, fn ->
        config = Mix.Rebar.load_config(path)

        Mix.Rebar.recur(config, fn config ->
          if config[:sub_dirs] == ['from_apps_another'] do
            Process.put(:inside_apps_another, true)
          end
        end)
      end)

      unless Process.get(:inside_apps_another) do
        flunk("Expected inside_apps_another to return true")
      end
    end
  end

  describe "integration with Mix" do
    test "inherits Rebar manager" do
      Mix.Project.push(Rebar3AsDep)
      deps = Mix.Dep.load_on_environment([])
      assert Enum.all?(deps, &(&1.manager == :rebar3))
    end

    test "parses Rebar dependencies from rebar.config" do
      Mix.Project.push(RebarAsDep)

      deps = Mix.Dep.load_on_environment([])
      assert Enum.find(deps, &(&1.app == :rebar_dep))

      assert Enum.find(deps, fn %Mix.Dep{app: app, opts: opts} ->
               if app == :git_rebar do
                 assert Enum.find(opts, &match?({:git, _}, &1))
                 assert Enum.find(opts, &match?({:ref, "master"}, &1))
                 true
               end
             end)
    end

    test "handles Rebar overrides" do
      Mix.Project.push(RebarOverrideAsDep)

      in_tmp("Rebar overrides", fn ->
        Mix.Tasks.Deps.Get.run([])

        assert Mix.Dep.load_on_environment([]) |> Enum.map(& &1.app) ==
                 [:git_repo, :git_rebar, :rebar_override]
      end)
    after
      purge([GitRepo.MixProject])
    end

    test "gets and compiles dependencies for Rebar" do
      Mix.Project.push(RebarAsDep)

      in_tmp("get and compile dependencies for Rebar", fn ->
        Mix.Tasks.Deps.Get.run([])
        assert_received {:mix_shell, :info, ["* Getting git_rebar" <> _]}

        Mix.Tasks.Deps.Compile.run([])
        assert_received {:mix_shell, :run, ["===> Compiling git_rebar\n"]}
        assert_received {:mix_shell, :run, ["===> Compiling rebar_dep\n"]}
        assert :git_rebar.any_function() == :ok
        assert :rebar_dep.any_function() == :ok

        load_paths =
          Mix.Dep.load_on_environment([])
          |> Enum.map(&Mix.Dep.load_paths(&1))
          |> Enum.concat()

        assert File.exists?("_build/dev/lib/rebar_dep/ebin/rebar_dep.beam")
        assert File.exists?("_build/dev/lib/git_rebar/ebin/git_rebar.beam")

        # Assert we have no .mix/compile.lock as a .mix/compile.lock
        # means we check for the Elixir version on every command.
        refute File.exists?("_build/dev/lib/rebar_dep/.mix/compile.lock")
        refute File.exists?("_build/dev/lib/git_rebar/.mix/compile.lock")

        assert Enum.any?(load_paths, &String.ends_with?(&1, "git_rebar/ebin"))
        assert Enum.any?(load_paths, &String.ends_with?(&1, "rebar_dep/ebin"))
      end)
    end

    # We run only on Unix because Windows has a hard time
    # removing the Rebar executable after executed.
    @tag [unix: true]
    test "applies variables from :system_env option when compiling dependencies for Rebar" do
      Mix.Project.push(RebarAsDepWithEnv)

      in_tmp("applies variables from system_env for Rebar", fn ->
        expected_file = Path.join(tmp_path("rebar_dep"), "rebar-test-rebar")
        File.rm(expected_file)

        Mix.Tasks.Deps.Get.run([])
        Mix.Tasks.Deps.Compile.run([])

        assert {:ok, "rebar"} = File.read(expected_file)
      end)
    end

    test "gets and compiles dependencies for Rebar3" do
      Mix.Project.push(Rebar3AsDep)

      in_tmp("get and compile dependencies for Rebar3", fn ->
        Mix.Tasks.Deps.Get.run([])
        assert_received {:mix_shell, :info, ["* Getting git_rebar " <> _]}

        Mix.Tasks.Deps.Compile.run([])
        assert_received {:mix_shell, :run, ["===> Compiling git_rebar\n"]}
        assert_received {:mix_shell, :run, ["===> Compiling rebar_dep\n"]}
        assert :git_rebar.any_function() == :ok
        assert :rebar_dep.any_function() == :ok

        load_paths =
          Mix.Dep.load_on_environment([])
          |> Enum.map(&Mix.Dep.load_paths(&1))
          |> Enum.concat()

        assert File.exists?("_build/dev/lib/rebar_dep/ebin/rebar_dep.beam")
        assert File.exists?("_build/dev/lib/git_rebar/ebin/git_rebar.beam")

        # Assert we have no .mix/compile.lock as a .mix/compile.lock
        # means we check for the Elixir version on every command.
        refute File.exists?("_build/dev/lib/rebar_dep/.mix/compile.lock")
        refute File.exists?("_build/dev/lib/git_rebar/.mix/compile.lock")

        assert Enum.any?(load_paths, &String.ends_with?(&1, "git_rebar/ebin"))
        assert Enum.any?(load_paths, &String.ends_with?(&1, "rebar_dep/ebin"))
      end)
    end

    # We run only on Unix because Windows has a hard time
    # removing the Rebar executable after executed.
    @tag [unix: true]
    test "applies variables from :system_env option when compiling dependencies for Rebar3" do
      Mix.Project.push(Rebar3AsDep)

      in_tmp("applies variables from system_env for Rebar3", fn ->
        expected_file = Path.join(tmp_path("rebar_dep"), "rebar-test-rebar3")
        File.rm(expected_file)

        Mix.Tasks.Deps.Get.run([])
        Mix.Tasks.Deps.Compile.run([])

        assert {:ok, "rebar3"} = File.read(expected_file)
      end)
    end

    test "gets and compiles dependencies for Rebar with Mix" do
      Mix.Project.push(RebarAsDep)

      in_tmp("get and compile dependencies for Rebar with Mix", fn ->
        File.write!(MixTest.Case.tmp_path("rebar_dep/mix.exs"), """
        defmodule RebarDep.MixProject do
          use Mix.Project

          def project do
            [app: :rebar_dep,
             version: "0.0.1"]
          end
        end
        """)

        Mix.Tasks.Deps.Compile.run([])
        assert_received {:mix_shell, :info, ["==> rebar_dep"]}
        assert_received {:mix_shell, :info, ["Generated rebar_dep app"]}
        assert File.regular?("_build/dev/lib/rebar_dep/ebin/rebar_dep.app")
      end)
    after
      File.rm(MixTest.Case.tmp_path("rebar_dep/mix.exs"))
    end
  end
end
