Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.RebarTest do
  use MixTest.Case

  # rebar_dep and git_rebar are loaded dynamically
  @compile {:no_warn_undefined, [:rebar_dep, :git_rebar]}

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
            path: MixTest.Case.tmp_path("rebar_override"), app: false
          }
        ]
      ]
    end
  end

  describe "load_config/1" do
    test "loads rebar.config" do
      path = MixTest.Case.fixture_path("rebar_dep")
      config = Mix.Rebar.load_config(path)
      assert config[:erl_opts] == [:warnings_as_errors]
      assert config[:SCRIPT] == ~c"rebar.config.script"
    end

    test "loads rebar.config.script on dependency directory" do
      path = MixTest.Case.fixture_path("rebar_dep_script")
      config = Mix.Rebar.load_config(path)
      assert config[:dir] == {:ok, String.to_charlist(path)}
    end
  end

  @git_rebar_charlist ~c"../../test/fixtures/git_rebar"
  @git_rebar_string "../../test/fixtures/git_rebar"

  describe "deps/1" do
    defp parse_dep(dep) do
      [deps: [dep]] |> Mix.Rebar.deps() |> hd()
    end

    test "parses Rebar dependencies" do
      assert parse_dep({:git_rebar, ~c"~> 1.0"}) == {:git_rebar, "~> 1.0", override: true}

      assert parse_dep({:git_rebar, ~c"~> 1.0", {:pkg, :rebar_fork}}) ==
               {:git_rebar, "~> 1.0", override: true, hex: :rebar_fork}

      assert parse_dep({:git_rebar, {:pkg, :rebar_fork}}) ==
               {:git_rebar, override: true, hex: :rebar_fork}

      assert parse_dep({:git_rebar, ~c"0.1..*", {:git, @git_rebar_charlist, :main}}) ==
               {:git_rebar, ~r"0.1..*", override: true, git: @git_rebar_string, ref: "main"}

      assert parse_dep({:git_rebar, {:git, @git_rebar_charlist, :main}}) ==
               {:git_rebar, override: true, git: @git_rebar_string, ref: "main"}

      assert parse_dep({:git_rebar, ~c"0.1..*", {:git, @git_rebar_charlist}, [:raw]}) ==
               {:git_rebar, ~r"0.1..*", override: true, git: @git_rebar_string, compile: false}

      assert parse_dep({:git_rebar, ~c"", {:git, @git_rebar_charlist, {:ref, ~c"64691eb"}}}) ==
               {:git_rebar, ~r"", override: true, git: @git_rebar_string, ref: "64691eb"}

      assert parse_dep(:git_rebar) == {:git_rebar, override: true}
    end
  end

  describe "apply_overrides/3" do
    test "applies overrides" do
      config = [deps: {:git_rebar, ~c"~> 2.0"}]
      overrides = [{:override, [deps: [{:git_rebar, ~c"~> 1.0"}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, ~c"~> 1.0"}], overrides: overrides]

      config = [deps: [{:git_rebar, ~c"~> 2.0"}]]
      overrides = [{:override, :bar, [deps: [{:git_rebar, ~c"~> 1.0"}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, ~c"~> 2.0"}], overrides: overrides]

      config = [deps: [{:git_rebar, ~c"~> 2.0"}]]
      overrides = [{:override, :foo, [deps: [{:git_rebar, ~c"~> 1.0"}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, ~c"~> 1.0"}], overrides: overrides]

      config = [deps: [{:git_rebar, ~c"~> 1.0"}]]
      overrides = [{:add, :foo, [deps: [{:git_rebar2, ~c"~> 2.0"}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar2, ~c"~> 2.0"}, {:git_rebar, ~c"~> 1.0"}], overrides: overrides]
    end

    test "concatenates overrides" do
      config = [deps: {:git_rebar, ~c"~> 2.0"}, overrides: [{:add, :bar, []}]]
      overrides = [{:override, [deps: [{:git_rebar, ~c"~> 1.0"}]]}]

      assert Mix.Rebar.apply_overrides(:foo, config, overrides) ==
               [deps: [{:git_rebar, ~c"~> 1.0"}], overrides: overrides ++ [{:add, :bar, []}]]
    end
  end

  describe "dependency_config/1" do
    test "converts Rebar config to dependency config" do
      config = Mix.Rebar.load_config(fixture_path("rebar_dep"))
      dep_config = Mix.Rebar.dependency_config(config)

      assert config[:erl_opts] == [:warnings_as_errors]
      assert config[:project_plugins] == [:remove_me]
      assert dep_config[:erl_opts] == []
      refute dep_config[:project_plugins]
    end
  end

  describe "integration with Mix" do
    test "inherits Rebar manager" do
      Mix.Project.push(RebarAsDep)
      deps = Mix.Dep.Converger.converge([])
      assert Enum.all?(deps, &(&1.manager == :rebar3))
    end

    test "parses Rebar dependencies from rebar.config" do
      Mix.Project.push(RebarAsDep)

      deps = Mix.Dep.Converger.converge([])
      assert Enum.all?(deps, &(&1.manager == :rebar3))
      assert Enum.find(deps, &(&1.app == :rebar_dep))

      assert Enum.find(deps, fn %Mix.Dep{app: app, opts: opts} ->
               if app == :git_rebar do
                 assert Enum.find(opts, &match?({:git, _}, &1))
                 assert Enum.find(opts, &match?({:ref, "main"}, &1))
                 true
               end
             end)
    end

    test "handles Rebar overrides" do
      in_tmp("Rebar overrides", fn ->
        Mix.Project.push(RebarOverrideAsDep)

        Mix.Tasks.Deps.Get.run([])

        assert Mix.Dep.Converger.converge([]) |> Enum.map(& &1.app) ==
                 [:git_repo, :git_rebar, :rebar_override]
      end)
    after
      purge([GitRepo.MixProject])
    end

    # We run only on Unix because Windows has a hard time
    # removing the Rebar executable after executed.
    @tag :unix
    test "gets and compiles dependencies" do
      in_tmp("get and compile dependencies", fn ->
        Mix.Project.push(RebarAsDep)

        Mix.Tasks.Deps.Get.run([])
        assert_received {:mix_shell, :info, ["* Getting git_rebar " <> _]}

        Mix.Tasks.Deps.Compile.run([])
        assert_received {:mix_shell, :run, ["===> Compiling git_rebar\n"]}
        assert_received {:mix_shell, :run, ["===> Compiling rebar_dep\n"]}
        assert :git_rebar.any_function() == :ok
        assert :rebar_dep.any_function() == :ok

        load_paths =
          Mix.Dep.Converger.converge([])
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
    @tag :unix
    test "applies variables from :system_env option on config/compilation" do
      in_tmp("applies variables from system_env", fn ->
        Mix.Project.push(RebarAsDepWithEnv)

        expected_file = Path.join(tmp_path("rebar_dep"), "rebar-test-rebar3")
        File.rm(expected_file)

        Mix.Tasks.Deps.Get.run([])
        Mix.Tasks.Deps.Compile.run([])

        assert {:ok, "rebar3"} = File.read(expected_file)
      end)
    end

    # We run only on Unix because Windows has a hard time
    # removing the Rebar executable after executed.
    @tag :unix
    test "gets and compiles dependencies with MIX_REBAR3 with spaces" do
      in_tmp("rebar3 env with spaces", fn ->
        File.cp!(Mix.Rebar.local_rebar_path(:rebar3), "rebar3")
        System.put_env("MIX_REBAR3", Path.absname("rebar3"))
        assert Mix.Rebar.rebar_args(:rebar3, []) |> elem(0) =~ " "

        Mix.Project.push(RebarAsDep)
        Mix.Tasks.Deps.Get.run([])
        assert_received {:mix_shell, :info, ["* Getting git_rebar " <> _]}

        Mix.Tasks.Deps.Compile.run([])
        assert_received {:mix_shell, :run, ["===> Compiling git_rebar\n"]}
        assert_received {:mix_shell, :run, ["===> Compiling rebar_dep\n"]}
      end)
    after
      System.delete_env("MIX_REBAR3")
    end

    test "gets and compiles dependencies with Mix" do
      in_tmp("get and compile dependencies with Mix", fn ->
        Mix.Project.push(RebarAsDep)

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
