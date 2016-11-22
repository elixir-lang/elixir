Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.RebarTest do
  use MixTest.Case

  defmodule RebarAsDep do
    def project do
      [app: :rebar_as_dep,
       version: "0.1.0",
       deps: [
         {:rebar_dep, path: MixTest.Case.tmp_path("rebar_dep"), app: false}
       ]]
    end
  end

  defmodule Rebar3AsDep do
    def project do
      [app: :rebar_as_dep,
       version: "0.1.0",
       deps: [
         {:rebar_dep, path: MixTest.Case.tmp_path("rebar_dep"), app: false, manager: :rebar3}
       ]]
    end
  end

  defmodule RebarOverrideAsDep do
    def project do
      [app: :rebar_as_dep,
       version: "0.1.0",
       deps: [
         {:rebar_override, path: MixTest.Case.tmp_path("rebar_override"), app: false, manager: :rebar3}
       ]]
    end
  end

  test "load Rebar config" do
    path = MixTest.Case.fixture_path("rebar_dep")
    config = Mix.Rebar.load_config(path)
    assert config[:sub_dirs] == ['apps/*']
    assert config[:SCRIPT] == 'rebar.config.script'
  end

  test "execute rebar.config.script on dependency directory" do
    path = MixTest.Case.fixture_path("rebar_dep_script")
    config = Mix.Rebar.load_config(path)
    assert config[:dir] == {:ok, String.to_charlist(path)}
  end

  test "parse Rebar dependencies" do
    config = [deps: [{:git_rebar, '~> 1.0'}]]
    assert [{:git_rebar, "~> 1.0"}] ==
           Mix.Rebar.deps(:foo, config, [])

    config = [deps: [{:git_rebar, '~> 1.0', {:pkg, :rebar_fork}}]]
    assert [{:git_rebar, "~> 1.0", hex: :rebar_fork}] ==
           Mix.Rebar.deps(:foo, config, [])

    config = [deps: [{:git_rebar, '0.1..*', {:git, '../../test/fixtures/git_rebar', :master}}]]
    assert [{:git_rebar, ~r"0.1..*", [git: "../../test/fixtures/git_rebar", ref: "master"]}] ==
           Mix.Rebar.deps(:foo, config, [])

    config = [deps: [{:git_rebar, {:git, '../../test/fixtures/git_rebar', :master}}]]
    assert [{:git_rebar, ">= 0.0.0", [git: "../../test/fixtures/git_rebar", ref: "master"]}] ==
           Mix.Rebar.deps(:foo, config, [])

    config = [deps: [{:git_rebar, '0.1..*', {:git, '../../test/fixtures/git_rebar'}, [:raw]}]]
    assert [{:git_rebar, ~r"0.1..*", [git: "../../test/fixtures/git_rebar", compile: false]}] ==
           Mix.Rebar.deps(:foo, config, [])

    config = [deps: [{:git_rebar, '', {:git, '../../test/fixtures/git_rebar', {:ref, '64691eb'}}}]]
    assert [{:git_rebar, ~r"", [git: "../../test/fixtures/git_rebar", ref: "64691eb"]}] ==
           Mix.Rebar.deps(:foo, config, [])

    config = [deps: {:git_rebar, '~> 2.0'}]
    overrides = [{:override, [deps: [{:git_rebar, '~> 1.0'}]]}]
    assert [{:git_rebar, "~> 1.0"}] ==
           Mix.Rebar.deps(:foo, config, overrides)

    config = [deps: [{:git_rebar, '~> 2.0'}]]
    overrides = [{:override, [deps: [{:git_rebar, '~> 1.0'}]]}]
    assert [{:git_rebar, "~> 1.0"}] ==
           Mix.Rebar.deps(:foo, config, overrides)

    config = [deps: [{:git_rebar, '~> 2.0'}]]
    overrides = [{:override, :bar, [deps: [{:git_rebar, '~> 1.0'}]]}]
    assert [{:git_rebar, "~> 2.0"}] ==
           Mix.Rebar.deps(:foo, config, overrides)

    config = [deps: [{:git_rebar, '~> 2.0'}]]
    overrides = [{:override, :foo, [deps: [{:git_rebar, '~> 1.0'}]]}]
    assert [{:git_rebar, "~> 1.0"}] ==
           Mix.Rebar.deps(:foo, config, overrides)

    config = [deps: [{:git_rebar, '~> 1.0'}]]
    overrides = [{:add, :foo, [deps: [{:git_rebar2, '~> 2.0'}]]}]
    assert [{:git_rebar2, "~> 2.0"}, {:git_rebar, "~> 1.0"}] ==
           Mix.Rebar.deps(:foo, config, overrides)

  end

  test "convert rebar config to dependency config" do
    config = Mix.Rebar.load_config(fixture_path("rebar_dep"))
    dep_config = Mix.Rebar.dependency_config(config)

    assert config[:erl_opts] == [:warnings_as_errors]
    assert dep_config[:erl_opts] == []
  end

  test "parse Rebar dependencies from rebar.config" do
    Mix.Project.push(RebarAsDep)

    deps = Mix.Dep.loaded([])
    assert Enum.find(deps, &(&1.app == :rebar_dep))

    assert Enum.find(deps, fn %Mix.Dep{app: app, opts: opts} ->
      if app == :git_rebar do
        assert Enum.find(opts, &match?({:git, _}, &1))
        assert Enum.find(opts, &match?({:ref, "master"}, &1))
        true
      end
    end)
  end

  test "inherit Rebar manager" do
    Mix.Project.push(Rebar3AsDep)

    deps = Mix.Dep.loaded([])
    assert Enum.all?(deps, &(&1.manager == :rebar3))
  end

  test "Rebar overrides" do
    Mix.Project.push(RebarOverrideAsDep)

    in_tmp "Rebar overrides", fn ->
      Mix.Tasks.Deps.Get.run []
      assert Mix.Dep.loaded([]) |> Enum.map(& &1.app) ==
             [:git_repo, :git_rebar, :rebar_override]
    end
  after
    purge [GitRepo.Mixfile]
  end

  test "recurs over sub dirs" do
    path = MixTest.Case.fixture_path("rebar_dep")

    File.cd! path, fn ->
     config = Mix.Rebar.load_config(path)

      Mix.Rebar.recur(config, fn config ->
        if config[:sub_dirs] == ['from_apps_another'] do
          Process.put(:inside_apps_another, true)
        end
      end)
    end

    unless Process.get(:inside_apps_another) do
      flunk "Expected inside_apps_another to return true"
    end
  end

  test "get and compile dependencies for Rebar" do
    Mix.Project.push(RebarAsDep)

    in_tmp "get and compile dependencies for Rebar", fn ->
      Mix.Tasks.Deps.Get.run []
      assert_received {:mix_shell, :info, ["* Getting git_rebar" <> _]}

      Mix.Tasks.Deps.Compile.run []
      assert_received {:mix_shell, :run, ["===> Compiling git_rebar\n"]}
      assert_received {:mix_shell, :run, ["===> Compiling rebar_dep\n"]}
      assert :git_rebar.any_function == :ok
      assert :rebar_dep.any_function == :ok

      load_paths = Mix.Dep.loaded([])
        |> Enum.map(&Mix.Dep.load_paths(&1))
        |> Enum.concat

      assert File.exists?("_build/dev/lib/rebar_dep/ebin/rebar_dep.beam")
      assert File.exists?("_build/dev/lib/git_rebar/ebin/git_rebar.beam")

      # Assert we have no .compile.lock as a .compile.lock
      # means we check for the Elixir version on every command.
      refute File.exists?("_build/dev/lib/rebar_dep/.compile.lock")
      refute File.exists?("_build/dev/lib/git_rebar/.compile.lock")

      assert Enum.any?(load_paths, &String.ends_with?(&1, "git_rebar/ebin"))
      assert Enum.any?(load_paths, &String.ends_with?(&1, "rebar_dep/ebin"))
    end
  end

  test "get and compile dependencies for rebar3" do
    Mix.Project.push(Rebar3AsDep)

    in_tmp "get and compile dependencies for rebar3", fn ->
      Mix.Tasks.Deps.Get.run []
      assert_received {:mix_shell, :info, ["* Getting git_rebar " <> _]}

      Mix.Tasks.Deps.Compile.run []
      assert_received {:mix_shell, :run, ["===> Compiling git_rebar\n"]}
      assert_received {:mix_shell, :run, ["===> Compiling rebar_dep\n"]}
      assert :git_rebar.any_function == :ok
      assert :rebar_dep.any_function == :ok

      load_paths = Mix.Dep.loaded([])
        |> Enum.map(&Mix.Dep.load_paths(&1))
        |> Enum.concat

      assert File.exists?("_build/dev/lib/rebar_dep/ebin/rebar_dep.beam")
      assert File.exists?("_build/dev/lib/git_rebar/ebin/git_rebar.beam")

      # Assert we have no .compile.lock as a .compile.lock
      # means we check for the Elixir version on every command.
      refute File.exists?("_build/dev/lib/rebar_dep/.compile.lock")
      refute File.exists?("_build/dev/lib/git_rebar/.compile.lock")

      assert Enum.any?(load_paths, &String.ends_with?(&1, "git_rebar/ebin"))
      assert Enum.any?(load_paths, &String.ends_with?(&1, "rebar_dep/ebin"))
    end
  end

  test "get and compile dependencies for Rebar with Mix" do
    Mix.Project.push(RebarAsDep)

    in_tmp "get and compile dependencies for Rebar with Mix", fn ->
      File.write! MixTest.Case.tmp_path("rebar_dep/mix.exs"), """
      defmodule RebarDep.Mixfile do
        use Mix.Project

        def project do
          [app: :rebar_dep,
           version: "0.0.1"]
        end
      end
      """

      Mix.Tasks.Deps.Compile.run []
      assert_received {:mix_shell, :info, ["==> rebar_dep"]}
      assert_received {:mix_shell, :info, ["Generated rebar_dep app"]}
      assert File.regular?("_build/dev/lib/rebar_dep/ebin/rebar_dep.app")
    end
  after
    File.rm MixTest.Case.tmp_path("rebar_dep/mix.exs")
  end
end
