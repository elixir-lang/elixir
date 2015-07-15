Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.RebarTest do
  use MixTest.Case

  defmodule RebarAsDep do
    def project do
      [ app: :rebar_as_dep,
        version: "0.1.0",
        deps: [
          {:rebar_dep, path: MixTest.Case.tmp_path("rebar_dep"), app: false}
        ]
      ]
    end
  end

  test "load rebar config" do
    path = MixTest.Case.fixture_path("rebar_dep")
    config = Mix.Rebar.load_config(path)
    assert config[:sub_dirs] == ['apps/*']
    assert config[:SCRIPT] == 'rebar.config.script'
  end

  test "execute rebar.config.script on dependency directory" do
    path = MixTest.Case.fixture_path("rebar_dep_script")
    config = Mix.Rebar.load_config(path)
    assert config[:dir] == {:ok, String.to_char_list(path)}
  end

  test "parse rebar dependencies" do
    config = [deps: [{:git_rebar, '.*',}]]
    assert [{:git_rebar, ~r".*", [path: "deps/git_rebar"]}] ==
           Mix.Rebar.deps(config)

    config = [deps: [{:git_rebar, '.*',}], deps_dir: "other_dir"]
    assert [{:git_rebar, ~r".*", [path: "other_dir/git_rebar"]}] ==
           Mix.Rebar.deps(config)

    config = [deps: [{:git_rebar, '0.1..*', {:git, '../../test/fixtures/git_rebar', :master}}]]
    assert [{:git_rebar, ~r"0.1..*", [git: "../../test/fixtures/git_rebar", ref: "master"]}] ==
           Mix.Rebar.deps(config)

    config = [deps: [{:git_rebar, '0.1..*', {:git, '../../test/fixtures/git_rebar'}, [:raw]}]]
    assert [{:git_rebar, ~r"0.1..*", [git: "../../test/fixtures/git_rebar", compile: false]}] ==
           Mix.Rebar.deps(config)

    config = [deps: [{:git_rebar, '', {:git, '../../test/fixtures/git_rebar', {:ref, '64691eb'}}}]]
    assert [{:git_rebar, ~r"", [git: "../../test/fixtures/git_rebar", ref: "64691eb"]}] ==
           Mix.Rebar.deps(config)

  end

  test "parse rebar dependencies from rebar.config" do
    Mix.Project.push(RebarAsDep)

    deps = Mix.Dep.loaded([])
    assert Enum.find(deps, &match?(%Mix.Dep{app: :rebar_dep}, &1))

    assert Enum.find(deps, fn %Mix.Dep{app: app, opts: opts} ->
      if app == :git_rebar do
        assert Enum.find(opts, &match?({:git, "../../test/fixtures/git_rebar"}, &1))
        assert Enum.find(opts, &match?({:ref, "master"}, &1))
        true
      end
    end)
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

  test "get and compile dependencies for rebar" do
    Mix.Project.push(RebarAsDep)

    in_tmp "get and compile dependencies for rebar", fn ->
      Mix.Tasks.Deps.Get.run []
      assert_received {:mix_shell, :info, ["* Getting git_rebar (../../test/fixtures/git_rebar)"]}

      Mix.Tasks.Deps.Compile.run []
      assert_received {:mix_shell, :run, ["==> git_rebar (compile)\n"]}
      assert_received {:mix_shell, :run, ["==> rebar_dep (compile)\n"]}
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

  test "get and compile dependencies for rebar with mix" do
    Mix.Project.push(RebarAsDep)

    in_tmp "get and compile dependencies for rebar with mix", fn ->
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
    end
  after
    File.rm MixTest.Case.tmp_path("rebar_dep/mix.exs")
  end
end
