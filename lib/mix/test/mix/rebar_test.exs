Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.RebarTest do
  use MixTest.Case

  defmodule RebarAsDep do
    def project do
      [ app: :rebar_as_dep,
        version: "0.1.0",
        deps: [
          { :rebar_dep, path: MixTest.Case.fixture_path("rebar_dep"), app: false }
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

  test "execute rebar.config.script on dependecy directory" do
    path = MixTest.Case.fixture_path("rebar_dep_script")
    config = Mix.Rebar.load_config(path)
    assert config[:dir] == {:ok, binary_to_list(path)}
  end

  test "rebar sub_dirs" do
    path = MixTest.Case.fixture_path("rebar_dep")
    assert Mix.Rebar.recur(path, fn _ -> File.cwd end)
           [path, Path.join([path, "apps", "rebar_sub_dep"])]
  end

  test "parse rebar dependencies" do
    Mix.Project.push(RebarAsDep)

    deps = Mix.Deps.all
    assert Enum.find(deps, match?(Mix.Dep[app: :rebar_dep], &1))

    assert Enum.find(deps, fn dep ->
      Mix.Dep[app: app, opts: opts] = dep
      if app == :git_rebar do
        assert Enum.find(opts, match?({:git, "../../test/fixtures/git_rebar"}, &1))
        assert Enum.find(opts, match?({:ref, "master"}, &1))
        true
      end
    end)
  after
    Mix.Project.pop
  end

  test "get and compile dependencies for rebar" do
    # Use rebar from project root
    System.put_env("MIX_HOME", MixTest.Case.elixir_root)

    Mix.Project.push(RebarAsDep)

    in_tmp "get and compile dependencies for rebar", fn ->
      Mix.Tasks.Deps.Get.run ["--no-compile"]
      assert_received { :mix_shell, :info, ["* Getting git_rebar [git: \"../../test/fixtures/git_rebar\"]"] }

      Mix.Tasks.Deps.Compile.run []
      assert_received { :mix_shell, :info, ["* Compiling git_rebar"] }
      assert_received { :mix_shell, :info, ["* Compiling rebar_dep"] }
      assert :git_rebar.any_function == :ok

      load_paths = Mix.Deps.all
        |> Enum.map(Mix.Deps.load_paths(&1))
        |> List.concat

      assert Enum.any?(load_paths, String.ends_with?(&1, "git_rebar/ebin"))
      assert Enum.any?(load_paths, String.ends_with?(&1, "rebar_dep/ebin"))
    end
  after
    Mix.Project.pop
  end
end
