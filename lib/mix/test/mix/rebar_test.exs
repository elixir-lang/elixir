Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.RebarTest do
  use MixTest.Case

  # Have our own path implementation that bypasses some
  # Path validation checks. We use this just for testing.
  defmodule MyPath do
    @behaviour Mix.SCM

    lc { name, arity } inlist Mix.SCM.Path.__info__(:functions) do
      args = tl Enum.map 0..arity, &{ :"x#{&1}", [], nil }
      def unquote(name)(unquote_splicing(args)) do
        Mix.SCM.Path.unquote(name)(unquote_splicing(args))
      end
    end
  end

  setup do
    available = Mix.SCM.available
    :application.set_env(:mix, :scm, [Mix.SCM.Git, MyPath])
    { :ok, [scm: available] }
  end

  teardown context do
    :application.set_env(:mix, :scm, context[:scm])
    :ok
  end

  defmodule RebarAsDep do
    def project do
      [ app: :rebar_as_dep,
        version: "0.1.0",
        deps: [
          { :rebar_dep, path: MixTest.Case.tmp_path("rebar_dep"), app: false }
        ]
      ]
    end
  end

  test "load rebar config" do
    path = MixTest.Case.tmp_path("rebar_dep")
    config = Mix.Rebar.load_config(path)
    assert config[:sub_dirs] == ['apps/*']
    assert config[:SCRIPT] == 'rebar.config.script'
  end

  test "execute rebar.config.script on dependecy directory" do
    path = MixTest.Case.fixture_path("rebar_dep_script")
    config = Mix.Rebar.load_config(path)
    assert config[:dir] == {:ok, String.to_char_list!(path)}
  end

  test "parse rebar dependencies" do
    Mix.Project.push(RebarAsDep)

    deps = Mix.Deps.loaded
    assert Enum.find(deps, &match?(Mix.Dep[app: :rebar_dep], &1))

    assert Enum.find(deps, fn dep ->
      Mix.Dep[app: app, opts: opts] = dep
      if app == :git_rebar do
        assert Enum.find(opts, &match?({:git, "../../test/fixtures/git_rebar"}, &1))
        assert Enum.find(opts, &match?({:ref, "master"}, &1))
        true
      end
    end)
  end

  test "recurs over sub dirs" do
    path = MixTest.Case.tmp_path("rebar_dep")

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
    # Use rebar from project root
    System.put_env("MIX_HOME", MixTest.Case.elixir_root)
    Mix.Project.push(RebarAsDep)

    in_tmp "get and compile dependencies for rebar", fn ->
      Mix.Tasks.Deps.Get.run ["--no-compile"]
      assert_received { :mix_shell, :info, ["* Getting git_rebar (../../test/fixtures/git_rebar)"] }

      Mix.Tasks.Deps.Compile.run []
      assert_received { :mix_shell, :info, ["* Compiling git_rebar"] }
      assert_received { :mix_shell, :info, ["* Compiling rebar_dep"] }
      assert :git_rebar.any_function == :ok
      assert :rebar_dep.any_function == :ok

      load_paths = Mix.Deps.loaded
        |> Enum.map(&Mix.Deps.load_paths(&1))
        |> Enum.concat

      assert File.exists?("_build/shared/lib/rebar_dep/ebin/rebar_dep.beam")
      assert File.exists?("_build/shared/lib/git_rebar/ebin/git_rebar.beam")
      assert Enum.any?(load_paths, &String.ends_with?(&1, "git_rebar/ebin"))
      assert Enum.any?(load_paths, &String.ends_with?(&1, "rebar_dep/ebin"))
    end
  end
end
