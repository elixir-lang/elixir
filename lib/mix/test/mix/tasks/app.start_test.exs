Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.App.StartTest do
  use MixTest.Case

  defmodule AppStartSample do
    def project do
      [app: :app_start_sample, version: "0.1.0"]
    end
  end

  defmodule BadReturnSample do
    def project do
      [app: :bad_return_app, version: "0.1.0"]
    end
    # Configuration for the OTP application
    def application do
      [mod: { Mix.Tasks.App.StartTest.BadReturnApp, [] }]
    end
  end

  defmodule BadReturnApp do
    use Application.Behaviour

    def start(_type, _args) do
      :bar # Bad return
    end
  end

  defmodule WrongElixirProject do
    def project do
      [app: :error, version: "0.1.0", elixir: "~> 0.8.1"]
    end
  end

  test "recompiles project if elixir version changed" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      purge [A, B, C]

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert System.version == Mix.Deps.Lock.elixir_vsn

      Mix.Task.clear
      File.write!("_build/shared/lib/sample/.compile.lock", "the_past")
      File.touch!("_build/shared/lib/sample/.compile.lock", { { 2010, 1, 1 }, { 0, 0, 0 } })

      Mix.Tasks.App.Start.run ["--no-start", "--no-compile"]
      assert System.version == Mix.Deps.Lock.elixir_vsn
      assert File.stat!("_build/shared/lib/sample/.compile.lock").mtime > { { 2010, 1, 1 }, { 0, 0, 0 } }
    end
  end

  test "compiles and starts the project" do
    Mix.Project.push AppStartSample

    in_fixture "no_mixfile", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.App.Start.run ["--no-compile"]
      end

      Mix.Tasks.App.Start.run ["--no-start"]
      assert File.regular?("_build/shared/lib/app_start_sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/shared/lib/app_start_sample/ebin/app_start_sample.app")
      refute List.keyfind(:application.loaded_applications, :app_start_sample, 0)

      Mix.Tasks.App.Start.run []
      assert List.keyfind(:application.loaded_applications, :app_start_sample, 0)
    end
  end

  test "validates the Elixir version requirement" do
    Mix.Project.push WrongElixirProject

    in_fixture "no_mixfile", fn ->
      error = assert_raise Mix.ElixirVersionError, fn ->
        Mix.Tasks.App.Start.run ["--no-start"]
      end

      assert error.message =~ %r/ to run :error /
    end
  after
    purge [A, B, C]
  end

  test "does not validate the Elixir version requirement when disabled" do
    Mix.Project.push WrongElixirProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.App.Start.run ["--no-start", "--no-elixir-version-check"]
    end
  after
    purge [A, B, C]
  end

  test "start does nothing if project[:app] is nil" do
    project = Mix.project
    assert Mix.Tasks.App.Start.start(project) == nil
  end

  test "start runs successfully in okay case" do
    Mix.Project.push MixTest.Case.Sample
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      project = Mix.project
      assert Mix.Tasks.App.Start.start(project) == :ok
    end
  end

  test "start raises an exception on :error" do
    Mix.Project.push AppStartSample

    in_fixture "no_mixfile", fn ->
      project = Mix.project
      assert_raise Mix.Error, fn ->
        Mix.Tasks.App.Start.start(project)
      end
    end
  end

  test "start raises a stacktrace on bad_return" do
    Mix.Project.push BadReturnSample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      project = Mix.project
      assert_raise ErlangError, fn ->
        Mix.Tasks.App.Start.start(project)
      end
    end
  end
end
