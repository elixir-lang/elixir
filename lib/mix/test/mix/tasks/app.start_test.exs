Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.App.StartTest do
  use MixTest.Case

  defmodule AppStartSample do
    def project do
      [app: :app_start_sample, version: "0.1.0"]
    end
  end

  defmodule WrongElixirProject do
    def project do
      [app: :error, version: "0.1.0", elixir: "~> 0.8.1"]
    end
  end

  defmodule InvalidElixirRequirement do
    def project do
      [app: :error, version: "0.1.0", elixir: "~> ~> 0.8.1"]
    end
  end

  setup config do
    if app = config[:app] do
      Logger.remove_backend(:console)

      on_exit fn ->
        :application.stop(app)
        :application.unload(app)
        Logger.add_backend(:console, flush: true)
      end
    end

    :ok
  end

  test "recompiles project if elixir version changed" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      purge [A, B, C]

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert System.version == Mix.Dep.Lock.elixir_vsn

      Mix.Task.clear
      File.write!("_build/dev/lib/sample/.compile.lock", "the_past")
      File.touch!("_build/dev/lib/sample/.compile.lock", {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.App.Start.run ["--no-start"]
      assert System.version == Mix.Dep.Lock.elixir_vsn
      assert File.stat!("_build/dev/lib/sample/.compile.lock").mtime > {{2010, 1, 1}, {0, 0, 0}}
    end
  end

  test "compiles and starts the project" do
    Mix.Project.push AppStartSample

    in_fixture "no_mixfile", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.App.Start.run ["--no-compile"]
      end

      Mix.Tasks.App.Start.run ["--no-start"]
      assert File.regular?("_build/dev/lib/app_start_sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/app_start_sample/ebin/app_start_sample.app")
      refute List.keyfind(:application.loaded_applications, :app_start_sample, 0)

      Mix.Tasks.App.Start.run []
      assert List.keyfind(:application.loaded_applications, :app_start_sample, 0)
    end
  end

  test "validates the Elixir version requirement" do
    Mix.Project.push WrongElixirProject

    in_fixture "no_mixfile", fn ->
      assert_raise Mix.ElixirVersionError, ~r/You're trying to run :error on Elixir/, fn ->
        Mix.Tasks.App.Start.run ["--no-start"]
      end
    end
  end

  test "validates invalid Elixir version requirement" do
    Mix.Project.push InvalidElixirRequirement

    in_fixture "no_mixfile", fn ->
      assert_raise  Mix.Error, ~r"Invalid Elixir version requirement", fn ->
        Mix.Tasks.App.Start.run ["--no-start"]
      end
    end
  end

  test "does not validate the Elixir version requirement when disabled" do
    Mix.Project.push WrongElixirProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.App.Start.run ["--no-start", "--no-elixir-version-check"]
    end
  end

  test "start does nothing if app is nil" do
    assert Mix.Tasks.App.Start.start(nil) == :error
  end

  defmodule ReturnSample do
    def project do
      [app: :return_sample, version: "0.1.0"]
    end

    def application do
      Process.get(:application_definition)
    end
  end

  defmodule ReturnApp do
    use Application

    def start(_type, return), do: return
  end

  @tag app: :return_sample
  test "start points to report on error" do
    Mix.Project.push ReturnSample
    in_fixture "no_mixfile", fn ->
      Process.put(:application_definition, mod: {ReturnApp, {:error, :bye}})
      Mix.Tasks.Compile.run []

      assert_raise Mix.Error, "Could not start application return_sample: Mix.Tasks.App.StartTest.ReturnApp.start(:normal, {:error, :bye}) returned an error: :bye",
      fn ->
        Mix.Tasks.App.Start.start(:return_sample)
      end
    end
  end

  @tag app: :return_sample
  test "start points to report on exception error" do
    Mix.Project.push ReturnSample
    in_fixture "no_mixfile", fn ->
      Process.put(:application_definition, mod: {ReturnApp, {:error,
          {:badarg, [{ReturnApp, :start, 2, []}] }}})
      Mix.Tasks.Compile.run []

      assert_raise Mix.Error, "Could not start application return_sample: Mix.Tasks.App.StartTest.ReturnApp.start(:normal, {:error, {:badarg, [{Mix.Tasks.App.StartTest.ReturnApp, :start, 2, []}]}}) returned an error: an exception was raised:\n" <>
        "    ** (ArgumentError) argument error\n" <>
        "        Mix.Tasks.App.StartTest.ReturnApp.start/2", fn ->
      Mix.Tasks.App.Start.start(:return_sample)
      end
    end
  end

  @tag app: :return_sample
  test "start points to report on bad return" do
    Mix.Project.push ReturnSample
    in_fixture "no_mixfile", fn ->
      Process.put(:application_definition, mod: {ReturnApp, :bad})
      Mix.Tasks.Compile.run []

      assert_raise Mix.Error, "Could not start application return_sample: Mix.Tasks.App.StartTest.ReturnApp.start(:normal, :bad) returned a bad value: :bad",
      fn ->
        Mix.Tasks.App.Start.start(:return_sample)
      end
    end
  end

  defmodule ExitSample do
    def project do
      [app: :exit_sample, version: "0.1.0"]
    end

    def application do
      Process.get(:application_definition)
    end
  end

  defmodule ExitApp do
    use Application

    def start(_type, reason), do: exit(reason)
  end

  @tag app: :exit_sample
  test "start points to report on exit" do
    Mix.Project.push ExitSample
    in_fixture "no_mixfile", fn ->
      Process.put(:application_definition, mod: {ExitApp, :bye})
      Mix.Tasks.Compile.run []

      assert_raise Mix.Error, "Could not start application exit_sample: exited in: Mix.Tasks.App.StartTest.ExitApp.start(:normal, :bye)\n" <>
      "    ** (EXIT) :bye",
         fn ->
        Mix.Tasks.App.Start.start(:exit_sample)
      end
    end
  end

  @tag app: :exit_sample
  test "start points to report on normal exit" do
    Mix.Project.push ExitSample
    in_fixture "no_mixfile", fn ->
      Process.put(:application_definition, mod: {ExitApp, :normal})
      Mix.Tasks.Compile.run []

      assert_raise Mix.Error, "Could not start application exit_sample: exited in: Mix.Tasks.App.StartTest.ExitApp.start(:normal, :normal)\n" <>
      "    ** (EXIT) normal",
         fn ->
        Mix.Tasks.App.Start.start(:exit_sample)
      end
    end
  end
end
