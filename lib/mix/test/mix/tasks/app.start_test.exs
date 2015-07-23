Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.App.StartTest do
  use MixTest.Case

  defmodule AppStartSample do
    def project do
      [app: :app_start_sample, version: "0.1.0", start_permanent: true]
    end

    def application do
      [applications: [:logger]]
    end
  end

  defmodule AppEmbeddedSample do
    def project do
      [app: :app_embedded_sample, version: "0.1.0", build_embedded: true]
    end
  end

  defmodule WrongElixirProject do
    def project do
      [app: :error, version: "0.1.0", elixir: "~> 0.8.1"]
    end
  end

  setup config do
    if app = config[:app] do
      Logger.remove_backend(:console)

      on_exit fn ->
        Application.stop(app)
        Application.unload(app)
        Logger.add_backend(:console, flush: true)
      end
    end

    :ok
  end

  test "compiles and starts the project" do
    Mix.Project.push AppStartSample

    in_fixture "no_mixfile", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.App.Start.run ["--no-compile"]
      end

      refute List.keyfind(:application.which_applications, :logger, 0)
      Application.start(:logger)

      Mix.Tasks.App.Start.run ["--no-start"]
      assert File.regular?("_build/dev/lib/app_start_sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/app_start_sample/ebin/app_start_sample.app")

      refute List.keyfind(:application.which_applications, :app_start_sample, 0)
      assert List.keyfind(:application.which_applications, :logger, 0)

      Mix.Tasks.App.Start.run []
      assert List.keyfind(:application.which_applications, :app_start_sample, 0)
      assert List.keyfind(:application.which_applications, :logger, 0)
    end
  end

  test "compiles and starts a project with build_embedded" do
    Mix.Project.push AppEmbeddedSample

    in_fixture "no_mixfile", fn ->
      assert_raise  Mix.Error, ~r"Cannot execute task because the project was not yet compiled", fn ->
        Mix.Tasks.App.Start.run []
      end

      Mix.Tasks.Compile.run([])
      Mix.Tasks.App.Start.run([])
    end
  end

  test "validates Elixir version requirement" do
    Mix.ProjectStack.post_config elixir: "~> ~> 0.8.1"
    Mix.Project.push WrongElixirProject

    in_fixture "no_mixfile", fn ->
      assert_raise  Mix.Error, ~r"Invalid Elixir version requirement", fn ->
        Mix.Tasks.App.Start.run ["--no-start"]
      end
    end
  end

  test "validates the Elixir version with requirement" do
    Mix.Project.push WrongElixirProject

    in_fixture "no_mixfile", fn ->
      assert_raise Mix.ElixirVersionError, ~r/You're trying to run :error on Elixir/, fn ->
        Mix.Tasks.App.Start.run ["--no-start"]
      end
    end
  end

  test "does not validate the Elixir version with requirement when disabled" do
    Mix.Project.push WrongElixirProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.App.Start.run ["--no-start", "--no-elixir-version-check"]
    end
  end

  test "start does nothing if app is nil" do
    assert Mix.Tasks.App.Start.start([app: nil], []) == :ok
  end

  test "allows type to be configured" do
    assert Mix.Tasks.App.Start.type([], [permanent: true]) == :permanent
    assert Mix.Tasks.App.Start.type([], [temporary: true]) == :temporary
    assert Mix.Tasks.App.Start.type([start_permanent: true], []) == :permanent
    assert Mix.Tasks.App.Start.type([], []) == :temporary
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

      message = "Could not start application return_sample: " <>
                "Mix.Tasks.App.StartTest.ReturnApp.start(:normal, {:error, :bye}) " <>
                "returned an error: :bye"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.App.Start.start([app: :return_sample], [])
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

      message = "Could not start application return_sample: " <>
                "Mix.Tasks.App.StartTest.ReturnApp.start(:normal, {:error, {:badarg, [{Mix.Tasks.App.StartTest.ReturnApp, :start, 2, []}]}}) " <>
                "returned an error: an exception was raised:\n" <>
                "    ** (ArgumentError) argument error\n" <>
                "        Mix.Tasks.App.StartTest.ReturnApp.start/2"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.App.Start.start([app: :return_sample], [])
      end
    end
  end

  @tag app: :return_sample
  test "start points to report on bad return" do
    Mix.Project.push ReturnSample
    in_fixture "no_mixfile", fn ->
      Process.put(:application_definition, mod: {ReturnApp, :bad})
      Mix.Tasks.Compile.run []

      message = "Could not start application return_sample: " <>
                "Mix.Tasks.App.StartTest.ReturnApp.start(:normal, :bad) " <>
                "returned a bad value: :bad"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.App.Start.start([app: :return_sample], [])
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

      message = "Could not start application exit_sample: exited in: " <>
                "Mix.Tasks.App.StartTest.ExitApp.start(:normal, :bye)\n" <>
                "    ** (EXIT) :bye"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.App.Start.start([app: :exit_sample], [])
      end
    end
  end

  @tag app: :exit_sample
  test "start points to report on normal exit" do
    Mix.Project.push ExitSample
    in_fixture "no_mixfile", fn ->
      Process.put(:application_definition, mod: {ExitApp, :normal})
      Mix.Tasks.Compile.run []

      message = "Could not start application exit_sample: exited in: " <>
                "Mix.Tasks.App.StartTest.ExitApp.start(:normal, :normal)\n" <>
                "    ** (EXIT) normal"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.App.Start.start([app: :exit_sample], [])
      end
    end
  end
end
