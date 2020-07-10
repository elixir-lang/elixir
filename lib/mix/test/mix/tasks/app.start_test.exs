Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.App.StartTest do
  use MixTest.Case

  defmodule AppStartSample do
    def project do
      [app: :app_start_sample, version: "0.1.0"]
    end

    def application do
      [applications: [:logger]]
    end
  end

  test "compiles and starts the project" do
    Mix.Project.push(AppStartSample)

    in_fixture("no_mixfile", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Task.run("app.start", ["--no-compile"])
      end

      refute List.keyfind(Application.started_applications(), :logger, 0)
      Application.start(:logger)

      Mix.Task.reenable("app.config")
      Mix.Task.reenable("app.start")
      Mix.Task.run("app.start", ["--no-start"])
      assert File.regular?("_build/dev/lib/app_start_sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/app_start_sample/ebin/app_start_sample.app")

      assert :code.is_loaded(A)
      refute List.keyfind(Application.started_applications(), :app_start_sample, 0)
      assert List.keyfind(Application.started_applications(), :logger, 0)
      purge([A])

      Mix.Task.reenable("app.config")
      Mix.Task.reenable("app.start")
      Mix.Task.run("app.start", [])
      refute :code.is_loaded(A)
      assert List.keyfind(Application.started_applications(), :app_start_sample, 0)
      assert List.keyfind(Application.started_applications(), :logger, 0)
    end)
  end

  describe "unit tests" do
    test "start does nothing if no apps are given" do
      assert Mix.Tasks.App.Start.start([], :temporary) == :ok
    end

    test "allows type to be configured" do
      assert Mix.Tasks.App.Start.type([], permanent: true) == :permanent
      assert Mix.Tasks.App.Start.type([], temporary: true) == :temporary
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

    test "start points to report on error", context do
      Mix.Project.push(ReturnSample)

      in_tmp(context.test, fn ->
        Process.put(:application_definition, mod: {ReturnApp, {:error, :bye}})
        Mix.Tasks.Compile.run([])

        message =
          "Could not start application return_sample: " <>
            "Mix.Tasks.App.StartTest.ReturnApp.start(:normal, {:error, :bye}) " <>
            "returned an error: :bye"

        assert_raise Mix.Error, message, fn ->
          Mix.Tasks.App.Start.start([:return_sample], :temporary)
        end
      end)
    end

    test "start points to report on exception error", context do
      Mix.Project.push(ReturnSample)

      in_tmp(context.test, fn ->
        mod = {ReturnApp, {:error, {:badarg, [{ReturnApp, :start, 2, []}]}}}
        Process.put(:application_definition, mod: mod)
        Mix.Tasks.Compile.run([])

        message =
          "Could not start application return_sample: " <>
            "Mix.Tasks.App.StartTest.ReturnApp.start(:normal, {:error, {:badarg, [{Mix.Tasks.App.StartTest.ReturnApp, :start, 2, []}]}}) " <>
            "returned an error: an exception was raised:\n" <>
            "    ** (ArgumentError) argument error\n" <>
            "        Mix.Tasks.App.StartTest.ReturnApp.start/2"

        assert_raise Mix.Error, message, fn ->
          Mix.Tasks.App.Start.start([:return_sample], :temporary)
        end
      end)
    end

    test "start points to report on bad return", context do
      Mix.Project.push(ReturnSample)

      in_tmp(context.test, fn ->
        Process.put(:application_definition, mod: {ReturnApp, :bad})
        Mix.Tasks.Compile.run([])

        message =
          "Could not start application return_sample: " <>
            "Mix.Tasks.App.StartTest.ReturnApp.start(:normal, :bad) " <>
            "returned a bad value: :bad"

        assert_raise Mix.Error, message, fn ->
          Mix.Tasks.App.Start.start([:return_sample], :temporary)
        end
      end)
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

    test "start points to report on exit", context do
      Mix.Project.push(ExitSample)

      in_tmp(context.test, fn ->
        Process.put(:application_definition, mod: {ExitApp, :bye})
        Mix.Tasks.Compile.run([])

        message =
          "Could not start application exit_sample: exited in: " <>
            "Mix.Tasks.App.StartTest.ExitApp.start(:normal, :bye)\n" <> "    ** (EXIT) :bye"

        assert_raise Mix.Error, message, fn ->
          Mix.Tasks.App.Start.start([:exit_sample], :temporary)
        end
      end)
    end

    test "start points to report on normal exit", context do
      Mix.Project.push(ExitSample)

      in_tmp(context.test, fn ->
        Process.put(:application_definition, mod: {ExitApp, :normal})
        Mix.Tasks.Compile.run([])

        message =
          "Could not start application exit_sample: exited in: " <>
            "Mix.Tasks.App.StartTest.ExitApp.start(:normal, :normal)\n" <> "    ** (EXIT) normal"

        assert_raise Mix.Error, message, fn ->
          Mix.Tasks.App.Start.start([:exit_sample], :temporary)
        end
      end)
    end
  end
end
