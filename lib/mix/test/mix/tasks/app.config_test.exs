Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.App.ConfigTest do
  use MixTest.Case

  test "loads project configuration" do
    Process.put(
      {MixTest.Case.Sample, :application},
      env: [from_env: :env, from_compile: :env, from_runtime: :env]
    )

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("config")

      File.write!("config/config.exs", """
      import Config
      config :sample, from_compile: :compile, from_runtime: :compile
      config :sample, :nested, from_compile: :compile, from_runtime: :compile
      """)

      File.write!("config/runtime.exs", """
      import Config
      config :sample, from_runtime: :runtime
      config :sample, :nested, from_compile: :compile, from_runtime: :compile
      """)

      Mix.Task.run("loadconfig")
      Mix.Task.run("app.config")

      assert Application.get_all_env(:sample) |> Enum.sort() == [
               from_compile: :compile,
               from_env: :env,
               from_runtime: :runtime,
               nested: [from_compile: :compile, from_runtime: :compile]
             ]
    end)
  after
    Application.delete_env(:sample, :nested, persistent: true)
    Application.delete_env(:sample, :from_env, persistent: true)
    Application.delete_env(:sample, :from_compile, persistent: true)
    Application.delete_env(:sample, :from_runtime, persistent: true)
  end

  test "sets config_env() and config_target()" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("config")

      File.write!("config/runtime.exs", """
      import Config
      config :sample, vars: {config_env(), config_target()}
      """)

      Mix.Task.run("app.config")
      assert Application.get_all_env(:sample) == [vars: {:dev, :host}]
    end)
  after
    Application.delete_env(:sample, :vars, persistent: true)
  end

  test "warns if kernel/stdlib are configured" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("config")

      File.write!("config/runtime.exs", """
      import Config
      config :kernel, this_wont: :work
      """)

      Mix.Task.run("loadconfig")
      Mix.Task.run("app.config")

      assert_received {:mix_shell, :error, ["Cannot configure base applications: [:kernel]" <> _]}
    end)
  end

  test "compiles and preloads the project" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      Mix.Task.run("app.config", ["--no-compile"])
      refute Code.loaded?(A)
      refute File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      refute File.regular?("_build/dev/lib/sample/ebin/sample.app")

      Mix.Task.rerun("app.config")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/sample.app")

      assert Code.loaded?(A)
      purge([A])

      Mix.Task.rerun("app.config", [])
      refute Code.loaded?(A)

      Mix.Task.rerun("app.config", ["--preload-modules"])
      assert Code.loaded?(A)
    end)
  end

  test "start checks for invalid configuration", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      :ok = :application.load({:application, :loaded_sample, [vsn: ~c"1.0.0", env: []]})
      Mix.ProjectStack.loaded_config([:sample, :unknown_sample, :loaded_sample], [])
      Mix.Tasks.App.Config.run([])

      assert_received {:mix_shell, :error,
                       ["You have configured application :unknown_sample" <> _]}

      refute_received {:mix_shell, :error,
                       ["You have configured application :loaded_sample" <> _]}
    end)
  end

  test "validates Elixir version requirement", context do
    Mix.ProjectStack.post_config(elixir: "~> ~> 0.8.1")

    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      assert_raise Mix.Error, ~r"Invalid Elixir version requirement", fn ->
        Mix.Tasks.App.Start.run(["--no-start"])
      end
    end)
  end

  test "validates the Elixir version with requirement", context do
    Mix.ProjectStack.post_config(elixir: "~> 0.8.1")

    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      assert_raise Mix.ElixirVersionError, ~r/You're trying to run :sample on Elixir/, fn ->
        Mix.Tasks.App.Start.run(["--no-start"])
      end
    end)
  end

  test "does not validate the Elixir version with requirement when disabled", context do
    Mix.ProjectStack.post_config(elixir: "~> 0.8.1")

    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      Mix.Tasks.App.Start.run(["--no-start", "--no-elixir-version-check"])
    end)
  end
end
