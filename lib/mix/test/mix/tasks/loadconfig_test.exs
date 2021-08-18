Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.LoadconfigTest do
  use MixTest.Case

  test "reads and persists project configuration", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      write_config("""
      [my_app: [key: :project]]
      """)

      assert Application.fetch_env(:my_app, :key) == :error
      Mix.Task.run("loadconfig", [])
      assert Application.fetch_env(:my_app, :key) == {:ok, :project}

      # App configuration should have lower precedence
      :ok = :application.load({:application, :my_app, [vsn: '1.0.0', env: [key: :app]]})
      assert Application.fetch_env(:my_app, :key) == {:ok, :project}

      # loadconfig can be called multiple times
      # Later values should have higher precedence
      Mix.Task.run("loadconfig", [fixture_path("config.exs")])
      assert Application.fetch_env(:my_app, :key) == {:ok, :value}
    end)
  end

  test "sets config_env() and config_target()", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      write_config("""
      import Config
      config :opts_app, :key, {config_env(), config_target()}
      """)

      assert Application.fetch_env(:opts_app, :key) == :error
      Mix.Task.run("loadconfig", [])
      assert Application.fetch_env(:opts_app, :key) == {:ok, {:dev, :host}}
    end)
  end

  test "reads from custom config_path", context do
    Mix.ProjectStack.post_config(config_path: "fresh.config")

    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      write_config("fresh.config", """
      [config_app: [key: :value]]
      """)

      assert Application.fetch_env(:config_app, :key) == :error
      Mix.Task.run("loadconfig", [])
      assert Application.fetch_env(:config_app, :key) == {:ok, :value}

      File.rm("fresh.config")

      assert_raise File.Error, ~r"could not read file .*/fresh\.config", fn ->
        Mix.Task.run("loadconfig", [])
      end
    end)
  end

  test "updates config files and config mtime", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)

      mtime = Mix.Project.config_mtime()
      config = Path.expand("config/config.exs")
      refute config in Mix.Project.config_files()

      write_config(config, "[]")
      Mix.Task.run("loadconfig", [config])
      assert config in Mix.Project.config_files()
      assert Mix.Project.config_mtime() > mtime

      # Touching it should not have any deadlocks
      File.touch!(config, {{2038, 1, 1}, {0, 0, 0}})
      Mix.Task.run("loadconfig", [config])
      assert config in Mix.Project.config_files()
      assert Mix.Project.config_mtime() > mtime
    end)
  end

  defp write_config(path \\ "config/config.exs", contents) do
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, contents)
  end
end
