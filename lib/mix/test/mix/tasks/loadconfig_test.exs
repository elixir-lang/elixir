Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.LoadconfigTest do
  use MixTest.Case

  test "reads and persists project configuration", context do
    Mix.Project.push(MixTest.Case.Sample)

    in_tmp(context.test, fn ->
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
      Mix.Task.run("loadconfig", [fixture_path("configs/good_config.exs")])
      assert Application.fetch_env(:my_app, :key) == {:ok, :value}
    end)
  end

  test "sets config_env() and config_target()", context do
    Mix.Project.push(MixTest.Case.Sample)

    in_tmp(context.test, fn ->
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
    Mix.Project.push(MixTest.Case.Sample)

    in_tmp(context.test, fn ->
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
    Mix.Project.push(MixTest.Case.Sample)

    in_tmp(context.test, fn ->
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

  describe "kernel.logger" do
    test "sets logger level to configured value", context do
      Mix.Project.push(MixTest.Case.Sample)

      in_tmp(context.test, fn ->
        write_config("""
        [kernel: [logger_level: :warning]]
        """)

        Mix.Task.run("loadconfig", [])
        assert %{level: :warning} = :logger.get_primary_config()
      end)
    end

    test "remove default handler when unset", context do
      Mix.Project.push(MixTest.Case.Sample)

      assert :ok = :logger.add_handler(:default, :logger_std_h, %{})

      try do
        in_tmp(context.test, fn ->
          write_config("""
          [kernel: [logger: [{:handler, :default, :undefined}]]]
          """)

          Mix.Task.run("loadconfig", [])
          assert {:error, {:not_found, :default}} = :logger.get_handler_config(:default)
        end)
      after
        :logger.remove_handler(:default)
      end
    end

    test "replace default handler if set", context do
      Mix.Project.push(MixTest.Case.Sample)

      assert :ok = :logger.add_handler(:default, :logger_std_h, %{})

      try do
        in_tmp(context.test, fn ->
          write_config("""
          [kernel: [logger: [{:handler, :default, :logger_disk_log_h, %{}}]]]
          """)

          Mix.Task.run("loadconfig", [])
          assert {:ok, %{module: :logger_disk_log_h}} = :logger.get_handler_config(:default)
        end)
      after
        :logger.remove_handler(:default)
      end
    end

    test "add new handler if set", context do
      Mix.Project.push(MixTest.Case.Sample)

      try do
        in_tmp(context.test, fn ->
          write_config("""
          [kernel: [logger: [{:handler, :foo_bar, :logger_std_h, %{}}]]]
          """)

          Mix.Task.run("loadconfig", [])
          assert {:ok, _} = :logger.get_handler_config(:foo_bar)
        end)
      after
        :logger.remove_handler(:foo_bar)
      end
    end

    test "primary filters are set", context do
      Mix.Project.push(MixTest.Case.Sample)

      primary_config = :logger.get_primary_config()

      try do
        in_tmp(context.test, fn ->
          write_config("""
          [kernel: [logger: [
            {:filters, :log, [test: {&:logger_filters.remote_gl/2, :log}]}
          ]]]
          """)

          Mix.Task.run("loadconfig", [])
          assert %{filter_default: :log, filters: filters} = :logger.get_primary_config()
          assert :test in Keyword.keys(filters)
        end)
      after
        :logger.set_primary_config(primary_config)
      end
    end

    test "sets module levels", context do
      Mix.Project.push(MixTest.Case.Sample)

      try do
        in_tmp(context.test, fn ->
          write_config("""
          [kernel: [logger: [
            {:module_level, :emergency, [FooBar]}
          ]]]
          """)

          Mix.Task.run("loadconfig", [])
          assert [{FooBar, :emergency}] = :logger.get_module_level()
        end)
      after
        :logger.unset_module_level()
      end
    end
  end

  defp write_config(path \\ "config/config.exs", contents) do
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, contents)
  end
end
