Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.LoadconfigTest do
  use MixTest.Case

  @tag apps: [:my_app]
  test "reads and persists project configuration", context do
    Mix.Project.push MixTest.Case.Sample

    in_tmp context.test, fn ->
      write_config """
      [my_app: [key: :project]]
      """

      assert Application.fetch_env(:my_app, :key) == :error
      Mix.Task.run "loadconfig", []
      assert Application.fetch_env(:my_app, :key) == {:ok, :project}

      # App configuration should have lower precedence
      :ok = :application.load({:application, :my_app, [vsn: '1.0.0', env: [key: :app]]})
      assert Application.fetch_env(:my_app, :key) == {:ok, :project}

      # loadconfig can be called multiple times
      # Later values should have higher precedence
      Mix.Task.run "loadconfig", [fixture_path("configs/good_config.exs")]
      assert Application.fetch_env(:my_app, :key) == {:ok, :value}
    end
  end

  @tag apps: [:config_app]
  test "reads from custom config_path", context do
    Mix.ProjectStack.post_config [config_path: "fresh.config"]
    Mix.Project.push MixTest.Case.Sample

    in_tmp context.test, fn ->
      write_config "fresh.config", """
      [config_app: [key: :value]]
      """
      assert Application.fetch_env(:config_app, :key) == :error
      Mix.Task.run "loadconfig", []
      assert Application.fetch_env(:config_app, :key) == {:ok, :value}

      File.rm "fresh.config"
      assert_raise Mix.Config.LoadError, ~r"could not load config fresh\.config", fn ->
        Mix.Task.run "loadconfig", []
      end
    end
  end

  defp write_config(path \\ "config/config.exs", contents) do
    File.mkdir_p! Path.dirname(path)
    File.write! path, contents
  end
end
