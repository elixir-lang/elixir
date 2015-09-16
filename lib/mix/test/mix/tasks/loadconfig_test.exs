Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.LoadconfigTest do
  use MixTest.Case

  @tag apps: [:my_app]
  test "reads and persists project configuration" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      write_config """
      [my_app: [key: :project]]
      """

      assert Application.fetch_env(:my_app, :key) == :error
      Mix.Task.run "loadconfig", []
      assert Application.fetch_env(:my_app, :key) == {:ok, :project}

      # App configuration should have lower precedence
      :ok = :application.load({:application, :my_app, [vsn: '1.0.0', env: [key: :app]]})
      assert Application.fetch_env(:my_app, :key) == {:ok, :project}

      # laodconfig can be called multiple times
      # Later values should have higher precedence
      Mix.Task.run "loadconfig", [fixture_path("configs/good_config.exs")]
      assert Application.fetch_env(:my_app, :key) == {:ok, :value}
    end
  end

  defp write_config(path \\ "config/config.exs", contents) do
    File.mkdir_p! Path.dirname(path)
    File.write! path, contents
  end
end
