Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.LoadconfigTest do
  use MixTest.Case

  @apps [:my_app, :other_app]

  setup do
    on_exit fn ->
      Enum.each @apps, fn app ->
        Enum.each Application.get_all_env(app), fn {key, _} ->
          Application.delete_env(app, key, persistent: true)
        end
      end
    end
    :ok
  end

  test "reads and persists application configuration" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      write_config """
      [my_app: [key: :value]]
      """

      assert Application.fetch_env(:my_app, :key) == :error
      Mix.Tasks.Loadconfig.run []
      assert Application.fetch_env(:my_app, :key) == {:ok, :value}
      :ok = :application.load({:application, :my_app, [vsn: '1.0.0', env: [key: :app]]})
      assert Application.fetch_env(:my_app, :key) == {:ok, :value}
    end
  end

  defp write_config(path \\ "config/config.exs", contents) do
    File.mkdir_p! Path.dirname(path)
    File.write! path, contents
  end
end
