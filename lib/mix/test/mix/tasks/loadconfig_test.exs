Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.LoadconfigTest do
  use MixTest.Case

  @apps [:my_app, :other_app]

  teardown do
    Enum.each @apps, fn app ->
      Enum.each Application.get_all_env(app), fn {key, _} ->
        Application.delete_env(app, key, persistent: true)
      end
    end
    :ok
  end

  defmodule Config do
    def project do
      [app: :myconfig, version: "0.1.0"]
    end
  end

  test "loads and sets application configuration" do
    Mix.Project.push Config

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

  test "logs on bad configuration" do
    Mix.Project.push Config

    in_fixture "no_mixfile", fn ->
      write_config """
      :oops
      """

      assert_raise ArgumentError, "expected config to return keyword list, got: :oops", fn ->
        Mix.Tasks.Loadconfig.run []
      end

      msg = "Could not load config config/config.exs from project #{inspect Config}"
      assert_received {:mix_shell, :error, [^msg]}
    end
  end

  test "merge umbrella children configs" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        write_config "apps/foo/config/config.exs", """
        [my_app: [key: :value]]
        """

        write_config "apps/bar/config/config.exs", """
        [other_app: [key: :value]]
        """

        Mix.Tasks.Loadconfig.run []
        assert Application.fetch_env(:my_app, :key) == {:ok, :value}
        assert Application.fetch_env(:other_app, :key) == {:ok, :value}
      end)
    end
  end

  test "raises on umbrella conflicts until resolved" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        write_config "apps/foo/config/config.exs", """
        [my_app: [key: :value1]]
        """

        write_config "apps/bar/config/config.exs", """
        [my_app: [key: :value2]]
        """

        msg = ~r":foo has set the configuration for key :key in app :my_app to :value1"
        assert_raise Mix.Error, msg, fn ->
          Mix.Tasks.Loadconfig.run []
        end

        write_config """
        [my_app: [key: :value3], other_app: [key: :value]]
        """

        Mix.Tasks.Loadconfig.run []
        assert Application.fetch_env(:other_app, :key) == {:ok, :value}
        assert Application.fetch_env(:my_app, :key) == {:ok, :value3}
      end)
    end
  end

  defp write_config(path \\ "config/config.exs", contents) do
    File.mkdir_p! Path.dirname(path)
    File.write! path, contents
  end
end
