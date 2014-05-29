Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ConfigTest do
  use MixTest.Case, async: true

  doctest Mix.Config

  test "config/2" do
    use Mix.Config
    assert var!(config, Mix.Config) == []

    config :lager, key: :value
    assert var!(config, Mix.Config) == [lager: [key: :value]]

    config :lager, other: :value
    assert var!(config, Mix.Config) == [lager: [other: :value, key: :value]]

    config :lager, key: :other
    assert var!(config, Mix.Config) == [lager: [key: :other, other: :value]]
  end

  test "config/3" do
    use Mix.Config

    config :app, Repo, key: :value
    assert var!(config, Mix.Config) == [app: [{Repo, key: :value}]]

    config :app, Repo, other: :value
    assert var!(config, Mix.Config) == [app: [{Repo, other: :value, key: :value}]]

    config :app, Repo, key: :other
    assert var!(config, Mix.Config) == [app: [{Repo, [key: :other, other: :value]}]]
  end

  test "import_config/1" do
    use Mix.Config
    import_config fixture_path("configs/good_config.exs")
    assert var!(config, Mix.Config) == [my_app: [key: :value]]
  end

  test "read!/1" do
    assert Mix.Config.read!(fixture_path("configs/good_config.exs")) ==
           [my_app: [key: :value]]

    assert Mix.Config.read!(fixture_path("configs/good_import.exs")) ==
           [my_app: [key: :value]]

    exception = assert_raise Mix.Config.LoadError, fn ->
      Mix.Config.read! fixture_path("configs/bad_app.exs")
    end

    assert Exception.message(exception) =~ ~r"could not load config .*bad_app\.exs\n"
    assert Exception.message(exception) =~ ~r"expected config for app :sample to return keyword list, got: :oops"

    exception = assert_raise Mix.Config.LoadError, fn ->
      Mix.Config.read! fixture_path("configs/bad_root.exs")
    end

    assert Exception.message(exception) =~ ~r"could not load config .*bad_root\.exs\n"
    assert Exception.message(exception) =~ ~r"expected config file to return keyword list, got: :oops"

    exception = assert_raise Mix.Config.LoadError, fn ->
      Mix.Config.read! fixture_path("configs/bad_import.exs")
    end

    assert Exception.message(exception) =~ ~r"could not load config .*bad_root\.exs\n"
    assert Exception.message(exception) =~ ~r"expected config file to return keyword list, got: :oops"
  end

  test "persist/1" do
    assert Application.get_env(:my_app, :key) == nil
    Mix.Config.persist [my_app: [key: :value]]
    assert Application.get_env(:my_app, :key) == :value
  after
    Application.delete_env(:my_app, :key)
  end
end
