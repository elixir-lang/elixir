Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.ConfigTest do
  use MixTest.Case, async: true

  import Mix.Config

  setup do
    Process.put({Config, :config}, [])
    Process.put({Config, :imports}, [])
    :ok
  end

  defp config do
    Process.get({Config, :config})
  end

  defp files do
    Process.get({Config, :imports})
  end

  test "config/2" do
    assert config() == []

    config :lager, key: :value
    assert config() == [lager: [key: :value]]

    config :lager, other: :value
    assert config() == [lager: [key: :value, other: :value]]

    config :lager, key: :other
    assert config() == [lager: [other: :value, key: :other]]

    # Works inside functions too...
    f = fn -> config(:lager, key: :fn) end
    f.()
    assert config() == [lager: [other: :value, key: :fn]]

    # ...and in for comprehensions.
    for _ <- 0..0, do: config(:lager, key: :for)
    assert config() == [lager: [other: :value, key: :for]]
  end

  test "config/3" do
    config :app, Repo, key: :value
    assert config() == [app: [{Repo, key: :value}]]

    config :app, Repo, other: :value
    assert config() == [app: [{Repo, key: :value, other: :value}]]

    config :app, Repo, key: :other
    assert config() == [app: [{Repo, other: :value, key: :other}]]

    config :app, Repo, key: [nested: false]
    assert config() == [app: [{Repo, other: :value, key: [nested: false]}]]

    config :app, Repo, key: [nested: true]
    assert config() == [app: [{Repo, other: :value, key: [nested: true]}]]

    config :app, Repo, key: :other
    assert config() == [app: [{Repo, other: :value, key: :other}]]
  end

  test "import_config/1" do
    import_config fixture_path("config.exs")
    assert config() == [my_app: [key: :value]]
    assert files() == [fixture_path("config.exs")]
  end

  test "import_config/1 with wildcards" do
    import_config fixture_path("confi*.exs")
    assert config() == [my_app: [key: :value]]
  end

  test "import_config/1 with wildcard with no matches" do
    import_config fixture_path("configs/nonexistent_*.exs")
    assert config() == []
  end
end
