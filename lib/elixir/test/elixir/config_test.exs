Code.require_file("test_helper.exs", __DIR__)

defmodule ConfigTest do
  use ExUnit.Case, async: true

  doctest Config
  import Config
  import PathHelpers

  setup config do
    Process.put({Config, :opts}, {config[:env], config[:target]})
    Process.put({Config, :config}, [])
    Process.put({Config, :imports}, config[:imports] || [])
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

  @tag env: :dev
  test "config_env/0" do
    assert config_env() == :dev
  end

  test "config_env/0 raises if no env is set" do
    assert_raise RuntimeError, "no :env key was given to this configuration file", fn ->
      config_env()
    end
  end

  @tag target: :host
  test "config_target/0" do
    assert config_target() == :host
  end

  test "config_target/0 raises if no env is set" do
    assert_raise RuntimeError, "no :target key was given to this configuration file", fn ->
      config_target()
    end
  end

  test "import_config/1" do
    import_config fixture_path("configs/good_config.exs")
    assert config() == [my_app: [key: :value]]
    assert files() == [fixture_path("configs/good_config.exs")]
  end

  @tag imports: :disabled
  test "import_config/1 raises when disabled" do
    assert_raise RuntimeError,
                 ~r"import_config/1 is not enabled for this configuration file",
                 fn -> import_config fixture_path("configs/good_config.exs") end
  end

  test "import_config/1 raises for recursive import" do
    assert_raise ArgumentError,
                 ~r"attempting to load configuration .*/imports_recursive.exs recursively",
                 fn -> import_config fixture_path("configs/imports_recursive.exs") end
  end

  test "import_config/1 with nested" do
    config :app, Repo, key: [nested: false, other: true]
    import_config fixture_path("configs/nested.exs")
    assert config() == [app: [{Repo, key: [other: true, nested: true]}]]
  end

  test "import_config/1 with bad path" do
    assert_raise File.Error, ~r"could not read file .*/configs/unknown.exs", fn ->
      import_config fixture_path("configs/unknown.exs")
    end
  end
end
