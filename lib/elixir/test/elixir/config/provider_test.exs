Code.require_file("../test_helper.exs", __DIR__)

defmodule Config.ProviderTest do
  use ExUnit.Case, async: true

  doctest Config.Provider
  alias Config.Provider
  import PathHelpers

  @tmp_path tmp_path("config_provider")
  @env_var "ELIXIR_CONFIG_PROVIDER_PATH"
  @sys_config Path.join(@tmp_path, "sys.config")

  setup context do
    System.put_env(@env_var, @tmp_path)

    File.rm_rf(@tmp_path)
    File.mkdir_p!(@tmp_path)
    File.write!(@sys_config, :io_lib.format("~p.~n", [context[:sys_config] || []]))

    on_exit(fn ->
      Application.delete_env(:elixir, :config_providers)
      System.delete_env(@env_var)
    end)
  end

  describe "config_path" do
    test "validate!" do
      assert Provider.validate_config_path!("/foo") == :ok
      assert Provider.validate_config_path!({:system, "foo", "bar"}) == :ok

      assert_raise ArgumentError, fn -> Provider.validate_config_path!({:system, 1, 2}) end
      assert_raise ArgumentError, fn -> Provider.validate_config_path!('baz') end
    end

    test "resolve!" do
      assert Provider.resolve_config_path!("/foo") == "/foo"
      assert Provider.resolve_config_path!({:system, @env_var, "/bar"}) == @tmp_path <> "/bar"
    end
  end

  describe "boot" do
    test "runs providers" do
      init()
      boot()
      assert_received :restart

      config = consult(@sys_config)
      assert config[:my_app] == [key: :value]
      assert config[:elixir] == [config_providers: :booted]
    end
  end

  defp init(opts \\ []) do
    reader = {Config.Reader, fixture_path("configs/good_config.exs")}
    init = Config.Provider.init([reader], Keyword.get(opts, :path, @sys_config), opts)
    Application.put_env(:elixir, :config_providers, init)
    init
  end

  defp boot do
    Provider.boot(:elixir, :config_providers, fn -> send(self(), :restart) end)
  end

  defp consult(file) do
    {:ok, [config]} = :file.consult(file)
    config
  end
end
