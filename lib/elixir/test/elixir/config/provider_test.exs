Code.require_file("../test_helper.exs", __DIR__)

defmodule Config.ProviderTest do
  use ExUnit.Case

  doctest Config.Provider
  alias Config.Provider
  import PathHelpers
  import ExUnit.CaptureIO

  @tmp_path tmp_path("config_provider")
  @env_var "ELIXIR_CONFIG_PROVIDER_PATH"
  @config_app :config_app
  @sys_config Path.join(@tmp_path, "sys.config")

  setup context do
    System.put_env(@env_var, @tmp_path)

    File.rm_rf(@tmp_path)
    File.mkdir_p!(@tmp_path)
    File.write!(@sys_config, :io_lib.format("~p.~n", [context[:sys_config] || []]))

    on_exit(fn ->
      Application.delete_env(@config_app, :config_providers)
      System.delete_env(@env_var)
      System.delete_env("ELIXIR_CONFIG_PROVIDER_BOOTED")
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
      init_and_assert_boot()
      config = consult(@sys_config)
      assert config[:my_app] == [key: :value]
      assert config[@config_app] == [config_providers: :booted]
    end

    test "writes extra config" do
      init_and_assert_boot(extra_config: [my_app: [key: :old_value, sys_key: :sys_value]])
      assert consult(@sys_config)[:my_app] == [sys_key: :sys_value, key: :value]
    end

    @tag sys_config: [my_app: [key: :old_value, sys_key: :sys_value, extra_config: :old_value]]
    test "overrides sys_config" do
      init_and_assert_boot(extra_config: [my_app: [extra_config: :value]])

      assert consult(@sys_config)[:my_app] ==
               [sys_key: :sys_value, extra_config: :value, key: :value]
    end

    test "returns :booted if already booted and keeps config file" do
      init_and_assert_boot()
      Application.put_all_env(Keyword.take(consult(@sys_config), [@config_app]))
      assert boot() == :booted
      refute_received :restart
      assert File.exists?(@sys_config)
    end

    test "returns :booted if already booted and prunes config file" do
      init_and_assert_boot(prune_after_boot: true)
      Application.put_all_env(Keyword.take(consult(@sys_config), [@config_app]))
      assert boot() == :booted
      refute_received :restart
      refute File.exists?(@sys_config)
    end

    test "raises if booting twice in a row" do
      init_and_assert_boot()

      assert_raise RuntimeError, ~r"Got infinite loop when running Config.Provider", fn ->
        assert capture_io(:stderr, fn -> init_and_assert_boot() end) != ""
      end
    end
  end

  defp init(opts) do
    reader = {Config.Reader, fixture_path("configs/good_config.exs")}
    init = Config.Provider.init([reader], Keyword.get(opts, :path, @sys_config), opts)
    Application.put_env(@config_app, :config_providers, init)
    init
  end

  defp boot do
    Provider.boot(@config_app, :config_providers, fn -> send(self(), :restart) end)
  end

  defp init_and_assert_boot(opts \\ []) do
    init(opts)
    boot()
    assert_received :restart
  end

  defp consult(file) do
    {:ok, [config]} = :file.consult(file)
    config
  end
end
