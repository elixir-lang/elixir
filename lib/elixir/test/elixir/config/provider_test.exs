Code.require_file("../test_helper.exs", __DIR__)

defmodule Config.ProviderTest do
  use ExUnit.Case

  doctest Config.Provider
  alias Config.Provider
  import PathHelpers
  import ExUnit.CaptureIO

  @tmp_path tmp_path("config_provider")
  @env_var "ELIXIR_CONFIG_PROVIDER_BOOTED"
  @config_app :config_app
  @sys_config Path.join(@tmp_path, "sys.config")

  setup context do
    File.rm_rf(@tmp_path)
    File.mkdir_p!(@tmp_path)
    write_sys_config!(context[:sys_config] || [])

    on_exit(fn ->
      Application.delete_env(@config_app, :config_providers)
      System.delete_env(@env_var)
    end)
  end

  test "validate_compile_env" do
    Config.Provider.validate_compile_env([{:elixir, [:unknown], :error}])

    Application.put_env(:elixir, :unknown, nested: [key: :value])
    Config.Provider.validate_compile_env([{:elixir, [:unknown], {:ok, [nested: [key: :value]]}}])
    Config.Provider.validate_compile_env([{:elixir, [:unknown, :nested], {:ok, [key: :value]}}])
    Config.Provider.validate_compile_env([{:elixir, [:unknown, :nested, :key], {:ok, :value}}])
    Config.Provider.validate_compile_env([{:elixir, [:unknown, :nested, :unknown], :error}])

    assert capture_abort(fn ->
             Config.Provider.validate_compile_env([{:elixir, [:unknown, :nested], :error}])
           end) =~ "Compile time value was not set"

    assert capture_abort(fn ->
             Config.Provider.validate_compile_env([
               {:elixir, [:unknown, :nested], {:ok, :another}}
             ])
           end) =~ "Compile time value was set to: :another"

    assert capture_abort(fn ->
             keys = [:unknown, :nested, :key, :too_deep]
             Config.Provider.validate_compile_env([{:elixir, keys, :error}])
           end) =~
             "application :elixir failed reading its compile environment for path [:nested, :key, :too_deep] inside key :unknown"
  after
    Application.delete_env(:elixir, :unknown)
  end

  describe "config_path" do
    test "validate!" do
      assert Provider.validate_config_path!("/foo") == :ok
      assert Provider.validate_config_path!({:system, "foo", "bar"}) == :ok

      assert_raise ArgumentError, fn -> Provider.validate_config_path!({:system, 1, 2}) end
      assert_raise ArgumentError, fn -> Provider.validate_config_path!('baz') end
    end

    test "resolve!" do
      env_var = "ELIXIR_CONFIG_PROVIDER_PATH"

      try do
        System.put_env(env_var, @tmp_path)
        assert Provider.resolve_config_path!("/foo") == "/foo"
        assert Provider.resolve_config_path!({:system, env_var, "/bar"}) == @tmp_path <> "/bar"
      after
        System.delete_env(env_var)
      end
    end
  end

  describe "boot" do
    test "runs providers" do
      init_and_assert_boot()
      config = consult(@sys_config)
      assert config[:my_app] == [key: :value]
      assert config[@config_app] == [config_providers: :booted]
    end

    @tag sys_config: [my_app: [encoding: {:"£", "£", '£'}]]
    test "writes sys_config with encoding" do
      init_and_assert_boot()
      config = consult(@sys_config)
      assert config[:my_app][:encoding] == {:"£", "£", '£'}
    end

    @tag sys_config: [my_app: [key: :old_value, sys_key: :sys_value, extra_config: :old_value]]
    test "writes extra config with overrides" do
      init_and_assert_boot(extra_config: [my_app: [key: :old_extra_value, extra_config: :value]])

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

      assert capture_abort(fn ->
               init_and_assert_boot()
             end) =~ "Got infinite loop when running Config.Provider"
    end

    test "returns without rebooting" do
      reader = {Config.Reader, fixture_path("configs/kernel.exs")}
      init = Config.Provider.init([reader], @sys_config, reboot_after_config: false)
      Application.put_env(@config_app, :config_providers, init)

      assert capture_abort(fn ->
               Provider.boot(@config_app, :config_providers, fn ->
                 raise "should not be called"
               end)
             end) =~ "Cannot configure :kernel because :reboot_after_config has been set to false"

      # Make sure values before and after match
      write_sys_config!(kernel: [elixir_reboot: true])
      Application.put_env(@config_app, :config_providers, init)
      System.delete_env(@env_var)

      Provider.boot(@config_app, :config_providers, fn -> raise "should not be called" end)
      assert Application.get_env(:kernel, :elixir_reboot) == true
      assert Application.get_env(:elixir_reboot, :key) == :value
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

  defp capture_abort(fun) do
    capture_io(:stderr, fn ->
      assert_raise ErlangError, fun
    end)
  end

  defp write_sys_config!(data) do
    File.write!(@sys_config, :io_lib.format("~tw.~n", [data]), [:utf8])
  end
end
