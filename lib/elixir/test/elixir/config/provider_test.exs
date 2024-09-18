Code.require_file("../test_helper.exs", __DIR__)

defmodule Config.ProviderTest do
  use ExUnit.Case

  doctest Config.Provider
  alias Config.Provider
  import PathHelpers
  import ExUnit.CaptureIO

  @tmp_path tmp_path("config_provider")
  @env_var "ELIXIR_CONFIG_PROVIDER_BOOTED"
  @sys_config Path.join(@tmp_path, "sys.config")

  setup context do
    File.rm_rf(@tmp_path)
    File.mkdir_p!(@tmp_path)
    write_sys_config!(context[:sys_config] || [])

    on_exit(fn ->
      Application.delete_env(:elixir, :config_provider_init)
      Application.delete_env(:elixir, :config_provider_booted)
      System.delete_env(@env_var)
    end)
  end

  test "validate_compile_env" do
    assert Config.Provider.validate_compile_env([{:elixir, [:unknown], :error}]) == :ok

    Application.put_env(:elixir, :unknown, nested: [key: :value])

    assert Config.Provider.validate_compile_env([
             {:elixir, [:unknown], {:ok, [nested: [key: :value]]}}
           ]) == :ok

    assert Config.Provider.validate_compile_env([
             {:elixir, [:unknown, :nested], {:ok, [key: :value]}}
           ]) == :ok

    assert Config.Provider.validate_compile_env([
             {:elixir, [:unknown, :nested, :key], {:ok, :value}}
           ]) == :ok

    assert Config.Provider.validate_compile_env([
             {:elixir, [:unknown, :nested, :unknown], :error}
           ]) == :ok

    assert {:error, msg} =
             Config.Provider.validate_compile_env([{:elixir, [:unknown, :nested], :error}])

    assert msg =~ "Compile time value was not set"

    assert {:error, msg} =
             Config.Provider.validate_compile_env([
               {:elixir, [:unknown, :nested], {:ok, :another}}
             ])

    assert msg =~ "Compile time value was set to: :another"

    keys = [:unknown, :nested, :key, :too_deep]

    assert {:error, msg} =
             Config.Provider.validate_compile_env([{:elixir, keys, :error}])

    assert msg =~
             "application :elixir failed reading its compile environment for path [:nested, :key, :too_deep] inside key :unknown"
  after
    Application.delete_env(:elixir, :unknown)
  end

  describe "config_path" do
    test "validate!" do
      assert Provider.validate_config_path!("/foo") == :ok
      assert Provider.validate_config_path!({:system, "foo", "bar"}) == :ok

      assert_raise ArgumentError, fn -> Provider.validate_config_path!({:system, 1, 2}) end
      assert_raise ArgumentError, fn -> Provider.validate_config_path!(~c"baz") end
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
      assert config[:elixir] == [config_provider_booted: {:booted, nil}]
    end

    @tag sys_config: [my_app: [encoding: {:_μ, :"£", "£", ~c"£"}]]
    test "writes sys_config with encoding" do
      init_and_assert_boot()
      config = consult(@sys_config)
      assert config[:my_app][:encoding] == {:_μ, :"£", "£", ~c"£"}
    end

    @tag sys_config: [my_app: [key: :old_value, sys_key: :sys_value, extra_config: :old_value]]
    test "writes extra config with overrides" do
      init_and_assert_boot(extra_config: [my_app: [key: :old_extra_value, extra_config: :value]])

      assert consult(@sys_config)[:my_app] ==
               [sys_key: :sys_value, extra_config: :value, key: :value]
    end

    test "returns :booted if already booted and keeps config file" do
      init_and_assert_boot()
      Application.put_all_env(Keyword.take(consult(@sys_config), [:elixir]))
      assert boot() == :booted
      refute_received :restart
      assert File.exists?(@sys_config)
    end

    test "returns :booted if already booted and prunes config file" do
      init_and_assert_boot(prune_runtime_sys_config_after_boot: true)
      Application.put_all_env(Keyword.take(consult(@sys_config), [:elixir]))
      assert boot() == :booted
      refute_received :restart
      refute File.exists?(@sys_config)
    end

    test "returns :booted if already booted and runs validate_compile_env" do
      init_and_assert_boot(
        prune_runtime_sys_config_after_boot: true,
        validate_compile_env: [{:elixir, [:unknown], {:ok, :value}}]
      )

      Application.put_all_env(Keyword.take(consult(@sys_config), [:elixir]))

      assert capture_abort(fn -> boot() end) =~
               "the application :elixir has a different value set for key :unknown"
    end

    test "returns without rebooting" do
      reader = {Config.Reader, fixture_path("configs/kernel.exs")}
      init = Config.Provider.init([reader], @sys_config, reboot_system_after_config: false)
      Application.put_all_env(init)

      assert capture_abort(fn ->
               Provider.boot(fn ->
                 raise "should not be called"
               end)
             end) =~
               "Cannot configure :kernel because :reboot_system_after_config has been set to false"

      # Make sure values before and after match
      write_sys_config!(kernel: [elixir_reboot: true])
      Application.put_all_env(init)
      System.delete_env(@env_var)

      Provider.boot(fn -> raise "should not be called" end)
      assert Application.get_env(:kernel, :elixir_reboot) == true
      assert Application.get_env(:elixir_reboot, :key) == :value
    end
  end

  defp init(opts) do
    reader = {Config.Reader, fixture_path("configs/good_config.exs")}
    init = Config.Provider.init([reader], Keyword.get(opts, :path, @sys_config), opts)
    Application.put_all_env(init)
    init
  end

  defp boot do
    Provider.boot(fn -> send(self(), :restart) end)
  end

  defp init_and_assert_boot(opts \\ []) do
    init(opts ++ [reboot_system_after_config: true])
    boot()
    assert_received :restart
  end

  defp consult(file) do
    {:ok, [config]} = :file.consult(file)
    config
  end

  defp capture_abort(fun) do
    capture_io(fn ->
      assert_raise ErlangError, fun
    end)
  end

  defp write_sys_config!(data) do
    File.write!(@sys_config, IO.chardata_to_string(:io_lib.format("~tw.~n", [data])))
  end
end
