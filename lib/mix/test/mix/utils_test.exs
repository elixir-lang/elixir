Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Cheers do
end

defmodule Mix.UtilsTest do
  use MixTest.Case
  doctest Mix.Utils

  setup do
    # Store state before test
    mix_home = System.get_env("MIX_HOME")

    # Clear all variables to get a reproducible test
    System.delete_env("MIX_HOME")
    System.delete_env("MIX_XDG")

    # Reset Env Variables
    on_exit(fn ->
      System.put_env("MIX_HOME", mix_home)
      System.delete_env("MIX_XDG")
    end)
  end

  test "command to module" do
    assert Mix.Utils.command_to_module("cheers", Mix.Tasks) == {:module, Mix.Tasks.Cheers}
    assert Mix.Utils.command_to_module("unknown", Mix.Tasks) == {:error, :nofile}
  end

  test "module name to command" do
    assert Mix.Utils.module_name_to_command(Mix.Tasks.Foo, 2) == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo", 2) == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo.Bar", 2) == "foo.bar"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.FooBar.Bing", 2) == "foo_bar.bing"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.FooBar.BingBang", 2) == "foo_bar.bing_bang"
  end

  test "command to module name" do
    assert Mix.Utils.command_to_module_name("foo") == "Foo"
    assert Mix.Utils.command_to_module_name("foo.bar") == "Foo.Bar"
    assert Mix.Utils.command_to_module_name("foo_bar.baz") == "FooBar.Baz"
    assert Mix.Utils.command_to_module_name("foo_bar.baz_bing") == "FooBar.BazBing"
  end

  test "extract files" do
    files = Mix.Utils.extract_files([Path.join(fixture_path(), "archive")], "*.ex")
    assert length(files) == 1
    assert Path.basename(hd(files)) == "local.sample.ex"
  end

  test "extract files with empty string returns empty list" do
    assert Mix.Utils.extract_files([""], ".ex") == []
  end

  test "extract stale" do
    # 2038-01-01 00:00:00
    time = 2_145_916_800
    assert Mix.Utils.extract_stale([__ENV__.file], [time]) == []

    # 2000-01-01 00:00:00
    time = 946_684_800
    assert Mix.Utils.extract_stale([__ENV__.file], [time]) == [__ENV__.file]

    assert Mix.Utils.extract_stale([__ENV__.file], [__ENV__.file]) == []
  end

  test "handles missing target files" do
    assert Mix.Utils.stale?([__ENV__.file], []) == true
  end

  test "symlink or copy" do
    in_fixture("archive", fn ->
      File.mkdir_p!("_build/archive")
      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
      assert_ebin_symlinked_or_copied(result)
    end)
  end

  test "symlink or copy removes previous directories" do
    in_fixture("archive", fn ->
      File.mkdir_p!("_build/archive/ebin")
      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
      assert_ebin_symlinked_or_copied(result)
    end)
  end

  @tag unix: true
  test "symlink or copy erases wrong symlinks" do
    in_fixture("archive", fn ->
      File.mkdir_p!("_build/archive")
      build_ebin = Path.expand("_build/archive/ebin")
      Mix.Utils.symlink_or_copy(Path.expand("priv"), build_ebin)

      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), build_ebin)
      assert_ebin_symlinked_or_copied(result)
    end)
  end

  test "proxy_config reads from env and returns credentials" do
    assert Mix.Utils.proxy_config("http://example.com") == []

    System.put_env("http_proxy", "http://nopass@example.com")
    assert Mix.Utils.proxy_config("http://example.com") == [proxy_auth: {'nopass', ''}]

    System.put_env("HTTP_PROXY", "http://my:proxy@example.com")
    assert Mix.Utils.proxy_config("http://example.com") == [proxy_auth: {'my', 'proxy'}]

    System.put_env("https_proxy", "https://another:proxy@example.com")
    assert Mix.Utils.proxy_config("https://example.com") == [proxy_auth: {'another', 'proxy'}]

    System.put_env("HTTPS_PROXY", "https://example.com")
    assert Mix.Utils.proxy_config("https://example.com") == []
  end

  # 10.0.0.0 is a non-routable address
  test "read_path timeouts requests" do
    # If the request finishes for a reason, it will fail due to the checksum.
    case Mix.Utils.read_path("http://10.0.0.0/", timeout: 0) do
      {:remote, "request timed out after 0ms"} -> :ok
      {:checksum, "fetching from URIs require a checksum to be given"} -> :ok
    end
  end

  describe "mix_home/0" do
    test "prefers MIX_HOME over MIX_XDG" do
      System.put_env("MIX_HOME", "mix_home")
      System.put_env("MIX_XDG", "true")
      assert "mix_home" = Mix.Utils.mix_home()
    end

    @tag :unix
    test "falls back to XDG_DATA_HOME/mix" do
      System.put_env("MIX_XDG", "1")
      assert Mix.Utils.mix_home() == :filename.basedir(:user_data, "mix", %{os: :linux})
    end

    test "falls back to $HOME/.mix" do
      assert Path.expand("~/.mix") == Mix.Utils.mix_home()
    end
  end

  describe "mix_config/0" do
    test "prefers MIX_HOME over MIX_XDG" do
      System.put_env("MIX_HOME", "mix_home")
      System.put_env("MIX_XDG", "true")
      assert "mix_home" = Mix.Utils.mix_config()
    end

    @tag :unix
    test "falls back to XDG_CONFIG_HOME/mix" do
      System.put_env("MIX_XDG", "1")
      assert Mix.Utils.mix_config() == :filename.basedir(:user_config, "mix", %{os: :linux})
    end

    test "falls back to $HOME/.mix" do
      assert Path.expand("~/.mix") == Mix.Utils.mix_config()
    end
  end

  defp assert_ebin_symlinked_or_copied(result) do
    case result do
      {:ok, paths} ->
        assert Path.expand("_build/archive/ebin") in paths

      :ok ->
        expected_link =
          case :os.type() do
            # relative symlink on Windows are broken, see symlink_or_copy/2
            {:win32, _} ->
              "ebin" |> Path.expand() |> String.to_charlist()

            _ ->
              '../../ebin'
          end

        {:ok, actual_link} = :file.read_link("_build/archive/ebin")
        assert actual_link == expected_link

      _ ->
        msg =
          "expected symlink_or_copy to return :ok or {:ok, list_of_paths}, got: #{inspect(result)}"

        flunk(msg)
    end
  end
end
