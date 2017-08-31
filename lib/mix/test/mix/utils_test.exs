Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.Tasks.Cheers do
end

defmodule Mix.UtilsTest do
  use MixTest.Case
  doctest Mix.Utils

  test "command to module" do
    assert Mix.Utils.command_to_module("cheers", Mix.Tasks)   == {:module, Mix.Tasks.Cheers}
    assert Mix.Utils.command_to_module("unknown", Mix.Tasks) == {:error, :nofile}
  end

  test "module name to command" do
    assert Mix.Utils.module_name_to_command(Mix.Tasks.Foo, 2)       == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo", 2)     == "foo"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.Foo.Bar", 2) == "foo.bar"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.FooBar.Bing", 2) == "foo_bar.bing"
    assert Mix.Utils.module_name_to_command("Mix.Tasks.FooBar.BingBang", 2) == "foo_bar.bing_bang"
  end

  test "command to module name" do
    assert Mix.Utils.command_to_module_name("foo")     == "Foo"
    assert Mix.Utils.command_to_module_name("foo.bar") == "Foo.Bar"
    assert Mix.Utils.command_to_module_name("foo_bar.baz") == "FooBar.Baz"
    assert Mix.Utils.command_to_module_name("foo_bar.baz_bing") == "FooBar.BazBing"
  end

  test "extract files" do
    files = Mix.Utils.extract_files [Path.join(fixture_path(), "archive")], "*.ex"
    assert length(files) == 1
    assert Path.basename(hd(files)) == "local.sample.ex"
  end


  test "extract files with empty string returns empty list" do
    assert Mix.Utils.extract_files([""], ".ex") == []
  end

  test "extract stale" do
    time = {{2030, 1, 1}, {0, 0, 0}}
    assert Mix.Utils.extract_stale([__ENV__.file], [time]) == []

    time = {{2000, 1, 1}, {0, 0, 0}}
    assert Mix.Utils.extract_stale([__ENV__.file], [time]) == [__ENV__.file]

    assert Mix.Utils.extract_stale([__ENV__.file], [__ENV__.file]) == []
  end

  test "symlink or copy" do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive")
      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
      assert_ebin_symlinked_or_copied(result)
    end
  end

  test "symlink or copy removes previous directories" do
    in_fixture "archive", fn ->
      File.mkdir_p!("_build/archive/ebin")
      result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
      assert_ebin_symlinked_or_copied(result)
    end
  end

  @windows? match?({:win32, _}, :os.type)
  unless @windows? do
    test "symlink or copy erases wrong symlinks" do
      in_fixture "archive", fn ->
        File.mkdir_p!("_build/archive")
        Mix.Utils.symlink_or_copy(Path.expand("priv"), Path.expand("_build/archive/ebin"))
        result = Mix.Utils.symlink_or_copy(Path.expand("ebin"), Path.expand("_build/archive/ebin"))
        assert_ebin_symlinked_or_copied(result)
      end
    end
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

  defp assert_ebin_symlinked_or_copied(result) do
    case result do
      {:ok, paths} -> assert Path.expand("_build/archive/ebin") in paths
      :ok ->
        expected_link =
          case :os.type do
            # relative symlink on Windows are broken, see symlink_or_copy/2
            {:win32, _} -> "ebin" |> Path.expand() |> String.to_charlist()
            _ -> '../../ebin'
          end
        {:ok, actual_link} = :file.read_link("_build/archive/ebin")
        assert actual_link == expected_link
      _ -> flunk "expected symlink_or_copy to return :ok or {:ok, list_of_paths}, got: #{inspect result}"
    end
  end
end
