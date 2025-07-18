# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Cheers do
end

defmodule Mix.UtilsTest do
  use MixTest.Case
  doctest Mix.Utils

  setup do
    Mix.ensure_application!(:inets)

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

  @tag :unix
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
    assert Mix.Utils.proxy_config("http://example.com") == [proxy_auth: {~c"nopass", ~c""}]

    System.put_env("HTTP_PROXY", "http://my:proxy@example.com")
    assert Mix.Utils.proxy_config("http://example.com") == [proxy_auth: {~c"my", ~c"proxy"}]

    System.put_env("https_proxy", "https://another:proxy@example.com")
    assert Mix.Utils.proxy_config("https://example.com") == [proxy_auth: {~c"another", ~c"proxy"}]

    System.put_env("HTTPS_PROXY", "https://example.com")
    assert Mix.Utils.proxy_config("https://example.com") == []
  after
    System.delete_env("http_proxy")
    System.delete_env("https_proxy")
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
    test "falls back to XDG_DATA_HOME/mix when MIX_XDG is set" do
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
    test "falls back to XDG_CONFIG_HOME/mix when MIX_XDG is set" do
      System.put_env("MIX_XDG", "1")
      assert Mix.Utils.mix_config() == :filename.basedir(:user_config, "mix", %{os: :linux})
    end

    test "falls back to $HOME/.mix" do
      assert Path.expand("~/.mix") == Mix.Utils.mix_config()
    end
  end

  describe "mix_cache/0" do
    @tag :unix
    test "prefers XDG_CACHE_HOME/mix when MIX_XDG is set" do
      System.put_env("MIX_XDG", "1")
      assert Mix.Utils.mix_cache() == :filename.basedir(:user_cache, "mix", %{os: :linux})
    end

    test "falls back to user cache dir" do
      assert Mix.Utils.mix_cache() == :filename.basedir(:user_cache, "mix")
    end
  end

  describe "write_according_to_opts/3" do
    test "verify that file writes with backups work as expected" do
      test_out = "test.out"
      test_out_bak = "test.out.bak"
      hello_world = "Hello World!"
      new_hello_world = "New Hello World!"
      default_out = "default.out"

      # ignore any error from this call.
      in_tmp("write to default file", fn ->
        # no optional override - write to the specified default file
        assert Mix.Utils.write_according_to_opts!(test_out, [hello_world], []) == test_out
        assert File.read!(test_out) == hello_world

        # no optional override - write to the specified default file again, with old file backed up
        assert Mix.Utils.write_according_to_opts!(test_out, [new_hello_world], []) == test_out
        assert File.read!(test_out) == new_hello_world
        assert File.read!(test_out_bak) == hello_world
      end)

      in_tmp("write to optional file override", fn ->
        # with optional override - write to the specified default file
        assert Mix.Utils.write_according_to_opts!(default_out, [hello_world], output: test_out) ==
                 test_out

        assert File.read!(test_out) == hello_world

        # with optional override - write to the specified default file again, with old file backed up
        assert Mix.Utils.write_according_to_opts!(default_out, [new_hello_world],
                 output: test_out
               ) ==
                 test_out

        assert File.read!(test_out) == new_hello_world
        assert File.read!(test_out_bak) == hello_world
      end)
    end

    test "verify that writing to STDOUT works as expected" do
      output =
        ExUnit.CaptureIO.capture_io(fn ->
          Mix.Utils.write_according_to_opts!("the_file.txt", ["some text"], output: "-")
        end)

      assert output == "some text"

      refute File.exists?("the_file.txt")
    end
  end

  describe "write_dot_graph!/4" do
    test "preserves newlines and other control characters" do
      in_tmp("dot_newlines", fn ->
        callback = fn node -> {{node, nil}, []} end

        Mix.Utils.write_dot_graph!("graph.dot", "graph", ["foo \nbar\r\nbaz"], callback, [])

        assert File.read!("graph.dot") == """
               digraph "graph" {
                 "foo 
               bar\r
               baz"
               }
               """
      end)
    end

    test "quote and backslash combinations" do
      in_tmp("dot_complex", fn ->
        callback = fn node -> {{node, nil}, []} end

        test_cases = [
          # "fo"o" -> "fo\"o"
          {"fo\"o", "fo\\\"o"},
          # "fo\"o" -> "fo\\\"o"
          {"fo\\\"o", "fo\\\\\"o"},
          # "fo\o" -> "fo\o"
          {"fo\\o", "fo\\o"},
          # "fo\\o" -> "fo\\o"
          {"fo\\\\o", "fo\\\\o"},
          # "fo\\\o" -> "fo\\\o"
          {"fo\\\\\\o", "fo\\\\\\o"}
        ]

        Enum.each(test_cases, fn {input, expected} ->
          Mix.Utils.write_dot_graph!("graph.dot", "graph", [input], callback, [])
          content = File.read!("graph.dot")
          assert content == "digraph \"graph\" {\n  \"#{expected}\"\n}\n"
        end)
      end)
    end

    test "escapes backslash at end of string" do
      in_tmp("dot_end_backslash", fn ->
        callback = fn node -> {{node, nil}, []} end

        test_cases = [
          # "fo\" -> "fo\\" (add backslash)
          {"fo\\", "fo\\\\"},
          # "fo\\" -> "fo\\" (already valid)
          {"fo\\\\", "fo\\\\"},
          # "fo\\\" -> "fo\\\\" (add backslash)
          {"fo\\\\\\", "fo\\\\\\\\"}
        ]

        Enum.each(test_cases, fn {input, expected} ->
          Mix.Utils.write_dot_graph!("graph.dot", "graph", [input], callback, [])
          content = File.read!("graph.dot")
          assert content == "digraph \"graph\" {\n  \"#{expected}\"\n}\n"
        end)
      end)
    end

    test "handles empty strings" do
      in_tmp("dot_empty", fn ->
        callback = fn node -> {{node, nil}, []} end

        Mix.Utils.write_dot_graph!("graph.dot", "graph", [""], callback, [])

        assert File.read!("graph.dot") == """
               digraph "graph" {
                 ""
               }
               """
      end)
    end

    test "handles edge labels with escaping" do
      in_tmp("dot_edge_labels", fn ->
        callback = fn node -> {{node, "edge \"label\""}, []} end

        Mix.Utils.write_dot_graph!("graph.dot", "graph", ["node"], callback, [])

        assert File.read!("graph.dot") == """
               digraph "graph" {
                 "node" [label="edge \\"label\\""]
               }
               """
      end)
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
              ~c"../../ebin"
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
