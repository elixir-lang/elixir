Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.ReleaseTest do
  use MixTest.Case

  test "ships a bootable release with ERTS" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        root = Path.absname("_build/dev/rel/release_test")
        Mix.Task.run("release")

        cookie = File.read!(Path.join(root, "releases/COOKIE"))
        assert System.cmd(Path.join(root, "bin/start"), []) == {"", 0}

        assert Code.eval_file("RELEASE_BOOTED") |> elem(0) == %{
                 app_dir: Path.join(root, "lib/release_test-0.1.0"),
                 cookie_env: cookie,
                 cookie_node: String.to_atom(cookie),
                 node: :"release_test@127.0.0.1",
                 protocols_consolidated?: true,
                 release_name: "release_test",
                 release_root: root,
                 release_vsn: "0.1.0",
                 root_dir: root,
                 static_config: :was_set
               }
      end)
    end)
  end

  test "ships a bootable release without ERTS" do
    in_fixture("release_test", fn ->
      config = [releases: [demo: [include_erts: false, cookie: "abcdefghijk"]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        root = Path.absname("_build/dev/rel/demo")
        Mix.Task.run("release")

        assert System.cmd(Path.join(root, "bin/start"), []) == {"", 0}

        assert Code.eval_file("RELEASE_BOOTED") |> elem(0) == %{
                 app_dir: Path.join(root, "lib/release_test-0.1.0"),
                 cookie_env: "abcdefghijk",
                 cookie_node: :abcdefghijk,
                 node: :"demo@127.0.0.1",
                 protocols_consolidated?: true,
                 release_name: "demo",
                 release_root: root,
                 release_vsn: "0.1.0",
                 root_dir: :code.root_dir() |> to_string(),
                 static_config: :was_set
               }
      end)
    end)
  end

  test "executes rpc instructions" do
    in_fixture("release_test", fn ->
      config = [releases: [permanent: [include_erts: false]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        root = Path.absname("_build/dev/rel/permanent")
        Mix.Task.run("release")

        task = Task.async(fn -> System.cmd(Path.join(root, "bin/start"), []) end)
        wait_until_file("RELEASE_BOOTED")

        assert {pid, 0} = System.cmd(Path.join(root, "bin/permanent"), ["pid"])
        assert pid != "\n"

        assert System.cmd(Path.join(root, "bin/permanent"), ["rpc", "ReleaseTest.hello_world"]) ==
                 {"hello world\n", 0}

        assert System.cmd(Path.join(root, "bin/permanent"), ["stop"]) ==
                 {"", 0}

        assert Task.await(task) == {"", 0}
      end)
    end)
  end

  @tag :unix
  test "runs in daemon mode" do
    in_fixture("release_test", fn ->
      config = [releases: [daemon: [include_erts: false]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        root = Path.absname("_build/dev/rel/daemon")
        Mix.Task.run("release")

        assert System.cmd(Path.join(root, "bin/daemon"), ["daemon", "iex"]) == {"", 0}
        wait_until_file("RELEASE_BOOTED")

        assert File.read!(Path.join(root, "tmp/log/erlang.log.1")) =~
                 "iex(daemon@127.0.0.1)1>"

        assert System.cmd(Path.join(root, "bin/daemon"), ["rpc", "ReleaseTest.hello_world"]) ==
                 {"hello world\n", 0}

        assert System.cmd(Path.join(root, "bin/daemon"), ["stop"]) ==
                 {"", 0}
      end)
    end)
  end

  defp wait_until_file(file) do
    if File.exists?(file) do
      :ok
    else
      Process.sleep(100)
      wait_until_file(file)
    end
  end

  # Test confirmation for existing release
  # Test command line flags
  # Test directory structure
end
