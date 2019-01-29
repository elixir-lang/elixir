Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.ReleaseTest do
  use MixTest.Case

  @erts_version :erlang.system_info(:version)

  test "assembles a bootable release with ERTS" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        root = Path.absname("_build/dev/rel/release_test")

        # Assert command
        Mix.Task.run("release")
        assert_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

        assert_received {:mix_shell, :info,
                         ["\nRelease created at _build/dev/rel/release_test!" <> _]}

        # Assert structure
        assert root |> Path.join("bin/release_test") |> File.exists?()
        assert root |> Path.join("erts-#{@erts_version}") |> File.exists?()
        assert root |> Path.join("lib/release_test-0.1.0/ebin") |> File.exists?()
        assert root |> Path.join("lib/release_test-0.1.0/priv/hello") |> File.exists?()
        assert root |> Path.join("releases/COOKIE") |> File.exists?()
        assert root |> Path.join("releases/start_erl.data") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/release_test.rel") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/sys.config") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/vm.args") |> File.exists?()

        assert root
               |> Path.join("lib/release_test-0.1.0/priv")
               |> File.read_link()
               |> elem(0) == :error

        cookie_string = File.read!(Path.join(root, "releases/COOKIE"))
        cookie_atom = String.to_atom(cookie_string)

        # Assert runtime
        assert System.cmd(Path.join(root, "bin/start"), []) == {"", 0}

        assert %{
                 app_dir: app_dir,
                 cookie_env: ^cookie_string,
                 cookie_node: ^cookie_atom,
                 node: :"release_test@127.0.0.1",
                 protocols_consolidated?: true,
                 release_name: "release_test",
                 release_node: "release_test@127.0.0.1",
                 release_root: release_root,
                 release_vsn: "0.1.0",
                 root_dir: root_dir,
                 static_config: :was_set
               } = Code.eval_file("RELEASE_BOOTED") |> elem(0)

        if match?({:win32, _}, :os.type()) do
          assert String.ends_with?(app_dir, "_build/dev/rel/RELEAS~1/lib/release_test-0.1.0")
          assert String.ends_with?(release_root, "_build\\dev\\rel\\RELEAS~1")
          assert String.ends_with?(root_dir, "_build/dev/rel/RELEAS~1")
        else
          assert app_dir == Path.join(root, "lib/release_test-0.1.0")
          assert release_root == root
          assert root_dir == root
        end
      end)
    end)
  end

  test "assembles a custom release" do
    in_fixture("release_test", fn ->
      config = [releases: [demo: [include_erts: false, cookie: "abcdefghijk"]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        root = Path.absname("demo")

        Mix.Task.run("release", ["demo", "--path", "demo", "--version", "0.2.0", "--quiet"])
        refute_received {:mix_shell, :info, ["* assembling " <> _]}
        refute_received {:mix_shell, :info, ["\nRelease created " <> _]}

        # Assert structure
        assert root |> Path.join("bin/demo") |> File.exists?()
        refute root |> Path.join("erts-#{@erts_version}") |> File.exists?()
        assert root |> Path.join("lib/release_test-0.1.0/ebin") |> File.exists?()
        assert root |> Path.join("lib/release_test-0.1.0/priv/hello") |> File.exists?()
        assert root |> Path.join("releases/COOKIE") |> File.exists?()
        assert root |> Path.join("releases/start_erl.data") |> File.exists?()
        assert root |> Path.join("releases/0.2.0/demo.rel") |> File.exists?()
        assert root |> Path.join("releases/0.2.0/sys.config") |> File.exists?()
        assert root |> Path.join("releases/0.2.0/vm.args") |> File.exists?()

        # Assert runtime
        assert System.cmd(Path.join(root, "bin/start"), []) == {"", 0}

        assert %{
                 app_dir: app_dir,
                 cookie_env: "abcdefghijk",
                 cookie_node: :abcdefghijk,
                 node: :"demo@127.0.0.1",
                 protocols_consolidated?: true,
                 release_name: "demo",
                 release_node: "demo@127.0.0.1",
                 release_root: release_root,
                 release_vsn: "0.2.0",
                 root_dir: root_dir,
                 static_config: :was_set
               } = Code.eval_file("RELEASE_BOOTED") |> elem(0)

        if match?({:win32, _}, :os.type()) do
          assert String.ends_with?(app_dir, "demo/lib/release_test-0.1.0")
          assert String.ends_with?(release_root, "demo")
        else
          assert app_dir == Path.join(root, "lib/release_test-0.1.0")
          assert release_root == root
        end

        assert root_dir == :code.root_dir() |> to_string()
      end)
    end)
  end

  test "executes rpc instructions" do
    in_fixture("release_test", fn ->
      config = [releases: [permanent1: [include_erts: false]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        root = Path.absname("_build/dev/rel/permanent1")
        Mix.Task.run("release")

        task = Task.async(fn -> System.cmd(Path.join(root, "bin/start"), []) end)
        wait_until_evaled("RELEASE_BOOTED")

        assert {pid, 0} = System.cmd(Path.join(root, "bin/permanent1"), ["pid"])
        assert pid != "\n"

        assert System.cmd(Path.join(root, "bin/permanent1"), ["rpc", "ReleaseTest.hello_world"]) ==
                 {"hello world\n", 0}

        assert System.cmd(Path.join(root, "bin/permanent1"), ["stop"]) ==
                 {"", 0}

        assert {_, 0} = Task.await(task, :infinity)
      end)
    end)
  end

  @tag :unix
  test "runs in daemon mode" do
    in_fixture("release_test", fn ->
      config = [releases: [permanent2: [include_erts: false, cookie: "abcdefghij"]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        root = Path.absname("_build/dev/rel/permanent2")
        Mix.Task.run("release")

        assert System.cmd(Path.join(root, "bin/permanent2"), ["daemon", "iex"]) == {"", 0}

        assert wait_until_evaled("RELEASE_BOOTED") == %{
                 app_dir: Path.join(root, "lib/release_test-0.1.0"),
                 cookie_env: "abcdefghij",
                 cookie_node: :abcdefghij,
                 node: :"permanent2@127.0.0.1",
                 protocols_consolidated?: true,
                 release_name: "permanent2",
                 release_node: "permanent2@127.0.0.1",
                 release_root: root,
                 release_vsn: "0.1.0",
                 root_dir: :code.root_dir() |> to_string(),
                 static_config: :was_set
               }

        assert wait_until(fn ->
                 File.read!(Path.join(root, "tmp/log/erlang.log.1")) =~
                   "iex(permanent2@127.0.0.1)1> "
               end)

        assert System.cmd(Path.join(root, "bin/permanent2"), ["rpc", "ReleaseTest.hello_world"]) ==
                 {"hello world\n", 0}

        assert System.cmd(Path.join(root, "bin/permanent2"), ["stop"]) ==
                 {"", 0}
      end)
    end)
  end

  test "requires confirmation if release already exists unless forcing" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        Mix.Task.rerun("release")
        assert_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

        send(self(), {:mix_shell_input, :yes?, false})
        Mix.Task.rerun("release")
        refute_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

        assert_received {:mix_shell, :yes?,
                         ["Release release_test-0.1.0 already exists. Overwrite?"]}

        Mix.Task.rerun("release", ["--force"])
        assert_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}
      end)
    end)
  end

  test "requires a matching name" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        assert_raise Mix.Error, ~r"Unknown release :unknown", fn ->
          Mix.Task.run("release", ["unknown"])
        end
      end)
    end)
  end

  defp wait_until_evaled(file) do
    wait_until(fn -> File.exists?(file) && Code.eval_file("RELEASE_BOOTED") |> elem(0) end)
  end

  defp wait_until(fun) do
    if value = fun.() do
      value
    else
      Process.sleep(10)
      wait_until(fun)
    end
  end
end
