Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.ReleaseTest do
  use MixTest.Case

  @erts_version :erlang.system_info(:version)
  @hostname :inet_db.gethostname()

  defmacrop release_node(name), do: :"#{name}@#{@hostname}"

  describe "customize" do
    test "rel with eex" do
      in_fixture("release_test", fn ->
        Mix.Project.in_project(:release_test, ".", fn _ ->
          File.mkdir_p!("rel")

          for file <- ~w(rel/vm.args.eex rel/env.sh.eex rel/env.bat.eex) do
            File.write!(file, """
            #{file} FOR <%= @release.name %>
            """)
          end

          root = Path.absname("_build/dev/rel/release_test")
          Mix.Task.run("release")
          assert_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

          assert root |> Path.join("releases/0.1.0/env.sh") |> File.read!() ==
                   "rel/env.sh.eex FOR release_test\n"

          assert root |> Path.join("releases/0.1.0/env.bat") |> File.read!() ==
                   "rel/env.bat.eex FOR release_test\n"

          assert root |> Path.join("releases/0.1.0/vm.args") |> File.read!() ==
                   "rel/vm.args.eex FOR release_test\n"
        end)
      end)
    end

    test "steps" do
      in_fixture("release_test", fn ->
        last_step = fn release ->
          send(self(), {:last_step, release})
          release
        end

        first_step = fn release ->
          send(self(), {:first_step, release})
          update_in(release.steps, &(&1 ++ [last_step]))
        end

        config = [releases: [demo: [steps: [first_step, :assemble]]]]

        Mix.Project.in_project(:release_test, ".", config, fn _ ->
          Mix.Task.run("release")
          assert_received {:mix_shell, :info, ["* assembling demo-0.1.0 on MIX_ENV=dev"]}

          # Discard info messages from inbox for upcoming assertions
          Mix.shell().flush(& &1)

          {:messages,
           [
             {:first_step, %Mix.Release{steps: [:assemble]}},
             {:last_step, %Mix.Release{steps: []}}
           ]} = Process.info(self(), :messages)
        end)
      end)
    end

    test "include_executables_for" do
      in_fixture("release_test", fn ->
        config = [releases: [release_test: [include_executables_for: []]]]

        Mix.Project.in_project(:release_test, ".", config, fn _ ->
          root = Path.absname("_build/dev/rel/release_test")
          Mix.Task.run("release")
          assert_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

          refute root |> Path.join("bin/start") |> File.exists?()
          refute root |> Path.join("bin/start.bat") |> File.exists?()
          refute root |> Path.join("releases/0.1.0/elixir") |> File.exists?()
          refute root |> Path.join("releases/0.1.0/elixir.bat") |> File.exists?()
          refute root |> Path.join("releases/0.1.0/iex") |> File.exists?()
          refute root |> Path.join("releases/0.1.0/iex.bat") |> File.exists?()
        end)
      end)
    end
  end

  test "assembles a bootable release with ERTS" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        root = Path.absname("_build/dev/rel/release_test")

        # Assert command
        Mix.Task.run("release")
        assert_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

        assert_received {:mix_shell, :info,
                         ["\nRelease created at _build/dev/rel/release_test!" <> _]}

        assert_received {:mix_shell, :info, ["* skipping runtime configuration" <> _]}

        # Assert structure
        assert root |> Path.join("erts-#{@erts_version}") |> File.exists?()
        assert root |> Path.join("lib/release_test-0.1.0/ebin") |> File.exists?()
        assert root |> Path.join("lib/release_test-0.1.0/priv/hello") |> File.exists?()
        assert root |> Path.join("releases/COOKIE") |> File.exists?()
        assert root |> Path.join("releases/start_erl.data") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/release_test.rel") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/sys.config") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/env.sh") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/env.bat") |> File.exists?()
        assert root |> Path.join("releases/0.1.0/vm.args") |> File.exists?()

        assert root
               |> Path.join("releases/0.1.0/sys.config")
               |> File.read!() =~ "RUNTIME_CONFIG=false"

        assert root
               |> Path.join("lib/release_test-0.1.0/priv")
               |> File.read_link()
               |> elem(0) == :error

        cookie = File.read!(Path.join(root, "releases/COOKIE"))

        # Assert runtime
        open_port(Path.join(root, "bin/release_test"), ['start'])

        assert %{
                 app_dir: app_dir,
                 cookie_env: ^cookie,
                 mode: :embedded,
                 node: release_node("release_test"),
                 protocols_consolidated?: true,
                 release_name: "release_test",
                 release_node: "release_test",
                 release_root: release_root,
                 release_vsn: "0.1.0",
                 root_dir: root_dir,
                 static_config: {:ok, :was_set},
                 runtime_config: :error,
                 sys_config_env: sys_config_env,
                 sys_config_init: sys_config_init
               } = wait_until_evaled(Path.join(root, "RELEASE_BOOTED"))

        if match?({:win32, _}, :os.type()) do
          # `RELEAS~1` is the DOS path name (8 character) for the `release_test` directory
          assert app_dir =~ ~r"_build/dev/rel/(release_test|RELEAS~1)/lib/release_test-0\.1\.0$"
          assert release_root =~ ~r"_build\\dev\\rel\\(release_test|RELEAS~1)$"
          assert root_dir =~ ~r"_build/dev/rel/(release_test|RELEAS~1)$"
          assert String.ends_with?(sys_config_env, "releases\\0.1.0\\sys")
          assert String.ends_with?(sys_config_init, "releases\\0.1.0\\sys")
        else
          assert app_dir == Path.join(root, "lib/release_test-0.1.0")
          assert release_root == root
          assert root_dir == root
          assert sys_config_env == Path.join(root, "releases/0.1.0/sys")
          assert sys_config_init == Path.join(root, "releases/0.1.0/sys")
        end
      end)
    end)
  end

  test "assembles a bootable release with runtime configuration" do
    in_fixture("release_test", fn ->
      config = [releases: [runtime_config: []]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        File.write!("config/releases.exs", """
        import Config
        config :release_test, :runtime, :was_set
        """)

        root = Path.absname("_build/dev/rel/runtime_config")

        # Assert command
        Mix.Task.run("release", ["runtime_config"])
        assert_received {:mix_shell, :info, ["* assembling runtime_config-0.1.0 on MIX_ENV=dev"]}

        assert_received {:mix_shell, :info,
                         ["* using config/releases.exs to configure the release at runtime"]}

        # Assert structure
        assert root
               |> Path.join("releases/0.1.0/sys.config")
               |> File.read!() =~ "RUNTIME_CONFIG=true"

        # Assert runtime
        open_port(Path.join(root, "bin/runtime_config"), ['start'])

        assert %{
                 mode: :embedded,
                 node: release_node("runtime_config"),
                 protocols_consolidated?: true,
                 release_name: "runtime_config",
                 release_node: "runtime_config",
                 release_vsn: "0.1.0",
                 static_config: {:ok, :was_set},
                 runtime_config: {:ok, :was_set},
                 sys_config_env: sys_config_env,
                 sys_config_init: sys_config_init
               } = wait_until_evaled(Path.join(root, "RELEASE_BOOTED"))

        if match?({:win32, _}, :os.type()) do
          assert sys_config_env =~ "tmp\\runtime_config-0.1.0"
          assert sys_config_init =~ "tmp\\runtime_config-0.1.0"
        else
          assert sys_config_env =~ "tmp/runtime_config-0.1.0"
          assert sys_config_init =~ "tmp/runtime_config-0.1.0"
        end
      end)
    end)
  end

  test "assembles a release without ERTS and with custom options" do
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
        open_port(Path.join(root, "bin/demo"), ['start'])

        assert %{
                 app_dir: app_dir,
                 cookie_env: "abcdefghijk",
                 mode: :embedded,
                 node: release_node("demo"),
                 protocols_consolidated?: true,
                 release_name: "demo",
                 release_node: "demo",
                 release_root: release_root,
                 release_vsn: "0.2.0",
                 root_dir: root_dir,
                 static_config: {:ok, :was_set},
                 runtime_config: :error
               } = wait_until_evaled(Path.join(root, "RELEASE_BOOTED"))

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
        script = Path.join(root, "bin/permanent1")

        open_port(script, ['start'])
        wait_until_evaled(Path.join(root, "RELEASE_BOOTED"))
        assert System.cmd(script, ["rpc", "ReleaseTest.hello_world"]) == {"hello world\n", 0}
        assert System.cmd(script, ["stop"]) == {"", 0}

        assert {pid, 0} = System.cmd(script, ["pid"])
        assert pid != "\n"
      end)
    end)
  end

  test "runs eval and version commands" do
    # In some Windows setups (mostly with Docker), `System.cmd/3` fails because
    # the path to the command/executable and one or more arguments contain spaces.
    tmp_dir = Path.join(inspect(__MODULE__), "runs_eval_and_version_commands")

    in_fixture("release_test", tmp_dir, fn ->
      config = [releases: [eval: [include_erts: false, cookie: "abcdefghij"]]]

      Mix.Project.in_project(:release_test, ".", config, fn _ ->
        File.write!("config/releases.exs", """
        import Config
        config :release_test, :runtime, :was_set
        """)

        root = Path.absname("_build/dev/rel/eval")
        Mix.Task.run("release")

        script = Path.join(root, "bin/eval")
        {version, 0} = System.cmd(script, ["version"])
        assert String.trim_trailing(version) == "eval 0.1.0"
        refute File.exists?(Path.join(root, "RELEASE_BOOTED"))

        {hello_world, 0} = System.cmd(script, ["eval", "IO.puts :hello_world"])
        assert String.trim_trailing(hello_world) == "hello_world"
        refute File.exists?(Path.join(root, "RELEASE_BOOTED"))

        open_port(script, ['eval', 'Application.ensure_all_started(:release_test)'])

        assert %{
                 cookie_env: "abcdefghij",
                 mode: :interactive,
                 node: :nonode@nohost,
                 protocols_consolidated?: true,
                 release_name: "eval",
                 release_node: "eval",
                 release_root: root,
                 release_vsn: "0.1.0",
                 static_config: {:ok, :was_set},
                 runtime_config: {:ok, :was_set}
               } = wait_until_evaled(Path.join(root, "RELEASE_BOOTED"))
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

        script = Path.join(root, "bin/permanent2")
        open_port(script, ['daemon_iex'])

        assert wait_until_evaled(Path.join(root, "RELEASE_BOOTED")) == %{
                 app_dir: Path.join(root, "lib/release_test-0.1.0"),
                 cookie_env: "abcdefghij",
                 mode: :embedded,
                 node: :"permanent2@#{@hostname}",
                 protocols_consolidated?: true,
                 release_name: "permanent2",
                 release_node: "permanent2",
                 release_root: root,
                 release_vsn: "0.1.0",
                 root_dir: :code.root_dir() |> to_string(),
                 static_config: {:ok, :was_set},
                 runtime_config: :error,
                 sys_config_env: Path.join(root, "releases/0.1.0/sys"),
                 sys_config_init: Path.join(root, "releases/0.1.0/sys")
               }

        assert wait_until(fn ->
                 File.read!(Path.join(root, "tmp/log/erlang.log.1")) =~
                   "iex(permanent2@#{@hostname})1> "
               end)

        assert System.cmd(script, ["rpc", "ReleaseTest.hello_world"]) == {"hello world\n", 0}
        assert System.cmd(script, ["stop"]) == {"", 0}
      end)
    end)
  end

  test "requires confirmation if release already exists unless overwriting" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        Mix.Task.rerun("release")
        assert_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

        send(self(), {:mix_shell_input, :yes?, false})
        Mix.Task.rerun("release")
        refute_received {:mix_shell, :info, ["* assembling release_test-0.1.0 on MIX_ENV=dev"]}

        assert_received {:mix_shell, :yes?,
                         ["Release release_test-0.1.0 already exists. Overwrite?"]}

        Mix.Task.rerun("release", ["--overwrite"])
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

  defp open_port(command, args) do
    Port.open({:spawn_executable, to_charlist(command)}, [:hide, args: args])
  end

  defp wait_until_evaled(file) do
    wait_until(fn -> File.exists?(file) && Code.eval_file(file) |> elem(0) end)
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
