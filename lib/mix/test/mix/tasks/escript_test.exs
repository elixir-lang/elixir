Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.EscriptTest do
  use MixTest.Case

  defmodule Escript do
    def project do
      [
        app: :escript_test,
        version: "0.0.1",
        escript: [main_module: EscriptTest, name: "escript_test", embed_elixir: true]
      ]
    end
  end

  defmodule EscriptErlangWithDeps do
    def project do
      [
        app: :escript_test,
        version: "0.0.1",
        language: :erlang,
        escript: [main_module: :escript_test],
        deps: [{:ok, path: fixture_path("deps_status/deps/ok")}]
      ]
    end

    def application do
      [applications: [], extra_applications: [:crypto, elixir: :optional]]
    end
  end

  test "generate escript" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript)

      Mix.Tasks.Escript.Build.run([])
      assert_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escript_test"]) == {"TEST\n", 0}
      assert count_abstract_code("escript_test") == 0

      # Consolidates protocols
      assert System.cmd("escript", ["escript_test", "--protocol", "Enumerable"]) == {"true\n", 0}

      # Each app has a distinct, valid path
      assert System.cmd("escript", ["escript_test", "--app-paths"]) == {"{true, true, true}\n", 0}

      # Does not include priv by default
      assert System.cmd("escript", ["escript_test", "--list-priv", "escript_test"]) ==
               {":error\n", 0}
    end)
  end

  test "generate escript with --no-compile option" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript)

      Mix.Tasks.Compile.run([])
      purge([EscriptTest])

      Mix.Tasks.Escript.Build.run(["--no-compile"])
      assert_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}
    end)
  end

  test "generate escript with compile config" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript)

      File.mkdir_p!("config")

      File.write!("config/config.exs", ~S"""
      import Config
      config :foobar, :value, "COMPILE #{config_env()} TARGET #{config_target()}"
      """)

      Mix.Tasks.Escript.Build.run([])
      assert_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escript_test"]) == {"COMPILE dev TARGET host\n", 0}
    end)
  end

  test "generate escript with runtime config" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript)

      File.mkdir_p!("config")

      File.write!("config/config.exs", """
      [foobar: [value: "OLD", nesting: [config: true, runtime: false]]]
      """)

      File.write!("config/runtime.exs", ~S"""
      import Config
      config :foobar, :value, "RUNTIME #{config_env()} TARGET #{config_target()} ARGV #{System.argv()}"
      config :foobar, :nesting, runtime: true
      """)

      Mix.Tasks.Escript.Build.run([])
      assert_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}

      assert System.cmd("escript", ["escript_test", "--foo", "--bar"]) ==
               {"RUNTIME dev TARGET host ARGV --foo--bar\n", 0}

      assert System.cmd("escript", ["escript_test", "--nesting"]) ==
               {"[config: true, runtime: true]\n", 0}
    end)
  end

  test "generate escript with debug information" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript, escript: [main_module: EscriptTest, strip_beams: false])

      Mix.Tasks.Escript.Build.run([])

      msg = "Generated escript escript_test with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^msg]}

      assert System.cmd("escript", ["escript_test"]) == {"TEST\n", 0}
      assert count_abstract_code("escript_test") > 0
    end)
  end

  defp count_abstract_code(escript_filename) do
    escript_filename
    |> read_beams()
    |> Enum.count(fn {_, beam} -> get_abstract_code(beam) end)
  end

  defp read_beams(escript_filename) do
    # :zip.unzip/2 cannot unzip an escript unless we remove the escript header
    zip_data = remove_escript_header(File.read!(escript_filename))
    {:ok, tuples} = :zip.unzip(zip_data, [:memory])

    for {filename, beam} <- tuples, Path.extname(filename) == ".beam" do
      {filename, beam}
    end
  end

  defp remove_escript_header(escript_data) do
    {offset, _length} = :binary.match(escript_data, "\nPK")
    zip_start = offset + 1
    binary_part(escript_data, zip_start, byte_size(escript_data) - zip_start)
  end

  defp get_abstract_code(beam) do
    case :beam_lib.chunks(beam, [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} -> abstract_code
      _ -> nil
    end
  end

  test "generate escript with path" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript,
        escript: [
          app: nil,
          embed_elixir: true,
          main_module: EscriptTest,
          path: Path.join("ebin", "escript_test")
        ]
      )

      Mix.Tasks.Escript.Build.run([])

      message = "Generated escript ebin/escript_test with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      assert System.cmd("escript", ["ebin/escript_test"]) == {"TEST\n", 0}
    end)
  end

  test "generate escript with deps" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript,
        escript: [main_module: EscriptTest],
        deps: [{:ok, path: fixture_path("deps_status/deps/ok")}]
      )

      Mix.Tasks.Escript.Build.run([])

      message = "Generated escript escript_test with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      assert System.cmd("escript", ["escript_test"]) == {"TEST\n", 0}

      # Does not include priv for deps by default
      assert System.cmd("escript", ["escript_test", "--list-priv", "ok"]) == {":error\n", 0}
    end)
  after
    purge([Ok.MixProject])
  end

  test "generate escript with Erlang and deps" do
    in_fixture("escript_test", fn ->
      push_project_with_config(EscriptErlangWithDeps)

      Mix.Tasks.Escript.Build.run([])

      message = "Generated escript escript_test with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      assert System.cmd("escript", ["escript_test", "arg1", "arg2"]) ==
               {~s(["arg1","arg2"]), 0}
    end)
  after
    purge([Ok.MixProject])
  end

  test "generate escript with Erlang main module" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript, escript: [main_module: :escript_test])

      Mix.Tasks.Escript.Build.run([])

      message = "Generated escript escript_test with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      assert System.cmd("escript", ["escript_test", "arg1", "arg2"]) ==
               {~s([<<"arg1">>,<<"arg2">>]), 0}
    end)
  after
    purge([Ok.MixProject])
  end

  test "generate escript with priv" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript,
        escript: [main_module: EscriptTest, include_priv_for: [:escript_test, :ok]],
        deps: [{:ok, path: fixture_path("deps_status/deps/ok")}]
      )

      Mix.Tasks.Escript.Build.run([])

      message = "Generated escript escript_test with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      assert System.cmd("escript", ["escript_test", "--list-priv", "escript_test"]) ==
               {~s/{:ok, [~c"hello"]}\n/, 0}

      assert System.cmd("escript", ["escript_test", "--list-priv", "ok"]) ==
               {~s/{:ok, [~c"sample"]}\n/, 0}
    end)
  end

  test "escript install and uninstall" do
    File.rm_rf!(tmp_path(".mix/escripts"))

    in_fixture("escript_test", fn ->
      push_project_with_config(Escript)

      # check that no escripts are installed
      Mix.Tasks.Escript.run([])
      assert_received {:mix_shell, :info, ["No escripts currently installed."]}

      # build and install our escript
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Escript.Install.run([])

      # check that it shows in the list
      Mix.Tasks.Escript.run([])
      assert_received {:mix_shell, :info, ["* escript_test"]}
      refute_received {:mix_shell, :info, ["* escript_test.bat"]}

      # check uninstall confirmation
      send(self(), {:mix_shell_input, :yes?, false})
      Mix.Tasks.Escript.Uninstall.run(["escript_test"])
      assert File.regular?(tmp_path(".mix/escripts/escript_test"))

      # uninstall the escript
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Escript.Uninstall.run(["escript_test"])
      refute File.regular?(tmp_path(".mix/escripts/escript_test"))
      refute File.regular?(tmp_path(".mix/escripts/escript_test.bat"))

      # check that no escripts remain
      Mix.Tasks.Escript.run([])
      assert_received {:mix_shell, :info, ["No escripts currently installed."]}
    end)
  end

  test "escript install and uninstall --force" do
    File.rm_rf!(tmp_path(".mix/escripts"))

    in_fixture("escript_test", fn ->
      push_project_with_config(Escript)

      Mix.Tasks.Escript.Install.run(["--force"])

      # check that it shows in the list
      Mix.Tasks.Escript.run([])
      assert_received {:mix_shell, :info, ["* escript_test"]}
      refute_received {:mix_shell, :info, ["* escript_test.bat"]}

      # uninstall the escript
      Mix.Tasks.Escript.Uninstall.run(["escript_test", "--force"])

      # check that no escripts remain
      Mix.Tasks.Escript.run([])
      assert_received {:mix_shell, :info, ["No escripts currently installed."]}
    end)
  end

  test "escript invalid install" do
    # Install our escript
    send(self(), {:mix_shell_input, :yes?, true})
    message = "The given path does not point to an escript, installation aborted"

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Escript.Install.run([__ENV__.file])
    end
  end

  test "escript invalid main module" do
    in_fixture("escript_test", fn ->
      push_project_with_config(Escript, escript: [main_module: BogusEscriptTest])

      message =
        "Could not generate escript, module Elixir.BogusEscriptTest defined as :main_module could not be loaded"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Escript.Build.run([])
      end
    end)
  end

  test "escript.install from Git" do
    in_fixture("git_repo", fn ->
      File.mkdir_p!("config")

      File.write!("config/config.exs", """
      import Config
      config :git_repo, :escript_config, true
      """)

      File.write!("lib/git_repo.ex", """
      require Application
      true = Application.compile_env!(:git_repo, :escript_config)

      defmodule GitRepo do
        def main(_argv) do
          IO.puts("TEST")
        end
      end
      """)

      File.write!("mix.exs", """
      defmodule GitRepo.MixProject do
        use Mix.Project

        def project do
          [app: :git_repo, version: "0.1.0", escript: [main_module: GitRepo]]
        end
      end
      """)

      System.cmd("git", ~w[add .])
      System.cmd("git", ~w[commit -m "ok"])

      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Escript.Install.run(["git", File.cwd!()])
      assert_received {:mix_shell, :info, ["Generated escript git_repo with MIX_ENV=prod"]}

      escript_path = Path.join([tmp_path(".mix"), "escripts", "git_repo"])
      assert System.cmd("escript", [escript_path]) == {"TEST\n", 0}
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  defp push_project_with_config(module, config \\ []) do
    Mix.ProjectStack.post_config(config)
    Mix.Project.push(module)
  end
end
