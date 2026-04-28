# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.CleanTest do
  use MixTest.Case

  defmodule Sample do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          {:ok, "0.1.0", path: "deps/ok"},
          {:unknown, "0.1.0", git: "deps/unknown"}
        ]
      ]
    end
  end

  test "cleans the application build" do
    in_fixture("deps_status", fn ->
      Mix.Project.push(Sample)

      File.mkdir_p!("_build/dev/lib/sample/consolidated")
      File.mkdir_p!("_build/dev/lib/sample")
      File.mkdir_p!("_build/test/lib/sample")
      File.mkdir_p!("_build/dev/lib/ok")

      Mix.Tasks.Clean.run([])
      refute File.exists?("_build/dev/lib/sample/consolidated")
      refute File.exists?("_build/dev/lib/sample")
      refute File.exists?("_build/test/lib/sample")
      assert File.exists?("_build/dev/lib/ok")
    end)
  end

  test "cleans dependencies build" do
    in_fixture("deps_status", fn ->
      Mix.Project.push(Sample)

      File.mkdir_p!("_build/dev/lib/ok")
      File.mkdir_p!("_build/test/lib/ok")

      Mix.Tasks.Clean.run(["--deps", "--only", "dev"])
      refute File.exists?("_build/dev")
      assert File.exists?("_build/test")

      Mix.Tasks.Clean.run(["--deps"])
      refute File.exists?("_build/test")
    end)
  end

  defmodule ParserOnly do
    def project do
      [
        app: :clean_with_yecc,
        version: "0.1.0",
        deps: [{:parser_dep, path: "deps/parser_dep"}]
      ]
    end
  end

  defp yecc_manifest(build_dir, relative_paths) do
    write_manifest(build_dir, "compile.yecc", relative_paths)
  end

  defp leex_manifest(build_dir, relative_paths) do
    write_manifest(build_dir, "compile.leex", relative_paths)
  end

  defp write_manifest(build_dir, name, relative_paths) do
    mix_dir = Path.join(build_dir, ".mix")
    File.mkdir_p!(mix_dir)
    entries = Enum.map(relative_paths, &{&1, []})
    File.write!(Path.join(mix_dir, name), :erlang.term_to_binary({1, entries}))
  end

  test "--deps removes yecc-generated .erl files from dep source" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)

      build_dir = "_build/dev/lib/parser_dep"
      File.mkdir_p!(Path.join(build_dir, "ebin"))
      yecc_manifest(build_dir, ["src/parser.erl"])

      Mix.Tasks.Clean.run(["--deps"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      assert File.exists?("deps/parser_dep/src/parser.yrl")
      refute File.exists?("_build/dev")
    end)
  end

  test "without --deps does not touch dep generated files" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)

      Mix.Tasks.Clean.run([])

      assert File.exists?("deps/parser_dep/src/parser.erl")
    end)
  end

  test "--deps --only dev scopes generated-file cleanup to dev environment" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)

      dev_dir = "_build/dev/lib/parser_dep"
      test_dir = "_build/test/lib/parser_dep"

      File.mkdir_p!(Path.join(dev_dir, "ebin"))
      File.mkdir_p!(Path.join(test_dir, "ebin"))

      # dev tracks parser.erl; test tracks lexer.erl
      yecc_manifest(dev_dir, ["src/parser.erl"])
      leex_manifest(test_dir, ["src/lexer.erl"])

      Mix.Tasks.Clean.run(["--deps", "--only", "dev"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      # test manifest was not processed
      assert File.exists?("deps/parser_dep/src/lexer.erl")
      refute File.exists?("_build/dev")
      assert File.exists?("_build/test")
    end)
  end

  test "invokes compiler hook defined in project" do
    Mix.ProjectStack.post_config(compilers: Mix.compilers() ++ [:testc])

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/testc.ex", """
      defmodule Mix.Tasks.Compile.Testc do
        use Mix.Task.Compiler

        @impl true
        def run(_args) do
          Mix.shell().info("Compiling...")
          :ok
        end

        @impl true
        def clean do
          Mix.shell().info("Cleaning...")
          :ok
        end
      end
      """)

      Mix.Task.run("compile")
      assert_received {:mix_shell, :info, ["Compiling..."]}
      purge([Mix.Tasks.Compile.Testc])

      Mix.Task.run("clean")
      assert_received {:mix_shell, :info, ["Cleaning..."]}
    end)
  end
end
