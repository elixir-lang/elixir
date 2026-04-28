# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Deps.CleanTest do
  use MixTest.Case

  # Project with both a Mix dep (parser_dep) and a rebar3 dep (rebar_dep).
  # Both use path: so they are available without network access in tests.
  defmodule BothDeps do
    def project do
      [
        app: :clean_with_yecc,
        version: "0.1.0",
        deps: [
          {:parser_dep, path: "deps/parser_dep"},
          {:rebar_dep, path: "deps/rebar_dep"}
        ]
      ]
    end
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

  # Writes a compile.yecc manifest exactly as the real compiler does:
  # relative paths from the dep's working directory, version tag 1.
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

  defp touch_build(app) do
    dir = "_build/dev/lib/#{app}"
    File.mkdir_p!(Path.join(dir, "ebin"))

    File.write!(
      Path.join([dir, "ebin", "#{app}.app"]),
      "{application, #{app}, [{vsn,\"0.1.0\"}]}."
    )

    dir
  end

  test "removes yecc-generated .erl, preserves .yrl source" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      yecc_manifest(build_dir, ["src/parser.erl"])

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      assert File.exists?("deps/parser_dep/src/parser.yrl")
    end)
  end

  test "removes leex-generated .erl, preserves .xrl source" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      leex_manifest(build_dir, ["src/lexer.erl"])

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      refute File.exists?("deps/parser_dep/src/lexer.erl")
      assert File.exists?("deps/parser_dep/src/lexer.xrl")
    end)
  end

  test "hand-written .erl not tracked in any manifest is never deleted" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      # manifest only tracks parser.erl — handwritten.erl is NOT listed
      yecc_manifest(build_dir, ["src/parser.erl"])

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      assert File.exists?("deps/parser_dep/src/handwritten.erl")
    end)
  end

  test "cleans both yecc and leex generated files in one pass" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      yecc_manifest(build_dir, ["src/parser.erl"])
      leex_manifest(build_dir, ["src/lexer.erl"])

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      refute File.exists?("deps/parser_dep/src/lexer.erl")
      assert File.exists?("deps/parser_dep/src/parser.yrl")
      assert File.exists?("deps/parser_dep/src/lexer.xrl")
    end)
  end

  test "generated .erl in non-standard source directory is deleted" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      # dep uses erlc_paths: ["grammar"] instead of the default "src"
      File.mkdir_p!("deps/parser_dep/grammar")
      File.write!("deps/parser_dep/grammar/dialect.erl", "-module(dialect).")
      yecc_manifest(build_dir, ["grammar/dialect.erl"])

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      refute File.exists?("deps/parser_dep/grammar/dialect.erl")
    end)
  end

  test "--build still removes generated source files, leaves dep directory" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      yecc_manifest(build_dir, ["src/parser.erl"])

      Mix.Tasks.Deps.Clean.run(["--build", "parser_dep"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      # --build keeps the dep source directory intact
      assert File.dir?("deps/parser_dep")
      assert File.exists?("deps/parser_dep/src/parser.yrl")
    end)
  end

  test "--all --build removes generated files from every dep" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(BothDeps)
      parser_build = touch_build(:parser_dep)
      rebar_build = touch_build(:rebar_dep)
      yecc_manifest(parser_build, ["src/parser.erl"])
      yecc_manifest(rebar_build, ["src/rb_parser.erl"])

      Mix.Tasks.Deps.Clean.run(["--all", "--build"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      refute File.exists?("deps/rebar_dep/src/rb_parser.erl")
      assert File.exists?("deps/parser_dep/src/parser.yrl")
      assert File.exists?("deps/rebar_dep/src/rb_parser.yrl")
    end)
  end

  test "manifests in multiple envs point to same .erl — Enum.uniq deduplicates before deletion" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)

      for env <- ["dev", "test"] do
        dir = "_build/#{env}/lib/parser_dep"
        File.mkdir_p!(Path.join(dir, "ebin"))
        yecc_manifest(dir, ["src/parser.erl"])
      end

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
    end)
  end

  test "--only scopes manifest reading to the specified environment" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)

      dev_dir = "_build/dev/lib/parser_dep"
      test_dir = "_build/test/lib/parser_dep"

      for dir <- [dev_dir, test_dir] do
        File.mkdir_p!(Path.join(dir, "ebin"))
      end

      # dev tracks src/parser.erl; test tracks src/lexer.erl
      yecc_manifest(dev_dir, ["src/parser.erl"])
      leex_manifest(test_dir, ["src/lexer.erl"])

      Mix.Tasks.Deps.Clean.run(["parser_dep", "--only", "dev"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      # test manifest was not processed
      assert File.exists?("deps/parser_dep/src/lexer.erl")
    end)
  end

  test "rebar3 dep: deletes .erl files inferred from co-located .yrl/.xrl when no manifests exist" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(BothDeps)
      # No build dir created for rebar_dep — rebar3 never writes Mix manifests.
      # The fixture has src/rb_parser.yrl and src/rb_parser.erl pre-populated.

      Mix.Tasks.Deps.Clean.run(["rebar_dep"])

      refute File.exists?("deps/rebar_dep/src/rb_parser.erl")
      assert File.exists?("deps/rebar_dep/src/rb_parser.yrl")
    end)
  end

  test "dep with no manifests removes .erl files inferred from .yrl/.xrl sources" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      # no _build at all — dep was never compiled by Mix
      # parser.erl/lexer.erl are inferred from parser.yrl/lexer.xrl and deleted

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      refute File.exists?("deps/parser_dep/src/parser.erl")
      refute File.exists?("deps/parser_dep/src/lexer.erl")
      assert File.exists?("deps/parser_dep/src/parser.yrl")
      assert File.exists?("deps/parser_dep/src/lexer.xrl")
      # hand-written .erl with no grammar counterpart is preserved
      assert File.exists?("deps/parser_dep/src/handwritten.erl")
    end)
  end

  test "idempotent: running deps.clean twice does not error" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      yecc_manifest(build_dir, ["src/parser.erl"])

      Mix.Tasks.Deps.Clean.run(["parser_dep"])
      # parser.erl is now gone and _build deleted; second run must not raise
      Mix.Tasks.Deps.Clean.run(["parser_dep"])
      # the second run warns that the dep is not in the build directory
      assert_received {:mix_shell, :error, ["warning: the dependency parser_dep " <> _]}
    end)
  end

  test "does not delete a file whose expanded path escapes the dep source root" do
    in_fixture("clean_with_yecc", fn ->
      Mix.Project.push(ParserOnly)
      build_dir = touch_build(:parser_dep)
      # A hypothetical manifest entry with path traversal
      write_manifest(build_dir, "compile.yecc", ["../../some_other_file.erl"])
      File.write!("some_other_file.erl", "not to be deleted")

      Mix.Tasks.Deps.Clean.run(["parser_dep"])

      assert File.exists?("some_other_file.erl")
    end)
  end
end
