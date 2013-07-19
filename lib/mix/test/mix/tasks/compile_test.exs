Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  test "mix compile --list without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run ["--list"]
      assert_received { :mix_shell, :info, ["\nEnabled compilers: yecc, leex, erlang, elixir"] }
    end
  end

  defmodule CustomApp do
    def project do
      [app: :custom_app, version: "0.1.0"]
    end
  end

  defmodule CustomCompilers do
    def project do
      [compilers: [:elixir, :app, :custom]]
    end
  end

  test "mix compile --list with mixfile" do
    Mix.Project.push CustomApp
    Mix.Tasks.Compile.run ["--list"]
    assert_received { :mix_shell, :info, ["\nEnabled compilers: yecc, leex, erlang, elixir, app"] }
    assert_received { :mix_shell, :info, ["mix compile.elixir # " <> _] }
  after
    Mix.Project.pop
  end

  test "mix compile --list with custom mixfile" do
    Mix.Project.push CustomCompilers
    Mix.Tasks.Compile.run ["--list"]
    assert_received { :mix_shell, :info, ["\nEnabled compilers: elixir, app, custom"] }
  after
    Mix.Project.pop
  end

  test "compile is no-op on empty project" do
    in_fixture "beams", fn ->
      Mix.Tasks.Compile.run []
      refute File.exists?("ebin")
    end
  end

  test "compile a project without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      assert File.regular?("ebin/Elixir.A.beam")
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  end

  test "compile a project with mixfile" do
    Mix.Project.push CustomApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      assert File.regular?("ebin/Elixir.A.beam")
      assert File.regular?("ebin/custom_app.app")
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Generated custom_app.app"] }
    end
  after
    Mix.Project.pop
  end

  test "compile a project with multiple compilers and a syntax error in an erlang file" do
    Mix.Project.push CustomApp

    in_fixture "no_mixfile", fn ->
      import ExUnit.CaptureIO

      File.mkdir!("src")
      File.write!("src/a.erl", """)
      -module(b).
      def b(), do: b
      """
      assert File.regular?("src/a.erl")

      assert_raise CompileError, fn ->
        capture_io fn -> Mix.Tasks.Compile.run ["--force"] end
      end

      refute File.regular?("ebin/Elixir.A.beam")
      refute File.regular?("ebin/Elixir.B.beam")
      refute File.regular?("ebin/Elixir.C.beam")
    end
  after
    purge [A, B, C]
    Mix.Project.pop
  end

end
