Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  test "mix compile --list without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run ["--list"]
      assert_received { :mix_shell, :info, ["\nEnabled compilers: elixir"] }
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
    assert_received { :mix_shell, :info, ["\nEnabled compilers: elixir, app"] }
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

  test "compile a project without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      assert File.regular?("ebin/Elixir-A.beam")
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  after
    purge [A, B, C]
  end

  test "compile only a specific file" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run ["-f", "lib/a.ex"]
      assert File.regular?("ebin/Elixir-A.beam")
      refute File.regular?("ebin/Elixir-B.beam")
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      refute_received { :mix_shell, :info, ["Compiled lib/b.ex"] }
    end
  after
    purge [A, B, C]
  end

  test "compile a project with mixfile" do
    Mix.Project.push CustomApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      assert File.regular?("ebin/Elixir-A.beam")
      assert File.regular?("ebin/custom_app.app")
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Generated custom_app.app"] }
    end
  after
    purge [A, B, C]
    Mix.Project.pop
  end
end