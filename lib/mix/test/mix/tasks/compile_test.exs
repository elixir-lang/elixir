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
  after
    purge [A, B, C]
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
    purge [A, B, C]
    Mix.Project.pop
  end

  test "compile a project with multiple compilers" do
    in_fixture "compile_multi", fn ->
      Mix.Tasks.Compile.run ["--force"]

      assert_received { :mix_shell, :info, ["Compiled src/yecc_ok.yrl"] }
      assert File.regular?("src/yecc_ok.erl")

      assert_received { :mix_shell, :info, ["Compiled src/leex_ok.xrl"] }
      assert File.regular?("src/leex_ok.erl")

      assert_received { :mix_shell, :info, ["Compiled src/b.erl"] }
      assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }
      assert File.regular?("ebin/b.beam")
      assert File.regular?("ebin/c.beam")

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert File.regular?("ebin/Elixir.A.beam")
    end
  after
    purge [A, B, C]
  end

  test "compile a project with multiple compilers and a syntax error in a yecc file" do
    in_fixture "compile_multi_broken_yecc", fn ->
      import ExUnit.CaptureIO

      assert_raise CompileError, fn ->
        capture_io fn -> Mix.Tasks.Compile.run ["--force"] end
      end

      refute File.regular?("src/yecc_broken.erl")

      refute File.regular?("src/leex_ok.erl")

      refute File.regular?("ebin/b.beam")
      refute File.regular?("ebin/c.beam")

      refute File.regular?("ebin/Elixir.A.beam")
    end
  after
    purge [A, B, C]
  end

  test "compile a project with multiple compilers and a syntax error in a leex file" do
    in_fixture "compile_multi_broken_leex", fn ->
      import ExUnit.CaptureIO

      assert_raise CompileError, fn ->
        capture_io fn -> Mix.Tasks.Compile.run ["--force"] end
      end

      assert_received { :mix_shell, :info, ["Compiled src/yecc_ok.yrl"] }
      assert File.regular?("src/yecc_ok.erl")

      refute File.regular?("src/leex_broken.erl")

      refute File.regular?("ebin/b.beam")
      refute File.regular?("ebin/c.beam")

      refute File.regular?("ebin/Elixir.A.beam")
    end
  after
    purge [A, B, C]
  end

  test "compile a project with multiple compilers and a syntax error in a erlang file" do
    in_fixture "compile_multi_broken_erlang", fn ->
      import ExUnit.CaptureIO

      assert_raise CompileError, fn ->
        capture_io fn -> Mix.Tasks.Compile.run ["--force"] end
      end

      assert_received { :mix_shell, :info, ["Compiled src/yecc_ok.yrl"] }
      assert File.regular?("src/yecc_ok.erl")

      assert_received { :mix_shell, :info, ["Compiled src/leex_ok.xrl"] }
      assert File.regular?("src/leex_ok.erl")

      refute File.regular?("ebin/b.beam")

      assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }
      assert File.regular?("ebin/c.beam")

      refute File.regular?("ebin/Elixir.A.beam")
    end
  after
    purge [A, B, C]
  end

  test "compile a project with multiple compilers and a syntax error in an elixir file" do
    in_fixture "compile_multi_broken_elixir", fn ->
      import ExUnit.CaptureIO

      assert_raise SyntaxError, fn ->
        capture_io fn -> Mix.Tasks.Compile.run ["--force"] end
      end

      assert_received { :mix_shell, :info, ["Compiled src/yecc_ok.yrl"] }
      assert File.regular?("src/yecc_ok.erl")

      assert_received { :mix_shell, :info, ["Compiled src/leex_ok.xrl"] }
      assert File.regular?("src/leex_ok.erl")

      assert_received { :mix_shell, :info, ["Compiled src/b.erl"] }
      assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }
      assert File.regular?("ebin/b.beam")
      assert File.regular?("ebin/c.beam")
    end
  after
    purge [A, B, C]
  end

  # test "compile a project that fails with multiple compilers" do
  #   in_fixture "compile_multi_broken", fn ->
  #     Mix.Tasks.Compile.run []

  #     refute File.regular?("src/test_broken.erl")

  #     # assert_received { :mix_shell, :info, ["Compiled src/b.erl"] }
  #     # assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }

  #     refute File.regular?("ebin/b.beam")
  #     refute File.regular?("ebin/c.beam")

  #     # assert_received { :mix_shell, :info, ["Compiled src/test_ok.yrl"] }
  #     # assert File.regular?("src/test_ok.erl")
  #   end
  # after
  #   purge [B, C]
  # end

end
