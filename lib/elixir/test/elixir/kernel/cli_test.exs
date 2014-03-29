Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.CLI.InitTest do
  use ExUnit.Case, async: true

  test "handles code on initialization" do
    assert elixir('-e "IO.puts [?3]"') == '3\n'

    result = elixir('-e "IO.puts inspect(System.argv)" #{fixture_path("init_sample.exs")} -o 1 2 3')
    assert result == '#{inspect ["-o", "1", "2", "3"]}\n3\n'
  end
end

defmodule Kernel.CLI.OptionParsingTest do
  use ExUnit.Case, async: true

  test "properly parses paths" do
    root = fixture_path("../../..") |> to_char_list
    list = elixir('-pa "#{root}/*" -pz "#{root}/lib/*" -e "IO.inspect(:code.get_path, limit: :infinity)"')
    { path, _ } = Code.eval_string list, []

    # pa
    assert Path.expand('ebin', root) in path
    assert Path.expand('lib', root) in path
    assert Path.expand('src', root) in path

    # pz
    assert Path.expand('lib/list', root) in path
  end
end

defmodule Kernel.CLI.AtExitTest do
  use ExUnit.Case, async: true

  test "invokes at_exit callbacks" do
    assert elixir(fixture_path("at_exit.exs") |> to_char_list) ==
      'goodbye cruel world with status 0\n'
  end
end

defmodule Kernel.CLI.ErrorTest do
  use ExUnit.Case, async: true

  test "properly format errors" do
    assert :string.str('** (throw) 1', elixir('-e "throw 1"')) == 0
    assert :string.str('** (ErlangError) erlang error: 1', elixir('-e "error 1"')) == 0

    # It does not catch exits with integers nor strings...
    assert elixir('-e "exit 1"') == ''
  end
end

defmodule Kernel.CLI.CompileTest do
  use ExUnit.Case, async: true

  test "compiles code" do
    fixture = fixture_path "compile_sample.ex"
    assert elixirc('#{fixture} -o #{tmp_path}') == ''
    assert File.regular?(tmp_path "Elixir.CompileSample.beam")
  after
    File.rm(tmp_path("Elixir.CompileSample.beam"))
  end

  test "compiles code with verbose mode" do
    fixture = fixture_path "compile_sample.ex"
    assert elixirc('#{fixture} -o #{tmp_path} --verbose') ==
      'Compiled #{fixture}\n'
    assert File.regular?(tmp_path "Elixir.CompileSample.beam")
  after
    File.rm(tmp_path("Elixir.CompileSample.beam"))
  end

  test "fails on missing patterns" do
    fixture = fixture_path "compile_sample.ex"
    output = elixirc('#{fixture} non_existing.ex -o #{tmp_path}')
    assert :string.str(output, 'non_existing.ex') > 0, "expected non_existing.ex to be mentionned"
    assert :string.str(output, 'compile_sample.ex') == 0, "expected compile_sample.ex to not be mentionned"
    refute File.exists?(tmp_path("Elixir.CompileSample.beam")) , "expected the sample to not be compiled"
  end
end

defmodule Kernel.CLI.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "compiles files solving dependencies" do
    fixtures = [fixture_path("parallel_compiler/bar.ex"), fixture_path("parallel_compiler/foo.ex")]
    assert capture_io(fn ->
      assert [Bar, Foo] = Kernel.ParallelCompiler.files fixtures
    end) =~ "message_from_foo"
  after
    Enum.map [Foo, Bar], fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  test "compiles files with structs solving dependencies" do
    fixtures = [fixture_path("parallel_struct/bar.ex"), fixture_path("parallel_struct/foo.ex")]
    assert [Bar, Foo] = Kernel.ParallelCompiler.files(fixtures) |> Enum.sort
  after
    Enum.map [Foo, Bar], fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  test "does not hang on missing dependencies" do
    fixtures = [fixture_path("parallel_compiler/bat.ex")]
    assert capture_io(fn ->
      assert_raise CompileError, fn ->
        Kernel.ParallelCompiler.files fixtures
      end
    end) =~ "Compilation error"
  end

  test "handles possible deadlocks" do
    fixtures = [fixture_path("parallel_deadlock/foo.ex"), fixture_path("parallel_deadlock/bar.ex")]

    msg = capture_io(fn ->
      assert_raise UndefinedFunctionError, fn ->
        Kernel.ParallelCompiler.files fixtures
      end
    end)

    assert msg =~ "* #{fixture_path "parallel_deadlock/foo.ex"} is missing module Bar"
    assert msg =~ "* #{fixture_path "parallel_deadlock/bar.ex"} is missing module Foo"
  end

  test "warnings as errors" do
    warnings_as_errors = Code.compiler_options[:warnings_as_errors]

    try do
      Code.compiler_options(warnings_as_errors: true)

      assert_raise CompileError, fn ->
        capture_io :stderr, fn ->
          Kernel.ParallelCompiler.files [fixture_path("warnings_sample.ex")]
        end
      end
    after
      Code.compiler_options(warnings_as_errors: warnings_as_errors)
    end
  end
end
