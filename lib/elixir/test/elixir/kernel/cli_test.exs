Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.CLI.InitTest do
  use ExUnit.Case, async: true

  test :code_init do
    assert elixir('-e "IO.puts 3"') == '3\n'

    result = elixir('-e "IO.puts inspect(System.argv)" #{fixture_path("init_sample.exs")} -o 1 2 3')
    assert result == '#{inspect ["-o", "1", "2", "3"]}\n3\n'
  end
end

defmodule Kernel.CLI.OptionParsingTest do
  use ExUnit.Case, async: true

  test :path do
    root = fixture_path("../../..") |> to_char_list
    list = elixir('-pa "#{root}/*" -pz "#{root}/lib/*" -e "IO.inspect :code.get_path"')
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

  test :at_exit do
    assert elixir(fixture_path("at_exit.exs") |> to_char_list) ==
      'goodbye cruel world with status 0\n'
  end
end

defmodule Kernel.CLI.ErrorTest do
  use ExUnit.Case, async: true

  test :code_error do
    assert :string.str('** (throw) 1', elixir('-e "throw 1"')) == 0
    assert :string.str('** (ErlangError) erlang error: 1', elixir('-e "error 1"')) == 0

    # It does not catch exits with integers nor strings...
    assert elixir('-e "exit 1"') == ''
  end
end

defmodule Kernel.CLI.SyntaxErrorTest do
  use ExUnit.Case, async: true

  def check_output(elixir_cmd, expected_msg) do
    o = elixir(elixir_cmd)
    expected_msg = :unicode.characters_to_list(expected_msg)
    assert :string.str(o, expected_msg) == 1, "Expected this output: `#{expected_msg}`\nbut got this output: `#{o}`"
  end

  test :syntax_code_error do
    check_output('-e "[1,2"', '** (TokenMissingError) nofile:1: missing terminator: ]')
    check_output('-e "case 1 end"', %C"** (SyntaxError) nofile:1: unexpected token: end")
  end
end

defmodule Kernel.CLI.CompileTest do
  use ExUnit.Case, async: true

  test :compile_code do
    fixture = fixture_path "compile_sample.ex"
    assert elixirc('#{fixture} -o #{tmp_path}') ==
      'Compiled #{fixture}\n'
    assert File.regular?(tmp_path "Elixir.CompileSample.beam")
  after
    File.rm(tmp_path("Elixir.CompileSample.beam"))
  end

  test :possible_deadlock do
    output = elixirc('#{fixture_path("parallel_deadlock")} -o #{tmp_path}')
    foo = '* #{fixture_path "parallel_deadlock/foo.ex"} is missing module Bar'
    bar = '* #{fixture_path "parallel_deadlock/bar.ex"} is missing module Foo'
    assert :string.str(output, foo) > 0, "expected foo.ex to miss module Bar"
    assert :string.str(output, bar) > 0, "expected bar.ex to miss module Foo"
    assert :string.str(output, 'elixir_compiler') == 0, "expected elixir_compiler to not be in output"
  end
end

defmodule Kernel.CLI.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test :files do
    fixtures = [fixture_path("parallel_compiler/bar.ex"), fixture_path("parallel_compiler/foo.ex")]
    assert capture_io(fn ->
      assert [{ Bar, bar }, { Foo, _foo }] = Kernel.ParallelCompiler.files fixtures
      assert is_binary(bar)
    end) =~ "message_from_foo"
  end

  test :warnings_as_errors do
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
