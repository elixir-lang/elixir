Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.CLI.InitTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test :code_init do
    assert elixir('-e "IO.puts 3"') == '3\n'

    result = elixir('-e "IO.puts inspect(System.argv)" #{fixture_path("init_sample.exs")} -o 1 2 3')
    assert result == '#{inspect ["-o", "1", "2", "3"]}\n3\n'
  end
end

defmodule Kernel.CLI.OptionParsingTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test :path do
    root = fixture_path("../../..") |> to_char_list
    list = elixir('-e "IO.inspect :code.get_path" -pa "#{root}/*" -pz "#{root}/lib/*"')
    { path, _ } = Code.eval list, []

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
  import PathHelpers

  test :at_exit do
    assert elixir(fixture_path("at_exit.exs") |> to_char_list) ==
      'goodbye cruel world with status 0\n'
  end
end

defmodule Kernel.CLI.ErrorTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test :code_error do
    assert :string.str('** (throw) 1', elixir('-e "throw 1"')) == 0
    assert :string.str('** (ErlangError) erlang error: 1', elixir('-e "error 1"')) == 0

    # It does not catch exits with integers nor strings...
    assert elixir('-e "exit 1"') == ''
  end
end

defmodule Kernel.CLI.SyntaxErrorTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test :syntax_code_error do
    message = '** (TokenMissingError) nofile:1: syntax error: expression is incomplete'
    assert :string.str(message, elixir('-e "[1,2"')) == 0
    message = '** (SyntaxError) nofile:1: syntax error before: \'end\''
    assert :string.str(message, elixir('-e "case 1 end"')) == 0
    message = '** (SyntaxError) nofile:1: invalid token: あ'
    assert :string.str(message, elixir('-e "あ"')) == 0
    message = '** (SyntaxError) nofile:1: invalid token: æ'
    assert :string.str(message, elixir('-e "æ"')) == 0
  end
end

defmodule Kernel.CLI.CompileTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test :compile_code do
    fixture = fixture_path "compile_sample.ex"
    assert elixirc('#{fixture} -o #{tmp_path}') ==
      'Compiled #{fixture}\n'
    assert File.regular?(tmp_path "Elixir-CompileSample.beam")
  end
end

defmodule Kernel.CLI.ParallelCompilerTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test :files do
    fixtures = [fixture_path("compile_sample.ex")]
    assert [{ CompileSample, binary }] = Kernel.ParallelCompiler.files fixtures
    assert is_binary(binary)
  end

  test :compile_code do
    output = elixirc('#{fixture_path("parallel_compiler")} -o #{tmp_path}')
    assert :string.str(output, 'message_from_foo') > 0,
      "Expected #{inspect output} to contain 'message_from_foo'"
    assert File.regular?(tmp_path "Elixir-Foo.beam")
    assert File.regular?(tmp_path "Elixir-Bar.beam")
  end

  test :possible_deadlock do
    output = elixirc('#{fixture_path("parallel_deadlock")} -o #{tmp_path}')
    foo = '* #{fixture_path "parallel_deadlock/foo.ex"} is missing module Bar'
    bar = '* #{fixture_path "parallel_deadlock/bar.ex"} is missing module Foo'
    assert :string.str(output, foo) > 0, "expected foo.ex to miss module Bar"
    assert :string.str(output, bar) > 0, "expected bar.ex to miss module Foo"
  end
end
