Code.require_file "../../test_helper", __FILE__

defmodule Elixir.CLI.InitTest do
  use ExUnit.Case
  import PathHelpers

  test :code_init do
    assert elixir('-e "IO.puts 3"') == '3\n'

    result = elixir('-e "IO.puts inspect(System.argv)" #{fixture_path("init_sample.exs")} -o 1 2 3')
    assert result == '#{inspect ["-o", "1", "2", "3"]}\n3\n'
  end
end

defmodule Elixir.CLI.OptionParsingTest do
  use ExUnit.Case
  import PathHelpers

  test :path do
    root = fixture_path("../../..") /> to_char_list
    list = elixir('-e "IO.inspect Erlang.code.get_path" -pa "#{root}/*" -pz "#{root}/lib/*"')
    { path, _ } = Code.eval list, []

    # pa
    assert File.expand_path('ebin', root) in path
    assert File.expand_path('lib', root) in path
    assert File.expand_path('src', root) in path

    # pz
    assert File.expand_path('lib/list', root) in path
  end

  test :require do
    options = ['-r', fixture_path('../../../lib/list/*') /> to_char_list, '-r', '/never/gonna/*/up']
    { config, _argv } = Elixir.CLI.process_options(options, Elixir.CLI.Config.new)
    assert {:require, fixture_path "../../../lib/list/chars.ex"} in config.commands
  end
end

defmodule Elixir.CLI.AtExitTest do
  use ExUnit.Case
  import PathHelpers

  test :at_exit do
    assert elixir(fixture_path("at_exit.exs") /> to_char_list) ==
      'goodbye cruel world with status 0\n'
  end
end

defmodule Elixir.CLI.ErrorTest do
  use ExUnit.Case
  import PathHelpers

  test :code_error do
    assert Erlang.string.str('** (throw) 1', elixir('-e "throw 1"')) == 0
    assert Erlang.string.str('** (ErlangError) erlang error: 1', elixir('-e "error 1"')) == 0

    # It does not catch exits with integers nor strings...
    assert elixir('-e "exit 1"') == ''
  end
end

defmodule Elixir.CLI.SyntaxErrorTest do
  use ExUnit.Case
  import PathHelpers

  test :syntax_code_error do
    message = '** (TokenMissingError) nofile:1: syntax error: expression is incomplete'
    assert Erlang.string.str(message, elixir('-e "[1,2"')) == 0
    message = '** (SyntaxError) nofile:1: syntax error before: \'end\''
    assert Erlang.string.str(message, elixir('-e "case 1 end"')) == 0
  end
end

defmodule Elixir.CLI.CompileTest do
  use ExUnit.Case
  import PathHelpers

  test :compile_code do
    fixture = fixture_path "compile_sample.exs"
    assert elixirc('#{fixture} -o #{tmp_path}') ==
      'Compiled #{fixture}\n'
    assert File.regular?(tmp_path "__MAIN__-CompileSample.beam")
  end
end

defmodule Elixir.CLI.ParallelCompilerTest do
  use ExUnit.Case
  import PathHelpers

  test :compile_code do
    output = elixirc('#{fixture_path("parallel_compiler")} -o #{tmp_path}')
    assert Erlang.string.str(output, 'message_from_foo') > 0,
      "Expected #{inspect output} to contain 'message_from_foo'"
    assert File.regular?(tmp_path "__MAIN__-Foo.beam")
    assert File.regular?(tmp_path "__MAIN__-Bar.beam")
  end

  test :deadlock_failure do
    output = elixirc('#{fixture_path("parallel_deadlock")} -o #{tmp_path}')
    foo = '== Compilation error on file #{fixture_path "parallel_deadlock/foo.ex"} (undefined module Bar) =='
    bar = '== Compilation error on file #{fixture_path "parallel_deadlock/bar.ex"} (undefined module Foo) =='
    assert Erlang.string.str(output, foo) > 0 or Erlang.string.str(output, bar) > 0,
      "Expected #{inspect output} to contain #{inspect foo} or #{inspect bar}"
  end
end
