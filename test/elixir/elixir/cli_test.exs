Code.require_file "../../test_helper", __FILE__

require Erlang.os, as: OS

defmodule Elixir.CLI.InitTest do
  use ExUnit.Case

  test :code_init do
    assert OS.cmd('bin/elixir -e "IO.puts 1 + 2"') == '3\n'

    result = OS.cmd('bin/elixir -e "IO.puts inspect(System.argv)" test/elixir/fixtures/init_sample.exs -o 1 2 3')
    assert result == '#{inspect ["-o", "1", "2", "3"]}\n3\n'
  end
end

defmodule Elixir.CLI.OptionParsingTest do
  use ExUnit.Case

  test :path do
    list = OS.cmd('bin/elixir -e "IO.inspect Erlang.code.get_path" -pa "*" -pz "exbin/*"')
    { path, _ } = Code.eval list, []

    # pa
    assert_member 'bin', path
    assert_member 'ebin', path
    assert_member 'exbin', path
    assert_member 'src', path
    assert_member 'lib', path
    assert_member 'include', path
    assert_member 'test', path

    # pz
    assert_member 'exbin/__MAIN__', path
  end

  test :require do
    options = ['-r', 'lib/list/*', '-r', '/never/gonna/*/up']
    { config, _argv } = Elixir.CLI.process_options(options, Elixir.CLI.Config.new)
    assert_member {:require, 'lib/list/chars.ex'}, config.commands
  end
end

defmodule Elixir.CLI.AtExitTest do
  use ExUnit.Case

  test :at_exit do
    assert OS.cmd('bin/elixir test/elixir/fixtures/at_exit.exs') ==
      'goodbye cruel world with status 0\n'
  end
end

defmodule Elixir.CLI.ErrorTest do
  use ExUnit.Case

  test :code_error do
    assert Erlang.string.str('** (throw) 1', OS.cmd('bin/elixir -e "throw 1"')) == 0
    assert Erlang.string.str('** (ErlangError) erlang error: 1', OS.cmd('bin/elixir -e "error 1"')) == 0

    # It does not catch exits with integers nor strings...
    assert OS.cmd('bin/elixir -e "exit 1"') == ''
  end
end

defmodule Elixir.CLI.SyntaxErrorTest do
  use ExUnit.Case

  test :syntax_code_error do
    message = '** (TokenMissingError) nofile:1: syntax error: expression is incomplete'
    assert Erlang.string.str(message, OS.cmd('bin/elixir -e "[1,2"')) == 0
    message = '** (SyntaxError) nofile:1: syntax error before: \'end\''
    assert Erlang.string.str(message, OS.cmd('bin/elixir -e "case 1 end"')) == 0
  end
end

defmodule Elixir.CLI.CompileTest do
  use ExUnit.Case

  test :compile_code do
    assert OS.cmd('bin/elixirc test/elixir/fixtures/compile_sample.exs -o test/tmp/') ==
      'Compiled test/elixir/fixtures/compile_sample.exs\n'
    assert File.regular?("test/tmp/__MAIN__/CompileSample.beam")
  after:
    Erlang.file.del_dir("test/tmp/")
  end
end

defmodule Elixir.CLI.ParallelCompilerTest do
  use ExUnit.Case

  test :compile_code do
    output = OS.cmd('bin/elixirc test/elixir/fixtures/parallel_compiler -o test/tmp/')
    assert Erlang.string.str(output, 'message_from_foo') > 0,
      "Expected #{inspect output} to contain 'message_from_foo'"
    assert File.regular?("test/tmp/__MAIN__/Foo.beam")
    assert File.regular?("test/tmp/__MAIN__/Bar.beam")
  after:
    Erlang.file.del_dir("test/tmp/")
  end

  test :deadlock_failure do
    output = OS.cmd('bin/elixirc test/elixir/fixtures/parallel_deadlock -o test/tmp/')
    foo = '== Compilation error on file test/elixir/fixtures/parallel_deadlock/foo.ex (undefined module Bar) =='
    bar = '== Compilation error on file test/elixir/fixtures/parallel_deadlock/bar.ex (undefined module Foo) =='
    assert Erlang.string.str(output, foo) > 0 or Erlang.string.str(output, bar) > 0,
      "Expected #{inspect output} to contain #{inspect foo} or #{inspect bar}"
  end
end
