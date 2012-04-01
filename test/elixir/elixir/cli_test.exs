Code.require_file "../../test_helper", __FILE__

require Erlang.os, as: OS

defmodule Elixir.CLI.InitTest do
  use ExUnit.Case

  test :code_init do
    assert_equal '3\n', OS.cmd('bin/elixir -e "IO.puts 1 + 2"')

    expected = '#{inspect ["-o", "1", "2", "3"]}\n3\n'
    assert_equal expected, OS.cmd('bin/elixir -e "IO.puts inspect(Env.argv)" test/elixir/fixtures/init_sample.exs -o 1 2 3')
  end
end

defmodule Elixir.CLI.AtExitTest do
  use ExUnit.Case

  test :at_exit do
    assert_equal 'goodbye cruel world with status 0\n', OS.cmd('bin/elixir test/elixir/fixtures/at_exit.exs')
  end
end

defmodule Elixir.CLI.ErrorTest do
  use ExUnit.Case

  test :code_error do
    assert Erlang.string.str('** (throw) 1', OS.cmd('bin/elixir -e "throw 1"')) == 0
    assert Erlang.string.str('** (ErlangError) erlang error: 1', OS.cmd('bin/elixir -e "error 1"')) == 0

    # It does not catch exits with integers nor strings...
    assert_equal '', OS.cmd('bin/elixir -e "exit 1"')
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
    assert_equal 'Compiling test/elixir/fixtures/compile_sample.exs\n',
      OS.cmd('bin/elixirc test/elixir/fixtures/compile_sample.exs -o test/tmp/')
    assert File.regular?("test/tmp/__MAIN__/CompileSample.beam")
  after:
    Erlang.file.del_dir("test/tmp/")
  end
end
