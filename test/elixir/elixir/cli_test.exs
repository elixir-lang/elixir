Code.require_file "../../test_helper", __FILE__

require Erlang.os, as: OS

defmodule Elixir::CLI::InitTest do
  use ExUnit::Case

  def test_code_init do
    '3\n'       = OS.cmd('bin/elixir -e "IO.puts 1 + 2"')
    '5\n3\n'    = OS.cmd('bin/elixir -f "IO.puts 1 + 2" -e "IO.puts 3 + 2"')
    '5\n3\n1\n' = OS.cmd('bin/elixir -f "IO.puts 1" -e "IO.puts 3 + 2" test/elixir/fixtures/init_sample.exs')

    expected  = '#{inspect ['-o', '1', '2', '3']}\n3\n'
    ^expected = OS.cmd('bin/elixir -e "IO.puts inspect(Code.argv)" test/elixir/fixtures/init_sample.exs -o 1 2 3')
  end
end

defmodule Elixir::CLI::ErrorTest do
  use ExUnit::Case

  def test_code_error do
    assert_included '** throw 1',  OS.cmd('bin/elixir -e "throw 1"')
    assert_included '** error 1',  OS.cmd('bin/elixir -e "error 1"')

    # It does not catch exits with integers nor strings...
    '' = OS.cmd('bin/elixir -e "exit 1"')
  end
end

defmodule Elixir::CLI::SyntaxErrorTest do
  use ExUnit::Case

  def test_syntax_code_error do
    assert_included '** error nofile:1: syntax error', OS.cmd('bin/elixir -e "[1,2"')
    assert_included '** error nofile:1: syntax error before: \'end\'', OS.cmd('bin/elixir -e "case 1 end"')
  end
end

defmodule Elixir::CLI::CompileTest do
  use ExUnit::Case

  def test_compile_code do
    try do
      'Compiling test/elixir/fixtures/compile_sample.exs\n' =
        OS.cmd('bin/elixirc test/elixir/fixtures/compile_sample.exs -o test/tmp/')
      true = File.regular?("test/tmp/::CompileSample.beam")
    catch: { :invalid, _, _ }
    after:
      Erlang.file.del_dir("test/tmp/")
    end
  end
end