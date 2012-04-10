Code.require_file "../test_helper", __FILE__

require Erlang.os, as: OS

defmodule SystemTest do
  use ExUnit.Case

  test :argv do
    list = OS.cmd('bin/elixir -e "IO.inspect System.argv" sample_script.exs -o opt arg1 arg2 --long-opt 10')
    {args, _} = Code.eval list, []
    expected = ["-o", "opt", "arg1", "arg2", "--long-opt", "10"]
    assert_equal expected, args
  end

  test :at_exit do
    output = OS.cmd('bin/elixir -e "System.at_exit(fn(x) -> IO.inspect x end)"')
    assert_equal '0\n', output
  end

  test :cmd do
    assert is_list(System.cmd 'list')
    assert is_binary(System.cmd "binary")
  end

  test :get_env do
    list_cmd = %c{SECRET_VAR=elixir bin/elixir  -e "IO.inspect System.get_env('SECRET_VAR')"}
    {output, _} = Code.eval OS.cmd(list_cmd), []
    assert_equal 'elixir', output

    bin_cmd = %c{SECRET_VAR=elixir bin/elixir  -e 'IO.inspect System.get_env("SECRET_VAR")'}
    {output, _} = Code.eval OS.cmd(bin_cmd), []
    assert_equal "elixir", output

    assert Enum.all? System.get_env, is_binary &1
  end

  test :put_env do
    assert_equal nil, System.get_env('SECRET_VAR')

    output = OS.cmd(%c{bin/elixir -e "System.put_env('SECRET_VAR', 'elixir'); IO.inspect System.get_env('SECRET_VAR')"})
    {varvalue, _} = Code.eval output, []
    assert_equal 'elixir', varvalue
  end

  test :pwd do
    assert is_binary(System.pwd)
    assert_equal Regex.replace_all(%r/\n/, OS.cmd('pwd'), ""), binary_to_list(System.pwd)
  end
end
