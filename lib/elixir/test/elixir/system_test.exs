Code.require_file "../test_helper.exs", __FILE__

require :os, as: OS

defmodule SystemTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test :build_info do
    assert not nil?(System.build_info[:version])
    assert not nil?(System.build_info[:tag])
    assert not nil?(System.build_info[:date])
  end

  test :argv do
    list = elixir('-e "IO.inspect System.argv" -- -o opt arg1 arg2 --long-opt 10')
    { args, _ } = Code.eval list, []
    assert args == ["-o", "opt", "arg1", "arg2", "--long-opt", "10"]
  end

  test :at_exit do
    output = elixir('-e "System.at_exit(fn x -> IO.inspect x end)"')
    assert output == '0\n'
  end

  test :env do
    assert System.get_env("SYSTEM_ENV_TEST_VAR") == nil
    System.put_env('SYSTEM_ENV_TEST_VAR', 'SAMPLE')
    assert System.get_env("SYSTEM_ENV_TEST_VAR") == "SAMPLE"
  end

  test :cmd do
    assert is_binary(System.cmd "binary")
    assert is_list(System.cmd 'binary')
  end

  test :find_executable_with_binary do
    assert System.find_executable("erl")
    assert is_binary System.find_executable("erl")
    assert !System.find_executable("does-not-really-exist-from-elixir")
  end

  test :find_executable_with_list do
    assert System.find_executable('erl')
    assert is_list System.find_executable('erl')
    assert !System.find_executable('does-not-really-exist-from-elixir')
  end
end