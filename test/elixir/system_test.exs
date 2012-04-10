Code.require_file "../test_helper", __FILE__

require Erlang.os, as: OS

defmodule System.ARGVTest do
  use ExUnit.Case

  test :argv do
    list = OS.cmd('bin/elixir -e "IO.inspect System.argv" -- -o opt arg1 arg2 --long-opt 10')
    { args, _ } = Code.eval list, []
    expected = ["-o", "opt", "arg1", "arg2", "--long-opt", "10"]
    assert_equal expected, args
  end
end

defmodule SystemTest do
  use ExUnit.Case

  test :at_exit do
    output = OS.cmd('bin/elixir -e "System.at_exit(fn(x) -> IO.inspect x end)"')
    assert_equal '0\n', output
  end

  test :env do
    assert_equal nil, System.get_env("SYSTEM_ENV_TEST_VAR")
    System.put_env('SYSTEM_ENV_TEST_VAR', 'SAMPLE')
    assert_equal "SAMPLE", System.get_env("SYSTEM_ENV_TEST_VAR")
  end

  test :cmd do
    assert is_binary(System.cmd "binary")
  end

  test :pwd do
    assert is_binary(System.pwd)
    assert_equal Regex.replace_all(%r/\n/, OS.cmd('pwd'), ""), binary_to_list(System.pwd)
  end
end
