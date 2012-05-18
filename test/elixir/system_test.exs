Code.require_file "../test_helper", __FILE__

require Erlang.os, as: OS

defmodule System.ARGVTest do
  use ExUnit.Case

  test :argv do
    list = OS.cmd('bin/elixir -e "IO.inspect System.argv" -- -o opt arg1 arg2 --long-opt 10')
    { args, _ } = Code.eval list, []
    assert args == ["-o", "opt", "arg1", "arg2", "--long-opt", "10"]
  end
end

defmodule SystemTest do
  use ExUnit.Case

  test :at_exit do
    output = OS.cmd('bin/elixir -e "System.at_exit(fn x -> IO.inspect x end)"')
    assert output == '0\n'
  end

  test :env do
    assert System.get_env("SYSTEM_ENV_TEST_VAR") == nil
    System.put_env('SYSTEM_ENV_TEST_VAR', 'SAMPLE')
    assert System.get_env("SYSTEM_ENV_TEST_VAR") == "SAMPLE"
  end

  test :cmd do
    assert is_binary(System.cmd "binary")
  end
end
