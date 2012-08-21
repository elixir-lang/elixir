Code.require_file "../test_helper.exs", __FILE__

defmodule MixTest do
  use MixTest.Case

  test :shell do
    assert Mix.shell == Mix.Shell.Process
  end
end