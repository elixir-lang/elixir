Code.require_file "../test_helper", __FILE__

defmodule MixTest do
  use MixTest.Case

  def setup(_) do
    Mix.mixfile(nil)
  end

  test :mixfile do
    assert Mix.mixfile == nil
    Mix.mixfile(MixTest)
    assert Mix.mixfile == MixTest
  end

  test :run do
    assert Mix.run(["hello"]) == "Hello, World!"
  end
end