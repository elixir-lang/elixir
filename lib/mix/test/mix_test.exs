Code.require_file "../test_helper", __FILE__

defmodule MixTest do
  use ExUnit.Case

  test :mixfile do
    assert Mix.mixfile == nil
    Mix.mixfile(MixTest)
    assert Mix.mixfile == MixTest
  end
end