Code.require_file "test_helper.exs", __DIR__

defmodule DigestTest do
  use ExUnit.Case, async: true

  test :md5 do
    assert "201730d4278e576b25515bd90c6072d3" == Digest.md5("lorem ipsum dolor sit amet")
    assert "5d41402abc4b2a76b9719d911017c592" == Digest.md5("hello")
    assert "d41d8cd98f00b204e9800998ecf8427e" == Digest.md5("")
  end

  test :to_hex do
    assert "6c6f72656d20697073756d20646f6c6f722073697420616d6574" = Digest.to_hex("lorem ipsum dolor sit amet")
    assert "74657374" == Digest.to_hex("test")
    assert "c3a6c39f" == Digest.to_hex("æß")
    assert ""         == Digest.to_hex("")
  end
end