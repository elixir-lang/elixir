Code.require_file "test_helper.exs", __DIR__

defmodule DigestTest do
  use ExUnit.Case, async: true

  test :md5 do
    assert "201730d4278e576b25515bd90c6072d3" == Digest.md5("lorem ipsum dolor sit amet")
    assert "5d41402abc4b2a76b9719d911017c592" == Digest.md5("hello")
    assert "d41d8cd98f00b204e9800998ecf8427e" == Digest.md5("")
  end

  test :to_hex do
    assert "6C6F72656D20697073756D20646F6C6F722073697420616D6574" = Digest.to_hex("lorem ipsum dolor sit amet")
    assert "74657374" == Digest.to_hex("test")
    assert "C3A6C39F" == Digest.to_hex("æß")
    assert ""         == Digest.to_hex("")
  end
end