Code.require_file "../test_helper", __FILE__

defmodule IOTest do
  use ExUnit.Case

  test :getb do
    { :ok, file } = :file.open(File.expand_path('../fixtures/foo.txt', __FILE__), [:read])
    assert 'F' == IO.getb(file)
    assert 'O' == IO.getb(file)
    assert 'O' == IO.getb(file)
    assert '\n' == IO.getb(file)
    assert :eof == IO.getb(file)
  end

  test :getb_with_count do
    { :ok, file } = :file.open(File.expand_path('../fixtures/foo.txt', __FILE__), [:read])
    assert 'FOO' == IO.getb(file, 3)
  end

  test :getb_with_utf8_and_binary do
    { :ok, file } = :file.open(File.expand_path('../fixtures/utf8.txt', __FILE__), [:read, :binary, {:encoding, :utf8}])
    assert "Русский" == IO.getb(file, 7)
  end

  test :gets do
    { :ok, file } = :file.open(File.expand_path('../fixtures/foo.txt', __FILE__), [:read])
    assert 'FOO\n' == IO.gets(file)
    assert :eof == IO.gets(file)
  end

  test :gets_with_utf8_and_binary do
    { :ok, file } = :file.open(File.expand_path('../fixtures/utf8.txt', __FILE__), [:read, :binary, {:encoding, :utf8}])
    assert "Русский\n" == IO.gets(file)
    assert "日\n" == IO.gets(file)
  end
end
