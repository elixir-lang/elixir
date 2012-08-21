Code.require_file "../test_helper.exs", __FILE__

defmodule IOTest do
  use ExUnit.Case, async: true

  test :read_with_count do
    { :ok, file } = File.open(File.expand_path('../fixtures/foo.txt', __FILE__), [:charlist])
    assert 'FOO' == IO.read(file, 3)
    assert File.close(file) == :ok
  end

  test :read_with_utf8_and_binary do
    { :ok, file } = File.open(File.expand_path('../fixtures/utf8.txt', __FILE__))
    assert "Русский" == IO.read(file, 7)
    assert File.close(file) == :ok
  end

  test :getb do
    { :ok, file } = File.open(File.expand_path('../fixtures/foo.txt', __FILE__))
    assert "F" == IO.getb(file, "")
    assert "O" == IO.getb(file, "")
    assert "O" == IO.getb(file, "")
    assert "\n" == IO.getb(file, "")
    assert :eof == IO.getb(file, "")
    assert File.close(file) == :ok
  end

  test :getb_with_count do
    { :ok, file } = File.open(File.expand_path('../fixtures/foo.txt', __FILE__), [:charlist])
    assert 'FOO' == IO.getb(file, "", 3)
    assert File.close(file) == :ok
  end

  test :getb_with_utf8_and_binary do
    { :ok, file } = File.open(File.expand_path('../fixtures/utf8.txt', __FILE__))
    assert "Русский" == IO.getb(file, "", 7)
    assert File.close(file) == :ok
  end

  test :gets do
    { :ok, file } = File.open(File.expand_path('../fixtures/foo.txt', __FILE__), [:charlist])
    assert 'FOO\n' == IO.gets(file, "")
    assert :eof == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test :gets_with_utf8_and_binary do
    { :ok, file } = File.open(File.expand_path('../fixtures/utf8.txt', __FILE__))
    assert "Русский\n" == IO.gets(file, "")
    assert "日\n" == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test :readline do
    { :ok, file } = File.open(File.expand_path('../fixtures/foo.txt', __FILE__))
    assert "FOO\n" == IO.readline(file)
    assert :eof == IO.readline(file)
    assert File.close(file) == :ok
  end

  test :readline_with_utf8_and_binary do
    { :ok, file } = File.open(File.expand_path('../fixtures/utf8.txt', __FILE__))
    assert "Русский\n" == IO.readline(file)
    assert "日\n" == IO.readline(file)
    assert File.close(file) == :ok
  end
end
