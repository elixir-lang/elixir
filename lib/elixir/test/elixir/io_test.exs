Code.require_file "test_helper.exs", __DIR__

defmodule IOTest do
  use ExUnit.Case, async: true

  test :read_with_count do
    { :ok, file } = File.open(Path.expand('../fixtures/file.txt', __FILE__), [:charlist])
    assert 'FOO' == IO.read(file, 3)
    assert File.close(file) == :ok
  end

  test :read_with_utf8_and_binary do
    { :ok, file } = File.open(Path.expand('../fixtures/utf8.txt', __FILE__), [:utf8])
    assert "Русский" == IO.read(file, 7)
    assert File.close(file) == :ok
  end

  test :binread do
    { :ok, file } = File.open(Path.expand('../fixtures/utf8.txt', __FILE__))
    assert "Русский" == IO.binread(file, 14)
    assert File.close(file) == :ok
  end

  test :getn do
    { :ok, file } = File.open(Path.expand('../fixtures/file.txt', __FILE__))
    assert "F" == IO.getn(file, "")
    assert "O" == IO.getn(file, "")
    assert "O" == IO.getn(file, "")
    assert "\n" == IO.getn(file, "")
    assert :eof == IO.getn(file, "")
    assert File.close(file) == :ok
  end

  test :getn_with_count do
    { :ok, file } = File.open(Path.expand('../fixtures/file.txt', __FILE__), [:charlist])
    assert 'FOO' == IO.getn(file, "", 3)
    assert File.close(file) == :ok
  end

  test :getn_with_utf8_and_binary do
    { :ok, file } = File.open(Path.expand('../fixtures/utf8.txt', __FILE__), [:utf8])
    assert "Русский" == IO.getn(file, "", 7)
    assert File.close(file) == :ok
  end

  test :gets do
    { :ok, file } = File.open(Path.expand('../fixtures/file.txt', __FILE__), [:charlist])
    assert 'FOO\n' == IO.gets(file, "")
    assert :eof == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test :gets_with_utf8_and_binary do
    { :ok, file } = File.open(Path.expand('../fixtures/utf8.txt', __FILE__), [:utf8])
    assert "Русский\n" == IO.gets(file, "")
    assert "日\n" == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test :readline do
    { :ok, file } = File.open(Path.expand('../fixtures/file.txt', __FILE__))
    assert "FOO\n" == IO.readline(file)
    assert :eof == IO.readline(file)
    assert File.close(file) == :ok
  end

  test :readline_with_utf8_and_binary do
    { :ok, file } = File.open(Path.expand('../fixtures/utf8.txt', __FILE__), [:utf8])
    assert "Русский\n" == IO.readline(file)
    assert "日\n" == IO.readline(file)
    assert File.close(file) == :ok
  end

  test :binreadline do
    { :ok, file } = File.open(Path.expand('../fixtures/utf8.txt', __FILE__))
    assert "Русский\n" == IO.binreadline(file)
    assert "日\n" == IO.binreadline(file)
    assert File.close(file) == :ok
  end
end
