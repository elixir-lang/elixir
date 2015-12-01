Code.require_file "test_helper.exs", __DIR__

defmodule IOTest do
  use ExUnit.Case, async: true

  doctest IO

  import ExUnit.CaptureIO

  test "read with count" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__), [:char_list])
    assert 'FOO' == IO.read(file, 3)
    assert File.close(file) == :ok
  end

  test "read with utf8 and binary" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__), [:utf8])
    assert "Русский" == IO.read(file, 7)
    assert File.close(file) == :ok
  end

  test "binread" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__))
    assert "Русский" == IO.binread(file, 14)
    assert File.close(file) == :ok
  end

  test "binread all" do
    {:ok, file} = File.open(Path.expand('fixtures/file.bin', __DIR__))
    assert "LF\nCR\rCRLF\r\nLFCR\n\r" == IO.binread(file, :all)
    assert File.close(file) == :ok
  end

  test "getn" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__))
    assert "F" == IO.getn(file, "")
    assert "O" == IO.getn(file, "")
    assert "O" == IO.getn(file, "")
    assert "\n" == IO.getn(file, "")
    assert :eof == IO.getn(file, "")
    assert File.close(file) == :ok
  end

  test "getn with count" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__), [:char_list])
    assert 'FOO' == IO.getn(file, "", 3)
    assert File.close(file) == :ok
  end

  test "getn with utf8 and binary" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__), [:utf8])
    assert "Русский" == IO.getn(file, "", 7)
    assert File.close(file) == :ok
  end

  test "gets" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__), [:char_list])
    assert 'FOO\n' == IO.gets(file, "")
    assert :eof == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test "gets with utf8 and binary" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__), [:utf8])
    assert "Русский\n" == IO.gets(file, "")
    assert "日\n" == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test "readall" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__))
    assert "FOO\n" == IO.read(file, :all)
    assert "" == IO.read(file, :all)
    assert File.close(file) == :ok
  end

  test "readall with utf8 and binary" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__), [:utf8])
    assert "Русский\n日\n" == IO.read(file, :all)
    assert "" == IO.read(file, :all)
    assert File.close(file) == :ok
  end

  test "readline" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__))
    assert "FOO\n" == IO.read(file, :line)
    assert :eof == IO.read(file, :line)
    assert File.close(file) == :ok
  end

  test "readline with utf8 and binary" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__), [:utf8])
    assert "Русский\n" == IO.read(file, :line)
    assert "日\n" == IO.read(file, :line)
    assert File.close(file) == :ok
  end

  test "binreadall" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__))
    assert "Русский\n日\n" == IO.binread(file, :all)
    assert "" == IO.binread(file, :all)
    assert File.close(file) == :ok
  end

  test "binreadline" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__))
    assert "Русский\n" == IO.binread(file, :line)
    assert "日\n" == IO.binread(file, :line)
    assert File.close(file) == :ok
  end

  test "puts with chardata" do
    assert capture_io(fn -> IO.puts("hello") end) == "hello\n"
    assert capture_io(fn -> IO.puts('hello') end) == "hello\n"
    assert capture_io(fn -> IO.puts(:hello) end) == "hello\n"
    assert capture_io(fn -> IO.puts(13) end) == "13\n"
  end

  test "write with chardata" do
    assert capture_io(fn -> IO.write("hello") end) == "hello"
    assert capture_io(fn -> IO.write('hello') end) == "hello"
    assert capture_io(fn -> IO.write(:hello) end) == "hello"
    assert capture_io(fn -> IO.write(13) end) == "13"
  end

  test "gets with chardata" do
    assert capture_io("foo\n", fn -> IO.gets("hello") end) == "hello"
    assert capture_io("foo\n", fn -> IO.gets('hello') end) == "hello"
    assert capture_io("foo\n", fn -> IO.gets(:hello) end) == "hello"
    assert capture_io("foo\n", fn -> IO.gets(13) end) == "13"
  end

  test "getn with chardata" do
    assert capture_io("foo\n", fn -> IO.getn("hello", 3) end) == "hello"
    assert capture_io("foo\n", fn -> IO.getn('hello', 3) end) == "hello"
    assert capture_io("foo\n", fn -> IO.getn(:hello, 3) end) == "hello"
    assert capture_io("foo\n", fn -> IO.getn(13, 3) end) == "13"
  end
end
