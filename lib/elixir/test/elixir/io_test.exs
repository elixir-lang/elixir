Code.require_file("test_helper.exs", __DIR__)

defmodule IOTest do
  use ExUnit.Case

  doctest IO

  import ExUnit.CaptureIO

  test "read with count" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__), [:charlist])
    assert 'FOO' == IO.read(file, 3)
    assert File.close(file) == :ok
  end

  test "read with UTF-8 and binary" do
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
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__), [:charlist])
    assert 'F' == IO.getn(file, "λ")
    assert 'OO' == IO.getn(file, "", 2)
    assert '\n' == IO.getn(file, "λ", 99)
    assert File.close(file) == :ok
  end

  test "getn with UTF-8 and binary" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__), [:utf8])
    assert "Русский" == IO.getn(file, "", 7)
    assert File.close(file) == :ok
  end

  test "gets" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__), [:charlist])
    assert 'FOO\n' == IO.gets(file, "")
    assert :eof == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test "gets with UTF-8 and binary" do
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

  test "readall with UTF-8 and binary" do
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

  test "readline with UTF-8 and binary" do
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

  test "warn with chardata" do
    assert capture_io(:stderr, fn -> IO.warn("hello") end) =~
             "hello\n  (ex_unit #{System.version()}) lib/ex_unit"

    assert capture_io(:stderr, fn -> IO.warn('hello') end) =~
             "hello\n  (ex_unit #{System.version()}) lib/ex_unit"

    assert capture_io(:stderr, fn -> IO.warn(:hello) end) =~
             "hello\n  (ex_unit #{System.version()}) lib/ex_unit"

    assert capture_io(:stderr, fn -> IO.warn(13) end) =~
             "13\n  (ex_unit #{System.version()}) lib/ex_unit"

    assert capture_io(:stderr, fn -> IO.warn("hello", []) end) =~ "hello\n"

    stacktrace = [{IEx.Evaluator, :eval, 4, [file: 'lib/iex/evaluator.ex', line: 108]}]

    assert capture_io(:stderr, fn -> IO.warn("hello", stacktrace) end) =~ """
           hello
             lib/iex/evaluator.ex:108: IEx.Evaluator.eval/4
           """
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

  test "getn with different arities" do
    assert capture_io("hello", fn ->
             input = IO.getn(">")
             IO.write(input)
           end) == ">h"

    assert capture_io("hello", fn ->
             input = IO.getn(">", 3)
             IO.write(input)
           end) == ">hel"

    assert capture_io("hello", fn ->
             input = IO.getn(Process.group_leader(), ">")
             IO.write(input)
           end) == ">h"

    assert capture_io("hello", fn ->
             input = IO.getn(Process.group_leader(), ">")
             IO.write(input)
           end) == ">h"

    assert capture_io("hello", fn ->
             input = IO.getn(Process.group_leader(), ">", 99)
             IO.write(input)
           end) == ">hello"
  end

  test "inspect" do
    assert capture_io(fn -> IO.inspect(1) end) == "1\n"
    assert capture_io(fn -> IO.inspect(1, label: "foo") end) == "foo: 1\n"
    assert capture_io(fn -> IO.inspect(1, label: :foo) end) == "foo: 1\n"
  end
end
