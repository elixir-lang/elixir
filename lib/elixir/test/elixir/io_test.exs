Code.require_file("test_helper.exs", __DIR__)

defmodule IOTest do
  use ExUnit.Case, async: true

  doctest IO

  import ExUnit.CaptureIO

  test "read with count" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.txt", __DIR__), [:charlist])
    assert ~c"FOO" == IO.read(file, 3)
    assert File.close(file) == :ok
  end

  test "read with UTF-8 and binary" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__), [:utf8])
    assert "Русский" == IO.read(file, 7)
    assert File.close(file) == :ok
  end

  test "read all charlist" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/multiline_file.txt", __DIR__), [:charlist])
    assert ~c"this is the first line\nthis is the second line\n" == IO.read(file, :eof)
    assert File.close(file) == :ok
  end

  test "read empty file" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/cp_mode", __DIR__), [])
    assert IO.read(file, :eof) == :eof
    assert File.close(file) == :ok

    {:ok, file} = File.open(Path.expand(~c"fixtures/cp_mode", __DIR__), [:charlist])
    assert IO.read(file, :eof) == :eof
    assert File.close(file) == :ok
  end

  test "binread" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__))
    assert "Русский" == IO.binread(file, 14)
    assert File.close(file) == :ok
  end

  test "binread eof" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.bin", __DIR__))
    assert "LF\nCR\rCRLF\r\nLFCR\n\r" == IO.binread(file, :eof)
    assert File.close(file) == :ok
  end

  test "getn" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.txt", __DIR__))
    assert "F" == IO.getn(file, "")
    assert "O" == IO.getn(file, "")
    assert "O" == IO.getn(file, "")
    assert "\n" == IO.getn(file, "")
    assert :eof == IO.getn(file, "")
    assert File.close(file) == :ok
  end

  test "getn with count" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.txt", __DIR__), [:charlist])
    assert ~c"F" == IO.getn(file, "λ")
    assert ~c"OO" == IO.getn(file, "", 2)
    assert ~c"\n" == IO.getn(file, "λ", 99)
    assert :eof == IO.getn(file, "λ", 1)
    assert File.close(file) == :ok
  end

  test "getn with eof" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.txt", __DIR__), [:charlist])
    assert ~c"F" == IO.getn(file, "λ")
    assert ~c"OO\n" == IO.getn(file, "", :eof)
    assert :eof == IO.getn(file, "", :eof)
    assert File.close(file) == :ok
  end

  test "getn with UTF-8 and binary" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__), [:utf8])
    assert "Русский" == IO.getn(file, "", 7)
    assert "\n日\n" == IO.getn(file, "", :eof)
    assert :eof == IO.getn(file, "", :eof)
    assert File.close(file) == :ok
  end

  test "gets" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.txt", __DIR__), [:charlist])
    assert ~c"FOO\n" == IO.gets(file, "")
    assert :eof == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test "gets with UTF-8 and binary" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__), [:utf8])
    assert "Русский\n" == IO.gets(file, "")
    assert "日\n" == IO.gets(file, "")
    assert File.close(file) == :ok
  end

  test "read with eof" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.txt", __DIR__))
    assert "FOO\n" == IO.read(file, :eof)
    assert :eof == IO.read(file, :eof)
    assert File.close(file) == :ok
  end

  test "read with eof and UTF-8 and binary" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__), [:utf8])
    assert "Русский\n日\n" == IO.read(file, :eof)
    assert :eof == IO.read(file, :eof)
    assert File.close(file) == :ok
  end

  test "readline" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/file.txt", __DIR__))
    assert "FOO\n" == IO.read(file, :line)
    assert :eof == IO.read(file, :line)
    assert File.close(file) == :ok
  end

  test "readline with UTF-8 and binary" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__), [:utf8])
    assert "Русский\n" == IO.read(file, :line)
    assert "日\n" == IO.read(file, :line)
    assert File.close(file) == :ok
  end

  test "binread with eof" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__))
    assert "Русский\n日\n" == IO.binread(file, :eof)
    assert :eof == IO.binread(file, :eof)
    assert File.close(file) == :ok
  end

  test "binread with line" do
    {:ok, file} = File.open(Path.expand(~c"fixtures/utf8.txt", __DIR__))
    assert "Русский\n" == IO.binread(file, :line)
    assert "日\n" == IO.binread(file, :line)
    assert File.close(file) == :ok
  end

  test "puts with chardata" do
    assert capture_io(fn -> IO.puts("hello") end) == "hello\n"
    assert capture_io(fn -> IO.puts(~c"hello") end) == "hello\n"
    assert capture_io(fn -> IO.puts(:hello) end) == "hello\n"
    assert capture_io(fn -> IO.puts(13) end) == "13\n"
  end

  describe "warn" do
    test "with chardata" do
      capture_io(:stderr, fn -> IO.warn("hello") end)
      |> assert_emits(["hello", "(ex_unit #{System.version()}) lib/ex_unit"])

      capture_io(:stderr, fn -> IO.warn(~c"hello") end)
      |> assert_emits(["hello", "(ex_unit #{System.version()}) lib/ex_unit"])

      capture_io(:stderr, fn -> IO.warn(:hello) end)
      |> assert_emits(["hello", "(ex_unit #{System.version()}) lib/ex_unit"])

      capture_io(:stderr, fn -> IO.warn(13) end)
      |> assert_emits(["13", "(ex_unit #{System.version()}) lib/ex_unit"])
    end

    test "no stacktrace" do
      assert capture_io(:stderr, fn -> IO.warn("hello", []) end) =~ "hello\n"
    end

    test "with stacktrace" do
      stacktrace = [{IEx.Evaluator, :eval, 4, [file: ~c"lib/iex/evaluator.ex", line: 108]}]

      output = capture_io(:stderr, fn -> IO.warn("hello", stacktrace) end)

      assert output =~ "hello"
      assert output =~ "lib/iex/evaluator.ex:108: IEx.Evaluator.eval/4"
    end

    test "with env" do
      output = capture_io(:stderr, fn -> IO.warn("hello", __ENV__) end)

      assert output =~ "hello"

      assert output =~
               ~r"(lib/elixir/)?test/elixir/io_test.exs:#{__ENV__.line - 5}: IOTest.\"test warn with env\"/1"
    end

    test "with options" do
      capture_io(:stderr, fn ->
        IO.warn("hello", line: 13, file: "lib/foo.ex", module: Foo, function: {:bar, 1})
      end)
      |> assert_emits(["hello", "lib/foo.ex:13: Foo.bar/1"])

      capture_io(:stderr, fn ->
        IO.warn("hello", file: "lib/foo.ex", module: Foo, function: {:bar, 1})
      end)
      |> assert_emits(["hello", "lib/foo.ex: Foo.bar/1"])

      capture_io(:stderr, fn -> IO.warn("hello", file: "lib/foo.ex", module: Foo) end)
      |> assert_emits(["hello", "lib/foo.ex: Foo (module)"])

      capture_io(:stderr, fn -> IO.warn("hello", file: "lib/foo.ex") end)
      |> assert_emits(["hello", "lib/foo.ex: (file)"])

      capture_io(:stderr, fn ->
        IO.warn("hello", file: "lib/foo.ex", function: {:bar, 1})
      end)
      |> assert_emits(["hello", "lib/foo.ex: (file)"])

      assert capture_io(:stderr, fn ->
               IO.warn("hello", line: 13, module: Foo, function: {:bar, 1})
             end) =~ "hello"
    end
  end

  test "write with chardata" do
    assert capture_io(fn -> IO.write("hello") end) == "hello"
    assert capture_io(fn -> IO.write(~c"hello") end) == "hello"
    assert capture_io(fn -> IO.write(:hello) end) == "hello"
    assert capture_io(fn -> IO.write(13) end) == "13"
  end

  test "gets with chardata" do
    assert capture_io("foo\n", fn -> IO.gets("hello") end) == "hello"
    assert capture_io("foo\n", fn -> IO.gets(~c"hello") end) == "hello"
    assert capture_io("foo\n", fn -> IO.gets(:hello) end) == "hello"
    assert capture_io("foo\n", fn -> IO.gets(13) end) == "13"
  end

  test "getn with chardata" do
    assert capture_io("foo\n", fn -> IO.getn("hello", 3) end) == "hello"
    assert capture_io("foo\n", fn -> IO.getn(~c"hello", 3) end) == "hello"
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

  test "stream" do
    assert IO.stream() == IO.stream(:stdio, :line)
    assert IO.binstream() == IO.binstream(:stdio, :line)
  end

  defp assert_emits(output, messages) do
    for m <- messages do
      assert output =~ m
    end
  end
end
