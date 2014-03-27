Code.require_file "test_helper.exs", __DIR__

defmodule StringIOTest do
  use ExUnit.Case, async: true

  test "start and stop" do
    { :ok, pid } = StringIO.open("")
    assert StringIO.close(pid) == { :ok, { "", "" } }
  end

  test "start_link and stop" do
    { :ok, pid } = StringIO.open("")
    assert StringIO.close(pid) == { :ok, { "", "" } }
  end

  test "peek" do
    { :ok, pid } = StringIO.open("abc")
    IO.write(pid, "edf")
    assert StringIO.contents(pid) == { "abc", "edf" }
  end

  ## IO module

  def start(string, opts \\ []) do
    StringIO.open(string, opts) |> elem(1)
  end

  def contents(pid) do
    StringIO.contents(pid)
  end

  test "IO.read :line with \\n" do
    pid = start("abc\n")
    assert IO.read(pid, :line) == "abc\n"
    assert IO.read(pid, :line) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.read :line with \\rn" do
    pid = start("abc\r\n")
    assert IO.read(pid, :line) == "abc\n"
    assert IO.read(pid, :line) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.read :line without line break" do
    pid = start("abc")
    assert IO.read(pid, :line) == "abc"
    assert IO.read(pid, :line) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.read :line with invalid utf8" do
    pid = start(<< 130, 227, 129, 132, 227, 129, 134 >>)
    assert IO.read(pid, :line) == { :error, :collect_line }
    assert contents(pid) == { << 130, 227, 129, 132, 227, 129, 134 >>, "" }
  end

  test "IO.read count" do
    pid = start("abc")
    assert IO.read(pid, 2) == "ab"
    assert IO.read(pid, 8) == "c"
    assert IO.read(pid, 1) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.read count with utf8" do
    pid = start("あいう")
    assert IO.read(pid, 2) == "あい"
    assert IO.read(pid, 8) == "う"
    assert IO.read(pid, 1) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.read count with invalid utf8" do
    pid = start(<< 130, 227, 129, 132, 227, 129, 134 >>)
    assert IO.read(pid, 2) == { :error, :invalid_unicode }
    assert contents(pid) == { << 130, 227, 129, 132, 227, 129, 134 >>, "" }
  end

  test "IO.binread :line with \\n" do
    pid = start("abc\n")
    assert IO.binread(pid, :line) == "abc\n"
    assert IO.binread(pid, :line) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.binread :line with \\r\\n" do
    pid = start("abc\r\n")
    assert IO.binread(pid, :line) == "abc\n"
    assert IO.binread(pid, :line) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.binread :line without line break" do
    pid = start("abc")
    assert IO.binread(pid, :line) == "abc"
    assert IO.binread(pid, :line) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.binread count" do
    pid = start("abc")
    assert IO.binread(pid, 2) == "ab"
    assert IO.binread(pid, 8) == "c"
    assert IO.binread(pid, 1) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.binread count with utf8" do
    pid = start("あいう")
    assert IO.binread(pid, 2) == << 227, 129 >>
    assert IO.binread(pid, 8) == << 130, 227, 129, 132, 227, 129, 134 >>
    assert IO.binread(pid, 1) == :eof
    assert contents(pid) == { "", "" }
  end

  test "IO.write" do
    pid = start("")
    assert IO.write(pid, "foo") == :ok
    assert contents(pid) == { "", "foo" }
  end

  test "IO.write with utf8" do
    pid = start("")
    assert IO.write(pid, "あいう") == :ok
    assert contents(pid) == { "", "あいう" }
  end

  test "IO.binwrite" do
    pid = start("")
    assert IO.binwrite(pid, "foo") == :ok
    assert contents(pid) == { "", "foo" }
  end

  test "IO.binwrite with utf8" do
    pid = start("")
    assert IO.binwrite(pid, "あいう") == :ok
    assert contents(pid) == { "", "あいう" }
  end

  test "IO.puts" do
    pid = start("")
    assert IO.puts(pid, "abc") == :ok
    assert contents(pid) == { "", "abc\n" }
  end

  test "IO.inspect" do
    pid = start("")
    assert IO.inspect(pid, {}, []) == {}
    assert contents(pid) == { "", "{}\n" }
  end

  test "IO.getn" do
    pid = start("abc")
    assert IO.getn(pid, ">", 2) == "ab"
    assert contents(pid) == { "c", "" }
  end

  test "IO.getn with utf8" do
    pid = start("あいう")
    assert IO.getn(pid, ">", 2) == "あい"
    assert contents(pid) == { "う", "" }
  end

  test "IO.getn with invalid utf8" do
    pid = start(<< 130, 227, 129, 132, 227, 129, 134 >>)
    assert IO.getn(pid, ">", 2) == { :error, :invalid_unicode }
    assert contents(pid) == { << 130, 227, 129, 132, 227, 129, 134 >>, "" }
  end

  test "IO.getn with capture_prompt" do
    pid = start("abc", capture_prompt: true)
    assert IO.getn(pid, ">", 2) == "ab"
    assert contents(pid) == { "c", ">" }
  end

  test "IO.gets with \\n" do
    pid = start("abc\nd")
    assert IO.gets(pid, ">") == "abc\n"
    assert contents(pid) == { "d", "" }
  end

  test "IO.gets with \\r\\n" do
    pid = start("abc\r\nd")
    assert IO.gets(pid, ">") == "abc\n"
    assert contents(pid) == { "d", "" }
  end

  test "IO.gets without line breaks" do
    pid = start("abc")
    assert IO.gets(pid, ">") == "abc"
    assert contents(pid) == { "", "" }
  end

  test "IO.gets with invalid utf8" do
    pid = start(<< 130, 227, 129, 132, 227, 129, 134 >>)
    assert IO.gets(pid, ">") == { :error, :collect_line }
    assert contents(pid) == { << 130, 227, 129, 132, 227, 129, 134 >>, "" }
  end

  test "IO.gets with capture_prompt" do
    pid = start("abc\n", capture_prompt: true)
    assert IO.gets(pid, ">") == "abc\n"
    assert contents(pid) == { "", ">" }
  end

  test ":io.get_password" do
    pid = start("abc\n")
    assert :io.get_password(pid) == "abc\n"
    assert contents(pid) == { "", "" }
  end

  test "IO.stream" do
    pid = start("abc")
    assert IO.stream(pid, 2) |> Enum.to_list == ["ab", "c"]
    assert contents(pid) == { "", "" }
  end

  test "IO.stream with invalid utf8" do
    pid = start(<< 130, 227, 129, 132, 227, 129, 134 >>)
    assert_raise IO.StreamError, fn->
      IO.stream(pid, 2) |> Enum.to_list
    end
    assert contents(pid) == { << 130, 227, 129, 132, 227, 129, 134 >>, "" }
  end

  test "IO.binstream" do
    pid = start("abc")
    assert IO.stream(pid, 2) |> Enum.to_list == ["ab", "c"]
    assert contents(pid) == { "", "" }
  end
end
