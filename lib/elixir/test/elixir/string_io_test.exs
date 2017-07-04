Code.require_file "test_helper.exs", __DIR__

defmodule StringIOTest do
  use ExUnit.Case, async: true

  doctest StringIO

  test "open and close" do
    {:ok, pid} = StringIO.open("")
    assert StringIO.close(pid) == {:ok, {"", ""}}
  end

  test "contents" do
    {:ok, pid} = StringIO.open("abc")
    IO.write(pid, "edf")
    assert StringIO.contents(pid) == {"abc", "edf"}
  end

  test "flush" do
    {:ok, pid} = StringIO.open("")
    IO.write(pid, "edf")
    assert StringIO.flush(pid) == "edf"
    assert StringIO.contents(pid) == {"", ""}
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
    assert contents(pid) == {"", ""}
  end

  test "IO.read :line with \\rn" do
    pid = start("abc\r\n")
    assert IO.read(pid, :line) == "abc\n"
    assert IO.read(pid, :line) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.read :line without line break" do
    pid = start("abc")
    assert IO.read(pid, :line) == "abc"
    assert IO.read(pid, :line) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.read :line with UTF-8" do
    pid = start("⼊\n")
    assert IO.read(pid, :line) == "⼊\n"
    assert IO.read(pid, :line) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.read :line with invalid UTF-8" do
    pid = start(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.read(pid, :line) == {:error, :collect_line}
    assert contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.read count" do
    pid = start("abc")
    assert IO.read(pid, 2) == "ab"
    assert IO.read(pid, 8) == "c"
    assert IO.read(pid, 1) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.read count with UTF-8" do
    pid = start("あいう")
    assert IO.read(pid, 2) == "あい"
    assert IO.read(pid, 8) == "う"
    assert IO.read(pid, 1) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.read count with invalid UTF-8" do
    pid = start(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.read(pid, 2) == {:error, :invalid_unicode}
    assert contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.binread :line with \\n" do
    pid = start("abc\n")
    assert IO.binread(pid, :line) == "abc\n"
    assert IO.binread(pid, :line) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.binread :line with \\r\\n" do
    pid = start("abc\r\n")
    assert IO.binread(pid, :line) == "abc\n"
    assert IO.binread(pid, :line) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.binread :line without line break" do
    pid = start("abc")
    assert IO.binread(pid, :line) == "abc"
    assert IO.binread(pid, :line) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.binread :line with raw bytes" do
    pid = start(<<181, 255, 194, ?\n>>)
    assert IO.binread(pid, :line) == <<181, 255, 194, ?\n>>
    assert IO.binread(pid, :line) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.binread count" do
    pid = start("abc")
    assert IO.binread(pid, 2) == "ab"
    assert IO.binread(pid, 8) == "c"
    assert IO.binread(pid, 1) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.binread count with UTF-8" do
    pid = start("あいう")
    assert IO.binread(pid, 2) == <<227, 129>>
    assert IO.binread(pid, 8) == <<130, 227, 129, 132, 227, 129, 134>>
    assert IO.binread(pid, 1) == :eof
    assert contents(pid) == {"", ""}
  end

  test "IO.write" do
    pid = start("")
    assert IO.write(pid, "foo") == :ok
    assert contents(pid) == {"", "foo"}
  end

  test "IO.write with UTF-8" do
    pid = start("")
    assert IO.write(pid, "あいう") == :ok
    assert contents(pid) == {"", "あいう"}
  end

  test "IO.binwrite" do
    pid = start("")
    assert IO.binwrite(pid, "foo") == :ok
    assert contents(pid) == {"", "foo"}
  end

  test "IO.binwrite with UTF-8" do
    pid = start("")
    assert IO.binwrite(pid, "あいう") == :ok
    assert contents(pid) == {"", <<195, 163, 194, 129, 194, 130, 195, 163, 194, 129, 194, 132, 195, 163, 194, 129, 194, 134>>}
  end

  test "IO.puts" do
    pid = start("")
    assert IO.puts(pid, "abc") == :ok
    assert contents(pid) == {"", "abc\n"}
  end

  test "IO.inspect" do
    pid = start("")
    assert IO.inspect(pid, {}, []) == {}
    assert contents(pid) == {"", "{}\n"}
  end

  test "IO.getn" do
    pid = start("abc")
    assert IO.getn(pid, ">", 2) == "ab"
    assert contents(pid) == {"c", ""}
  end

  test "IO.getn with UTF-8" do
    pid = start("あいう")
    assert IO.getn(pid, ">", 2) == "あい"
    assert contents(pid) == {"う", ""}
  end

  test "IO.getn with invalid UTF-8" do
    pid = start(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.getn(pid, ">", 2) == {:error, :invalid_unicode}
    assert contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.getn with capture_prompt" do
    pid = start("abc", capture_prompt: true)
    assert IO.getn(pid, ">", 2) == "ab"
    assert contents(pid) == {"c", ">"}
  end

  test "IO.gets with \\n" do
    pid = start("abc\nd")
    assert IO.gets(pid, ">") == "abc\n"
    assert contents(pid) == {"d", ""}
  end

  test "IO.gets with \\r\\n" do
    pid = start("abc\r\nd")
    assert IO.gets(pid, ">") == "abc\n"
    assert contents(pid) == {"d", ""}
  end

  test "IO.gets without line breaks" do
    pid = start("abc")
    assert IO.gets(pid, ">") == "abc"
    assert contents(pid) == {"", ""}
  end

  test "IO.gets with invalid UTF-8" do
    pid = start(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.gets(pid, ">") == {:error, :collect_line}
    assert contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.gets with capture_prompt" do
    pid = start("abc\n", capture_prompt: true)
    assert IO.gets(pid, ">") == "abc\n"
    assert contents(pid) == {"", ">"}
  end

  test ":io.get_password" do
    pid = start("abc\n")
    assert :io.get_password(pid) == "abc\n"
    assert contents(pid) == {"", ""}
  end

  test "IO.stream" do
    pid = start("abc")
    assert IO.stream(pid, 2) |> Enum.to_list == ["ab", "c"]
    assert contents(pid) == {"", ""}
  end

  test "IO.stream with invalid UTF-8" do
    pid = start(<<130, 227, 129, 132, 227, 129, 134>>)
    assert_raise IO.StreamError, fn->
      IO.stream(pid, 2) |> Enum.to_list
    end
    assert contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.binstream" do
    pid = start("abc")
    assert IO.stream(pid, 2) |> Enum.to_list == ["ab", "c"]
    assert contents(pid) == {"", ""}
  end

  defp get_until(pid, encoding, prompt, module, function, extra_args \\ []) do
    :io.request(pid, {:get_until, encoding, prompt, module, function, extra_args})
  end

  defmodule GetUntilCallbacks do
    def until_eof(continuation, :eof) do
      {:done, continuation, :eof}
    end

    def until_eof(continuation, content) do
      {:more, continuation ++ content}
    end

    def until_eof_then_try_more('magic-stop-prefix' ++ continuation, :eof) do
      {:done, continuation, :eof}
    end

    def until_eof_then_try_more(continuation, :eof) do
      {:more, 'magic-stop-prefix' ++ continuation}
    end

    def until_eof_then_try_more(continuation, content) do
      {:more, continuation ++ content}
    end

    def up_to_3_bytes(continuation, :eof) do
      {:done, continuation, :eof}
    end

    def up_to_3_bytes(continuation, content) do
      case continuation ++ content do
        [a, b, c | tail] -> {:done, [a, b, c], tail}
        str -> {:more, str}
      end
    end

    def up_to_3_bytes_discard_rest(continuation, :eof) do
      {:done, continuation, :eof}
    end

    def up_to_3_bytes_discard_rest(continuation, content) do
      case continuation ++ content do
        [a, b, c | _tail] -> {:done, [a, b, c], :eof}
        str -> {:more, str}
      end
    end
  end

  test "get_until with up_to_3_bytes" do
    pid = start("abcdefg")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :up_to_3_bytes)
    assert result == "abc"
    assert IO.read(pid, :all) == "defg"
  end

  test "get_until with up_to_3_bytes_discard_rest" do
    pid = start("abcdefg")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :up_to_3_bytes_discard_rest)
    assert result == "abc"
    assert IO.read(pid, :all) == ""
  end

  test "get_until with until_eof" do
    pid = start("abc\nd")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof)
    assert result == "abc\nd"
  end

  test "get_until with until_eof and \\r\\n" do
    pid = start("abc\r\nd")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof)
    assert result == "abc\r\nd"
  end

  test "get_until with until_eof capturing prompt" do
    pid = start("abc\nd", capture_prompt: true)
    result = get_until(pid, :unicode, ">", GetUntilCallbacks, :until_eof)
    assert result == "abc\nd"
    assert StringIO.contents(pid) == {"", ">>>"}
  end

  test "get_until with until_eof_then_try_more" do
    pid = start("abc\nd")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof_then_try_more)
    assert result == "abc\nd"
  end

  test "get_until with invalid UTF-8" do
    pid = start(<<130, 227, 129, 132, 227, 129, 134>>)
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof)
    assert result == :error
  end

  test "get_until with raw bytes (latin1)" do
    pid = start(<<181, 255, 194, ?\n>>)
    result = get_until(pid, :latin1, "", GetUntilCallbacks, :until_eof)
    assert result == <<181, 255, 194, ?\n>>
  end

  test ":io.erl_scan_form/2" do
    pid = start("1.")
    result = :io.scan_erl_form(pid, 'p>')
    assert result == {:ok, [{:integer, 1, 1}, {:dot, 1}], 1}
    assert StringIO.contents(pid) == {"", ""}
  end

  test ":io.erl_scan_form/2 with capture_prompt" do
    pid = start("1.", capture_prompt: true)
    result = :io.scan_erl_form(pid, 'p>')
    assert result == {:ok, [{:integer, 1, 1}, {:dot, 1}], 1}
    assert StringIO.contents(pid) == {"", "p>p>"}
  end
end
