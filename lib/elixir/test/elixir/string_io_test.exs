Code.require_file("test_helper.exs", __DIR__)

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

  test "IO.read :line with \\n" do
    {:ok, pid} = StringIO.open("abc\n")
    assert IO.read(pid, :line) == "abc\n"
    assert IO.read(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.read :line with \\rn" do
    {:ok, pid} = StringIO.open("abc\r\n")
    assert IO.read(pid, :line) == "abc\n"
    assert IO.read(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.read :line without line break" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.read(pid, :line) == "abc"
    assert IO.read(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.read :line with UTF-8" do
    {:ok, pid} = StringIO.open("⼊\n")
    assert IO.read(pid, :line) == "⼊\n"
    assert IO.read(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.read :line with invalid UTF-8" do
    {:ok, pid} = StringIO.open(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.read(pid, :line) == {:error, :collect_line}
    assert StringIO.contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.read count" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.read(pid, 2) == "ab"
    assert IO.read(pid, 8) == "c"
    assert IO.read(pid, 1) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.read count with UTF-8" do
    {:ok, pid} = StringIO.open("あいう")
    assert IO.read(pid, 2) == "あい"
    assert IO.read(pid, 8) == "う"
    assert IO.read(pid, 1) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.read count with invalid UTF-8" do
    {:ok, pid} = StringIO.open(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.read(pid, 2) == {:error, :invalid_unicode}
    assert StringIO.contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.binread :line with \\n" do
    {:ok, pid} = StringIO.open("abc\n")
    assert IO.binread(pid, :line) == "abc\n"
    assert IO.binread(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.binread :line with \\r\\n" do
    {:ok, pid} = StringIO.open("abc\r\n")
    assert IO.binread(pid, :line) == "abc\n"
    assert IO.binread(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.binread :line without line break" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.binread(pid, :line) == "abc"
    assert IO.binread(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.binread :line with raw bytes" do
    {:ok, pid} = StringIO.open(<<181, 255, 194, ?\n>>)
    assert IO.binread(pid, :line) == <<181, 255, 194, ?\n>>
    assert IO.binread(pid, :line) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.binread count" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.binread(pid, 2) == "ab"
    assert IO.binread(pid, 8) == "c"
    assert IO.binread(pid, 1) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.binread count with UTF-8" do
    {:ok, pid} = StringIO.open("あいう")
    assert IO.binread(pid, 2) == <<227, 129>>
    assert IO.binread(pid, 8) == <<130, 227, 129, 132, 227, 129, 134>>
    assert IO.binread(pid, 1) == :eof
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.write" do
    {:ok, pid} = StringIO.open("")
    assert IO.write(pid, "foo") == :ok
    assert StringIO.contents(pid) == {"", "foo"}
  end

  test "IO.write with UTF-8" do
    {:ok, pid} = StringIO.open("")
    assert IO.write(pid, "あいう") == :ok
    assert StringIO.contents(pid) == {"", "あいう"}
  end

  test "IO.write with non-printable arguments" do
    {:ok, pid} = StringIO.open("")

    assert_raise ArgumentError, fn ->
      IO.write(pid, [<<1::1>>])
    end
  end

  test "IO.binwrite" do
    {:ok, pid} = StringIO.open("")
    assert IO.binwrite(pid, "foo") == :ok
    assert StringIO.contents(pid) == {"", "foo"}
  end

  test "IO.binwrite with UTF-8" do
    {:ok, pid} = StringIO.open("")
    assert IO.binwrite(pid, "あいう") == :ok

    binary =
      <<195, 163, 194, 129, 194, 130, 195, 163>> <>
        <<194, 129, 194, 132, 195, 163, 194, 129, 194, 134>>

    assert StringIO.contents(pid) == {"", binary}
  end

  test "IO.binwrite with bytes" do
    {:ok, pid} = StringIO.open("")
    assert IO.binwrite(pid, <<127, 128>>) == :ok
    assert StringIO.contents(pid) == {"", <<127, 194, 128>>}
  end

  test "IO.binwrite with bytes and latin1 encoding" do
    {:ok, pid} = StringIO.open("")
    assert :io.setopts(pid, encoding: :latin1) == :ok
    assert IO.binwrite(pid, <<127, 128>>) == :ok
    assert StringIO.contents(pid) == {"", <<127, 128>>}
  end

  test "IO.puts" do
    {:ok, pid} = StringIO.open("")
    assert IO.puts(pid, "abc") == :ok
    assert StringIO.contents(pid) == {"", "abc\n"}
  end

  test "IO.puts with non-printable arguments" do
    {:ok, pid} = StringIO.open("")

    assert_raise ArgumentError, fn ->
      IO.puts(pid, [<<1::1>>])
    end
  end

  test "IO.inspect" do
    {:ok, pid} = StringIO.open("")
    assert IO.inspect(pid, {}, []) == {}
    assert StringIO.contents(pid) == {"", "{}\n"}
  end

  test "IO.getn" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.getn(pid, ">", 2) == "ab"
    assert StringIO.contents(pid) == {"c", ""}
  end

  test "IO.getn with UTF-8" do
    {:ok, pid} = StringIO.open("あいう")
    assert IO.getn(pid, ">", 2) == "あい"
    assert StringIO.contents(pid) == {"う", ""}
  end

  test "IO.getn with invalid UTF-8" do
    {:ok, pid} = StringIO.open(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.getn(pid, ">", 2) == {:error, :invalid_unicode}
    assert StringIO.contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.getn with capture_prompt" do
    {:ok, pid} = StringIO.open("abc", capture_prompt: true)
    assert IO.getn(pid, ">", 2) == "ab"
    assert StringIO.contents(pid) == {"c", ">"}
  end

  test "IO.gets with \\n" do
    {:ok, pid} = StringIO.open("abc\nd")
    assert IO.gets(pid, ">") == "abc\n"
    assert StringIO.contents(pid) == {"d", ""}
  end

  test "IO.gets with \\r\\n" do
    {:ok, pid} = StringIO.open("abc\r\nd")
    assert IO.gets(pid, ">") == "abc\n"
    assert StringIO.contents(pid) == {"d", ""}
  end

  test "IO.gets without line breaks" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.gets(pid, ">") == "abc"
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.gets with invalid UTF-8" do
    {:ok, pid} = StringIO.open(<<130, 227, 129, 132, 227, 129, 134>>)
    assert IO.gets(pid, ">") == {:error, :collect_line}
    assert StringIO.contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.gets with capture_prompt" do
    {:ok, pid} = StringIO.open("abc\n", capture_prompt: true)
    assert IO.gets(pid, ">") == "abc\n"
    assert StringIO.contents(pid) == {"", ">"}
  end

  test ":io.get_password" do
    {:ok, pid} = StringIO.open("abc\n")
    assert :io.get_password(pid) == "abc\n"
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.stream" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.stream(pid, 2) |> Enum.to_list() == ["ab", "c"]
    assert StringIO.contents(pid) == {"", ""}
  end

  test "IO.stream with invalid UTF-8" do
    {:ok, pid} = StringIO.open(<<130, 227, 129, 132, 227, 129, 134>>)

    assert_raise IO.StreamError, fn ->
      IO.stream(pid, 2) |> Enum.to_list()
    end

    assert StringIO.contents(pid) == {<<130, 227, 129, 132, 227, 129, 134>>, ""}
  end

  test "IO.binstream" do
    {:ok, pid} = StringIO.open("abc")
    assert IO.stream(pid, 2) |> Enum.to_list() == ["ab", "c"]
    assert StringIO.contents(pid) == {"", ""}
  end

  defp get_until(pid, encoding, prompt, module, function) do
    :io.request(pid, {:get_until, encoding, prompt, module, function, []})
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
    {:ok, pid} = StringIO.open("abcdefg")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :up_to_3_bytes)
    assert result == "abc"
    assert IO.read(pid, :all) == "defg"
  end

  test "get_until with up_to_3_bytes_discard_rest" do
    {:ok, pid} = StringIO.open("abcdefg")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :up_to_3_bytes_discard_rest)
    assert result == "abc"
    assert IO.read(pid, :all) == ""
  end

  test "get_until with until_eof" do
    {:ok, pid} = StringIO.open("abc\nd")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof)
    assert result == "abc\nd"
  end

  test "get_until with until_eof and \\r\\n" do
    {:ok, pid} = StringIO.open("abc\r\nd")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof)
    assert result == "abc\r\nd"
  end

  test "get_until with until_eof capturing prompt" do
    {:ok, pid} = StringIO.open("abc\nd", capture_prompt: true)
    result = get_until(pid, :unicode, ">", GetUntilCallbacks, :until_eof)
    assert result == "abc\nd"
    assert StringIO.contents(pid) == {"", ">>>"}
  end

  test "get_until with until_eof_then_try_more" do
    {:ok, pid} = StringIO.open("abc\nd")
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof_then_try_more)
    assert result == "abc\nd"
  end

  test "get_until with invalid UTF-8" do
    {:ok, pid} = StringIO.open(<<130, 227, 129, 132, 227, 129, 134>>)
    result = get_until(pid, :unicode, "", GetUntilCallbacks, :until_eof)
    assert result == :error
  end

  test "get_until with raw bytes (latin1)" do
    {:ok, pid} = StringIO.open(<<181, 255, 194, ?\n>>)
    result = get_until(pid, :latin1, "", GetUntilCallbacks, :until_eof)
    assert result == <<181, 255, 194, ?\n>>
  end

  test ":io.erl_scan_form/2" do
    {:ok, pid} = StringIO.open("1.")
    result = :io.scan_erl_form(pid, 'p>')
    assert result == {:ok, [{:integer, 1, 1}, {:dot, 1}], 1}
    assert StringIO.contents(pid) == {"", ""}
  end

  test ":io.erl_scan_form/2 with capture_prompt" do
    {:ok, pid} = StringIO.open("1.", capture_prompt: true)
    result = :io.scan_erl_form(pid, 'p>')
    assert result == {:ok, [{:integer, 1, 1}, {:dot, 1}], 1}
    assert StringIO.contents(pid) == {"", "p>p>"}
  end
end
