Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CaptureIOWithResultTest do
  use ExUnit.Case

  defmodule GetUntil do
    def until_new_line(_, :eof, _) do
      {:done, :eof, []}
    end

    def until_new_line(this_far, chars, stop_char) do
      case Enum.split_while(chars, fn(c) -> c != stop_char end) do
        {l, []} ->
          {:more, this_far ++ l}
        {l, [stop_char|rest]} ->
          {:done, this_far ++ l ++ [stop_char], rest}
      end
    end

    def get_line(device \\ Process.group_leader) do
      send device, {:io_request, self, device, {:get_until, :unicode, "", __MODULE__, :until_new_line, [?\n]}}
      receive do
        {:io_reply, _, data} -> data
      end
    end
  end

  import ExUnit.CaptureIO
  doctest ExUnit.CaptureIO, import: true

  test "no leakage on failures" do
    group_leader = Process.group_leader()

    test = self()
    assert_raise ArgumentError, fn ->
      capture_io_with_result(fn ->
        send(test, {:string_io, Process.group_leader()})
        raise ArgumentError
      end)
    end

    receive do
      {:string_io, pid} ->
        ref = Process.monitor(pid)
        assert_receive {:DOWN, ^ref, _, _, _}
    end
    assert Process.group_leader() == group_leader
  end

  test "with no output" do
    assert capture_io_with_result(fn -> nil end) == {"", nil}
  end

  test "with put chars" do
    assert capture_io_with_result(fn ->
      :io.put_chars("")
    end) == {"", :ok}

    assert capture_io_with_result(fn ->
      :io.put_chars("a")
      :io.put_chars("b")
    end) == {"ab", :ok}

    assert capture_io_with_result(fn ->
      :io.put_chars("josé")
    end) == {"josé", :ok}

    assert capture_io_with_result(fn ->
      spawn(fn -> :io.put_chars("a") end)
      :timer.sleep(10)
    end) == {"a", :ok}

    assert capture_io_with_result(fn ->
      assert :io.put_chars("a") == :ok
    end)
  end

  test "with put chars to stderr" do
    assert capture_io_with_result(:stderr, fn ->
      :io.put_chars(:standard_error, "a")
    end) == {"a", :ok}
  end

  test "with get chars" do
    assert capture_io_with_result(fn ->
      :io.get_chars(">", 3)
    end) == {">", :eof}

    assert capture_io_with_result([capture_prompt: false], fn ->
      :io.get_chars(">", 3)
    end) == {"", :eof}

    capture_io_with_result(fn ->
      assert :io.get_chars(">", 3) == :eof
    end)

    capture_io_with_result("", fn ->
      assert :io.get_chars(">", 3) == :eof
    end)

    capture_io_with_result("abc\ndef", fn ->
      assert :io.get_chars(">", 3) == "abc"
      assert :io.get_chars(">", 5) == "\ndef"
      assert :io.get_chars(">", 7) == :eof
    end)

    capture_io_with_result("あいう", fn ->
      assert :io.get_chars(">", 2) == "あい"
      assert :io.get_chars(">", 1) == "う"
      assert :io.get_chars(">", 1) == :eof
    end)
  end

  test "with get line" do
    assert capture_io_with_result(fn ->
      :io.get_line ">"
    end) == {">", :eof}

    assert capture_io_with_result([capture_prompt: false], fn ->
      :io.get_line ">"
    end) == {"", :eof}

    capture_io_with_result(fn ->
      assert :io.get_line(">") == :eof
    end)

    capture_io_with_result("", fn ->
      assert :io.get_line(">") == :eof
    end)

    capture_io_with_result("\n", fn ->
      assert :io.get_line(">") == "\n"
      assert :io.get_line(">") == :eof
    end)

    capture_io_with_result("a", fn ->
      assert :io.get_line(">") == "a"
      assert :io.get_line(">") == :eof
    end)

    capture_io_with_result("a\n", fn ->
      assert :io.get_line(">") == "a\n"
      assert :io.get_line(">") == :eof
    end)

    capture_io_with_result("a\nb", fn ->
      assert :io.get_line(">") == "a\n"
      assert :io.get_line(">") == "b"
      assert :io.get_line(">") == :eof
    end)

    capture_io_with_result("あい\nう", fn ->
      assert :io.get_line(">") == "あい\n"
      assert :io.get_line(">") == "う"
      assert :io.get_line(">") == :eof
    end)
  end

  test "with get password" do
    capture_io_with_result(fn ->
      assert :io.get_password() == :eof
    end)

    capture_io_with_result("", fn ->
      assert :io.get_password() == :eof
    end)

    capture_io_with_result("abc", fn ->
      assert :io.get_password() == "abc"
      assert :io.get_password() == :eof
    end)

    capture_io_with_result("abc\n", fn ->
      assert :io.get_password() == "abc\n"
      assert :io.get_password() == :eof
    end)

    capture_io_with_result("\n", fn ->
      assert :io.get_password() == "\n"
      assert :io.get_password() == :eof
    end)

    capture_io_with_result("a\nb", fn ->
      assert :io.get_password() == "a\n"
      assert :io.get_password() == "b"
      assert :io.get_password() == :eof
    end)

    capture_io_with_result("あい\nう", fn ->
      assert :io.get_password() == "あい\n"
      assert :io.get_password() == "う"
      assert :io.get_password() == :eof
    end)
  end

  test "with get until" do
    assert capture_io_with_result(fn ->
      :io.scan_erl_form('>')
    end) == {">", {:eof, 1}}

    assert capture_io_with_result("1.\n", fn ->
      :io.scan_erl_form('>')
    end) == {">", {:ok, [{:integer, 1, 1}, {:dot, 1}], 2}}

    assert capture_io_with_result("1\n.\n", fn ->
       :io.scan_erl_form('>')
    end) == {">>", {:ok, [{:integer, 1, 1}, {:dot, 2}], 3}}

    assert capture_io_with_result([capture_prompt: false], fn ->
      :io.scan_erl_form('>')
    end) == {"", {:eof, 1}}

    capture_io_with_result(fn ->
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

   capture_io_with_result("1", fn ->
     assert :io.scan_erl_form('>') == {:ok, [{:integer, 1, 1}], 1}
     assert :io.scan_erl_form('>') == {:eof, 1}
   end)

    capture_io_with_result("1\n.", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:integer, 1, 1}, {:dot, 2}], 2}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io_with_result("1.\n.", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:integer, 1, 1}, {:dot, 1}], 2}
      assert :io.scan_erl_form('>') == {:ok, [dot: 1], 1}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io_with_result("\"a", fn ->
      assert :io.scan_erl_form('>') == {:error, {1, :erl_scan, {:string, 34, 'a'}}, 1}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io_with_result("\"a\n\"", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:string, 1, 'a\n'}], 2}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io_with_result(":erl. mof*,,l", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:":", 1}, {:atom, 1, :erl}, {:dot, 1}], 1}
      assert :io.scan_erl_form('>') == {:ok, [{:atom, 1, :mof}, {:*, 1}, {:",", 1}, {:",", 1}, {:atom, 1, :l}], 1}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io_with_result("a\nb\nc", fn ->
      assert GetUntil.get_line == "a\n"
      assert GetUntil.get_line == "b\n"
      assert GetUntil.get_line == :eof
    end)
  end

  test "with setopts" do
    assert capture_io_with_result(fn ->
      assert :io.setopts({:encoding, :latin1}) == {:error, :enotsup}
    end) == {"", true}
  end

  test "with getopts" do
    assert capture_io_with_result(fn ->
      assert :io.getopts == {:ok, [binary: true, encoding: :unicode]}
    end) == {"", true}
  end

  test "with columns" do
    assert capture_io_with_result(fn ->
      :io.columns
    end) == {"", {:error, :enotsup}}

    capture_io_with_result(fn ->
      assert :io.columns == {:error, :enotsup}
    end)
  end

  test "with rows" do
    assert capture_io_with_result(fn ->
      :io.rows
    end) == {"", {:error, :enotsup}}

    capture_io_with_result(fn ->
      assert :io.rows == {:error, :enotsup}
    end)
  end

  test "with multiple io requests" do
    assert capture_io_with_result(fn ->
      send_and_receive_io({:requests, [{:put_chars, :unicode, "a"},
                                        {:put_chars, :unicode, "b"}]})
    end) == {"ab", :ok}

    capture_io_with_result(fn ->
      assert send_and_receive_io({:requests, [{:put_chars, :unicode, "a"},
                                               {:put_chars, :unicode, "b"}]}) == :ok
    end)
  end

  test "with unknown io request" do
    assert capture_io_with_result(fn ->
      send_and_receive_io(:unknown)
    end) == {"", {:error, :request}}

    capture_io_with_result(fn ->
      assert send_and_receive_io(:unknown) == {:error, :request}
    end)
  end

  test "device re-registering" do
    {_pid, ref} = spawn_monitor(fn ->
      capture_io_with_result(:stderr, fn ->
        spawn_link(Kernel, :exit, [:shutdown])
        :timer.sleep(:infinity)
      end)
    end)

    # Assert the process is down then invoke capture_io
    # to trigger the ExUnit.Server, ensuring the DOWN
    # message from previous capture_io has been processed
    assert_receive {:DOWN, ^ref, _, _, :shutdown}
    _ = capture_io_with_result(fn -> "trigger" end)
    assert capture_io_with_result(:stderr, fn -> nil end)
  end

  test "with assert inside" do
    try do
      capture_io_with_result(fn ->
        assert false
      end)
    rescue
      error in [ExUnit.AssertionError] ->
        assert error.message == "Expected truthy, got false"
    end
  end

  test "capture :stderr by two processes (first with result)" do
    spawn(fn -> capture_io_with_result(:stderr, fn -> :timer.sleep(100) end) end)
    :timer.sleep(10)
    assert_raise RuntimeError, "IO device registered at :standard_error is already captured", fn ->
      capture_io(:stderr, fn -> nil end)
    end
    :timer.sleep(100)
  end

  test "capture :stderr by two processes (second with result)" do
    spawn(fn -> capture_io(:stderr, fn -> :timer.sleep(100) end) end)
    :timer.sleep(10)
    assert_raise RuntimeError, "IO device registered at :standard_error is already captured", fn ->
      capture_io_with_result(:stderr, fn -> nil end)
    end
    :timer.sleep(100)
  end

  test "capture :stderr by two processes (both with result)" do
    spawn(fn -> capture_io_with_result(:stderr, fn -> :timer.sleep(100) end) end)
    :timer.sleep(10)
    assert_raise RuntimeError, "IO device registered at :standard_error is already captured", fn ->
      capture_io_with_result(:stderr, fn -> nil end)
    end
    :timer.sleep(100)
  end
  defp send_and_receive_io(req) do
    send :erlang.group_leader, {:io_request, self, self, req}
    s = self
    receive do
      {:io_reply, ^s, res} -> res
    end
  end
end
