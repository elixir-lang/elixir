Code.require_file "../test_helper.exs", __DIR__

defmodule TestServer do
  use GenServer

  def handle_call(:puts_to_stdout, _from, nil) do
    IO.puts "from stdout"
    {:reply, :ok, nil}
  end

  def handle_call(:puts_to_stderr, _from, nil) do
    IO.puts :stderr, "from stderr"
    {:reply, :ok, nil}
  end

  def handle_call(:get_line, _from, nil) do
    {:reply, {:ok, :io.get_line(">")}, nil}
  end
end

defmodule ExUnit.CaptureIOTest do
  use ExUnit.Case

  defmodule GetUntil do
    def until_new_line(_, :eof, _) do
      {:done, :eof, []}
    end

    def until_new_line(this_far, chars, stop_char) do
      case Enum.split_while(chars, fn(c) -> c != stop_char end) do
        {l, []} ->
          {:more, this_far ++ l}
        {l, [stop_char | rest]} ->
          {:done, this_far ++ l ++ [stop_char], rest}
      end
    end

    def get_line(device \\ Process.group_leader) do
      send device, {:io_request, self(), device, {:get_until, :unicode, "", __MODULE__, :until_new_line, [?\n]}}
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
      capture_io(fn ->
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
    assert capture_io(fn -> nil end) == ""
  end

  test "concurrent stdio captures" do
    for i <- 0..3 do
      wait = :rand.uniform(10)
      test_pid = self()
      spawn(fn ->
        result = capture_io(fn ->
          for _j <- 0..3 do
            Process.sleep(wait)
            IO.puts i
          end
        end)
        send test_pid, {i, result}
      end)
    end

    assert_receive {0, "0\n0\n0\n0\n"}
    assert_receive {1, "1\n1\n1\n1\n"}
    assert_receive {2, "2\n2\n2\n2\n"}
    assert_receive {3, "3\n3\n3\n3\n"}
  end

  describe "child processes" do
    setup do
      {:ok, pid} = GenServer.start_link(TestServer, nil)

      on_exit fn -> Process.exit(pid, :normal) end

      [pid: pid]
    end

    test "can be captured", context do
      assert capture_io(context[:pid], fn ->
        :ok = GenServer.call(context[:pid], :puts_to_stdout)
      end) == "from stdout\n"

      assert capture_io(:stderr, fn ->
        :ok = GenServer.call(context[:pid], :puts_to_stderr)
      end) == "from stderr\n"

      capture_io(context[:pid], [input: "my input\n"], fn ->
        {:ok, line} = GenServer.call(context[:pid], :get_line)
        send self(), {:result, line}
      end)

      assert_received {:result, "my input\n"}
    end

    test "concurrent captures raise an error", context do
      spawn(fn -> capture_io(context[:pid], fn -> Process.sleep(100) end) end)
      Process.sleep(10)
      assert_raise RuntimeError,
        "The process with PID #{inspect context[:pid]} is already captured",
        fn -> capture_io(context[:pid], fn -> nil end) end
    end
  end

  test "with put chars" do
    assert capture_io(fn ->
      :io.put_chars("")
    end) == ""

    assert capture_io(fn ->
      :io.put_chars("a")
      :io.put_chars("b")
    end) == "ab"

    assert capture_io(fn ->
      :io.put_chars("josé")
    end) == "josé"

    assert capture_io(fn ->
      spawn(fn -> :io.put_chars("a") end)
      Process.sleep(10)
    end) == "a"

    assert capture_io(fn ->
      assert :io.put_chars("a") == :ok
    end)
  end

  test "with put chars to stderr" do
    assert capture_io(:stderr, fn ->
      :io.put_chars(:standard_error, "a")
    end) == "a"
  end

  test "with get chars" do
    assert capture_io(fn ->
      :io.get_chars(">", 3)
    end) == ">"

    assert capture_io([capture_prompt: false], fn ->
      :io.get_chars(">", 3)
    end) == ""

    capture_io(fn ->
      assert :io.get_chars(">", 3) == :eof
    end)

    capture_io("", fn ->
      assert :io.get_chars(">", 3) == :eof
    end)

    capture_io("abc\ndef", fn ->
      assert :io.get_chars(">", 3) == "abc"
      assert :io.get_chars(">", 5) == "\ndef"
      assert :io.get_chars(">", 7) == :eof
    end)

    capture_io("あいう", fn ->
      assert :io.get_chars(">", 2) == "あい"
      assert :io.get_chars(">", 1) == "う"
      assert :io.get_chars(">", 1) == :eof
    end)
  end

  test "with get line" do
    assert capture_io(fn ->
      :io.get_line ">"
    end) == ">"

    assert capture_io([capture_prompt: false], fn ->
      :io.get_line ">"
    end) == ""

    capture_io(fn ->
      assert :io.get_line(">") == :eof
    end)

    capture_io("", fn ->
      assert :io.get_line(">") == :eof
    end)

    capture_io("\n", fn ->
      assert :io.get_line(">") == "\n"
      assert :io.get_line(">") == :eof
    end)

    capture_io("a", fn ->
      assert :io.get_line(">") == "a"
      assert :io.get_line(">") == :eof
    end)

    capture_io("a\n", fn ->
      assert :io.get_line(">") == "a\n"
      assert :io.get_line(">") == :eof
    end)

    capture_io("a\nb", fn ->
      assert :io.get_line(">") == "a\n"
      assert :io.get_line(">") == "b"
      assert :io.get_line(">") == :eof
    end)

    capture_io("あい\nう", fn ->
      assert :io.get_line(">") == "あい\n"
      assert :io.get_line(">") == "う"
      assert :io.get_line(">") == :eof
    end)
  end

  test "with get password" do
    capture_io(fn ->
      assert :io.get_password() == :eof
    end)

    capture_io("", fn ->
      assert :io.get_password() == :eof
    end)

    capture_io("abc", fn ->
      assert :io.get_password() == "abc"
      assert :io.get_password() == :eof
    end)

    capture_io("abc\n", fn ->
      assert :io.get_password() == "abc\n"
      assert :io.get_password() == :eof
    end)

    capture_io("\n", fn ->
      assert :io.get_password() == "\n"
      assert :io.get_password() == :eof
    end)

    capture_io("a\nb", fn ->
      assert :io.get_password() == "a\n"
      assert :io.get_password() == "b"
      assert :io.get_password() == :eof
    end)

    capture_io("あい\nう", fn ->
      assert :io.get_password() == "あい\n"
      assert :io.get_password() == "う"
      assert :io.get_password() == :eof
    end)
  end

  test "with get until" do
    assert capture_io(fn ->
      :io.scan_erl_form('>')
    end) == ">"

    assert capture_io("1.\n", fn ->
      :io.scan_erl_form('>')
    end) == ">"

    assert capture_io("1\n.\n", fn ->
       :io.scan_erl_form('>')
    end) == ">>"

    assert capture_io([capture_prompt: false], fn ->
      :io.scan_erl_form('>')
    end) == ""

    capture_io(fn ->
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

   capture_io("1", fn ->
     assert :io.scan_erl_form('>') == {:ok, [{:integer, 1, 1}], 1}
     assert :io.scan_erl_form('>') == {:eof, 1}
   end)

    capture_io("1\n.", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:integer, 1, 1}, {:dot, 2}], 2}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io("1.\n.", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:integer, 1, 1}, {:dot, 1}], 2}
      assert :io.scan_erl_form('>') == {:ok, [dot: 1], 1}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io("\"a", fn ->
      assert :io.scan_erl_form('>') == {:error, {1, :erl_scan, {:string, 34, 'a'}}, 1}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io("\"a\n\"", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:string, 1, 'a\n'}], 2}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io(":erl. mof*,,l", fn ->
      assert :io.scan_erl_form('>') == {:ok, [{:":", 1}, {:atom, 1, :erl}, {:dot, 1}], 1}
      assert :io.scan_erl_form('>') == {:ok, [{:atom, 1, :mof}, {:*, 1}, {:",", 1}, {:",", 1}, {:atom, 1, :l}], 1}
      assert :io.scan_erl_form('>') == {:eof, 1}
    end)

    capture_io("a\nb\nc", fn ->
      assert GetUntil.get_line == "a\n"
      assert GetUntil.get_line == "b\n"
      assert GetUntil.get_line == :eof
    end)
  end

  test "with setopts" do
    assert capture_io(fn ->
      assert :io.setopts({:encoding, :latin1}) == {:error, :enotsup}
    end) == ""
  end

  test "with getopts" do
    assert capture_io(fn ->
      assert :io.getopts == {:ok, [binary: true, encoding: :unicode]}
    end) == ""
  end

  test "with columns" do
    assert capture_io(fn ->
      :io.columns
    end) == ""

    capture_io(fn ->
      assert :io.columns == {:error, :enotsup}
    end)
  end

  test "with rows" do
    assert capture_io(fn ->
      :io.rows
    end) == ""

    capture_io(fn ->
      assert :io.rows == {:error, :enotsup}
    end)
  end

  test "with multiple IO requests" do
    assert capture_io(fn ->
      send_and_receive_io({:requests, [{:put_chars, :unicode, "a"},
                                        {:put_chars, :unicode, "b"}]})
    end) == "ab"

    capture_io(fn ->
      assert send_and_receive_io({:requests, [{:put_chars, :unicode, "a"},
                                               {:put_chars, :unicode, "b"}]}) == :ok
    end)
  end

  test "with unknown IO request" do
    assert capture_io(fn ->
      send_and_receive_io(:unknown)
    end) == ""

    capture_io(fn ->
      assert send_and_receive_io(:unknown) == {:error, :request}
    end)
  end

  test "device re-registering" do
    {_pid, ref} = spawn_monitor(fn ->
      capture_io(:stderr, fn ->
        spawn_link(Kernel, :exit, [:shutdown])
        Process.sleep(:infinity)
      end)
    end)

    # Assert the process is down then invoke capture_io
    # to trigger the ExUnit.Server, ensuring the DOWN
    # message from previous capture_io has been processed
    assert_receive {:DOWN, ^ref, _, _, :shutdown}
    _ = capture_io(fn -> "trigger" end)
    assert capture_io(:stderr, fn -> nil end)
  end

  test "with assert inside" do
    try do
      capture_io(fn ->
        assert false
      end)
    rescue
      error in [ExUnit.AssertionError] ->
        assert error.message == "Expected truthy, got false"
    end
  end

  test "capture :stderr by two processes" do
    spawn(fn -> capture_io(:stderr, fn -> Process.sleep(100) end) end)
    Process.sleep(10)
    assert_raise RuntimeError, "IO device registered at :standard_error is already captured", fn ->
      capture_io(:stderr, fn -> nil end)
    end
    Process.sleep(100)
  end

  defp send_and_receive_io(req) do
    pid = self()
    send :erlang.group_leader, {:io_request, pid, pid, req}
    receive do
      {:io_reply, ^pid, res} -> res
    end
  end
end
