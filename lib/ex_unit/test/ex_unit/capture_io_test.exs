Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CaptureIOTest do
  use ExUnit.Case

  defmodule GetUntil do
    def until_new_line(_, :eof, _) do
      {:done, :eof, []}
    end

    def until_new_line(this_far, chars, stop_char) do
      case Enum.split_while(chars, fn c -> c != stop_char end) do
        {l, []} ->
          {:more, this_far ++ l}

        {l, [stop_char | rest]} ->
          {:done, this_far ++ l ++ [stop_char], rest}
      end
    end

    def get_line(device \\ Process.group_leader()) do
      request = {:get_until, :unicode, "", __MODULE__, :until_new_line, [?\n]}
      send(device, {:io_request, self(), device, request})

      receive do
        {:io_reply, _, data} -> data
      end
    end
  end

  defmodule MockProc do
    use GenServer

    def start_link(gl) do
      GenServer.start_link(__MODULE__, gl)
    end

    @impl GenServer
    def init(gl) do
      Process.group_leader(self(), gl)
      {:ok, nil}
    end

    @impl GenServer
    def handle_call({:stdio, message}, _from, state) do
      IO.puts(message)
      {:reply, :ok, state}
    end

    @impl GenServer
    def handle_call({:prompt, prompt}, _from, state) do
      prompt
      |> IO.gets()
      |> IO.puts()

      {:reply, :ok, state}
    end

    @impl GenServer
    def handle_call({:stderr, message}, _from, state) do
      IO.puts(:stderr, message)
      {:reply, :ok, state}
    end
  end

  import ExUnit.CaptureIO
  doctest ExUnit.CaptureIO, import: true

  describe "I/O protocol" do
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

    test "with fwrite" do
      assert capture_io(fn ->
               :io.fwrite(<<127, 128>>)
             end) == <<127, 194, 128>>

      assert capture_io([encoding: :latin1], fn ->
               :io.fwrite(<<127, 128>>)
             end) == <<127, 128>>
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
               :io.get_line(">")
             end) == ">"

      assert capture_io([capture_prompt: false], fn ->
               :io.get_line(">")
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
               :io.scan_erl_form(~c">")
             end) == ">"

      assert capture_io("1.\n", fn ->
               :io.scan_erl_form(~c">")
             end) == ">"

      assert capture_io("1\n.\n", fn ->
               :io.scan_erl_form(~c">")
             end) == ">>"

      assert capture_io([capture_prompt: false], fn ->
               :io.scan_erl_form(~c">")
             end) == ""

      capture_io(fn ->
        assert :io.scan_erl_form(~c">") == {:eof, 1}
      end)

      capture_io("1", fn ->
        assert :io.scan_erl_form(~c">") == {:ok, [{:integer, 1, 1}], 1}
        assert :io.scan_erl_form(~c">") == {:eof, 1}
      end)

      capture_io("1\n.", fn ->
        assert :io.scan_erl_form(~c">") == {:ok, [{:integer, 1, 1}, {:dot, 2}], 2}
        assert :io.scan_erl_form(~c">") == {:eof, 1}
      end)

      capture_io("1.\n.", fn ->
        assert :io.scan_erl_form(~c">") == {:ok, [{:integer, 1, 1}, {:dot, 1}], 2}
        assert :io.scan_erl_form(~c">") == {:ok, [dot: 1], 1}
        assert :io.scan_erl_form(~c">") == {:eof, 1}
      end)

      capture_io("\"a", fn ->
        # TODO: Remove me when we require Erlang/OTP 27+
        expected_error =
          if System.otp_release() >= "27" do
            {1, :erl_scan, {:unterminated, :string, ~c"a"}}
          else
            {1, :erl_scan, {:string, 34, ~c"a"}}
          end

        assert :io.scan_erl_form(~c">") == {:error, expected_error, 1}
        assert :io.scan_erl_form(~c">") == {:eof, 1}
      end)

      capture_io("\"a\n\"", fn ->
        assert :io.scan_erl_form(~c">") == {:ok, [{:string, 1, ~c"a\n"}], 2}
        assert :io.scan_erl_form(~c">") == {:eof, 1}
      end)

      capture_io(":erl. mof*,,l", fn ->
        assert :io.scan_erl_form(~c">") == {:ok, [{:":", 1}, {:atom, 1, :erl}, {:dot, 1}], 1}

        expected_tokens = [{:atom, 1, :mof}, {:*, 1}, {:",", 1}, {:",", 1}, {:atom, 1, :l}]
        assert :io.scan_erl_form(~c">") == {:ok, expected_tokens, 1}

        assert :io.scan_erl_form(~c">") == {:eof, 1}
      end)

      capture_io("a\nb\nc", fn ->
        assert GetUntil.get_line() == "a\n"
        assert GetUntil.get_line() == "b\n"
        assert GetUntil.get_line() == :eof
      end)
    end

    test "with setopts" do
      assert capture_io(fn ->
               assert :io.setopts({:encoding, :latin1}) == {:error, :enotsup}
             end) == ""
    end

    test "with getopts" do
      assert capture_io(fn ->
               assert :io.getopts() == [binary: true, encoding: :unicode]
             end) == ""
    end

    test "with columns" do
      assert capture_io(fn ->
               :io.columns()
             end) == ""

      capture_io(fn ->
        assert :io.columns() == {:error, :enotsup}
      end)
    end

    test "with rows" do
      assert capture_io(fn ->
               :io.rows()
             end) == ""

      capture_io(fn ->
        assert :io.rows() == {:error, :enotsup}
      end)
    end

    test "with multiple requests" do
      requests = [{:put_chars, :unicode, "a"}, {:put_chars, :unicode, "b"}]

      assert capture_io(fn ->
               send_and_receive_io({:requests, requests})
             end) == "ab"

      capture_io(fn ->
        assert send_and_receive_io({:requests, requests}) == :ok
      end)
    end

    test "with unknown request" do
      assert capture_io(fn ->
               send_and_receive_io(:unknown)
             end) == ""

      capture_io(fn ->
        assert send_and_receive_io(:unknown) == {:error, :request}
      end)
    end
  end

  describe "stdin" do
    test "double capture from the same process" do
      assert capture_io(fn ->
               IO.puts("hello")
               assert capture_io(fn -> IO.puts("middle") end) == "middle\n"
               IO.puts("world")
             end) == "hello\nworld\n"
    end

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
  end

  describe "stderr" do
    test "supports writes" do
      assert capture_io(:stderr, fn ->
               :io.put_chars(:standard_error, "a")
             end) == "a"
    end

    test "double capture from the same process" do
      assert capture_io(:stderr, fn ->
               IO.puts(:stderr, "hello")
               assert capture_io(:stderr, fn -> IO.puts(:stderr, "middle") end) == "middle\n"
               IO.puts(:stderr, "world")
             end) == "hello\nmiddle\nworld\n"
    end

    test "supports async capture_io" do
      parent = self()

      [pid1, pid2, pid3] =
        for num <- 1..3 do
          pid =
            spawn_link(fn ->
              captured =
                capture_io(:stderr, fn ->
                  :io.put_chars(:standard_error, "before:#{num}\n")
                  send(parent, {self(), :logged})
                  assert_receive :continue
                  :io.put_chars(:standard_error, "after:#{num}\n")
                end)

              send(parent, captured)
            end)

          assert_receive {^pid, :logged}
          pid
        end

      send(pid3, :continue)
      assert_receive "before:3\nafter:3\n"

      send(pid2, :continue)
      assert_receive "before:2\nbefore:3\nafter:3\nafter:2\n"

      send(pid1, :continue)
      assert_receive "before:1\nbefore:2\nbefore:3\nafter:3\nafter:2\nafter:1\n"
    end

    test "raises when async capturing a named device with a different encoding than the first" do
      parent = self()

      pid =
        spawn_link(fn ->
          output =
            capture_io(:stderr, [encoding: :latin1], fn ->
              :io.put_chars(:standard_error, "a")
              send(parent, {self(), :logged})
              assert_receive :continue
            end)

          send(parent, output)
        end)

      assert_receive {^pid, :logged}

      assert_raise ArgumentError,
                   ~r"attempted to change the encoding for a currently captured device :standard_error",
                   fn ->
                     capture_io(:stderr, [encoding: :unicode], fn ->
                       :io.put_chars(:standard_error, "b")
                     end)
                   end

      assert capture_io(:stderr, [encoding: :latin1], fn ->
               :io.put_chars(:standard_error, "c")
             end) == "c"

      send(pid, :continue)
      assert_receive "ac"
    end

    test "raises when async capturing a named device with an input given to an already captured device" do
      parent = self()

      pid =
        spawn_link(fn ->
          capture_io(:stderr, [input: "first"], fn ->
            send(parent, {self(), :logged})
            Process.sleep(:infinity)
          end)
        end)

      assert_receive {^pid, :logged}

      message =
        "attempted multiple captures on device :standard_error with input. If you need to give an input to a captured device, you cannot run your test asynchronously"

      assert_raise ArgumentError, message, fn ->
        capture_io(:stderr, [input: "second"], fn ->
          :io.put_chars(:standard_error, "b")
        end)
      end

      assert_raise ArgumentError, message, fn ->
        capture_io(:stderr, [input: ""], fn ->
          :io.put_chars(:standard_error, "b")
        end)
      end
    end

    test "no leakage on failures" do
      parent = self()

      pid =
        spawn(fn ->
          capture_io(:stderr, [input: "a"], fn ->
            send(parent, :ready)
            Process.sleep(:infinity)
          end)
        end)

      assert_receive :ready

      ref = Process.monitor(pid)

      # Kill the process and make sure the capture is released
      Process.exit(pid, :shutdown)

      # Make sure the process has exited before we try and start a new capture
      assert_receive {:DOWN, ^ref, _, _, _}

      assert capture_io(:stderr, [input: "b"], fn -> :ok end)
    end
  end

  test "capture_io with a separate process" do
    {:ok, gl} = StringIO.open("")
    pid = start_supervised!({MockProc, gl})

    assert Process.info(pid, :group_leader) == {:group_leader, gl}

    assert capture_io(pid, fn ->
             GenServer.call(pid, {:stdio, "a"})
           end) == "a\n"

    assert capture_io(pid, [input: "b"], fn ->
             GenServer.call(pid, {:prompt, "> "})
           end) == "> b\n"

    assert capture_io(pid, "c", fn ->
             GenServer.call(pid, {:prompt, "> "})
           end) == "> c\n"

    assert capture_io(pid, [input: "d", capture_prompt: false], fn ->
             GenServer.call(pid, {:prompt, "> "})
           end) == "d\n"

    assert capture_io(:stderr, fn ->
             GenServer.call(pid, {:stderr, "uhoh"})
           end) == "uhoh\n"

    assert Process.info(pid, :group_leader) == {:group_leader, gl}
    assert StringIO.contents(gl) == {"", ""}
  end

  test "with_io" do
    assert with_io(fn ->
             :io.put_chars("xyz")
             2 + 2
           end) == {4, "xyz"}
  end

  defp send_and_receive_io(req) do
    pid = self()
    send(:erlang.group_leader(), {:io_request, pid, pid, req})

    receive do
      {:io_reply, ^pid, res} -> res
    end
  end
end
