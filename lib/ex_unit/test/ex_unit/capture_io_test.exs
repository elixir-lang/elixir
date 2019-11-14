Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CaptureIOTest do
  use ExUnit.Case, async: true

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

  test "async capture_io works with put chars to stderr" do
    me = self()

    Enum.each(1..6, fn num ->
      spawn(fn ->
        captured =
          capture_io(:stderr, fn ->
            :io.put_chars(:standard_error, "#{num}\n")
            Process.sleep(10)
            :io.put_chars(:standard_error, "#{num + 10}\n")
          end)

        send(me, captured)
      end)

      Process.sleep(1)
    end)

    expected = [
      {~r/^1\n/, ~r/11\n/},
      {~r/^2\n/, ~r/12\n/},
      {~r/^3\n/, ~r/13\n/},
      {~r/^4\n/, ~r/14\n/},
      {~r/^5\n/, ~r/15\n/},
      {~r/^6\n/, ~r/16\n/}
    ]

    assert_received_regex(expected)
  end

  test "raises when async capturing a named device with a different encoding than the first" do
    me = self()

    Enum.each([:latin1, :unicode, :latin1], fn encoding ->
      spawn(fn ->
        try do
          capture_io(:stderr, [encoding: encoding], fn ->
            :io.put_chars(:standard_error, "a")
            Process.sleep(3)
          end)
        rescue
          e in [ArgumentError] ->
            send(me, e.message)
        end
      end)
    end)

    assert_receive "Attempted to change the encoding already set for the captured named device `:standard_error`\nCurrently set as: :latin1\nGiven: :unicode" <>
                     _

    refute_receive "Attempted to change the encoding already set for the captured named device `:standard_error`\nCurrently set as: :unicode\nGiven: :latin1" <>
                     _
  end

  test "raises when async capturing a named device with an input given to an already captured device" do
    me = self()

    Enum.each(["first", "second", ""], fn input ->
      spawn(fn ->
        try do
          capture_io(:stderr, [input: input], fn ->
            :io.put_chars(:standard_error, "a")
            Process.sleep(3)
          end)
        rescue
          e in [ArgumentError] ->
            send(me, e.message)
        end
      end)
    end)

    assert_receive "Attempted to give an input \"second\" for a currently captured named device `:standard_error`"

    refute_receive "Attempted to give an input \"\" for a currently captured named device `:standard_error`"
  end

  test "CaptureServer monitors calling processes" do
    parent = self()

    pid =
      spawn(fn ->
        capture_io(:stderr, fn ->
          send(parent, :ready)
          Process.sleep(:infinity)
        end)
      end)

    assert_receive :ready

    # Kill the process and make sure the capture server receives the down	
    :erlang.trace(Process.whereis(ExUnit.CaptureServer), true, [:receive, tracer: self()])
    Process.exit(pid, :shutdown)
    assert_receive {:trace, _, :receive, {:DOWN, _, _, _, :shutdown}}, 1000

    assert capture_io(:stderr, fn -> :ok end)
  after
    :erlang.trace(Process.whereis(ExUnit.CaptureServer), false, [:receive, tracer: self()])
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

      expected_tokens = [{:atom, 1, :mof}, {:*, 1}, {:",", 1}, {:",", 1}, {:atom, 1, :l}]
      assert :io.scan_erl_form('>') == {:ok, expected_tokens, 1}

      assert :io.scan_erl_form('>') == {:eof, 1}
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

  test "with multiple IO requests" do
    requests = [{:put_chars, :unicode, "a"}, {:put_chars, :unicode, "b"}]

    assert capture_io(fn ->
             send_and_receive_io({:requests, requests})
           end) == "ab"

    capture_io(fn ->
      assert send_and_receive_io({:requests, requests}) == :ok
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

  defp send_and_receive_io(req) do
    pid = self()
    send(:erlang.group_leader(), {:io_request, pid, pid, req})

    receive do
      {:io_reply, ^pid, res} -> res
    end
  end

  defp assert_received_regex([]), do: :ok

  defp assert_received_regex(regexes) do
    receive do
      output ->
        index =
          Enum.find_index(regexes, fn {first, second} ->
            output =~ first and output =~ second
          end)

        case index do
          nil ->
            raise "Cound not find match for #{inspect(output)} in #{inspect(regexes)}"

          _ ->
            regexes
            |> List.delete_at(index)
            |> assert_received_regex()
        end
    after
      100 ->
        raise "Expected to receive message but did not"
    end
  end
end
