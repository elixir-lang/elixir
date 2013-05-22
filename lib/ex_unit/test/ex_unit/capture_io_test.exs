Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CaptureIOTest.Value do
  def binary, do: "a"
end

alias ExUnit.CaptureIOTest.Value

defmodule ExUnit.CaptureIOTest.GetUntil do
  def until_new_line(_, :eof, _) do
    { :done, :eof, [] }
  end

  def until_new_line(this_far, chars, stop_char) do
    case Enum.split_while(chars, fn(c) -> c != stop_char end) do
      { l, [] } ->
        { :more, this_far ++ l }
      { l, [stop_char|rest] } ->
        { :done, this_far ++ l ++ [stop_char], rest }
    end
  end

  def get_line(device // Process.group_leader) do
    device <- { :io_request, self, device, { :get_until, :unicode, "", __MODULE__, :until_new_line, [?\n] } }
    receive do
      { :io_reply, _, data } -> data
    end
  end
end

alias ExUnit.CaptureIOTest.GetUntil

defmodule ExUnit.CaptureIOTest do
  use ExUnit.Case, async: true

  doctest ExUnit.CaptureIO, import: true

  import ExUnit.CaptureIO

  test :capture_io_with_nothing do
    assert capture_io(fn ->
    end) == nil
  end

  test :capture_io_with_put_chars do
    assert capture_io(fn ->
      :io.put_chars("")
    end) == ""

    assert capture_io(fn ->
      :io.put_chars("a")
      :io.put_chars("b")
    end) == "ab"

    assert capture_io(fn ->
      send_and_receive_io({ :put_chars, :unicode, Value, :binary, [] })
    end) == "a"

    assert capture_io(fn ->
      :io.put_chars("josé")
    end) == "josé"

    assert capture_io(fn ->
      spawn(fn -> :io.put_chars("a") end)
    end) == "a"

    assert capture_io(fn ->
      assert :io.put_chars("a") == :ok
    end)
  end

  test :capture_io_with_put_chars_to_stderr do
    assert capture_io(:stderr, fn ->
      :io.put_chars(:standard_error, "a")
    end) == "a"
  end

  test :capture_io_with_get_chars do
    assert capture_io(fn ->
      :io.get_chars(">", 3)
    end) == nil

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

  test :capture_io_with_get_line do
    assert capture_io(fn ->
      :io.get_line ">"
    end) == nil

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

  test :capture_io_with_get_until do
    assert capture_io(fn ->
      assert :io.scan_erl_form('>')
    end) == nil

    capture_io(fn ->
      assert :io.scan_erl_form('>') == { :eof, 1 }
    end)

   capture_io("1", fn ->
     assert :io.scan_erl_form('>') == { :ok, [{ :integer, 1, 1 }], 1 }
     assert :io.scan_erl_form('>') == { :eof, 1 }
   end)

    capture_io("1\n.", fn ->
      assert :io.scan_erl_form('>') == { :ok, [{ :integer, 1, 1 }, { :dot, 2 }], 2 }
      assert :io.scan_erl_form('>') == { :eof, 1 }
    end)

    capture_io("1.\n.", fn ->
      assert :io.scan_erl_form('>') == { :ok, [{ :integer, 1, 1 }, { :dot, 1 }], 2 }
      assert :io.scan_erl_form('>') == { :ok, [dot: 1], 1}
      assert :io.scan_erl_form('>') == { :eof, 1 }
    end)

    capture_io("\"a", fn ->
      assert :io.scan_erl_form('>') == { :error, { 1, :erl_scan, { :string, 34, 'a' } }, 1 }
      assert :io.scan_erl_form('>') == { :eof, 1 }
    end)

    capture_io("\"a\n\"", fn ->
      assert :io.scan_erl_form('>') == { :ok, [{ :string, 1, 'a\n' }], 2 }
      assert :io.scan_erl_form('>') == { :eof, 1 }
    end)

    capture_io(":erl. mof*,,l", fn ->
      assert :io.scan_erl_form('>') == { :ok, [{ :":", 1 }, { :atom, 1, :erl }, { :dot, 1 }], 1 }
      assert :io.scan_erl_form('>') == { :ok, [{ :atom, 1, :mof }, { :*, 1 }, { :"," , 1 }, { :",", 1 }, { :atom, 1, :l }], 1 }
      assert :io.scan_erl_form('>') == { :eof, 1 }
    end)

    capture_io("a\nb\nc", fn ->
      assert GetUntil.get_line == 'a\n'
      assert GetUntil.get_line == 'b\n'
      assert GetUntil.get_line == :eof
    end)
  end

  test :capture_io_with_setopts do
    assert capture_io(fn ->
      :io.setopts({ :encoding, :latin1 })
    end) == nil

    capture_io(fn ->
      assert :io.setopts({ :encoding, :latin1 }) == :ok
    end)
  end

  test :capture_io_with_getopts do
    assert capture_io(fn ->
      :io.getopts
    end) == nil

    capture_io(fn ->
      assert :io.getopts == { :error, :enotsup }
    end)
  end

  test :capture_io_with_columns do
    assert capture_io(fn ->
      :io.columns
    end) == nil

    capture_io(fn ->
      assert :io.columns == { :error, :enotsup }
    end)
  end

  test :capture_io_with_rows do
    assert capture_io(fn ->
      :io.rows
    end) == nil

    capture_io(fn ->
      assert :io.rows == { :error, :enotsup }
    end)
  end

  test :capture_io_with_multiple_io_requests do
    assert capture_io(fn ->
      send_and_receive_io({ :requests, [{ :put_chars, :unicode, "a" },
                                        { :put_chars, :unicode, "b" }]})
    end) == "ab"

    capture_io(fn ->
      assert send_and_receive_io({ :requests, [{ :put_chars, :unicode, "a" },
                                               { :put_chars, :unicode, "b" }]}) == :ok
    end)
  end

  test :caputure_io_with_unknown_io_request do
    assert capture_io(fn ->
      send_and_receive_io(:unknown)
    end) == nil

    capture_io(fn ->
      assert send_and_receive_io(:unknown) == { :error, :request }
    end)
  end

  test :capture_io_with_inside_assert do
    group_leader = :erlang.group_leader

    try do
      capture_io(fn ->
        assert false
      end)
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected false to be true" = error.message
    end

    # Ensure no leakage on failures
    assert group_leader == :erlang.group_leader
  end

  defp send_and_receive_io(req) do
    :erlang.group_leader <- { :io_request, self, self, req }
    s = self
    receive do
      { :io_reply, ^s, res} -> res
    end
  end
end
