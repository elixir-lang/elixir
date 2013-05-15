Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CaptureIOTest.Value do
  def binary, do: "a"
end

alias ExUnit.CaptureIOTest.Value

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
  end

  test :capture_io_with_get_line do
    assert capture_io(fn ->
      :io.get_line ">"
    end) == nil

    capture_io(fn ->
      assert :io.get_line(">") == :eof
    end)
  end

  test :capture_io_with_get_until do
    assert capture_io(fn ->
       send_and_receive_io({ :get_until, '>', :m, :f, :as })
    end) == nil

    capture_io(fn ->
       assert send_and_receive_io({ :get_until, '>', :m, :f, :as }) == :eof
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
