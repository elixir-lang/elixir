defmodule ExUnit.CaptureIO do
  @moduledoc """
  This module provides functionality to capture IO to test it.
  The way to use this module is to import them into your module.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureIO

        test :example do
          assert capture_io(fn ->
            IO.puts "a"
          end) == "a\n"
        end
      end

  """

  @doc """
  Captures IO. Returns nil in case of no output.
  Otherwise returns the binary which is captured outputs.
  The input is mocked to return :eof.

  ## Examples

      iex> capture_io(fn -> IO.write "josé" end) == "josé"
      true
      iex> capture_io(fn -> :ok end) == nil
      true

  """
  def capture_io(fun) do
    original_gl = :erlang.group_leader
    capture_gl = new_group_leader(self)

    :erlang.group_leader(capture_gl, self)
    fun.()
    :erlang.group_leader(original_gl, self)

    group_leader_sync(capture_gl)
  end

  defp new_group_leader(runner) do
    spawn_link(fn -> group_leader_process(runner) end)
  end

  defp group_leader_process(runner) do
    group_leader_loop(runner, :infinity, [])
  end

  defp group_leader_loop(runner, wait, buf) do
    receive do
      { :io_request, from, reply_as, req } ->
        p = :erlang.process_flag(:priority, :normal)
        buf = io_request(from, reply_as, req, buf)
        :erlang.process_flag(:priority, p)
        group_leader_loop(runner, wait, buf)
      :stop ->
        receive after: (2 -> :ok)
        :erlang.process_flag(:priority, :low)
        group_leader_loop(runner, 0, buf)
      _ ->
        group_leader_loop(runner, 0, buf)
    after wait ->
      :erlang.process_flag(:priority, :normal)
      runner <- { self, buffer_to_result(buf) }
    end
  end

  defp group_leader_sync(gl) do
    gl <- :stop

    receive do
      { ^gl, buf } -> buf
    end
  end

  defp io_request(from, reply_as, req, buf) do
    { reply, buf1 } = io_request(req, buf)
    io_reply(from, reply_as, reply)
    buf1
  end

  defp io_reply(from, reply_as, reply) do
    from <- { :io_reply, reply_as, reply }
  end

  defp io_request({ :put_chars, chars }, buf) do
    { :ok, [chars|buf] }
  end

  defp io_request({ :put_chars, m, f, as }, buf) do
    chars = apply(m ,f, as)
    { :ok, [chars|buf] }
  end

  defp io_request({ :put_chars, _enc, chars }, buf) do
    io_request({ :put_chars, chars }, buf)
  end

  defp io_request({ :put_chars, _enc, mod, func, args }, buf) do
    io_request({ :put_chars, mod, func, args }, buf)
  end

  defp io_request({ :get_chars, _enc, _propmpt, _n }, buf) do
    { :eof, buf }
  end

  defp io_request({ :get_chars, _prompt, _n }, buf) do
    { :eof, buf }
  end

  defp io_request({ :get_line, _prompt }, buf) do
    { :eof, buf }
  end

  defp io_request({ :get_line, _enc, _prompt }, buf) do
    { :eof, buf }
  end

  defp io_request({ :get_until, _prompt, _m, _f, _as }, buf) do
    { :eof, buf }
  end

  defp io_request({ :setopts, _opts }, buf) do
    { :ok, buf }
  end

  defp io_request(:getopts, buf) do
    { { :error, :enotsup }, buf }
  end

  defp io_request({ :get_geometry, :columns }, buf) do
    { { :error, :enotsup }, buf }
  end

  defp io_request({ :get_geometry, :rows }, buf) do
    { { :error, :enotsup }, buf }
  end

  defp io_request({ :requests, reqs }, buf) do
    io_requests(reqs, { :ok, buf })
  end

  defp io_request(_, buf) do
    { { :error, :request }, buf }
  end

  defp io_requests([r|rs], { :ok, buf }) do
    io_requests(rs, io_request(r, buf))
  end

  defp io_requests(_, result) do
    result
  end

  defp buffer_to_result([]) do
    nil
  end

  defp buffer_to_result([bin]) when is_binary(bin) do
    bin
  end

  defp buffer_to_result(buf) do
    buf |> :lists.reverse |> list_to_binary
  end
end
