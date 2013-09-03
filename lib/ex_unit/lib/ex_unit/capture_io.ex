defmodule ExUnit.CaptureIO do
  @moduledoc %S"""
  This module provides functionality to capture IO to test it.

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
  Captures IO. Returns nil in case of no output,
  otherwise returns the binary which is the captured output.

  By default, capture_io replaces the group_leader (`:stdio`)
  for the current process. However, the capturing of any other
  named device like `:stderr` is also possible globally by
  giving the registered device name explicitly as argument.

  When capturing `:stdio` and the `:capture_prompt` option is `false`,
  prompts (specified as arguments in IO.get* functions) are not
  captured.

  A developer can set a string as an input. The default
  input is `:eof`.

  ## Examples

      iex> capture_io(fn -> IO.write "josé" end) == "josé"
      true
      iex> capture_io(fn -> :ok end) == nil
      true
      iex> capture_io(:stderr, fn -> IO.write(:stderr, "josé") end) == "josé"
      true
      iex> capture_io("this is input", fn ->
      ...>   input = IO.gets ">"
      ...>   IO.write input
      ...> end) == ">this is input"
      true
      iex> capture_io([input: "this is input", capture_prompt: false], fn ->
      ...>   input = IO.gets ">"
      ...>   IO.write input
      ...> end) == "this is input"
      true

  """
  def capture_io(fun) do
    do_capture_io(:standard_io, [], fun)
  end

  def capture_io(device, fun) when is_atom(device) do
    capture_io(device, [], fun)
  end

  def capture_io(input, fun) when is_binary(input) do
    capture_io(:standard_io, [input: input], fun)
  end

  def capture_io(options, fun) when is_list(options) do
    capture_io(:standard_io, options, fun)
  end

  def capture_io(device, input, fun) when is_binary(input) do
    capture_io(device, [input: input], fun)
  end

  def capture_io(device, options, fun) when is_list(options) do
    do_capture_io(map_dev(device), options, fun)
  end

  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other),   do: other

  defp do_capture_io(:standard_io, options, fun) do
    original_gl = :erlang.group_leader
    capture_gl = new_group_leader(self, options)
    :erlang.group_leader(capture_gl, self)

    try do
      fun.()
    after
      :erlang.group_leader(original_gl, self)
      capture_gl <- :stop
    end

    receive do
      { ^capture_gl, buf } -> buf
    end
  end

  defp do_capture_io(device, options, fun) do
    unless original_io = Process.whereis(device) do
      raise "could not find IO device registered at #{inspect device}"
    end

    options = Keyword.put(options, :capture_prompt, false)

    Process.unregister(device)
    capture_io = new_group_leader(self, options)
    Process.register(capture_io, device)

    try do
      fun.()
    after
      Process.unregister(device)
      Process.register(original_io, device)
      capture_io <- :stop
    end

    receive do
      { ^capture_io, buf } -> buf
    end
  end

  defp new_group_leader(runner, options) do
    spawn_link(fn -> group_leader_process(runner, options) end)
  end

  defp group_leader_process(runner, options) do
    prompt_config = Keyword.get(options, :capture_prompt, true)
    input = Keyword.get(options, :input, "")

    register_input(input)
    register_prompt_config(prompt_config)
    group_leader_loop(runner, :infinity, [])
  end

  defp register_input(input) do
    chars = String.to_char_list!(input)
    set_input(chars)
  end

  defp register_prompt_config(bool) do
    Process.put(:capture_io_prompt_config, bool)
  end

  defp set_input(:eof) do
    set_input([])
  end

  defp set_input(input) do
    Process.put(:capture_io_input, input)
  end

  defp get_input do
    Process.get(:capture_io_input)
  end

  defp need_prompt? do
    Process.get(:capture_io_prompt_config)
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
    chars = apply(m, f, as)
    { :ok, [chars|buf] }
  end

  defp io_request({ :put_chars, _enc, chars }, buf) do
    io_request({ :put_chars, chars }, buf)
  end

  defp io_request({ :put_chars, _enc, mod, func, args }, buf) do
    io_request({ :put_chars, mod, func, args }, buf)
  end

  defp io_request({ :get_chars, _enc, prompt, n }, buf) when n >= 0 do
    io_request({ :get_chars, prompt, n }, buf)
  end

  defp io_request({ :get_chars, prompt, n }, buf) when n >= 0 do
    if need_prompt? do
      buf = [prompt|buf]
    end

    { get_chars(n), buf }
  end

  defp io_request({ :get_line, _enc, prompt }, buf) do
    io_request({ :get_line, prompt }, buf)
  end

  defp io_request({ :get_line, prompt }, buf) do
    if need_prompt? do
      buf = [prompt|buf]
    end

    { get_line, buf }
  end

  defp io_request({ :get_until, _encoding, prompt, mod, fun, args}, buf) do
    io_request({ :get_until, prompt, mod, fun, args}, buf)
  end

  defp io_request({ :get_until, prompt, mod, fun, args }, buf) do
    { result, count } = get_until(mod, fun, args)

    if need_prompt? do
      buf = [:lists.duplicate(count, prompt)|buf]
    end

    { result, buf }
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

  defp get_line do
    input = get_input

    case input do
      [] ->
        :eof
      _ ->
        { line, rest } = Enum.split_while(input, fn(char) -> char != ?\n end)
        case rest do
          [] ->
            set_input([])
            String.from_char_list!(line)
          [_|t] ->
            set_input(t)
            String.from_char_list!(line) <> "\n"
        end
    end
  end

  defp get_chars(n) do
    input = get_input

    case input do
      [] ->
        :eof
      _ ->
        { chars, rest } = Enum.split(input, n)
        set_input(rest)
        String.from_char_list!(chars)
    end
  end

  defp get_until(mod, fun, args) do
    input = get_input
    do_get_until(input, mod, fun, args)
  end

  defp do_get_until([], mod, fun, args, continuation // [], count // 0)

  defp do_get_until([], mod, fun, args, continuation, count) do
    case apply(mod, fun, [continuation, :eof | args]) do
      { :done, result, rest_chars } ->
        set_input(rest_chars)
        { result, count + 1 }
      { :more, next_continuation } ->
        do_get_until([], mod, fun, args, next_continuation, count + 1)
    end
  end

  defp do_get_until(input, mod, fun, args, continuation, count) do
    { line, rest } = Enum.split_while(input, fn(char) -> char != ?\n end)

    case rest do
      [] ->
        case apply(mod, fun, [continuation, line | args]) do
          { :done, result, rest_chars } ->
            set_input(rest_chars)
            { result, count + 1 }
          { :more, next_continuation } ->
            do_get_until([], mod, fun, args, next_continuation, count + 1)
        end
      [_|t] ->
        case apply(mod, fun, [continuation, line ++ '\n' | args]) do
          { :done, result, rest_chars } ->
            set_input(rest_chars ++ t)
            { result, count + 1 }
          { :more, next_continuation } ->
            do_get_until(t, mod, fun, args, next_continuation, count + 1)
        end
    end
  end

  defp buffer_to_result([]) do
    nil
  end

  defp buffer_to_result(buf) do
    buf |> :lists.reverse |> iolist_to_binary
  end
end
