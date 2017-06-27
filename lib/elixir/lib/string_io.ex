defmodule StringIO do
  @moduledoc """
  Controls an IO device process that wraps a string.

  A `StringIO` IO device can be passed as a "device" to
  most of the functions in the `IO` module.

  ## Examples

      iex> {:ok, pid} = StringIO.open("foo")
      iex> IO.read(pid, 2)
      "fo"

  """

  use GenServer

  @doc """
  Creates an IO device.

  `string` will be the initial input of the newly created
  device.

  If the `:capture_prompt` option is set to `true`,
  prompts (specified as arguments to `IO.get*` functions)
  are captured.

  ## Examples

      iex> {:ok, pid} = StringIO.open("foo")
      iex> IO.gets(pid, ">")
      "foo"
      iex> StringIO.contents(pid)
      {"", ""}

      iex> {:ok, pid} = StringIO.open("foo", capture_prompt: true)
      iex> IO.gets(pid, ">")
      "foo"
      iex> StringIO.contents(pid)
      {"", ">"}

  """
  @spec open(binary, keyword) :: {:ok, pid}
  def open(string, options \\ []) when is_binary(string) do
    GenServer.start_link(__MODULE__, {string, options}, [])
  end

  @doc """
  Returns the current input/output buffers for the given IO
  device.

  ## Examples

      iex> {:ok, pid} = StringIO.open("in")
      iex> IO.write(pid, "out")
      iex> StringIO.contents(pid)
      {"in", "out"}

  """
  @spec contents(pid) :: {binary, binary}
  def contents(pid) when is_pid(pid) do
    GenServer.call(pid, :contents)
  end

  @doc """
  Flushes the output buffer and returns its current contents.

  ## Examples

      iex> {:ok, pid} = StringIO.open("in")
      iex> IO.write(pid, "out")
      iex> StringIO.flush(pid)
      "out"
      iex> StringIO.contents(pid)
      {"in", ""}

  """
  @spec flush(pid) :: binary
  def flush(pid) when is_pid(pid) do
    GenServer.call(pid, :flush)
  end

  @doc """
  Stops the IO device and returns the remaining input/output
  buffers.

  ## Examples

      iex> {:ok, pid} = StringIO.open("in")
      iex> IO.write(pid, "out")
      iex> StringIO.close(pid)
      {:ok, {"in", "out"}}

  """
  @spec close(pid) :: {:ok, {binary, binary}}
  def close(pid) when is_pid(pid) do
    GenServer.call(pid, :close)
  end

  ## callbacks

  def init({string, options}) do
    capture_prompt = options[:capture_prompt] || false
    {:ok, %{input: string, output: "", capture_prompt: capture_prompt}}
  end

  def handle_info({:io_request, from, reply_as, req}, s) do
    s = io_request(from, reply_as, req, s)
    {:noreply, s}
  end

  def handle_info(msg, s) do
    super(msg, s)
  end

  def handle_call(:contents, _from, %{input: input, output: output} = s) do
    {:reply, {input, output}, s}
  end

  def handle_call(:flush, _from, %{output: output} = s) do
    {:reply, output, %{s | output: ""}}
  end

  def handle_call(:close, _from, %{input: input, output: output} = s) do
    {:stop, :normal, {:ok, {input, output}}, s}
  end

  def handle_call(request, from, s) do
    super(request, from, s)
  end

  defp io_request(from, reply_as, req, s) do
    {reply, s} = io_request(req, s)
    io_reply(from, reply_as, to_reply(reply))
    s
  end

  defp io_request({:put_chars, chars} = req, s) do
    put_chars(:latin1, chars, req, s)
  end

  defp io_request({:put_chars, m, f, as} = req, s) do
    put_chars(:latin1, apply(m, f, as), req, s)
  end

  defp io_request({:put_chars, encoding, chars} = req, s) do
    put_chars(encoding, chars, req, s)
  end

  defp io_request({:put_chars, encoding, mod, func, args} = req, s) do
    put_chars(encoding, apply(mod, func, args), req, s)
  end

  defp io_request({:get_chars, prompt, n}, s) when n >= 0 do
    io_request({:get_chars, :latin1, prompt, n}, s)
  end

  defp io_request({:get_chars, encoding, prompt, n}, s) when n >= 0 do
    get_chars(encoding, prompt, n, s)
  end

  defp io_request({:get_line, prompt}, s) do
    io_request({:get_line, :latin1, prompt}, s)
  end

  defp io_request({:get_line, encoding, prompt}, s) do
    get_line(encoding, prompt, s)
  end

  defp io_request({:get_until, prompt, mod, fun, args}, s) do
    io_request({:get_until, :latin1, prompt, mod, fun, args}, s)
  end

  defp io_request({:get_until, encoding, prompt, mod, fun, args}, s) do
    get_until(encoding, prompt, mod, fun, args, s)
  end

  defp io_request({:get_password, encoding}, s) do
    get_line(encoding, "", s)
  end

  defp io_request({:setopts, _opts}, s) do
    {{:error, :enotsup}, s}
  end

  defp io_request(:getopts, s) do
    {{:ok, [binary: true, encoding: :unicode]}, s}
  end

  defp io_request({:get_geometry, :columns}, s) do
    {{:error, :enotsup}, s}
  end

  defp io_request({:get_geometry, :rows}, s) do
    {{:error, :enotsup}, s}
  end

  defp io_request({:requests, reqs}, s) do
    io_requests(reqs, {:ok, s})
  end

  defp io_request(_, s) do
    {{:error, :request}, s}
  end

  ## put_chars

  defp put_chars(encoding, chars, req, %{output: output} = s) do
    case :unicode.characters_to_binary(chars, encoding, :unicode) do
      string when is_binary(string) ->
        {:ok, %{s | output: output <> string}}
      {_, _, _} ->
        {{:error, req}, s}
    end
  end

  ## get_chars

  defp get_chars(encoding, prompt, n, %{input: input} = s) do
    case do_get_chars(input, encoding, n) do
      {:error, _} = error ->
        {error, s}
      {result, input} ->
        {result, state_after_read(s, input, prompt)}
    end
  end

  defp do_get_chars("", _encoding, _n) do
    {:eof, ""}
  end

  defp do_get_chars(input, :latin1, n) when byte_size(input) < n do
    {input, ""}
  end

  defp do_get_chars(input, :latin1, n) do
    <<chars::binary-size(n), rest::binary>> = input
    {chars, rest}
  end

  defp do_get_chars(input, encoding, n) do
    try do
      case :file_io_server.count_and_find(input, n, encoding) do
        {buf_count, split_pos} when buf_count < n or split_pos == :none ->
          {input, ""}
        {_buf_count, split_pos} ->
          <<chars::binary-size(split_pos), rest::binary>> = input
          {chars, rest}
      end
    catch
      :exit, :invalid_unicode ->
        {:error, :invalid_unicode}
    end
  end

  ## get_line

  defp get_line(encoding, prompt, %{input: input} = s) do
    case bytes_until_eol(input, encoding, 0) do
      {:split, 0} ->
        {:eof, state_after_read(s, "", prompt)}
      {:split, count} ->
        {result, remainder} = :erlang.split_binary(input, count)

        {result, state_after_read(s, remainder, prompt)}
      {:replace_split, count} ->
        {result, remainder} = :erlang.split_binary(input, count)

        {binary_part(result, 0, byte_size(result) - 2) <> "\n", state_after_read(s, remainder, prompt)}
      :error
        -> {{:error, :collect_line}, s}
    end
  end

  ## get_until

  defp get_until(encoding, prompt, mod, fun, args, %{input: input} = s) do
    case do_get_until(input, encoding, mod, fun, args) do
      {result, input, count} ->
        input =
         case input do
           :eof -> ""
           _ -> list_to_binary(input, encoding)
         end

        {get_until_result(result, encoding), state_after_read(s, input, prompt, count)}

      :error ->
        {:error, s}
    end
  end

  defp do_get_until(chars, encoding, mod, fun, args, continuation \\ [], count \\ 0)

  defp do_get_until("", encoding, mod, fun, args, continuation, count) do
    case apply(mod, fun, [continuation, :eof | args]) do
      {:done, result, rest} ->
        {result, rest, count + 1}
      {:more, next_continuation} ->
        do_get_until("", encoding, mod, fun, args, next_continuation, count + 1)
    end
  end

  defp do_get_until(chars, encoding, mod, fun, args, continuation, count) do
    case bytes_until_eol(chars, encoding, 0) do
      {r, c} when r in [:split, :replace_split] ->
        {line, rest} = :erlang.split_binary(chars, c)

        case apply(mod, fun, [continuation, binary_to_list(line, encoding) | args]) do
          {:done, result, :eof} ->
            {result, rest, count + 1}
          {:done, result, extra} ->
            {result, extra ++ binary_to_list(rest, encoding), count + 1}
          {:more, next_continuation} ->
            do_get_until(rest, encoding, mod, fun, args, next_continuation, count + 1)
        end

      :error ->
        :error
    end
  end

  defp binary_to_list(l, _)        when is_list(l),   do: l
  defp binary_to_list(b, :unicode) when is_binary(b), do: to_charlist(b)
  defp binary_to_list(b, :latin1)  when is_binary(b), do: :binary.bin_to_list(b)
  defp list_to_binary(b, _)        when is_binary(b), do: b
  defp list_to_binary(l, :unicode) when is_list(l),   do: to_string(l)
  defp list_to_binary(l, :latin1)  when is_list(l),   do: :binary.list_to_bin(l)

  # From http://erlang.org/doc/apps/stdlib/io_protocol.html: Result can be any
  # Erlang term, but if it is a list(), the I/O server can convert it to a binary().
  defp get_until_result(l, encoding) when is_list(l), do: list_to_binary(l, encoding)
  defp get_until_result(other, _), do: other

  ## io_requests

  defp io_requests([r | rs], {:ok, s}) do
    io_requests(rs, io_request(r, s))
  end

  defp io_requests(_, result) do
    result
  end

  ## helpers

  defp state_after_read(state, remainder, prompt, count \\ 1)

  defp state_after_read(%{capture_prompt: false} = s, remainder, _prompt, _count) do
    %{s | input: remainder}
  end

  defp state_after_read(%{capture_prompt: true, output: output} = s, remainder, prompt, count) do
    %{s | input: remainder, output: <<output::binary, :binary.copy(IO.chardata_to_string(prompt), count)::binary>>}
  end

  defp bytes_until_eol("", _, count), do: {:split, count}
  defp bytes_until_eol(<<"\r\n"::binary, _::binary>>, _, count), do: {:replace_split, count + 2}
  defp bytes_until_eol(<<"\n"::binary, _::binary>>, _, count), do: {:split, count + 1}

  defp bytes_until_eol(<<head::utf8, tail::binary>>, :unicode, count) do
    bytes_until_eol(tail, :unicode, count + byte_size(<<head::utf8>>))
  end

  defp bytes_until_eol(<<_, tail::binary>>, :latin1, count) do
    bytes_until_eol(tail, :latin1, count + 1)
  end

  defp bytes_until_eol(<<_::binary>>, _, _), do: :error

  defp io_reply(from, reply_as, reply) do
    send from, {:io_reply, reply_as, reply}
  end

  defp to_reply(list) when is_list(list), do: IO.chardata_to_string(list)
  defp to_reply(other), do: other
end
