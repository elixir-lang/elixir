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

  @doc ~S"""
  Creates an IO device.

  `string` will be the initial input of the newly created
  device.

  If the `:capture_prompt` option is set to `true`,
  prompts (specified as arguments to `IO.get*` functions)
  are captured in the output.

  The device will be created and sent to the function given.
  When the function returns, the device will be closed. The final
  result will be a tuple with `:ok` and the result of the function.

  ## Examples

      iex> StringIO.open("foo", [], fn pid ->
      ...>   input = IO.gets(pid, ">")
      ...>   IO.write(pid, "The input was #{input}")
      ...>   StringIO.contents(pid)
      ...> end)
      {:ok, {"", "The input was foo"}}

      iex> StringIO.open("foo", [capture_prompt: true], fn pid ->
      ...>   input = IO.gets(pid, ">")
      ...>   IO.write(pid, "The input was #{input}")
      ...>   StringIO.contents(pid)
      ...> end)
      {:ok, {"", ">The input was foo"}}

  """
  @doc since: "1.7.0"
  @spec open(binary, keyword, (pid -> res)) :: {:ok, res} when res: var
  def open(string, options, function)
      when is_binary(string) and is_list(options) and is_function(function, 1) do
    {:ok, pid} = GenServer.start_link(__MODULE__, {string, options}, [])

    try do
      {:ok, function.(pid)}
    after
      {:ok, {_input, _output}} = close(pid)
    end
  end

  @doc ~S"""
  Creates an IO device.

  `string` will be the initial input of the newly created
  device.

  `options_or_function` can be a keyword list of options or
  a function.

  If options are provided, the result will be `{:ok, pid}`, returning the
  IO device created. The option `:capture_prompt`, when set to `true`, causes
  prompts (which are specified as arguments to `IO.get*` functions) to be
  included in the device's output.

  If a function is provided, the device will be created and sent to the
  function. When the function returns, the device will be closed. The final
  result will be a tuple with `:ok` and the result of the function.

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

      iex> StringIO.open("foo", fn pid ->
      ...>   input = IO.gets(pid, ">")
      ...>   IO.write(pid, "The input was #{input}")
      ...>   StringIO.contents(pid)
      ...> end)
      {:ok, {"", "The input was foo"}}

  """
  @spec open(binary, keyword) :: {:ok, pid}
  @spec open(binary, (pid -> res)) :: {:ok, res} when res: var
  def open(string, options_or_function \\ [])

  def open(string, options_or_function) when is_binary(string) and is_list(options_or_function) do
    GenServer.start_link(__MODULE__, {string, options_or_function}, [])
  end

  def open(string, options_or_function)
      when is_binary(string) and is_function(options_or_function, 1) do
    open(string, [], options_or_function)
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

  @impl true
  def init({string, options}) do
    capture_prompt = options[:capture_prompt] || false
    {:ok, %{input: string, output: "", capture_prompt: capture_prompt}}
  end

  @impl true
  def handle_info({:io_request, from, reply_as, req}, state) do
    state = io_request(from, reply_as, req, state)
    {:noreply, state}
  end

  def handle_info(_message, state) do
    {:noreply, state}
  end

  @impl true
  def handle_call(:contents, _from, %{input: input, output: output} = state) do
    {:reply, {input, output}, state}
  end

  def handle_call(:flush, _from, %{output: output} = state) do
    {:reply, output, %{state | output: ""}}
  end

  def handle_call(:close, _from, %{input: input, output: output} = state) do
    {:stop, :normal, {:ok, {input, output}}, state}
  end

  defp io_request(from, reply_as, req, state) do
    {reply, state} = io_request(req, state)
    io_reply(from, reply_as, to_reply(reply))
    state
  end

  defp io_request({:put_chars, chars} = req, state) do
    put_chars(:latin1, chars, req, state)
  end

  defp io_request({:put_chars, mod, fun, args} = req, state) do
    put_chars(:latin1, apply(mod, fun, args), req, state)
  end

  defp io_request({:put_chars, encoding, chars} = req, state) do
    put_chars(encoding, chars, req, state)
  end

  defp io_request({:put_chars, encoding, mod, fun, args} = req, state) do
    put_chars(encoding, apply(mod, fun, args), req, state)
  end

  defp io_request({:get_chars, prompt, count}, state) when count >= 0 do
    io_request({:get_chars, :latin1, prompt, count}, state)
  end

  defp io_request({:get_chars, encoding, prompt, count}, state) when count >= 0 do
    get_chars(encoding, prompt, count, state)
  end

  defp io_request({:get_line, prompt}, state) do
    io_request({:get_line, :latin1, prompt}, state)
  end

  defp io_request({:get_line, encoding, prompt}, state) do
    get_line(encoding, prompt, state)
  end

  defp io_request({:get_until, prompt, mod, fun, args}, state) do
    io_request({:get_until, :latin1, prompt, mod, fun, args}, state)
  end

  defp io_request({:get_until, encoding, prompt, mod, fun, args}, state) do
    get_until(encoding, prompt, mod, fun, args, state)
  end

  defp io_request({:get_password, encoding}, state) do
    get_line(encoding, "", state)
  end

  defp io_request({:setopts, _opts}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request(:getopts, state) do
    {{:ok, [binary: true, encoding: :unicode]}, state}
  end

  defp io_request({:get_geometry, :columns}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_geometry, :rows}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:requests, reqs}, state) do
    io_requests(reqs, {:ok, state})
  end

  defp io_request(_, state) do
    {{:error, :request}, state}
  end

  ## put_chars

  defp put_chars(encoding, chars, req, %{output: output} = state) do
    case :unicode.characters_to_binary(chars, encoding, :unicode) do
      string when is_binary(string) ->
        {:ok, %{state | output: output <> string}}

      {_, _, _} ->
        {{:error, req}, state}
    end
  rescue
    ArgumentError -> {{:error, req}, state}
  end

  ## get_chars

  defp get_chars(encoding, prompt, count, %{input: input} = state) do
    case get_chars(input, encoding, count) do
      {:error, _} = error ->
        {error, state}

      {result, input} ->
        {result, state_after_read(state, input, prompt, 1)}
    end
  end

  defp get_chars("", _encoding, _count) do
    {:eof, ""}
  end

  defp get_chars(input, :latin1, count) when byte_size(input) < count do
    {input, ""}
  end

  defp get_chars(input, :latin1, count) do
    <<chars::binary-size(count), rest::binary>> = input
    {chars, rest}
  end

  defp get_chars(input, :unicode, count) do
    with {:ok, count} <- split_at(input, count, 0) do
      <<chars::binary-size(count), rest::binary>> = input
      {chars, rest}
    end
  end

  defp split_at(_, 0, acc),
    do: {:ok, acc}

  defp split_at(<<h::utf8, t::binary>>, count, acc),
    do: split_at(t, count - 1, acc + byte_size(<<h::utf8>>))

  defp split_at(<<_, _::binary>>, _count, _acc),
    do: {:error, :invalid_unicode}

  defp split_at(<<>>, _count, acc),
    do: {:ok, acc}

  ## get_line

  defp get_line(encoding, prompt, %{input: input} = state) do
    case bytes_until_eol(input, encoding, 0) do
      {:split, 0} ->
        {:eof, state_after_read(state, "", prompt, 1)}

      {:split, count} ->
        {result, remainder} = :erlang.split_binary(input, count)
        {result, state_after_read(state, remainder, prompt, 1)}

      {:replace_split, count} ->
        {result, remainder} = :erlang.split_binary(input, count)
        result = binary_part(result, 0, byte_size(result) - 2) <> "\n"
        {result, state_after_read(state, remainder, prompt, 1)}

      :error ->
        {{:error, :collect_line}, state}
    end
  end

  ## get_until

  defp get_until(encoding, prompt, mod, fun, args, %{input: input} = state) do
    case get_until(input, encoding, mod, fun, args, [], 0) do
      {result, input, count} ->
        input =
          case input do
            :eof -> ""
            _ -> list_to_binary(input, encoding)
          end

        {get_until_result(result, encoding), state_after_read(state, input, prompt, count)}

      :error ->
        {:error, state}
    end
  end

  defp get_until("", encoding, mod, fun, args, continuation, count) do
    case apply(mod, fun, [continuation, :eof | args]) do
      {:done, result, rest} ->
        {result, rest, count + 1}

      {:more, next_continuation} ->
        get_until("", encoding, mod, fun, args, next_continuation, count + 1)
    end
  end

  defp get_until(chars, encoding, mod, fun, args, continuation, count) do
    case bytes_until_eol(chars, encoding, 0) do
      {kind, size} when kind in [:split, :replace_split] ->
        {line, rest} = :erlang.split_binary(chars, size)

        case apply(mod, fun, [continuation, binary_to_list(line, encoding) | args]) do
          {:done, result, :eof} ->
            {result, rest, count + 1}

          {:done, result, extra} ->
            {result, extra ++ binary_to_list(rest, encoding), count + 1}

          {:more, next_continuation} ->
            get_until(rest, encoding, mod, fun, args, next_continuation, count + 1)
        end

      :error ->
        :error
    end
  end

  defp binary_to_list(data, _) when is_list(data), do: data
  defp binary_to_list(data, :unicode) when is_binary(data), do: String.to_charlist(data)
  defp binary_to_list(data, :latin1) when is_binary(data), do: :erlang.binary_to_list(data)

  defp list_to_binary(data, _) when is_binary(data), do: data
  defp list_to_binary(data, :unicode) when is_list(data), do: List.to_string(data)
  defp list_to_binary(data, :latin1) when is_list(data), do: :erlang.list_to_binary(data)

  # From http://erlang.org/doc/apps/stdlib/io_protocol.html: result can be any
  # Erlang term, but if it is a list(), the I/O server can convert it to a binary().
  defp get_until_result(data, encoding) when is_list(data), do: list_to_binary(data, encoding)
  defp get_until_result(data, _), do: data

  ## io_requests

  defp io_requests([req | rest], {:ok, state}) do
    io_requests(rest, io_request(req, state))
  end

  defp io_requests(_, result) do
    result
  end

  ## helpers

  defp state_after_read(%{capture_prompt: false} = state, remainder, _prompt, _count) do
    %{state | input: remainder}
  end

  defp state_after_read(%{capture_prompt: true, output: output} = state, remainder, prompt, count) do
    output = <<output::binary, :binary.copy(IO.chardata_to_string(prompt), count)::binary>>
    %{state | input: remainder, output: output}
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
    send(from, {:io_reply, reply_as, reply})
  end

  defp to_reply(list) when is_list(list), do: IO.chardata_to_string(list)
  defp to_reply(other), do: other
end
