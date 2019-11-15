defmodule ExUnit.CaptureIO do
  @moduledoc ~S"""
  Functionality to capture IO for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureIO

        test "example" do
          assert capture_io(fn -> IO.puts("a") end) == "a\n"
        end
      end

  """

  @doc """
  Captures IO generated when evaluating `fun`.

  Returns the binary which is the captured output.

  By default, `capture_io` replaces the `group_leader` (`:stdio`)
  for the current process. Capturing the group leader is done per
  process and therefore can be done concurrently.

  However, the capturing of any other named device, such as `:stderr`,
  happens globally and persists until the function has ended. While this means
  it is safe to run your tests with `async: true` in many cases, captured output
  may include output from a different test and care must be taken when using
  `capture_io` with a named process asynchronously.

  A developer can set a string as an input. The default input is an empty
  string. If capturing a named device asynchronously, an input can only be given
  to the first capture. Any further capture that is given to a capture on that
  device will raise an exception and would indicate that the test should be run
  synchronously.

  Similarly, once a capture on a named device has begun, the encoding on that
  device cannot be changed in a subsequent concurrent capture. An error will
  be raised in this case.

  ## IO devices

  You may capture the IO from any registered IO device. The device name given
  must be an atom representing the name of a registered process. In addition,
  Elixir provides two shortcuts:

    * `:stdio` - a shortcut for `:standard_io`, which maps to
      the current `Process.group_leader/0` in Erlang

    * `:stderr` - a shortcut for the named process `:standard_error`
      provided in Erlang

  ## Options

    * `:capture_prompt` - Define if prompts (specified as arguments to
      `IO.get*` functions) should be captured. Defaults to `true`. For
      IO devices other than `:stdio`, the option is ignored.

    * `:encoding` (since v1.10.0) - encoding of the IO device. Allowed
      values are `:unicode` (default) and `:latin1`.

  ## Examples

      iex> capture_io(fn -> IO.write("john") end) == "john"
      true

      iex> capture_io(:stderr, fn -> IO.write(:stderr, "john") end) == "john"
      true

      iex> capture_io(:standard_error, fn -> IO.write(:stderr, "john") end) == "john"
      true

      iex> capture_io("this is input", fn ->
      ...>   input = IO.gets("> ")
      ...>   IO.write(input)
      ...> end) == "> this is input"
      true

      iex> capture_io([input: "this is input", capture_prompt: false], fn ->
      ...>   input = IO.gets("> ")
      ...>   IO.write(input)
      ...> end) == "this is input"
      true

  ## Returning values

  As seen in the examples above, `capture_io` returns the captured output.
  If you want to also capture the result of the function executed inside
  the `capture_io`, you can use `Kernel.send/2` to send yourself a message
  and use `ExUnit.Assertions.assert_received/2` to match on the results:

      capture_io([input: "this is input", capture_prompt: false], fn ->
        send(self(), {:block_result, 42})
        # ...
      end)

      assert_received {:block_result, 42}

  """
  @spec capture_io((() -> any())) :: String.t()
  def capture_io(fun) when is_function(fun, 0) do
    capture_io(:stdio, [], fun)
  end

  @spec capture_io(atom(), (() -> any())) :: String.t()
  def capture_io(device, fun) when is_atom(device) and is_function(fun, 0) do
    capture_io(device, [], fun)
  end

  @spec capture_io(String.t(), (() -> any())) :: String.t()
  def capture_io(input, fun) when is_binary(input) and is_function(fun, 0) do
    capture_io(:stdio, [input: input], fun)
  end

  @spec capture_io(keyword(), (() -> any())) :: String.t()
  def capture_io(options, fun) when is_list(options) and is_function(fun, 0) do
    capture_io(:stdio, options, fun)
  end

  @spec capture_io(atom(), String.t(), (() -> any())) :: String.t()
  def capture_io(device, input, fun)
      when is_atom(device) and is_binary(input) and is_function(fun, 0) do
    capture_io(device, [input: input], fun)
  end

  @spec capture_io(atom(), keyword(), (() -> any())) :: String.t()
  def capture_io(device, options, fun)
      when is_atom(device) and is_list(options) and is_function(fun, 0) do
    do_capture_io(map_dev(device), options, fun)
  end

  defp map_dev(:stdio), do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other), do: other

  defp do_capture_io(:standard_io, options, fun) do
    prompt_config = Keyword.get(options, :capture_prompt, true)
    encoding = Keyword.get(options, :encoding, :unicode)
    input = Keyword.get(options, :input, "")

    original_gl = Process.group_leader()
    {:ok, capture_gl} = StringIO.open(input, capture_prompt: prompt_config, encoding: encoding)

    try do
      Process.group_leader(self(), capture_gl)
      do_capture_gl(capture_gl, fun)
    after
      Process.group_leader(self(), original_gl)
    end
  end

  defp do_capture_io(device, options, fun) do
    input = Keyword.get(options, :input, "")
    encoding = Keyword.get(options, :encoding, :unicode)

    case ExUnit.CaptureServer.device_capture_on(device, encoding, input) do
      {:ok, ref} ->
        try do
          fun.()
          ExUnit.CaptureServer.device_output(device, ref)
        after
          ExUnit.CaptureServer.device_capture_off(ref)
        end

      {:error, :no_device} ->
        raise "could not find IO device registered at #{inspect(device)}"

      {:error, {:changed_encoding, current_encoding}} ->
        raise ArgumentError, """
        attempted to change the encoding for a currently captured device #{inspect(device)}.

        Currently set as: #{inspect(current_encoding)}
        Given: #{inspect(encoding)}

        If you need to use multiple encodings on a captured device, you cannot \
        run your test asynchronously
        """

      {:error, :input_on_already_captured_device} ->
        raise ArgumentError,
              "attempted multiple captures on device #{inspect(device)} with input. " <>
                "If you need to give an input to a captured device, you cannot run your test asynchronously"
    end
  end

  defp do_capture_gl(string_io, fun) do
    try do
      fun.()
    catch
      kind, reason ->
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      _ ->
        {:ok, {_input, output}} = StringIO.close(string_io)
        output
    end
  end
end
