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

        test "checking the return value and the IO output" do
          fun = fn ->
            assert Enum.each(["some", "example"], &IO.puts(&1)) == :ok
          end

          assert capture_io(fun) == "some\nexample\n"
          # tip: or use only: "capture_io(fun)" to silence the IO output (so only assert the return value)
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
  happens globally and requires `async: false`.

  When capturing `:stdio`, if the `:capture_prompt` option is `false`,
  prompts (specified as arguments to `IO.get*` functions) are not
  captured.

  A developer can set a string as an input. The default input
  is an empty string (which is equivalent to `:eof`).

  ## Examples

      iex> capture_io(fn -> IO.write("john") end) == "john"
      true

      iex> capture_io(:stderr, fn -> IO.write(:stderr, "john") end) == "john"
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
  def capture_io(fun) when is_function(fun, 0) do
    capture_io(:stdio, [], fun)
  end

  def capture_io(device, fun) when is_atom(device) and is_function(fun, 0) do
    capture_io(device, [], fun)
  end

  def capture_io(input, fun) when is_binary(input) and is_function(fun, 0) do
    capture_io(:stdio, [input: input], fun)
  end

  def capture_io(options, fun) when is_list(options) and is_function(fun, 0) do
    capture_io(:stdio, options, fun)
  end

  def capture_io(device, input, fun)
      when is_atom(device) and is_binary(input) and is_function(fun, 0) do
    capture_io(device, [input: input], fun)
  end

  def capture_io(device, options, fun)
      when is_atom(device) and is_list(options) and is_function(fun, 0) do
    do_capture_io(map_dev(device), options, fun)
  end

  defp map_dev(:stdio), do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other), do: other

  defp do_capture_io(:standard_io, options, fun) do
    prompt_config = Keyword.get(options, :capture_prompt, true)
    input = Keyword.get(options, :input, "")

    original_gl = Process.group_leader()
    {:ok, capture_gl} = StringIO.open(input, capture_prompt: prompt_config)

    try do
      Process.group_leader(self(), capture_gl)
      do_capture_io(capture_gl, fun)
    after
      Process.group_leader(self(), original_gl)
    end
  end

  defp do_capture_io(device, options, fun) do
    input = Keyword.get(options, :input, "")
    {:ok, string_io} = StringIO.open(input)

    case ExUnit.CaptureServer.device_capture_on(device, string_io) do
      {:ok, ref} ->
        try do
          do_capture_io(string_io, fun)
        after
          ExUnit.CaptureServer.device_capture_off(ref)
        end

      {:error, :no_device} ->
        _ = StringIO.close(string_io)
        raise "could not find IO device registered at #{inspect(device)}"

      {:error, :already_captured} ->
        _ = StringIO.close(string_io)
        raise "IO device registered at #{inspect(device)} is already captured"
    end
  end

  defp do_capture_io(string_io, fun) do
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
