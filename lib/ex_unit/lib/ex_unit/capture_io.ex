defmodule ExUnit.CaptureIO do
  @moduledoc ~S"""
  Functionality to capture IO for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureIO

        test "example" do
          assert capture_io(fn ->
            IO.puts "a"
          end) == "a\n"
        end

        test "checking the return value and the IO output" do
          fun = fn ->
            assert Enum.each(["some", "example"], &(IO.puts &1)) == :ok
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
  for the current process. However, the capturing of any other
  named device, such as `:stderr`, is also possible globally by
  giving the registered device name explicitly as an argument.

  Note that when capturing something other than `:stdio`,
  the test should run with async false.

  When capturing `:stdio`, if the `:capture_prompt` option is `false`,
  prompts (specified as arguments to `IO.get*` functions) are not
  captured.

  A developer can set a string as an input. The default input is `:eof`.

  ## Examples

      iex> capture_io(fn -> IO.write "john" end) == "john"
      true

      iex> capture_io(:stderr, fn -> IO.write(:stderr, "john") end) == "john"
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

  ## Returning values

  As seen in the examples above, `capture_io` returns the captured output.
  If you want to also capture the result of the function executed inside
  the `capture_io`, you can use `Kernel.send/2` to send yourself a message
  and use `ExUnit.Assertions.assert_received/2` to match on the results:

      capture_io([input: "this is input", capture_prompt: false], fn ->
        send self(), {:block_result, 42}
        # ...
      end)

      assert_received {:block_result, 42}

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

  @doc """
  Captures IO generated when evaluating `fun` and the result of the evaluation of `fun`.

  Returns a tuple with the binary which is the captured output, and the result of the
  evaluation of `fun`.

  Behaves exactly as `capture_io` concerning everything else.

  ## Examples

      iex> capture_io_with_result("this is input", fn ->
      ...>   input = IO.gets ">"
      ...>   IO.write input
      ...>   42
      ...> end) == {">this is input", 42}
      true

  Also useful to simply ignore the generated output to avoid noise in tests.
  """
  def capture_io_with_result(fun) do
    do_capture_io(:standard_io, [with_result: true], fun)
  end

  def capture_io_with_result(device, fun) when is_atom(device) do
    capture_io_with_result(device, [], fun)
  end

  def capture_io_with_result(input, fun) when is_binary(input) do
    capture_io_with_result(:standard_io, [input: input], fun)
  end

  def capture_io_with_result(options, fun) when is_list(options) do
    capture_io_with_result(:standard_io, options, fun)
  end

  def capture_io_with_result(device, input, fun) when is_binary(input) do
    capture_io_with_result(device, [input: input], fun)
  end

  def capture_io_with_result(device, options, fun) when is_list(options) do
    do_capture_io(map_dev(device), Keyword.merge(options, with_result: true), fun)
  end

  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other),   do: other

  defp do_capture_io(:standard_io, options, fun) do
    prompt_config = Keyword.get(options, :capture_prompt, true)
    with_result   = Keyword.get(options, :with_result,    false)
    input = Keyword.get(options, :input, "")

    original_gl = Process.group_leader()
    {:ok, capture_gl} = StringIO.open(input, capture_prompt: prompt_config)
    try do
      Process.group_leader(self(), capture_gl)
      if with_result do
        do_capture_io_with_result(capture_gl, fun)
      else
        do_capture_io(capture_gl, fun)
      end
    after
      Process.group_leader(self(), original_gl)
    end
  end

  defp do_capture_io(device, options, fun) do
    input = Keyword.get(options, :input, "")
    with_result   = Keyword.get(options, :with_result,    false)
    {:ok, string_io} = StringIO.open(input)
    case ExUnit.CaptureServer.device_capture_on(device, string_io) do
      {:ok, ref} ->
        try do
          if with_result do
            do_capture_io_with_result(string_io, fun)
          else
            do_capture_io(string_io, fun)
          end
        after
          ExUnit.CaptureServer.device_capture_off(ref)
        end
      {:error, :no_device} ->
        _ = StringIO.close(string_io)
        raise "could not find IO device registered at #{inspect device}"
      {:error, :already_captured} ->
        _ = StringIO.close(string_io)
        raise "IO device registered at #{inspect device} is already captured"
    end
  end

  defp do_capture_io(string_io, fun) do
    try do
       _ = fun.()
      :ok
    catch
      kind, reason ->
        stack = System.stacktrace()
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, stack)
    else
      :ok ->
        {:ok, output} = StringIO.close(string_io)
        elem(output, 1)
    end
  end

  defp do_capture_io_with_result(string_io, fun) do
    try do
      {:ok, fun.()}
    catch
      kind, reason ->
        stack = System.stacktrace()
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, stack)
    else
      {:ok, result} ->
        {:ok, output} = StringIO.close(string_io)
        {elem(output, 1), result}
    end
  end
end
