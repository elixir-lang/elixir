defmodule ExUnit.CaptureIO do
  @moduledoc ~S"""
  Functionality to capture IO for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureIO

        test "checking the return value and the IO output" do
          fun = fn ->
            assert Enum.each(["some", "example"], &(IO.puts &1)) == :ok
          end

          assert capture_io(fun) == "some\nexample\n"

          # Or use only: `capture_io(fun)` to silence the
          # IO output (so only assert the return value)
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

  A developer can set a string as an input. The default
  input is `:eof`.

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
    unless original_io = Process.whereis(device) do
      raise "could not find IO device registered at #{inspect device}"
    end

    unless ExUnit.Server.add_device(device) do
      raise "IO device registered at #{inspect device} is already captured"
    end

    input = Keyword.get(options, :input, "")

    Process.unregister(device)
    {:ok, capture_io} = StringIO.open(input)
    Process.register(capture_io, device)

    try do
      do_capture_io(capture_io, fun)
    after
      try do
        Process.unregister(device)
      rescue
        ArgumentError -> nil
      end
      Process.register(original_io, device)
      ExUnit.Server.remove_device(device)
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
end
