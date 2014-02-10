:application.start(:iex)
ExUnit.start [trace: "--trace" in System.argv]

defmodule IEx.Case do
  @moduledoc false

  #
  # Provides convenience functions for testing IEx-related functionality.
  # Use this module inside your test module like this:
  #
  #   defmodule IEx.InteractionTest do
  #     use IEx.Case
  #
  #     test :input do
  #       assert capture_iex("1+2") == "3"
  #     end
  #   end
  #
  # The environment provided by capture_iex is mostly similar to the normal IEx
  # session, except colors are disabled by default and .iex files are not
  # loaded.
  #
  # You can provide your own IEx.Options and a path to a .iex file as
  # additional arguments to the capture_iex function.
  #

  defmacro __using__(_) do
    quote do
      use ExUnit.Case, async: false
      import ExUnit.CaptureIO
      import unquote(__MODULE__)

      setup do
        opts = IEx.Options.get
        IEx.Options.set :colors, [enabled: false]
        { :ok, [iex_opts: opts] }
      end

      teardown context do
        IEx.Options.set context[:iex_opts]
        :ok
      end
    end
  end

  @doc """
  Starts an IEx eval loop, feeds it the provided input and returns produced
  output. The output is stripped of the first intro line and of any trailing
  whitespace.

  Options, if provided, will be set before the eval loop is started.

  If you provide server options, it will be passed to
  IEx.Server.start to be used in the normal .iex loading process.
  """
  def capture_iex(input, options \\ [], server_options \\ [], capture_prompt \\ false) do
    Enum.each options, fn { opt, value } ->
      IEx.Options.set(opt, value)
    end

    ExUnit.CaptureIO.capture_io([input: input, capture_prompt: capture_prompt], fn ->
      server_options = Keyword.put_new(server_options, :dot_iex_path, "")
      IEx.Server.start(server_options, fn -> end)
    end) |> strip_iex
  end

  defp strip_iex(string) do
    string
    |> strip_line   # strip the greeting
    |> String.strip
  end

  defp strip_line(string) do
    Regex.replace ~r/\A.+?$/ms, string, ""
  end
end

