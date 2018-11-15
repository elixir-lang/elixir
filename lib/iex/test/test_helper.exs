assert_timeout = String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT") || "500")

System.put_env("ELIXIR_EDITOR", "echo")

:ok = Application.start(:iex)
IEx.configure(colors: [enabled: false])
ExUnit.start(trace: "--trace" in System.argv(), assert_receive_timeout: assert_timeout)

defmodule IEx.Case do
  use ExUnit.CaseTemplate
  @moduledoc false

  # Provides convenience functions for testing IEx-related functionality.
  # Use this module inside your test module like this:
  #
  #   defmodule IEx.InteractionTest do
  #     use IEx.Case
  #
  #     test "input" do
  #       assert capture_iex("1+2") == "3"
  #     end
  #   end
  #
  # The environment provided by capture_iex is mostly similar to the normal IEx
  # session, except colors are disabled by default and .iex files are not
  # loaded.
  #
  # You can provide your own IEx configuration and a path to a .iex file as
  # additional arguments to the capture_iex function.

  using do
    quote do
      import ExUnit.CaptureIO
      import ExUnit.CaptureLog
      import unquote(__MODULE__)
    end
  end

  keys = [:default_prompt, :alive_prompt, :inspect, :colors, :history_size]
  @iex_env Application.get_all_env(:iex) |> Keyword.take(keys)

  setup do
    on_exit(fn ->
      env = @iex_env
      Enum.each(env, fn {k, _} -> Application.delete_env(:iex, k) end)
      IEx.configure(env)
    end)

    :ok
  end

  @doc """
  Starts an IEx eval loop, feeds it the provided input and returns produced
  output. The output is stripped of the first intro line and of any trailing
  whitespace.

  Options, if provided, will be set before the eval loop is started.

  If you provide server options, it will be passed to
  IEx.Server.run to be used in the normal .iex loading process.
  """
  def capture_iex(input, options \\ [], server_options \\ [], capture_prompt \\ false) do
    IEx.configure(options)

    ExUnit.CaptureIO.capture_io([input: input, capture_prompt: capture_prompt], fn ->
      server_options = Keyword.put_new(server_options, :dot_iex_path, "")
      IEx.Server.run(server_options)
    end)
    |> strip_iex
  end

  defp strip_iex(string) do
    string
    |> String.split("\n", parts: 2)
    |> Enum.at(1)
    |> String.trim()
  end
end
