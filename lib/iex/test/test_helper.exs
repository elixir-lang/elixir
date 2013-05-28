:application.start(:iex)
ExUnit.start []

defmodule IEx.Case do
  defmacro __using__(_) do
    quote do
      use ExUnit.Case, async: false
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

  def capture_iex(input, options // [], dot_iex_path // "") do
    Enum.each options, fn { opt, value } ->
      IEx.Options.set(opt, value)
    end

    ExUnit.CaptureIO.capture_io(input, fn ->
      IEx.Server.start(iex_config(dot_iex_path: dot_iex_path))
    end) |> strip_iex
  end

  def iex_exception(name, message // "") do
    %r/#{iex_format_exception(name, message)}/
  end

  def iex_format_exception(name, message // "") do
    "\\*\\* \\(#{Module.to_binary name}\\) (.*?)#{message}"
  end

  defp iex_config(opts) do
    IEx.boot_config(opts)
  end

  defp strip_iex(string) do
    string
    |> strip_line   # strip the greeting
    |> String.strip
  end

  defp strip_line(string) do
    Regex.replace %r/\A.+?$/ms, string, ""
  end
end

