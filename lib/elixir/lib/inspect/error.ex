# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule Inspect.Error do
  @moduledoc """
  Raised when a struct cannot be inspected.
  """
  @enforce_keys [:exception_module, :exception_message, :stacktrace, :inspected_struct]
  defexception @enforce_keys

  @impl true
  def exception(arguments) when is_list(arguments) do
    exception = Keyword.fetch!(arguments, :exception)
    exception_module = exception.__struct__
    exception_message = Exception.message(exception) |> String.trim_trailing("\n")
    stacktrace = Keyword.fetch!(arguments, :stacktrace)
    inspected_struct = Keyword.fetch!(arguments, :inspected_struct)

    %Inspect.Error{
      exception_module: exception_module,
      exception_message: exception_message,
      stacktrace: stacktrace,
      inspected_struct: inspected_struct
    }
  end

  @impl true
  def message(%__MODULE__{
        exception_module: exception_module,
        exception_message: exception_message,
        inspected_struct: inspected_struct
      }) do
    ~s'''
    got #{inspect(exception_module)} with message:

        """
    #{pad(exception_message, 4)}
        """

    while inspecting:

    #{pad(inspected_struct, 4)}
    '''
  end

  @doc false
  def pad(message, padding_length)
      when is_binary(message) and is_integer(padding_length) and padding_length >= 0 do
    padding = String.duplicate(" ", padding_length)

    message
    |> String.split("\n")
    |> Enum.map(fn
      "" -> "\n"
      line -> [padding, line, ?\n]
    end)
    |> IO.iodata_to_binary()
    |> String.trim_trailing("\n")
  end
end

defimpl Inspect, for: Inspect.Error do
  @impl true
  def inspect(%{stacktrace: stacktrace} = inspect_error, _opts) do
    message = Exception.message(inspect_error)
    format_output(message, stacktrace)
  end

  defp format_output(message, [_ | _] = stacktrace) do
    stacktrace = Exception.format_stacktrace(stacktrace)

    """
    #Inspect.Error<
    #{Inspect.Error.pad(message, 2)}

      Stacktrace:

    #{stacktrace}
    >\
    """
  end

  defp format_output(message, []) do
    """
    #Inspect.Error<
      #{Inspect.Error.pad(message, 2)}
    >\
    """
  end
end
