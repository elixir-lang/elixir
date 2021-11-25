defmodule Inspect.Utils do
  @moduledoc false

  def pad(message, padding_length)
      when is_binary(message) and is_integer(padding_length) and padding_length >= 0 do
    padding = String.duplicate(" ", padding_length)

    message
    |> String.replace("\\n", "\n")
    |> String.split("\n")
    |> Enum.map(fn
      "" -> ""
      line -> padding <> line
    end)
    |> Enum.join("\n")
    |> String.trim_trailing("\n")
  end
end
