defmodule IEx.UnicodeIO do
  @moduledoc """
  This module implements the API used by IEX to
  interact with the console. This API may change
  in the future without warnings.
  """

  @doc """
  Implements the get IO API used by IEx. It receives the
  code cache, the instructions counter and needs to
  return a list with the new characters inserted.
  """
  def get(config) do
    prefix = if config.cache != [], do: "..."

    prompt =
      if is_alive do
        "#{prefix || remote_prefix}(#{node})#{config.counter}> "
      else
        "#{prefix || "iex"}(#{config.counter})> "
      end

    case IO.gets(prompt) do
      { :error, _ } -> ''
      data -> :unicode.characters_to_list(data)
    end
  end

  defp remote_prefix do
    if node == node(:erlang.group_leader), do: "iex", else: "rem"
  end

  @doc """
  Implements the put IO API used by IEx. It receives the
  result and prints it.
  """
  def put(result) do
    IO.inspect result
  end

  @doc """
  Implements the error IO API used by IEx. It prints error
  messages.
  """
  def error(result) do
    IO.puts result
  end
end
