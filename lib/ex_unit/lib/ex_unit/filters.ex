defmodule ExUnit.Filters do
  @moduledoc """
  Conveniences for parsing and evaluating filters.
  """

  @doc """
  Parses the given filters, as one would receive from the command line.

  ## Examples

      iex> ExUnit.Filters.parse(["foo:bar", "baz"])
      [foo: "bar", baz: true]

  """
  @spec parse([String.t]) :: Keyword.t
  def parse(filters) do
    Enum.map filters, fn filter ->
      case String.split(filter, ":", global: false) do
        [key, value] -> { binary_to_atom(key), parse_value(value) }
        [key] -> { binary_to_atom(key), true }
      end
    end
  end

  defp parse_value("true"),  do: true
  defp parse_value("false"), do: false
  defp parse_value(value),   do: value

  @doc """
  Evaluates the include and exclude filters against the
  given tags. Expects filters to be normalized into a keyword
  list where each key is an atom and the value is a list.

  ## Examples

      iex> ExUnit.Filters.eval([foo: ["bar"]], [], [foo: "bar"])
      :ok

      iex> ExUnit.Filters.eval([foo: ["bar"]], [], [foo: "baz"])
      { :error, :foo }

  """
  @spec eval(Keyword.t(list), Keyword.t(list), Keyword.t) :: :ok | { :error, atom }
  def eval(include, exclude, tags) do
    Enum.find_value tags, :ok, fn { tag_key, _ } = tag ->
      unless tag_accepted?(include, exclude, tag), do: { :error, tag_key }
    end
  end

  defp tag_accepted?(include, exclude, tag) do
    tag_include?(include, tag) and not tag_exclude?(exclude, tag)
  end

  defp tag_include?(dict, { tag_key, tag_value }) do
    case Dict.fetch(dict, tag_key) do
      { :ok, list } -> tag_value in list
      :error -> true
    end
  end

  defp tag_exclude?(dict, { tag_key, tag_value }) do
    case Dict.fetch(dict, tag_key) do
      { :ok, list } -> tag_value in list
      :error -> false
    end
  end
end
  