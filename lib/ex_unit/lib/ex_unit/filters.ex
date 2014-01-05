defmodule ExUnit.Filters do
  @moduledoc """
  Conveniences for parsing and evaluating filters.
  """

  @doc """
  Parses the given filters, as one would receive from the command line.

  ## Examples

      iex> ExUnit.Filters.parse(["foo:bar", "baz"])
      [{:foo, "bar"}, :baz]

  """
  @spec parse([String.t]) :: Keyword.t
  def parse(filters) do
    Enum.map filters, fn filter ->
      case String.split(filter, ":", global: false) do
        [key, value] -> { binary_to_atom(key), parse_value(value) }
        [key] -> binary_to_atom(key)
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

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], [foo: "bar"])
      :ok

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], [foo: "baz"])
      { :error, :foo }

  """
  @spec eval(Keyword.t(list), Keyword.t(list), Keyword.t) :: :ok | { :error, atom }
  def eval(include, exclude, tags) do
    excluded = Enum.find_value exclude, &has_tag(&1, tags)
    if !excluded or Enum.any?(include, &has_tag(&1, tags)) do
      :ok
    else
      { :error, excluded }
    end
  end

  defp has_tag({ key, value }, tags) when is_atom(key),
    do: Keyword.fetch(tags, key) == { :ok, value } and key
  defp has_tag(key, tags) when is_atom(key),
    do: Keyword.has_key?(tags, key) and key
end
