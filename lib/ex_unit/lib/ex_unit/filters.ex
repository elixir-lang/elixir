defmodule ExUnit.Filters do
  @moduledoc """
  Conveniences for parsing and evaluating filters.
  """

  @type t :: list({atom, Regex.t | String.Chars.t} | atom)

  @doc """
  Parses filters out of a path.

  Determines whether a given file path (supplied to ExUnit/Mix as arguments
  on the command line) includes a line number filter, and if so returns the
  appropriate ExUnit configuration options.
  """
  @spec parse_path(String.t) :: {String.t, any}
  def parse_path(file) do
    case Regex.run(~r/^(.+):(\d+)$/, file, capture: :all_but_first) do
      [file, line_number] ->
        {file, exclude: [:test], include: [line: line_number]}
      nil ->
        {file, []}
    end
  end

  @doc """
  Normalizes include and excludes to remove duplicates
  and keep precedence.

  ## Examples

      iex> ExUnit.Filters.normalize(nil, nil)
      {[], []}

      iex> ExUnit.Filters.normalize([:foo, :bar, :bar], [:foo, :baz])
      {[:foo, :bar], [:baz]}

  """
  @spec normalize(t | nil, t | nil) :: {t, t}
  def normalize(include, exclude) do
    include = include |> List.wrap |> Enum.uniq
    exclude = exclude |> List.wrap |> Enum.uniq |> Kernel.--(include)
    {include, exclude}
  end

  @doc """
  Parses the given filters, as one would receive from the command line.

  ## Examples

      iex> ExUnit.Filters.parse(["foo:bar", "baz", "line:9", "bool:true"])
      [{:foo, "bar"}, :baz, {:line, "9"}, {:bool, "true"}]

  """
  @spec parse([String.t]) :: t
  def parse(filters) do
    Enum.map filters, fn filter ->
      case String.split(filter, ":", parts: 2) do
        [key, value] -> {String.to_atom(key), value}
        [key] -> String.to_atom(key)
      end
    end
  end

  @doc """
  Evaluates the `include` and `exclude` filters against the given `tags`.

  Some filters, like `:line`, may require the whole test collection to
  find the closest line, that's why it must also be passed as argument.

  Filters can either be a regular expression or any data structure
  that implements to `String.Chars`, which is invoked before comparing
  the filter with the tag value.

  ## Examples

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], %{foo: "bar"}, [])
      :ok

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], %{foo: "baz"}, [])
      {:error, "due to foo filter"}

  """
  @spec eval(t, t, map, [ExUnit.Test.t]) :: :ok | {:error, atom}
  def eval(include, exclude, tags, collection) when is_map(tags) do
    case Map.fetch(tags, :skip) do
      {:ok, msg} when is_binary(msg) ->
        {:error, msg}
      {:ok, true} ->
        {:error, "due to skip tag"}
      _ ->
        excluded = Enum.find_value exclude, &has_tag(&1, tags, collection)
        if !excluded or Enum.any?(include, &has_tag(&1, tags, collection)) do
          :ok
        else
          {:error, "due to #{excluded} filter"}
        end
    end
  end

  defp has_tag({:line, line}, tags, collection) do
    line = to_integer(line)
    tags.line <= line and
      closest_test_before_line(line, collection).tags.line == tags.line
  end

  defp has_tag({key, %Regex{} = value}, tags, _collection) when is_atom(key) do
    case Map.fetch(tags, key) do
      {:ok, tag} -> to_string(tag) =~ value and key
      _ -> false
    end
  end

  defp has_tag({key, value}, tags, _collection) when is_atom(key) do
    case Map.fetch(tags, key) do
      {:ok, ^value} -> key
      {:ok, tag} -> compare(to_string(tag), to_string(value)) and key
      _ -> false
    end
  end

  defp has_tag(key, tags, _collection) when is_atom(key),
    do: Map.has_key?(tags, key) and key

  defp to_integer(integer) when is_integer(integer), do: integer
  defp to_integer(integer) when is_binary(integer),  do: String.to_integer(integer)

  defp compare("Elixir." <> tag1, tag2), do: compare(tag1, tag2)
  defp compare(tag1, "Elixir." <> tag2), do: compare(tag1, tag2)
  defp compare(tag, tag), do: true
  defp compare(_, _), do: false

  defp closest_test_before_line(line, collection) do
    Enum.min_by(collection, fn %ExUnit.Test{tags: %{line: test_line}} ->
      if line - test_line >= 0 do
        line - test_line
      else
        :infinity
      end
    end)
  end
end
