defmodule ExUnit.Filters do
  @moduledoc """
  Conveniences for parsing and evaluating filters.
  """

  @type t :: list({atom, any} | atom)

  @doc """
  Determines whether a given file path (supplied to ExUnit/Mix as arguments
  on the command line) includes a line number filter, and if so returns the
  appropriate ExUnit configuration options.
  """
  @spec parse_file_path(String.t) :: {String.t, any}
  def parse_file_path(file) do
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
      case String.split(filter, ":", global: false) do
        [key, value] -> {binary_to_atom(key), value}
        [key] -> binary_to_atom(key)
      end
    end
  end

  @doc """
  Evaluates the include and exclude filters against the
  given tags. Expects filters to be normalized into a keyword
  list where each key is an atom and the value is a list.

  ## Examples

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], [foo: "bar"])
      :ok

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], [foo: "baz"])
      {:error, :foo}

  """
  @spec eval(t, t, Keyword.t) :: :ok | {:error, atom}
  def eval(include, exclude, tags) do
    excluded = Enum.find_value exclude, &has_tag(&1, tags)
    if !excluded or Enum.any?(include, &has_tag(&1, tags)) do
      :ok
    else
      {:error, excluded}
    end
  end

  defp has_tag({key, value}, tags) when is_atom(key) do
    case Keyword.fetch(tags, key) do
      {:ok, ^value} -> key
      {:ok, different_value} -> to_string(different_value) == value and key
      _ -> false
   end
  end
  defp has_tag(key, tags) when is_atom(key),
    do: Keyword.has_key?(tags, key) and key
end
