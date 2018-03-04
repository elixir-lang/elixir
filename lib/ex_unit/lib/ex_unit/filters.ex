defmodule ExUnit.Filters do
  alias ExUnit.Manifest

  @moduledoc """
  Conveniences for parsing and evaluating filters.
  """

  @type t :: list({atom, Regex.t() | String.Chars.t()} | atom)

  @doc """
  Parses filters out of a path.

  Determines whether a given file path (supplied to ExUnit/Mix as arguments
  on the command line) includes a line number filter, and if so returns the
  appropriate ExUnit configuration options.
  """
  @spec parse_path(String.t()) :: {String.t(), any}
  def parse_path(file) do
    {paths, [line]} = file |> String.split(":") |> Enum.split(-1)

    case Integer.parse(line) do
      {_, ""} ->
        {Enum.join(paths, ":"), exclude: [:test], include: [line: line]}

      _ ->
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

      iex> ExUnit.Filters.normalize([foo: "true"], [:foo])
      {[foo: "true"], [:foo]}

      iex> ExUnit.Filters.normalize([:foo], [foo: "true"])
      {[:foo], []}

      iex> ExUnit.Filters.normalize([foo: "true"], [foo: true])
      {[foo: "true"], []}

      iex> ExUnit.Filters.normalize([foo: true], [foo: "true"])
      {[foo: true], []}

  """
  @spec normalize(t | nil, t | nil) :: {t, t}
  def normalize(include, exclude) do
    {include_atoms, include_tags} =
      include |> List.wrap() |> Enum.uniq() |> Enum.split_with(&is_atom/1)

    {exclude_atoms, exclude_tags} =
      exclude |> List.wrap() |> Enum.uniq() |> Enum.split_with(&is_atom/1)

    exclude_tags = Map.new(exclude_tags)

    exclude_included =
      for include_tag <- include_tags, key = has_tag(include_tag, exclude_tags), do: key

    exclude_tags =
      exclude_tags |> Map.drop(include_atoms) |> Map.drop(exclude_included) |> Map.to_list()

    {include_atoms ++ include_tags, (exclude_atoms -- include_atoms) ++ exclude_tags}
  end

  @doc """
  Parses the given filters, as one would receive from the command line.

  ## Examples

      iex> ExUnit.Filters.parse(["foo:bar", "baz", "line:9", "bool:true"])
      [{:foo, "bar"}, :baz, {:line, "9"}, {:bool, "true"}]

  """
  @spec parse([String.t()]) :: t
  def parse(filters) do
    Enum.map(filters, fn filter ->
      case String.split(filter, ":", parts: 2) do
        [key, value] -> {String.to_atom(key), value}
        [key] -> String.to_atom(key)
      end
    end)
  end

  @doc """
  Returns a tuple containing useful information about test failures from the
  manifest. The tuple contains:

    - A set of files that contain tests that failed the last time they ran.
      The paths are absolute paths.
    - A set of test ids that failed the last time they ran
  """
  @spec failure_info(Path.t()) :: {MapSet.t(Path.t()), MapSet.t(Manifest.test_id())}
  def failure_info(manifest_file) do
    manifest = Manifest.read(manifest_file)
    {Manifest.files_with_failures(manifest), Manifest.failed_test_ids(manifest)}
  end

  @doc """
  Evaluates the `include` and `exclude` filters against the given `tags` to
  determine if tests should be skipped or excluded.

  Some filters, like `:line`, may require the whole test collection to
  find the closest line, that's why it must also be passed as argument.

  Filters can either be a regular expression or any data structure
  that implements to `String.Chars`, which is invoked before comparing
  the filter with the tag value.

  ## Examples

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], %{foo: "bar"}, [])
      :ok

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], %{foo: "baz"}, [])
      {:excluded, "due to foo filter"}

  """
  @spec eval(t, t, map, [ExUnit.Test.t()]) :: :ok | {:error, binary}
  def eval(include, exclude, tags, collection) when is_map(tags) do
    skip? = not Enum.any?(include, &has_tag(&1, %{skip: true}, collection))

    case Map.fetch(tags, :skip) do
      {:ok, msg} when is_binary(msg) and skip? ->
        {:skipped, msg}

      {:ok, true} when skip? ->
        {:skipped, "due to skip tag"}

      _ ->
        excluded = Enum.find_value(exclude, &has_tag(&1, tags, collection))

        if !excluded or Enum.any?(include, &has_tag(&1, tags, collection)) do
          :ok
        else
          {:excluded, "due to #{excluded} filter"}
        end
    end
  end

  defp has_tag({:line, line}, %{line: _, describe_line: describe_line} = tags, collection) do
    line = to_integer(line)

    cond do
      describe_line == line ->
        true

      describe_block?(line, collection) ->
        false

      true ->
        tags.line <= line and closest_test_before_line(line, collection).tags.line == tags.line
    end
  end

  defp has_tag(pair, tags, _collection) do
    has_tag(pair, tags)
  end

  defp has_tag({key, %Regex{} = value}, tags) when is_atom(key) do
    case Map.fetch(tags, key) do
      {:ok, tag} -> to_string(tag) =~ value and key
      _ -> false
    end
  end

  defp has_tag({key, value}, tags) when is_atom(key) do
    case Map.fetch(tags, key) do
      {:ok, ^value} -> key
      {:ok, tag} -> compare(to_string(tag), to_string(value)) and key
      _ -> false
    end
  end

  defp has_tag(key, tags) when is_atom(key), do: Map.has_key?(tags, key) and key

  defp to_integer(integer) when is_integer(integer), do: integer
  defp to_integer(integer) when is_binary(integer), do: String.to_integer(integer)

  defp compare("Elixir." <> tag1, tag2), do: compare(tag1, tag2)
  defp compare(tag1, "Elixir." <> tag2), do: compare(tag1, tag2)
  defp compare(tag, tag), do: true
  defp compare(_, _), do: false

  defp describe_block?(line, collection) do
    Enum.any?(collection, fn %ExUnit.Test{tags: %{describe_line: describe_line}} ->
      line == describe_line
    end)
  end

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
