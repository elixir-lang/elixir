defmodule ExUnit.Filters do
  @moduledoc """
  Conveniences for parsing and evaluating filters.
  """
  alias ExUnit.FailuresManifest

  @type t :: list({atom, Regex.t() | String.Chars.t()} | atom)
  @type location :: {:location, {String.t(), pos_integer | [pos_integer, ...]}}
  @type ex_unit_opts :: [exclude: [:test], include: [location, ...]] | []

  @doc """
  Parses filters out of a path.

  Determines whether a given file path (supplied to ExUnit/Mix as arguments
  on the command line) includes a line number filter, and if so returns the
  appropriate ExUnit configuration options.
  """
  @spec parse_path(String.t()) :: {String.t(), ex_unit_opts}
  # TODO: Deprecate this on Elixir v1.20
  def parse_path(file_path) do
    {[parsed_path], ex_unit_opts} = parse_paths([file_path])
    {parsed_path, ex_unit_opts}
  end

  @doc """
  Like `parse_path/1` but for multiple paths.

  ExUnit filter options are combined.
  """
  @spec parse_paths([String.t()]) :: {[String.t()], ex_unit_opts}
  def parse_paths(file_paths) do
    {parsed_paths, locations} =
      Enum.map_reduce(file_paths, [], fn file_path, locations ->
        case extract_line_numbers(file_path) do
          {path, []} -> {path, locations}
          {path, lines} -> {path, [{:location, {path, lines}} | locations]}
        end
      end)

    ex_unit_opts =
      if locations == [], do: [], else: [exclude: [:test], include: Enum.reverse(locations)]

    {parsed_paths, ex_unit_opts}
  end

  defp extract_line_numbers(file_path) do
    case Path.relative_to_cwd(file_path) |> String.split(":") do
      [path] ->
        {path, []}

      [path | parts] ->
        {path_parts, line_numbers} = Enum.split_while(parts, &(to_line_number(&1) == nil))
        path = Enum.join([path | path_parts], ":") |> Path.split() |> Path.join()
        lines = for n <- line_numbers, valid_number = validate_line_number(n), do: valid_number

        case lines do
          [line] -> {path, line}
          lines -> {path, lines}
        end
    end
  end

  defp to_line_number(str) do
    case Integer.parse(str) do
      {x, ""} when x > 0 -> x
      _ -> nil
    end
  end

  defp validate_line_number(str) do
    number = to_line_number(str)
    number == nil && IO.warn("invalid line number given as ExUnit filter: #{str}", [])
    number
  end

  @doc """
  Normalizes `include` and `exclude` filters to remove duplicates
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

      iex> ExUnit.Filters.normalize([foo: 1, foo: 1, foo: 2], [])
      {[foo: 1, foo: 2], []}

      iex> ExUnit.Filters.normalize([], [foo: 1, foo: 1, foo: 2])
      {[], [foo: 1, foo: 2]}

  """
  @spec normalize(t | nil, t | nil) :: {t, t}
  def normalize(include, exclude) do
    {include_atoms, include_tags} =
      include |> List.wrap() |> Enum.uniq() |> Enum.split_with(&is_atom/1)

    {exclude_atoms, exclude_tags} =
      exclude |> List.wrap() |> Enum.uniq() |> Enum.split_with(&is_atom/1)

    exclude_tags_map = Map.new(exclude_tags)

    exclude_included =
      for include_tag <- include_tags, key = has_tag(include_tag, exclude_tags_map), do: key

    exclude_tags = exclude_tags |> Keyword.drop(include_atoms) |> Keyword.drop(exclude_included)

    {include_atoms ++ include_tags, (exclude_atoms -- include_atoms) ++ exclude_tags}
  end

  @doc """
  Parses the given filters, as one would receive from the command line.

  ## Examples

      iex> ExUnit.Filters.parse(["foo:bar", "baz", "line:9", "bool:true"])
      [{:foo, "bar"}, :baz, {:line, 9}, {:bool, "true"}]

  """
  @spec parse([String.t()]) :: t
  def parse(filters) do
    Enum.map(filters, fn filter ->
      case :binary.split(filter, ":") do
        [key, value] -> parse_kv(String.to_atom(key), value)
        [key] -> String.to_atom(key)
      end
    end)
  end

  defp parse_kv(:line, line) when is_binary(line), do: {:line, String.to_integer(line)}
  defp parse_kv(:location, loc) when is_binary(loc), do: {:location, extract_line_numbers(loc)}
  defp parse_kv(key, value), do: {key, value}

  @doc """
  Returns failure information from the manifest file.

  It returns either `:all`, meaning all tests should be considered as stale,
  or a tuple containing:

    * A set of files that contain tests that failed the last time they ran.
      The paths are absolute paths.

    * A set of test IDs that failed the last time they ran

  """
  @spec failure_info(Path.t()) :: {MapSet.t(Path.t()), MapSet.t(ExUnit.test_id())} | :all
  def failure_info(manifest_file) do
    FailuresManifest.info(manifest_file)
  end

  @doc """
  Marks the whole suite as failed in the manifest.

  This is useful when the test suite cannot be loaded
  and there is a desire to make all tests fail.
  """
  @spec fail_all!(Path.t()) :: :ok
  def fail_all!(manifest_file) do
    FailuresManifest.fail_all!(manifest_file)
  end

  @doc """
  Evaluates the `include` and `exclude` filters against the given `tags` to
  determine if tests should be skipped or excluded.

  Some filters, like `:line`, may require the whole test `collection` to
  find the closest line, that's why it must also be passed as an argument.

  Filters can either be a regular expression or any data structure
  that implements the `String.Chars` protocol, which is invoked before comparing
  the filter with the `:tag` value.

  ## Precedence

  Tests are first excluded, then included, and then skipped (if any left).

  If a `:skip` tag is found in `tags`, `{:skipped, message}` is returned if the test
  has been left after the `exclude` and `include` filters. Otherwise `{:exclude, message}`
  is returned.

  The only exception to this rule is that `:skip` is found in the `include` filter,
  `:ok` is returned regardless of whether the test was excluded or not.

  ## Examples

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], %{foo: "bar"}, [])
      :ok

      iex> ExUnit.Filters.eval([foo: "bar"], [:foo], %{foo: "baz"}, [])
      {:excluded, "due to foo filter"}

  """
  @spec eval(t, t, map, [ExUnit.Test.t()]) ::
          :ok | {:excluded, String.t()} | {:skipped, String.t()}
  def eval(include, exclude, tags, collection) when is_map(tags) do
    cond do
      Enum.any?(include, &has_tag(&1, tags, collection)) ->
        maybe_skipped(include, tags, collection)

      excluded = Enum.find_value(exclude, &has_tag(&1, tags, collection)) ->
        {:excluded, "due to #{excluded} filter"}

      true ->
        maybe_skipped(include, tags, collection)
    end
  end

  defp maybe_skipped(include, tags, collection) do
    case tags do
      %{skip: skip} when is_binary(skip) or skip == true ->
        skip_tags = %{skip: skip}
        skip_included_explicitly? = Enum.any?(include, &has_tag(&1, skip_tags, collection))

        cond do
          skip_included_explicitly? -> :ok
          is_binary(skip) -> {:skipped, skip}
          skip -> {:skipped, "due to skip tag"}
        end

      _ ->
        :ok
    end
  end

  defp has_tag({:location, {path, lines}}, %{line: _, describe_line: _} = tags, collection) do
    String.ends_with?(tags.file, path) and
      lines |> List.wrap() |> Enum.any?(&has_tag({:line, &1}, tags, collection))
  end

  defp has_tag({:line, line}, %{line: _, describe_line: _} = tags, collection)
       when is_integer(line) do
    cond do
      tags.describe_line == line ->
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
