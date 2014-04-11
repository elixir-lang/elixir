defmodule Mix.Utils do
  @moduledoc """
  Utilities used throughout Mix and tasks.
  """

  @doc """
  Get the mix home.

  It defaults to `~/.mix` unless the `MIX_HOME`
  environment variable is set.
  """
  def mix_home do
    System.get_env("MIX_HOME") || Path.expand("~/.mix")
  end

  @doc """
  Get all paths defined in the MIX_PATH env variable.

  `MIX_PATH` may contain multiple paths. If on Windows, those
  paths should be separated by `;`, if on unix systems, use `:`.
  """
  def mix_paths do
    if path = System.get_env("MIX_PATH") do
      String.split(path, path_separator)
    else
      []
    end
  end

  defp path_separator do
    case :os.type do
      { :win32, _ } -> ";"
      { :unix, _ }  -> ":"
    end
  end

  @doc """
  Take a `command` name and attempts to load a module
  with the command name converted to a module name
  in the given `at` scope.

  Returns `{ :module, module }` in case a module
  exists and is loaded, `{ :error, reason }` otherwise.

  ## Examples

      iex> Mix.Utils.command_to_module("compile", Mix.Tasks)
      { :module, Mix.Tasks.Compile }

  """
  def command_to_module(command, at \\ Elixir) do
    module = Module.concat(at, command_to_module_name(command))
    Code.ensure_loaded(module)
  end

  @doc """
  Returns `true` if any of the `sources` are stale
  compared to the given `targets`.
  """
  def stale?(sources, targets) do
    Enum.any? stale_stream(sources, targets)
  end

  @doc """
  Extract all stale `sources` compared to the given `targets`.
  """
  def extract_stale(_sources, []), do: []

  def extract_stale(sources, targets) do
    stale_stream(sources, targets) |> Enum.to_list
  end

  defp stale_stream(sources, targets) do
    modified_target = targets |> Enum.map(&last_modified(&1)) |> Enum.min

    Stream.filter(sources, fn(source) ->
      source_mtime(source) > modified_target
    end)
  end

  defp source_mtime({ _, { { _, _, _ }, { _, _, _ } } = source }) do
    source
  end

  defp source_mtime(source) do
    last_modified(source)
  end

  defp last_modified(path) do
    case File.stat(path) do
      { :ok, File.Stat[mtime: mtime] } -> mtime
      { :error, _ } -> { { 1970, 1, 1 }, { 0, 0, 0 } }
    end
  end

  @doc ~S"""
  Reads the given file as a manifest and returns each entry
  as a list.

  A manifest is a tabular file where each line is a row
  and each entry in a row is separated by "\t". The first
  entry must always be a path to a compiled artifact.

  In case there is no manifest file, returns an empty list.
  """
  def read_manifest(file) do
    case File.read(file) do
      { :ok, contents } -> String.split(contents, "\n")
      { :error, _ } -> []
    end
  end

  @doc """
  Writes a manifest file with the given `entries` list.
  """
  def write_manifest(file, entries) do
    Path.dirname(file) |> File.mkdir_p!
    File.write!(file, Enum.join(entries, "\n"))
  end

  @doc """
  Extract files from a list of paths.

  `exts_or_pattern` may be a list of extensions or a
  `Path.wildcard/1` pattern.

  If the path in `paths` is a file, it is included in
  the return result. If it is a directory, it is searched
  recursively for files with the given extensions or matching
  the given patterns.

  Any file starting with `"."` is ignored.
  """
  def extract_files(paths, exts_or_pattern)

  def extract_files(paths, exts) when is_list(exts) do
    extract_files(paths, "*.{#{Enum.join(exts, ",")}}")
  end

  def extract_files(paths, pattern) do
    files = Enum.flat_map(paths, fn path ->
      if File.regular?(path), do: [path], else: Path.wildcard("#{path}/**/#{pattern}")
    end)
    files |> exclude_files |> Enum.uniq
  end

  defp exclude_files(files) do
    filter = fn(x) -> not match?("." <> _, Path.basename(x)) end
    Enum.filter files, filter
  end

  @doc """
  Merges two configs recursively, merging keyword lists
  and concatenating normal lists.
  """
  def config_merge(old, new) do
    Keyword.merge(old, new, fn(_, x, y) ->
      if is_list(x) and is_list(y) do
        if Keyword.keyword?(x) and Keyword.keyword?(y) do
          config_merge(x, y)
        else
          x ++ y
        end
      else
        y
      end
    end)
  end

  @doc """
  Converts the given atom or binary to underscore format.

  If an atom is given, it is assumed to be an Elixir module,
  so it is converted to a binary and then processed.

  ## Examples

      iex> Mix.Utils.underscore "FooBar"
      "foo_bar"

      iex> Mix.Utils.underscore "Foo.Bar"
      "foo/bar"

      iex> Mix.Utils.underscore Foo.Bar
      "foo/bar"

  In general, `underscore` can be thought of as the reverse of
  `camelize`, however, in some cases formatting may be lost:

      Mix.Utils.underscore "SAPExample"  #=> "sap_example"
      Mix.Utils.camelize   "sap_example" #=> "SapExample"

  """
  def underscore(atom) when is_atom(atom) do
    "Elixir." <> rest = atom_to_binary(atom)
    underscore(rest)
  end

  def underscore(""), do: ""

  def underscore(<<h, t :: binary>>) do
    <<to_lower_char(h)>> <> do_underscore(t, h)
  end

  defp do_underscore(<<h, t, rest :: binary>>, _) when h in ?A..?Z and not t in ?A..?Z do
    <<?_, to_lower_char(h), t>> <> do_underscore(rest, t)
  end

  defp do_underscore(<<h, t :: binary>>, prev) when h in ?A..?Z and not prev in ?A..?Z do
    <<?_, to_lower_char(h)>> <> do_underscore(t, h)
  end

  defp do_underscore(<<?-, t :: binary>>, _) do
    <<?_>> <> do_underscore(t, ?-)
  end

  defp do_underscore(<< "..", t :: binary>>, _) do
    <<"..">> <> underscore(t)
  end

  defp do_underscore(<<?.>>, _), do: <<?.>>

  defp do_underscore(<<?., t :: binary>>, _) do
    <<?/>> <> underscore(t)
  end

  defp do_underscore(<<h, t :: binary>>, _) do
    <<to_lower_char(h)>> <> do_underscore(t, h)
  end

  defp do_underscore(<<>>, _) do
    <<>>
  end

  @doc """
  Converts the given string to CamelCase format.

  ## Examples

      iex> Mix.Utils.camelize "foo_bar"
      "FooBar"

  """
  def camelize(""), do: ""

  def camelize(<<?_, t :: binary>>) do
    camelize(t)
  end

  def camelize(<<h, t :: binary>>) do
    <<to_upper_char(h)>> <> do_camelize(t)
  end

  defp do_camelize(<<?_, ?_, t :: binary>>) do
    do_camelize(<< ?_, t :: binary >>)
  end

  defp do_camelize(<<?_, h, t :: binary>>) when h in ?a..?z do
    <<to_upper_char(h)>> <> do_camelize(t)
  end

  defp do_camelize(<<?_>>) do
    <<>>
  end

  defp do_camelize(<<?/, t :: binary>>) do
    <<?.>> <> camelize(t)
  end

  defp do_camelize(<<h, t :: binary>>) do
    <<h>> <> do_camelize(t)
  end

  defp do_camelize(<<>>) do
    <<>>
  end

  @doc """
  Takes a module and converts it to a command.

  The nesting argument can be given in order to remove
  the nesting of a module.

  ## Examples

      iex> Mix.Utils.module_name_to_command(Mix.Tasks.Compile, 2)
      "compile"

      iex> Mix.Utils.module_name_to_command("Mix.Tasks.Compile.Elixir", 2)
      "compile.elixir"

  """
  def module_name_to_command(module, nesting \\ 0)

  def module_name_to_command(module, nesting) when is_atom(module) do
    module_name_to_command(inspect(module), nesting)
  end

  def module_name_to_command(module, nesting) do
    t = Regex.split(~r/\./, to_string(module))
    t |> Enum.drop(nesting) |> Enum.map(&first_to_lower(&1)) |> Enum.join(".")
  end

  @doc """
  Takes a command and converts it to the module name format.

  ## Examples

      iex> Mix.Utils.command_to_module_name("compile.elixir")
      "Compile.Elixir"

  """
  def command_to_module_name(s) do
    Regex.split(~r/\./, to_string(s)) |>
      Enum.map(&first_to_upper(&1)) |>
      Enum.join(".")
  end

  defp first_to_upper(<<s, t :: binary>>), do: <<to_upper_char(s)>> <> t
  defp first_to_upper(<<>>), do: <<>>

  defp first_to_lower(<<s, t :: binary>>), do: <<to_lower_char(s)>> <> t
  defp first_to_lower(<<>>), do: <<>>

  defp to_upper_char(char) when char in ?a..?z, do: char - 32
  defp to_upper_char(char), do: char

  defp to_lower_char(char) when char in ?A..?Z, do: char + 32
  defp to_lower_char(char), do: char

  @doc """
  Symlink directory `source` to `target` or copy it recursively
  in case symlink fails.

  Expect source and target to be absolute paths as it generates
  a relative symlink.
  """
  def symlink_or_copy(source, target) do
    if File.exists?(source) do
      source_list = String.to_char_list!(source)
      case :file.read_link(target) do
        { :ok, ^source_list } ->
          :ok
        { :ok, _ } ->
          File.rm!(target)
          do_symlink_or_copy(source, target)
        { :error, :enoent } ->
          do_symlink_or_copy(source, target)
        { :error, _ } ->
          File.rm_rf!(target)
          do_symlink_or_copy(source, target)
      end
    else
      { :error, :enoent }
    end
  end

  defp do_symlink_or_copy(source, target) do
    symlink_source = make_relative_path(source, target)
    case :file.make_symlink(symlink_source, target) do
      :ok -> :ok
      { :error, _ } -> File.cp_r!(source, target)
    end
  end

  # Make a relative path in between two paths.
  # Expects both paths to be fully expanded.
  defp make_relative_path(source, target) do
    do_make_relative_path(Path.split(source), Path.split(target))
  end

  defp do_make_relative_path([h|t1], [h|t2]) do
    do_make_relative_path(t1, t2)
  end

  defp do_make_relative_path(source, target) do
    base = List.duplicate("..", max(length(target) - 1, 0))
    Path.join(base ++ source)
  end

  @doc """
  Opens and reads content from either a URL or a local filesystem path.

  Used by tasks like `local.install` and `local.rebar` that support
  installation either from a URL or a local file.

  Raises if the given path is not a url, nor a file or if the
  file or url are invalid.
  """
  def read_path!(path) do
    cond do
      url?(path)  -> read_url(path)
      file?(path) -> read_file(path)
      :else       -> raise Mix.Error, message: "Expected #{path} to be a url or a local file path"
    end
  end

  defp read_file(path) do
    File.read!(path)
  end

  defp read_url(path) do
    :ssl.start
    :inets.start

    headers = [ { 'user-agent', 'Mix/#{System.version}' } ]
    request = { :binary.bin_to_list(path), headers }

    case :httpc.request(:get, request, [], body_format: :binary) do
      { :ok, { { _, status, _ }, _, body } } when status in 200..299 ->
        body
      { :ok, { { _, status, _ }, _, _ } } ->
        raise Mix.Error, message: "Could not access url #{path}, got status: #{status}"
      { :error, reason } ->
        raise Mix.Error, message: "Could not access url #{path}, error: #{inspect reason}"
    end
  end

  defp file?(path) do
    File.regular?(path)
  end

  defp url?(path) do
    URI.parse(path).scheme in ["http", "https"]
  end
end
