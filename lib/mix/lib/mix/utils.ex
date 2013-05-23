defmodule Mix.Utils do
  @moduledoc """
  Utilities used throughout Mix and tasks.

  ## Conversions

  This module handles two types of conversions:

  * From command names to module names, i.e. how the command
    `deps.get` translates to `Deps.Get` and vice-versa;

  * From underscore to camelize, i.e. how the file path
    `my_project` translates to `MyProject`;

  """

  @doc """
  Gets the mix home. It defaults to `~/.mix` unless the
  MIX_HOME environment variable is set.
  """
  def mix_home do
    System.get_env("MIX_HOME") || Path.expand("~/.mix")
  end

  @doc """
  Gets all extra paths defined in the environment variable
  MIX_PATH. MIX_PATH may contain multiple paths. If on windows,
  those paths should be separated by `;`, if on unix systems,
  use `:`.
  """
  def mix_path do
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
  Gets the source location of a module as a binary.
  """
  def source(module) do
    source = module.__info__(:compile)[:source]
    source && list_to_binary(source)
  end

  @doc """
  Takes a `command` name and try to load a module
  with the command name converted to a module name
  in the given `at` scope.

  Returns `{ :module, module }` in case a module
  exists and is loaded, `{ :error, reason }` otherwise.

  ## Examples

      Mix.Utils.command_to_module("compile", Mix.Tasks)
      #=> { :module, Mix.Tasks.Compile }

  """
  def command_to_module(command, at // Elixir) do
    module = Module.concat(at, command_to_module_name(command))
    Code.ensure_loaded(module)
  end

  @doc """
  Returns true if any of the sources are stale
  compared to the given target.
  """
  def stale?(sources, targets) do
    extract_stale(sources, targets) != []
  end

  @doc """
  Extract all stale sources compared to the given targets.
  """
  def extract_stale(sources, targets) do
    last_modifieds = Enum.map(targets, last_modified(&1))

    Enum.filter sources, fn(source) ->
      source_stat = source_mtime(source)
      Enum.any?(last_modifieds, source_stat > &1)
    end
  end

  defp source_mtime({ _, { { _, _, _ }, { _, _, _ } } = source }) do
    source
  end

  defp source_mtime(source) do
    File.stat!(source).mtime
  end

  defp last_modified(path) do
    case File.stat(path) do
      { :ok, File.Stat[mtime: mtime] } -> mtime
      { :error, _ } -> { { 1970, 1, 1 }, { 0, 0, 0 } }
    end
  end

  @doc """
  Generates a manifest containing all files generated
  during a given compilation step. It receives the manifest
  file name and a function to execute. The result of the
  function is compared to the manifest in order do detect
  the files removed from the manifest file.
  """
  def manifest(file, fun) do
    old =
      case File.read(file) do
        { :ok, contents } -> String.split(contents, "\n")
        { :error, _ } -> []
      end
    current = fun.()
    File.write!(file, Enum.join(current, "\n"))
    { current, old -- current }
  end

  @doc """
  Extract files from a list of paths.

  In case any of the paths is a directory, the directory is looped
  recursively searching for the given extensions or the given pattern.
  When looking up directories, files starting with "." are ignored.
  """
  def extract_files(paths, exts_or_pattern)

  def extract_files(paths, exts) when is_list(exts) do
    extract_files(paths, "*.{#{Enum.join(exts, ",")}}")
  end

  def extract_files(paths, pattern) do
    files = List.concat(lc path inlist paths do
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
    Keyword.merge old, new, fn(_, x, y) ->
      if is_list(x) and is_list(y) do
        if is_keyword(x) and is_keyword(y) do
          config_merge(x, y)
        else
          x ++ y
        end
      else
        y
      end
    end
  end

  defp is_keyword(x) do
    Enum.all? x, match?({ atom, _ } when is_atom(atom), &1)
  end

  @doc """
  Converts the given atom or binary to underscore format.

  If an atom is given, it is assumed to be an Elixir module,
  so it is converted to a binary and then processed.

  ## Examples

      Mix.Utils.underscore "FooBar"  #=> "foo_bar"
      Mix.Utils.underscore "Foo.Bar" #=> "foo/bar"
      Mix.Utils.underscore Foo.Bar   #=> "foo/bar"

  In general, underscore can be thought as the reverse of
  camelize, however, in some cases formatting may be lost:

      Mix.Utils.underscore "SAPExample"  #=> "sap_example"
      Mix.Utils.camelize   "sap_example" #=> "SapExample"

  """
  def underscore(atom) when is_atom(atom) do
    "Elixir." <> rest = atom_to_binary(atom)
    underscore(rest)
  end

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
  Converts the given string to camelize format.

  ## Examples

      Mix.Utils.camelize "foo_bar" #=> "FooBar"

  """
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
  Returns the given path string relative to the current
  working directory.
  """
  def relative_to_cwd(path) do
    case File.cwd do
      { :ok, base } -> Path.relative_to(path, base)
      _ -> path
    end
  end

  @doc """
  Takes a module and converts it to a command. The nesting
  argument can be given in order to remove the nesting of
  module.

  ## Examples

      module_name_to_command(Mix.Tasks.Compile, 2)
      #=> "compile"

      module_name_to_command("Mix.Tasks.Compile.Elixir", 2)
      #=> "compile.elixir"

  """
  def module_name_to_command(module, nesting // 0)

  def module_name_to_command(module, nesting) when is_atom(module) do
    module_name_to_command(inspect(module), nesting)
  end

  def module_name_to_command(module, nesting) do
    t = Regex.split(%r/\./, to_binary(module))
    t |> Enum.drop(nesting) |> Enum.map(first_to_lower(&1)) |> Enum.join(".")
  end

  @doc """
  Takes a command and converts it to a module name format.

  ## Examples

      command_to_module_name("compile.elixir")
      #=> "Compile.Elixir"

  """
  def command_to_module_name(s) do
    Regex.split(%r/\./, to_binary(s)) |>
      Enum.map(first_to_upper(&1)) |>
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
	Opens and reads content from either a URL or a local filesystem path.
	Used by local.install and local.rebar.
	"""
	def read_path(path) do
    cond do
      is_url?(path)  -> read_url(path)
      is_file?(path) -> read_file(path)
      :else          -> raise Mix.Error, message: "expected #{path} to be a url or a local file path"
    end
  end

  defp read_file(path) do
    File.read!(path)
  end

  defp read_url(path) do
    if URI.parse(path).scheme == "https" do
      :ssl.start
    end

    :inets.start

    case :httpc.request(binary_to_list(path)) do
      { :ok, { { _, status, _ }, _, body } } when status in 200..299 ->
        iolist_to_binary(body)
      { :ok, { { _, status, _ }, _, _ } } ->
        raise Mix.Error, message: "could not access url #{path}, got status: #{status}"
      { :error, reason } ->
        raise Mix.Error, message: "could not access url #{path}, error: #{inspect reason}"
    end
  end

  defp is_file?(path) do
    File.regular?(path)
  end

  defp is_url?(path) do
    URI.parse(path).scheme in ["http", "https"]
  end
end
