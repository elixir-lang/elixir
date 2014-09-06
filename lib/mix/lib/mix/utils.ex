defmodule Mix.Utils do
  @moduledoc """
  Utilities used throughout Mix and tasks.
  """

  @doc """
  Get the mix home.

  It defaults to `~/.mix` unless the `MIX_HOME`
  environment variable is set.

  Developers should only store entries in the
  `MIX_HOME` directory which are guaranteed to
  work across multiple Elixir versions, as it is
  not recommended to swap the `MIX_HOME` directory
  as configuration and other important data may be
  stored there.
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
      {:win32, _} -> ";"
      {:unix, _}  -> ":"
    end
  end

  @doc """
  Take a `command` name and attempts to load a module
  with the command name converted to a module name
  in the given `at` scope.

  Returns `{:module, module}` in case a module
  exists and is loaded, `{:error, reason}` otherwise.

  ## Examples

      iex> Mix.Utils.command_to_module("compile", Mix.Tasks)
      {:module, Mix.Tasks.Compile}

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
  def extract_stale([], _targets), do: []

  def extract_stale(sources, targets) do
    stale_stream(sources, targets) |> Enum.to_list
  end

  defp stale_stream(sources, targets) do
    modified_target = targets |> Enum.map(&last_modified(&1)) |> Enum.min

    Stream.filter(sources, fn(source) ->
      last_modified(source) > modified_target
    end)
  end

  @doc """
  Returns the date the given path was last modified.

  If the path does not exist, it returns the unix epoch
  (1970-01-01 00:00:00).
  """
  def last_modified(path)

  def last_modified({{_, _, _}, {_, _, _}} = timestamp) do
    timestamp
  end

  def last_modified(path) do
    now = :calendar.local_time

    case File.stat(path) do
      {:ok, %File.Stat{mtime: mtime}} when mtime > now ->
        Mix.shell.error("warning: mtime (modified time) for \"#{path}\" was set to the future, resetting to now")
        File.touch!(path, now)
        mtime
      {:ok, %File.Stat{mtime: mtime}} ->
        mtime
      {:error, _} ->
        {{1970, 1, 1}, {0, 0, 0}}
    end
  end

  @doc """
  Extract files from a list of paths.

  `exts_or_pattern` may be a list of extensions or a
  `Path.wildcard/1` pattern.

  If the path in `paths` is a file, it is included in
  the return result. If it is a directory, it is searched
  recursively for files with the given extensions or matching
  the given patterns.
  """
  def extract_files(paths, exts_or_pattern)

  def extract_files(paths, exts) when is_list(exts) do
    extract_files(paths, "*.{#{Enum.join(exts, ",")}}")
  end

  def extract_files(paths, pattern) do
    Enum.flat_map(paths, fn path ->
      if File.regular?(path), do: [path], else: Path.wildcard("#{path}/**/#{pattern}")
    end) |> Enum.uniq
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
    "Elixir." <> rest = Atom.to_string(atom)
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
      source_list = String.to_char_list(source)
      case :file.read_link(target) do
        {:ok, ^source_list} ->
          :ok
        {:ok, _} ->
          File.rm!(target)
          do_symlink_or_copy(source, target)
        {:error, :enoent} ->
          do_symlink_or_copy(source, target)
        {:error, _} ->
          _ = File.rm_rf!(target)
          do_symlink_or_copy(source, target)
      end
    else
      {:error, :enoent}
    end
  end

  defp do_symlink_or_copy(source, target) do

    # relative symbolic links on windows are broken
    source_path = case :os.type do
      {:win32, _} -> source
      _ -> make_relative_path(source, target)
    end

    case :file.make_symlink(source_path, target) do
      :ok -> :ok
      {:error, _} -> {:ok, File.cp_r!(source, target)}
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

  Used by tasks like `archive.install` and `local.rebar` that support
  installation either from a URL or a local file.

  Raises if the given path is not a URL, nor a file or if the
  file or URL are invalid.

  ## Options

    * `:shell` - Forces the use of `wget` or `curl` to fetch the file if the
      given path is a URL.
  """
  def read_path!(path, opts \\ []) do
    cond do
      url?(path) && opts[:shell] ->
        read_shell(path)
      url?(path) ->
        read_httpc(path)
      file?(path) ->
        read_file(path)
      true ->
        Mix.raise "Expected #{path} to be a url or a local file path"
    end
  end

  defp read_file(path) do
    File.read!(path)
  end

  defp read_httpc(path) do
    {:ok, _} = Application.ensure_all_started(:ssl)
    {:ok, _} = Application.ensure_all_started(:inets)

    # Starting a http client profile allows us to scope
    # the effects of using a http proxy to this function
    {:ok, pid} = :inets.start(:httpc, [{:profile, :mix}])

    headers = [{'user-agent', 'Mix/#{System.version}'}]
    request = {:binary.bin_to_list(path), headers}

    # If a proxy environment variable was supplied add a proxy to httpc
    http_proxy  = System.get_env("HTTP_PROXY")  || System.get_env("http_proxy")
    https_proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy")
    if http_proxy,  do: proxy(http_proxy)
    if https_proxy, do: proxy(https_proxy)

    # We are using relaxed: true because some clients is returning a Location
    # header with relative paths, which does not follow the spec. This would
    # cause the request to fail with {:error, :no_scheme} unless :relaxed
    # is given.
    case :httpc.request(:get, request, [relaxed: true], [body_format: :binary], :mix) do
      {:ok, {{_, status, _}, _, body}} when status in 200..299 ->
        body
      {:ok, {{_, status, _}, _, _}} ->
        Mix.raise "Could not access url #{path}, got status: #{status}"
      {:error, reason} ->
        Mix.raise "Could not access url #{path}, error: #{inspect reason}"
    end
  after
    :inets.stop(:httpc, :mix)
  end

  defp proxy(proxy) do
    uri = URI.parse(proxy)
    :ok = :httpc.set_options([{proxy_scheme(uri.scheme),
            {{String.to_char_list(uri.host), uri.port}, []}}], :mix)
  end

  defp proxy_scheme(scheme) do
    case scheme do
      "http" -> :proxy
      "https" -> :https_proxy
    end
  end

  defp read_shell(path) do
    filename = URI.parse(path).path |> Path.basename
    out_path = Path.join(System.tmp_dir!, filename)
    File.rm(out_path)

    status = cond do
      System.find_executable("wget") ->
        Mix.shell.cmd(~s(wget -nv -O "#{out_path}" "#{path}"))
      System.find_executable("curl") ->
        Mix.shell.cmd(~s(curl -s -S -L -o "#{out_path}" "#{path}"))
      windows? && System.find_executable("powershell") ->
        command = ~s[$ErrorActionPreference = 'Stop'; ] <>
                  ~s[$client = new-object System.Net.WebClient; ] <>
                  ~s[$client.DownloadFile(\\"#{path}\\", \\"#{out_path}\\")]
        Mix.shell.cmd(~s[powershell -Command "& {#{command}}"])
      true ->
        Mix.shell.error "wget or curl not installed"
        1
    end

    check_command!(status, path, out_path)

    data = File.read!(out_path)
    File.rm!(out_path)
    data
  end

  defp check_command!(0, _path, _out_path), do: :ok
  defp check_command!(_status, path, out_path) do
    Mix.raise "Could not fetch data, please download manually from " <>
              "#{inspect path} and copy it to #{inspect out_path}"
  end

  defp windows? do
    match?({:win32, _}, :os.type)
  end

  defp file?(path) do
    File.regular?(path)
  end

  defp url?(path) do
    URI.parse(path).scheme in ["http", "https"]
  end
end
