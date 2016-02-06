defmodule Mix.Utils do
  @moduledoc """
  Utilities used throughout Mix and tasks.
  """

  @doc """
  Gets the Mix home.

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
  Gets all paths defined in the MIX_PATH env variable.

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
  Takes a `command` name and attempts to load a module
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
  Extracts all stale `sources` compared to the given `targets`.
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
    now = :calendar.universal_time

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
  Extracts files from a list of paths.

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
  Prints the given tree according to the callback.

  The callback will be invoked for each node and it
  must either return `{printed, children}` tuple or
  `false` if the given node must not be printed.
  """
  @spec print_tree([term], (term -> {String.t, [term]}), Keyword.t) :: :ok
  def print_tree(nodes, callback, opts \\ []) do
    pretty = Keyword.get(opts, :pretty, elem(:os.type, 0) != :win32)
    print_tree(nodes, [], pretty, callback)
  end

  defp print_tree([], _depth, _pretty, _callback), do: :ok
  defp print_tree([node | nodes], depth, pretty, callback) do
    {print, children} =  callback.(node)
    Mix.shell.info("#{depth(pretty, depth)}#{prefix(pretty, depth, nodes)}#{print}")
    print_tree(children, [(nodes != []) | depth], pretty, callback)
    print_tree(nodes, depth, pretty, callback)
  end

  defp depth(_pretty, []),    do: ""
  defp depth(pretty, depth), do: Enum.reverse(depth) |> tl |> Enum.map(&entry(pretty, &1))

  defp entry(false, true),  do: "|   "
  defp entry(false, false), do: "    "
  defp entry(true, true),   do: "│   "
  defp entry(true, false),  do: "    "

  defp prefix(false, [], _), do: ""
  defp prefix(false, _, []), do: "`-- "
  defp prefix(false, _, _),  do: "|-- "
  defp prefix(true, [], _),  do: ""
  defp prefix(true, _, []),  do: "└── "
  defp prefix(true, _, _),   do: "├── "

  @doc false
  # TODO: Deprecate by 1.4
  def underscore(value) do
    Macro.underscore(value)
  end

  @doc false
  # TODO: Deprecate by 1.4
  def camelize(value) do
    Macro.camelize(value)
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
    Regex.split(~r/\./, to_string(module))
    |> Enum.drop(nesting)
    |> Enum.map(&underscore(&1))
    |> Enum.join(".")
  end

  @doc """
  Takes a command and converts it to the module name format.

  ## Examples

      iex> Mix.Utils.command_to_module_name("compile.elixir")
      "Compile.Elixir"

  """
  def command_to_module_name(s) do
    Regex.split(~r/\./, to_string(s))
    |> Enum.map(&camelize(&1))
    |> Enum.join(".")
  end

  @doc """
  Symlinks directory `source` to `target` or copies it recursively
  in case symlink fails.

  Expects source and target to be absolute paths as it generates
  a relative symlink.
  """
  def symlink_or_copy(source, target) do
    if File.exists?(source) do
      # Relative symbolic links on windows are broken
      link = case :os.type do
        {:win32, _} -> source
        _           -> make_relative_path(source, target)
      end |> String.to_char_list

      case :file.read_link(target) do
        {:ok, ^link} ->
          :ok
        {:ok, _} ->
          File.rm!(target)
          do_symlink_or_copy(source, target, link)
        {:error, :enoent} ->
          do_symlink_or_copy(source, target, link)
        {:error, _} ->
          _ = File.rm_rf!(target)
          do_symlink_or_copy(source, target, link)
      end
    else
      {:error, :enoent}
    end
  end

  defp do_symlink_or_copy(source, target, link) do
    case :file.make_symlink(link, target) do
      :ok -> :ok
      {:error, _} -> {:ok, File.cp_r!(source, target)}
    end
  end

  # Make a relative path between the two given paths.
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
  Opens and reads content from either a URL or a local filesystem path
  and returns the contents as a `{:ok, binary}`, `:badpath` for invalid
  paths or `{:local, message}` for local errors and `{:remote, message}`
  for remote ones.

  ## Options

    * `:sha512` - checks against the given sha512 checksum. Returns
      `{:checksum, message}` in case it fails
  """
  @spec read_path(String.t, Keyword.t) ::
        {:ok, binary} | :badpath | {:remote, String.t} |
        {:local, String.t} | {:checksum, String.t}
  def read_path(path, opts \\ []) do
    cond do
      url?(path) ->
        read_httpc(path) |> checksum(opts)
      file?(path) ->
        read_file(path) |> checksum(opts)
      true ->
        :badpath
    end
  end

  @checksums [:sha512]

  defp checksum({:ok, binary} = return, opts) do
    Enum.find_value @checksums, return, fn hash ->
      with expected when expected != nil  <- opts[hash],
           actual when actual != expected <- hexhash(binary, hash) do
        {:checksum, """
          Data does not match the given sha512 checksum.

          Expected: #{expected}
            Actual: #{actual}
          """}
      else
        _ -> nil
      end
    end
  end

  defp checksum({_, _} = error, _opts) do
    error
  end

  defp hexhash(binary, hash) do
    Base.encode16 :crypto.hash(hash, binary), case: :lower
  end

  @doc """
  Prompts the user to overwrite the file if it exists. Returns
  the user input.
  """
  def can_write?(path) do
    if File.exists?(path) do
      full = Path.expand(path)
      Mix.shell.yes?(Path.relative_to_cwd(full) <> " already exists, overwrite?")
    else
      true
    end
  end

  defp read_file(path) do
    try do
      {:ok, File.read!(path)}
    rescue
      e in [File.Error] -> {:local, Exception.message(e)}
    end
  end

  defp read_httpc(path) do
    {:ok, _} = Application.ensure_all_started(:ssl)
    {:ok, _} = Application.ensure_all_started(:inets)

    # Starting an http client profile allows us to scope
    # the effects of using an http proxy to this function
    {:ok, _pid} = :inets.start(:httpc, [{:profile, :mix}])

    headers = [{'user-agent', 'Mix/#{System.version}'}]
    request = {:binary.bin_to_list(path), headers}

    # We are using relaxed: true because some servers is returning a Location
    # header with relative paths, which does not follow the spec. This would
    # cause the request to fail with {:error, :no_scheme} unless :relaxed
    # is given.
    #
    # If a proxy environment variable was supplied add a proxy to httpc.
    http_options = [relaxed: true] ++ proxy_config(path)

    case :httpc.request(:get, request, http_options, [body_format: :binary], :mix) do
      {:ok, {{_, status, _}, _, body}} when status in 200..299 ->
        {:ok, body}
      {:ok, {{_, status, _}, _, _}} ->
        {:remote, "httpc request failed with: {:bad_status_code, #{status}}"}
      {:error, reason} ->
        {:remote, "httpc request failed with: #{inspect reason}"}
    end
  after
    :inets.stop(:httpc, :mix)
  end

  defp file?(path) do
    File.regular?(path)
  end

  defp url?(path) do
    URI.parse(path).scheme in ["http", "https"]
  end

  def proxy_config(url) do
    {http_proxy, https_proxy} = proxy_env

    proxy_auth(URI.parse(url), http_proxy, https_proxy)
  end

  defp proxy_env do
    http_proxy  = System.get_env("HTTP_PROXY")  || System.get_env("http_proxy")
    https_proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy")

    {proxy_setup(:http, http_proxy), proxy_setup(:https, https_proxy)}
  end

  defp proxy_setup(scheme, proxy) do
    uri = URI.parse(proxy || "")

    if uri.host && uri.port do
      host = String.to_char_list(uri.host)
      :httpc.set_options([{proxy_scheme(scheme), {{host, uri.port}, []}}], :mix)
    end

    uri
  end

  defp proxy_scheme(scheme) do
    case scheme do
      :http  -> :proxy
      :https -> :https_proxy
    end
  end

  defp proxy_auth(%URI{scheme: "http"}, http_proxy, _https_proxy),
    do: proxy_auth(http_proxy)
  defp proxy_auth(%URI{scheme: "https"}, _http_proxy, https_proxy),
    do: proxy_auth(https_proxy)

  defp proxy_auth(nil),
    do: []
  defp proxy_auth(%URI{userinfo: nil}),
    do: []
  defp proxy_auth(%URI{userinfo: auth}) do
    destructure [user, pass], String.split(auth, ":", parts: 2)

    user = String.to_char_list(user)
    pass = String.to_char_list(pass || "")

    [proxy_auth: {user, pass}]
  end
end
