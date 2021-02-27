defmodule Mix.Utils do
  @moduledoc false

  @doc """
  Gets the Mix home.

  It uses the the locations `MIX_HOME`, `XDG_DATA_HOME/mix`,
  `~/.mix` with decreasing priority.

  Developers should only store entries in the
  `MIX_HOME` directory which are guaranteed to
  work across multiple Elixir versions, as it is
  not recommended to swap the `MIX_HOME` directory
  as configuration and other important data may be
  stored there.
  """
  def mix_home do
    mix_home_xdg_lookup(:user_data)
  end

  @doc """
  Gets possible location of global Mix configuration.

  Possible locations:

     * `MIX_HOME`
     * `XDG_CONFIG_HOME/mix` (if `MIX_XDG` is set)
     * `~/.mix`

  """
  def mix_config do
    mix_home_xdg_lookup(:user_config)
  end

  defp mix_home_xdg_lookup(xdg) do
    cond do
      dir = System.get_env("MIX_HOME") ->
        dir

      System.get_env("MIX_XDG") in ["1", "true"] ->
        :filename.basedir(xdg, "mix", %{os: :linux})

      true ->
        Path.expand("~/.mix")
    end
  end

  @doc """
  Parses a string into module, function and arity.

  It returns `{:ok, mfa_list}`, where a `mfa_list` is
  `[module, function, arity]`, `[module, function]` or `[module]`,
  or the atom `:error`.

      iex> Mix.Utils.parse_mfa("Foo.bar/1")
      {:ok, [Foo, :bar, 1]}
      iex> Mix.Utils.parse_mfa(":foo.bar/1")
      {:ok, [:foo, :bar, 1]}
      iex> Mix.Utils.parse_mfa(":foo.bar")
      {:ok, [:foo, :bar]}
      iex> Mix.Utils.parse_mfa(":foo")
      {:ok, [:foo]}
      iex> Mix.Utils.parse_mfa("Foo")
      {:ok, [Foo]}

      iex> Mix.Utils.parse_mfa("Foo.")
      :error
      iex> Mix.Utils.parse_mfa("Foo.bar.baz")
      :error
      iex> Mix.Utils.parse_mfa("Foo.bar/2/2")
      :error

  """
  def parse_mfa(mfa) do
    with {:ok, quoted} <- Code.string_to_quoted(mfa),
         [_ | _] = mfa_list <- quoted_to_mfa(quoted) do
      {:ok, mfa_list}
    else
      _ -> :error
    end
  end

  defp quoted_to_mfa({:/, _, [dispatch, arity]}) when is_integer(arity) do
    quoted_to_mf(dispatch, [arity])
  end

  defp quoted_to_mfa(dispatch) do
    quoted_to_mf(dispatch, [])
  end

  defp quoted_to_mf({{:., _, [module, fun]}, _, []}, acc) when is_atom(fun) do
    quoted_to_m(module, [fun | acc])
  end

  defp quoted_to_mf(module, acc) do
    quoted_to_m(module, acc)
  end

  defp quoted_to_m({:__aliases__, _, aliases}, acc) do
    [Module.concat(aliases) | acc]
  end

  defp quoted_to_m(atom, acc) when is_atom(atom) do
    [atom | acc]
  end

  defp quoted_to_m(_, _acc) do
    []
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
    Enum.any?(stale_stream(sources, targets))
  end

  @doc """
  Extracts all stale `sources` compared to the given `targets`.
  """
  def extract_stale(_sources, []), do: []
  def extract_stale([], _targets), do: []

  def extract_stale(sources, targets) do
    stale_stream(sources, targets) |> Enum.to_list()
  end

  defp stale_stream(sources, []) do
    sources
  end

  defp stale_stream(sources, targets) do
    modified_target = targets |> Enum.map(&last_modified/1) |> Enum.min()

    Stream.filter(sources, fn source ->
      last_modified(source) > modified_target
    end)
  end

  @doc """
  Returns the date the given path was last modified in posix time.

  If the path does not exist, it returns the Unix epoch
  (1970-01-01 00:00:00).
  """
  def last_modified(path)

  def last_modified(timestamp) when is_integer(timestamp) do
    timestamp
  end

  def last_modified(path) do
    {mtime, _size} = last_modified_and_size(path)
    mtime
  end

  @doc """
  Returns the date the given path was last modified in posix time
  and the size.

  If the path does not exist, it returns the Unix epoch
  (1970-01-01 00:00:00).
  """
  def last_modified_and_size(path) do
    now = System.os_time(:second)

    case :elixir_utils.read_posix_mtime_and_size(path) do
      {:ok, mtime, size} when mtime > now ->
        message =
          "warning: mtime (modified time) for #{inspect(path)} was set to the future, resetting to now"

        Mix.shell().error(message)
        File.touch(path, now)
        {mtime, size}

      {:ok, mtime, size} ->
        {mtime, size}

      {:error, _} ->
        {0, 0}
    end
  end

  @doc """
  Prints n files are being compiled with the given extension.
  """
  def compiling_n(1, ext), do: Mix.shell().info("Compiling 1 file (.#{ext})")
  def compiling_n(n, ext), do: Mix.shell().info("Compiling #{n} files (.#{ext})")

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
      case :elixir_utils.read_file_type(path) do
        {:ok, :directory} -> Path.wildcard("#{path}/**/#{pattern}")
        {:ok, :regular} -> [path]
        _ -> []
      end
    end)
    |> Enum.uniq()
  end

  @type formatted_node :: {name :: String.Chars.t(), edge_info :: String.Chars.t()}

  @doc """
  Prints the given tree according to the callback.

  The callback will be invoked for each node and it
  must return a `{printed, children}` tuple.
  """
  @spec print_tree([node], (node -> {formatted_node, [node]}), keyword) :: :ok when node: term()
  def print_tree(nodes, callback, opts \\ []) do
    pretty? =
      case Keyword.get(opts, :format) do
        "pretty" -> true
        "plain" -> false
        _other -> elem(:os.type(), 0) != :win32
      end

    print_tree(nodes, _depth = [], _seen = %{}, pretty?, callback)
    :ok
  end

  defp print_tree(nodes, depth, seen, pretty?, callback) do
    # We perform a breadth first traversal so we always show a dependency
    # a node with its children as high as possible in tree. This helps avoid
    # very deep trees.
    {nodes, seen} =
      Enum.flat_map_reduce(nodes, seen, fn node, seen ->
        {{name, info}, children} = callback.(node)

        if Map.has_key?(seen, name) do
          {[{name, info, []}], seen}
        else
          {[{name, info, children}], Map.put(seen, name, true)}
        end
      end)

    print_each_node(nodes, depth, seen, pretty?, callback)
  end

  defp print_each_node([], _depth, seen, _pretty?, _callback) do
    seen
  end

  defp print_each_node([{name, info, children} | nodes], depth, seen, pretty?, callback) do
    info = if(info, do: " #{info}", else: "")
    Mix.shell().info("#{depth(pretty?, depth)}#{prefix(pretty?, depth, nodes)}#{name}#{info}")

    seen = print_tree(children, [nodes != [] | depth], seen, pretty?, callback)
    print_each_node(nodes, depth, seen, pretty?, callback)
  end

  defp depth(_pretty?, []), do: ""
  defp depth(pretty?, depth), do: Enum.reverse(depth) |> tl |> Enum.map(&entry(pretty?, &1))

  defp entry(false, true), do: "|   "
  defp entry(false, false), do: "    "
  defp entry(true, true), do: "│   "
  defp entry(true, false), do: "    "

  defp prefix(false, [], _), do: ""
  defp prefix(false, _, []), do: "`-- "
  defp prefix(false, _, _), do: "|-- "
  defp prefix(true, [], _), do: ""
  defp prefix(true, _, []), do: "└── "
  defp prefix(true, _, _), do: "├── "

  @doc """
  Outputs the given tree according to the callback as a DOT graph.

  The callback will be invoked for each node and it
  must return a `{printed, children}` tuple.
  """
  @spec write_dot_graph!(
          Path.t(),
          String.t(),
          [node],
          (node -> {formatted_node, [node]}),
          keyword
        ) :: :ok
        when node: term()
  def write_dot_graph!(path, title, nodes, callback, _opts \\ []) do
    {dot, _} = build_dot_graph(make_ref(), nodes, MapSet.new(), callback)
    File.write!(path, ["digraph ", quoted(title), " {\n", dot, "}\n"])
  end

  defp build_dot_graph(_parent, [], seen, _callback), do: {[], seen}

  defp build_dot_graph(parent, [node | nodes], seen, callback) do
    {{name, edge_info}, children} = callback.(node)
    key = {parent, name}

    if MapSet.member?(seen, key) do
      {[], seen}
    else
      seen = MapSet.put(seen, key)
      current = build_dot_current(parent, name, edge_info)
      {children, seen} = build_dot_graph(name, children, seen, callback)
      {siblings, seen} = build_dot_graph(parent, nodes, seen, callback)
      {[current, children | siblings], seen}
    end
  end

  defp build_dot_current(parent, name, edge_info) do
    edge_info =
      if edge_info do
        [" [label=", quoted(edge_info), "]"]
      else
        []
      end

    parent =
      if is_reference(parent) do
        []
      else
        [quoted(parent), " -> "]
      end

    ["  ", parent, quoted(name), edge_info, ?\n]
  end

  defp quoted(data), do: [?", to_string(data), ?"]

  @doc false
  @deprecated "Use Macro.underscore/1 instead"
  def underscore(value) do
    Macro.underscore(value)
  end

  @doc false
  @deprecated "Use Macro.camelize/1 instead"
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
    module
    |> to_string()
    |> String.split(".")
    |> Enum.drop(nesting)
    |> Enum.map_join(".", &Macro.underscore/1)
  end

  @doc """
  Takes a command and converts it to the module name format.

  ## Examples

      iex> Mix.Utils.command_to_module_name("compile.elixir")
      "Compile.Elixir"

  """
  def command_to_module_name(command) do
    command
    |> to_string()
    |> String.split(".")
    |> Enum.map_join(".", &Macro.camelize/1)
  end

  @doc """
  Symlinks directory `source` to `target` or copies it recursively
  in case symlink fails.

  Expects source and target to be absolute paths as it generates
  a relative symlink.
  """
  def symlink_or_copy(source, target) do
    if File.exists?(source) do
      # Relative symbolic links on Windows are broken
      link =
        case :os.type() do
          {:win32, _} -> source
          _ -> make_relative_path(source, target)
        end
        |> String.to_charlist()

      case :file.read_link(target) do
        {:ok, ^link} ->
          :ok

        {:ok, _} ->
          case File.rm(target) do
            :ok ->
              :ok

            {:error, reason} ->
              reason = IO.iodata_to_binary(:file.format_error(reason))

              Mix.raise("""
              Cannot remove symlink #{inspect(target)} due to reason: #{reason}"

                * Make sure you have permission to access the _build directory
                  (you may have the wrong permission if you change users or ran as admin)

                * If you are using Windows, avoid using substitute drives,
                  as they don't play well with symlinks

                * In case the issue continues, consider removing the _build directory
              """)
          end

          do_symlink_or_copy(source, target, link)

        {:error, :enoent} ->
          do_symlink_or_copy(source, target, link)

        {:error, _} ->
          unless File.dir?(target) do
            File.rm_rf!(target)
          end

          do_symlink_or_copy(source, target, link)
      end
    else
      {:error, :enoent}
    end
  end

  defp do_symlink_or_copy(source, target, link) do
    case :file.make_symlink(link, target) do
      :ok ->
        :ok

      {:error, _} ->
        file =
          File.cp_r!(source, target, fn orig, dest ->
            File.stat!(orig).mtime > File.stat!(dest).mtime
          end)

        {:ok, file}
    end
  end

  # Make a relative path between the two given paths.
  # Expects both paths to be fully expanded.
  defp make_relative_path(source, target) do
    do_make_relative_path(Path.split(source), Path.split(target))
  end

  defp do_make_relative_path([h | t1], [h | t2]) do
    do_make_relative_path(t1, t2)
  end

  defp do_make_relative_path(source, target) do
    base = List.duplicate("..", max(length(target) - 1, 0))
    Path.join(base ++ source)
  end

  @doc """
  Opens and reads content from either a URL or a local file system path.

  Returns the contents as a `{:ok, binary}`, `:badpath` for invalid
  paths or `{:local, message}` for local errors and `{:remote, message}`
  for remote ones.

  ## Options

    * `:sha512` - checks against the given SHA-512 checksum. Returns
      `{:checksum, message}` in case it fails. This option is required
      for URLs unless the `:unsafe_uri` is given (WHICH IS NOT RECOMMENDED
      unless another security mechanism is in place, such as private keys)

    * `:timeout` - times out the request after the given milliseconds.
      Returns `{:remote, timeout_message}` if it fails. Defaults to 60
      seconds

  """
  @spec read_path(String.t(), keyword) ::
          {:ok, binary}
          | :badpath
          | {:remote, String.t()}
          | {:local, String.t()}
          | {:checksum, String.t()}
  def read_path(path, opts \\ []) do
    cond do
      url?(path) ->
        task =
          Task.async(fn ->
            with :ok <- require_checksum(opts),
                 {:ok, binary} <- read_httpc(path) do
              checksum(binary, opts)
            end
          end)

        timeout = Keyword.get(opts, :timeout, 60_000)

        case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
          {:ok, result} -> result
          _ -> {:remote, "request timed out after #{timeout}ms"}
        end

      file?(path) ->
        with {:ok, binary} <- read_file(path) do
          checksum(binary, opts)
        end

      true ->
        :badpath
    end
  end

  @checksums [:sha512]

  defp require_checksum(opts) do
    cond do
      Keyword.take(opts, @checksums) != [] ->
        :ok

      Keyword.get(opts, :unsafe_uri) ->
        :ok

      true ->
        {:checksum, "fetching from URIs require a checksum to be given"}
    end
  end

  defp checksum(binary, opts) do
    Enum.find_value(@checksums, {:ok, binary}, fn hash ->
      with expected when expected != nil <- opts[hash],
           actual when actual != expected <- hexhash(binary, hash) do
        message = """
        Data does not match the given SHA-512 checksum.

        Expected: #{expected}
          Actual: #{actual}
        """

        {:checksum, message}
      else
        _ -> nil
      end
    end)
  end

  defp hexhash(binary, hash) do
    Base.encode16(:crypto.hash(hash, binary), case: :lower)
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

    # Starting an HTTP client profile allows us to scope
    # the effects of using an HTTP proxy to this function
    {:ok, _pid} = :inets.start(:httpc, profile: :mix)

    headers = [{'user-agent', 'Mix/#{System.version()}'}]
    request = {:binary.bin_to_list(path), headers}

    # We are using relaxed: true because some servers is returning a Location
    # header with relative paths, which does not follow the spec. This would
    # cause the request to fail with {:error, :no_scheme} unless :relaxed
    # is given.
    #
    # If a proxy environment variable was supplied add a proxy to httpc.
    http_options = [relaxed: true] ++ proxy_config(path)

    case httpc_request(request, http_options) do
      {:error, {:failed_connect, [{:to_address, _}, {inet, _, reason}]}}
      when inet in [:inet, :inet6] and reason in [:ehostunreach, :enetunreach] ->
        :httpc.set_options([ipfamily: fallback(inet)], :mix)
        request |> httpc_request(http_options) |> httpc_response()

      response ->
        httpc_response(response)
    end
  after
    :inets.stop(:httpc, :mix)
  end

  defp fallback(:inet), do: :inet6
  defp fallback(:inet6), do: :inet

  defp httpc_request(request, http_options) do
    :httpc.request(:get, request, http_options, [body_format: :binary], :mix)
  end

  defp httpc_response(response) do
    case response do
      {:ok, {{_, status, _}, _, body}} when status in 200..299 ->
        {:ok, body}

      {:ok, {{_, status, _}, _, _}} ->
        {:remote, "httpc request failed with: {:bad_status_code, #{status}}"}

      {:error, reason} ->
        {:remote, "httpc request failed with: #{inspect(reason)}"}
    end
  end

  defp file?(path) do
    File.regular?(path)
  end

  defp url?(path) do
    URI.parse(path).scheme in ["http", "https"]
  end

  def proxy_config(url) do
    {http_proxy, https_proxy} = proxy_env()

    proxy_auth(URI.parse(url), http_proxy, https_proxy)
  end

  defp proxy_env do
    http_proxy = System.get_env("HTTP_PROXY") || System.get_env("http_proxy")
    https_proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy")
    no_proxy = no_proxy_env() |> no_proxy_list()

    {proxy_setup(:http, http_proxy, no_proxy), proxy_setup(:https, https_proxy, no_proxy)}
  end

  defp no_proxy_env() do
    System.get_env("NO_PROXY") || System.get_env("no_proxy")
  end

  defp no_proxy_list(nil) do
    []
  end

  defp no_proxy_list(no_proxy) do
    no_proxy
    |> String.split(",")
    |> Enum.map(&String.to_charlist/1)
  end

  defp proxy_setup(scheme, proxy, no_proxy) do
    uri = URI.parse(proxy || "")

    if uri.host && uri.port do
      host = String.to_charlist(uri.host)
      :httpc.set_options([{proxy_scheme(scheme), {{host, uri.port}, no_proxy}}], :mix)
    end

    uri
  end

  defp proxy_scheme(scheme) do
    case scheme do
      :http -> :proxy
      :https -> :https_proxy
    end
  end

  defp proxy_auth(%URI{scheme: "http"}, http_proxy, _https_proxy), do: proxy_auth(http_proxy)
  defp proxy_auth(%URI{scheme: "https"}, _http_proxy, https_proxy), do: proxy_auth(https_proxy)

  defp proxy_auth(nil), do: []
  defp proxy_auth(%URI{userinfo: nil}), do: []

  defp proxy_auth(%URI{userinfo: auth}) do
    destructure [user, pass], String.split(auth, ":", parts: 2)

    user = String.to_charlist(user)
    pass = String.to_charlist(pass || "")

    [proxy_auth: {user, pass}]
  end
end
