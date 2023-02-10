defmodule Mix.AppLoader do
  @moduledoc false

  @manifest_vsn 1
  @manifest "compile.app_cache"

  @doc """
  Reads the app cache.
  """
  def read_cache(config \\ Mix.Project.config()) do
    try do
      manifest(config) |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ -> []
    else
      {@manifest_vsn, app_to_modules} -> app_to_modules
    end
  end

  @doc """
  Writes to the cache.
  """
  def write_cache(manifest, contents) when is_map(contents) do
    term = {@manifest_vsn, contents}
    File.mkdir_p!(Path.dirname(manifest))
    File.write!(manifest, :erlang.term_to_binary(term, [:compressed]))
  end

  @doc """
  Returns the path to the cache only if it is stale.
  """
  def stale_cache(config \\ Mix.Project.config()) do
    manifest = manifest(config)
    modified = Mix.Utils.last_modified(manifest)

    if Mix.Utils.stale?([Mix.Project.config_mtime(), Mix.Project.project_file()], [modified]) do
      manifest
    else
      List.first(
        for %{app: app, scm: scm, opts: opts} <- Mix.Dep.cached(),
            not scm.fetchable?,
            Mix.Utils.last_modified(Path.join([opts[:build], "ebin", "#{app}.app"])) >
              modified do
          manifest
        end
      )
    end
  end

  defp manifest(config) do
    Path.join(Mix.Project.manifest_path(config), @manifest)
  end

  @doc """
  Loads the given application from `ebin_path`.

  Returns either `{:ok, children}` or `{:error, message}`.
  """
  def load_app(app, ebin_path, validate_compile_env?) do
    if Application.spec(app, :vsn) do
      {:ok, children(app)}
    else
      with true <- ebin_path != nil,
           {:ok, bin} <- File.read(app_join(ebin_path, app, ~c".app")),
           {:ok, {:application, _, properties} = application_data} <- consult_app_file(bin),
           :ok <- :application.load(application_data) do
        with [_ | _] = compile_env <- validate_compile_env? && properties[:compile_env],
             {:error, message} <- Config.Provider.validate_compile_env(compile_env, false) do
          {:error, message}
        else
          _ -> {:ok, children(app)}
        end
      else
        # Optional applications won't be available
        _ -> {:ok, []}
      end
    end
  end

  @doc """
  Loads the given applications.
  """
  def load_apps(apps, deps, config, validate_compile_env?, acc, fun) do
    lib_path = to_charlist(Path.join(Mix.Project.build_path(config), "lib"))
    deps_children = for dep <- deps, into: %{}, do: {dep.app, Enum.map(dep.deps, & &1.app)}
    deps_paths = for dep <- deps, into: %{}, do: {dep.app, {:lib, lib_path}}
    builtin_paths = Mix.State.builtin_apps()
    paths = Map.merge(builtin_paths, deps_paths)

    ref = make_ref()
    parent = self()
    opts = [ordered: false, timeout: :infinity]

    stream =
      (extra_apps(config) ++ apps)
      |> stream_apps(deps_children, paths, ref)
      |> Task.async_stream(&load_stream_app(&1, ref, parent, validate_compile_env?), opts)

    Enum.reduce(stream, acc, fn {:ok, res}, acc -> fun.(res, acc) end)
  end

  defp extra_apps(config) do
    case Keyword.get(config, :language, :elixir) do
      :elixir -> [:ex_unit, :iex, :mix, :elixir]
      :erlang -> [:compiler]
    end
  end

  defp load_stream_app({app, app_path}, ref, parent, validate_compile_env?) do
    ebin_path = app_path_to_ebin_path(app, app_path)
    send(parent, {ref, app, load_app(app, ebin_path, validate_compile_env?)})
    {app, ebin_path}
  end

  defp stream_apps(initial, deps_children, paths, ref) do
    Stream.unfold({initial, %{}, %{}, deps_children, paths, ref}, &stream_app/1)
  end

  # We already processed this app, skip it.
  defp stream_app({[app | apps], seen, done, deps_children, paths, ref}) when is_map_key(seen, app) do
    stream_app({apps, seen, done, deps_children, paths, ref})
  end

  # We haven't processed this app, emit it.
  defp stream_app({[app | apps], seen, done, deps_children, paths, ref}) do
    {{app, paths[app]}, {apps, Map.put(seen, app, true), done, deps_children, paths, ref}}
  end

  # We have processed all apps and all seen have been done.
  defp stream_app({[], seen, done, _deps_children, _paths, _ref}) when map_size(seen) == map_size(done) do
    nil
  end

  # We have processed all apps but there is work being done.
  defp stream_app({[], seen, done, deps_children, paths, ref}) do
    receive do
      {^ref, app, {:ok, children}} ->
        dep_children = Map.get(deps_children, app, [])
        children = (dep_children -- children) ++ children
        stream_app({children, seen, Map.put(done, app, true), deps_children, paths, ref})

      {^ref, _app, {:error, message}} ->
        Mix.raise(message)
    end
  end

  defp children(app) do
    Application.spec(app, :applications) ++ Application.spec(app, :included_applications)
  end

  defp app_path_to_ebin_path(app, {:lib, lib_path}), do: app_join(lib_path, app, ~c"/ebin")
  defp app_path_to_ebin_path(_app, {:ebin, ebin_path}), do: ebin_path
  defp app_path_to_ebin_path(_app, nil), do: nil

  defp app_join(path, app, suffix), do: path ++ ~c"/" ++ Atom.to_charlist(app) ++ suffix

  defp consult_app_file(bin) do
    # The path could be located in an .ez archive, so we use the prim loader.
    with {:ok, tokens, _} <- :erl_scan.string(String.to_charlist(bin)) do
      :erl_parse.parse_term(tokens)
    end
  end
end
