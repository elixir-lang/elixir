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
  Loads the given app from path in an optimized format and returns its contents.
  """
  def load_app(app, app_path) do
    case File.read(app_path) do
      {:ok, bin} ->
        with {:ok, tokens, _} <- :erl_scan.string(String.to_charlist(bin)),
             {:ok, {:application, ^app, properties} = app_data} <- :erl_parse.parse_term(tokens),
             :ok <- ensure_loaded(app_data) do
          {:ok, properties}
        else
          _ -> :invalid
        end

      {:error, _} ->
        :missing
    end
  end

  defp ensure_loaded(app_data) do
    case :application.load(app_data) do
      :ok -> :ok
      {:error, {:already_loaded, _}} -> :ok
      {:error, error} -> {:error, error}
    end
  end

  @doc """
  Loads the given applications.
  """
  def load_apps(apps, deps, config, acc, fun) do
    lib_path = to_charlist(Path.join(Mix.Project.build_path(config), "lib"))
    deps_children = for dep <- deps, into: %{}, do: {dep.app, Enum.map(dep.deps, & &1.app)}
    builtin_paths = Mix.State.builtin_apps()

    (extra_apps(config) ++ apps)
    |> traverse_apps(%{}, deps_children, builtin_paths, lib_path)
    |> Enum.reduce(acc, fun)
  end

  defp extra_apps(config) do
    case Keyword.get(config, :language, :elixir) do
      :elixir -> [:ex_unit, :iex, :mix, :elixir]
      :erlang -> [:compiler]
    end
  end

  # We already processed this app, skip it.
  defp traverse_apps([app | apps], seen, deps_children, builtin_paths, lib_path)
       when is_map_key(seen, app) do
    traverse_apps(apps, seen, deps_children, builtin_paths, lib_path)
  end

  # We haven't processed this app, emit it.
  defp traverse_apps([app | apps], seen, deps_children, builtin_paths, lib_path) do
    {ebin_path, dep_children} =
      case deps_children do
        %{^app => dep_children} -> {app_join(lib_path, app, ~c"/ebin"), dep_children}
        _ -> {builtin_paths[app], []}
      end

    app_children =
      if Application.spec(app, :vsn) do
        app_children(app)
      else
        with true <- ebin_path != nil,
             {:ok, _} <- load_app(app, app_join(ebin_path, app, ~c".app")) do
          app_children(app)
        else
          # Optional applications won't be available
          _ -> []
        end
      end

    children = (dep_children -- app_children) ++ app_children
    seen = Map.put(seen, app, true)
    apps = children ++ apps
    [{app, ebin_path} | traverse_apps(apps, seen, deps_children, builtin_paths, lib_path)]
  end

  # We have processed all apps.
  defp traverse_apps([], _seen, _deps_children, _builtin_paths, _lib_path) do
    []
  end

  defp app_children(app) do
    Application.spec(app, :applications) ++ Application.spec(app, :included_applications)
  end

  defp app_join(path, app, suffix), do: path ++ ~c"/" ++ Atom.to_charlist(app) ++ suffix
end
