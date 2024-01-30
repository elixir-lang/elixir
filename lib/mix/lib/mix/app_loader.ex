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

    # We depend both on the lockfile via compile.lock (a build artifact) via
    # `config_mtime` and `project_file`. Ideally we compare the `project_file`
    # timestamp (a source artifact) against its old timestamp (instead of the
    # manifest timestamp which is a build artifact), but at the moment there
    # is no trivial place to store it.
    if Mix.Utils.stale?([Mix.Project.config_mtime(), Mix.Project.project_file()], [modified]) do
      manifest
    else
      List.first(
        for %{app: app, scm: scm, opts: opts} <- Mix.Dep.cached(),
            not scm.fetchable?(),
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
  Reads the given app from path in an optimized format and returns its contents.
  """
  def read_app(app, app_path) do
    case File.read(app_path) do
      {:ok, bin} ->
        with {:ok, tokens, _} <- :erl_scan.string(String.to_charlist(bin)),
             {:ok, {:application, ^app, properties}} <- :erl_parse.parse_term(tokens) do
          {:ok, properties}
        else
          _ -> :invalid
        end

      {:error, _} ->
        :missing
    end
  end

  @doc """
  Loads the given app from path in an optimized format and returns its contents.
  """
  def load_app(app, app_path) do
    with {:ok, properties} <- read_app(app, app_path) do
      case :application.load({:application, app, properties}) do
        :ok -> {:ok, properties}
        {:error, {:already_loaded, _}} -> {:ok, properties}
        {:error, _} -> :invalid
      end
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
  defp traverse_apps([], seen, _deps_children, builtin_paths, _lib_path) do
    # We want to keep erts in the load path but it doesn't require to be loaded.
    case builtin_paths do
      %{erts: path} when not is_map_key(seen, :erts) -> [{:erts, path}]
      %{} -> []
    end
  end

  defp app_children(app) do
    Application.spec(app, :applications) ++ Application.spec(app, :included_applications)
  end

  defp app_join(path, app, suffix), do: path ++ ~c"/" ++ Atom.to_charlist(app) ++ suffix
end
