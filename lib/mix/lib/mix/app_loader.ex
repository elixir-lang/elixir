defmodule Mix.AppLoader do
  @moduledoc false

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
  def load_apps(apps, deps, config, validate_compile_env?) do
    lib_path = to_charlist(Path.join(Mix.Project.build_path(config), "lib"))
    deps_paths = for dep <- deps, into: %{}, do: {dep.app, {:lib, lib_path}}
    builtin_paths = Mix.State.builtin_apps()
    paths = Map.merge(builtin_paths, deps_paths)

    ref = make_ref()
    parent = self()
    opts = [ordered: false, timeout: :infinity]

    stream =
      apps
      |> stream_apps(paths, ref)
      |> Task.async_stream(&load_stream_app(&1, ref, parent, validate_compile_env?), opts)

    # We keep Mix, ExUnit, and IEx always loaded to avoid warnings
    included_paths =
      for app <- [:mix, :ex_unit, :iex] do
        {:ebin, path} = builtin_paths[app]
        path
      end

    for {:ok, path} <- stream, path != nil, reduce: included_paths do
      paths -> [path | paths]
    end
  end

  defp load_stream_app({app, app_path}, ref, parent, validate_compile_env?) do
    ebin_path = app_path_to_ebin_path(app, app_path)
    send(parent, {ref, app, load_app(app, ebin_path, validate_compile_env?)})
    ebin_path
  end

  defp stream_apps(initial, paths, ref) do
    Stream.unfold({initial, %{}, %{}, paths, ref}, &stream_app/1)
  end

  # We already processed this app, skip it.
  defp stream_app({[app | apps], seen, done, paths, ref}) when is_map_key(seen, app) do
    stream_app({apps, seen, done, paths, ref})
  end

  # We haven't processed this app, emit it.
  defp stream_app({[app | apps], seen, done, paths, ref}) do
    {{app, paths[app]}, {apps, Map.put(seen, app, true), done, paths, ref}}
  end

  # We have processed all apps and all seen have been done.
  defp stream_app({[], seen, done, _paths, _ref}) when map_size(seen) == map_size(done) do
    nil
  end

  # We have processed all apps but there is work being done.
  defp stream_app({[], seen, done, paths, ref}) do
    receive do
      {^ref, app, {:ok, children}} ->
        stream_app({children, seen, Map.put(done, app, true), paths, ref})

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
