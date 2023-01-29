defmodule Mix.Compilers.ApplicationTracer do
  # TODO: Move this to app loader
  @moduledoc false
  @manifest "compile.app_tracer"
  @table __MODULE__

  def init do
    config = Mix.Project.config()
    manifest = manifest(config)

    try do
      # The table is actually @table during tracing
      # but we pass it around as var for clarity.
      {:ok, table} = :ets.file2tab(String.to_charlist(manifest))
      table
    rescue
      _ -> nil
    end
  end

  def app_modules(table, app) do
    with true <- table != nil,
         [{_, modules}] <- :ets.lookup(table, {:application, app}) do
      modules
    else
      _ -> nil
    end
  end

  def prepare(table) do
    config = Mix.Project.config()
    manifest = manifest(config)
    modified = Mix.Utils.last_modified(manifest)

    pending_save =
      cond do
        # There is no table, cache it for the first time
        is_nil(table) ->
          File.mkdir_p!(Path.dirname(manifest))
          build_manifest(config)
          write_manifest(manifest)
          nil

        # If the application definition changed or dependencies were updated,
        # we need to rebuild. We don't actually depend on config/*.exs files
        # (only the lock) but this command is fast, so we are happy with
        # rebuilding even when configs change.
        Mix.Utils.stale?([Mix.Project.config_mtime(), Mix.Project.project_file()], [modified]) ->
          :ets.delete(@table)
          build_manifest(config)
          manifest

        true ->
          stale_local_deps =
            for %{app: app, scm: scm, opts: opts} <- Mix.Dep.cached(),
                not scm.fetchable?,
                Mix.Utils.last_modified(Path.join([opts[:build], "ebin", "#{app}.app"])) >
                  modified do
              delete_app(table, app)
              store_app(table, app)
              :ok
            end

          if stale_local_deps != [], do: manifest
      end

    pending_save
  end

  def stop(pending_save_manifest \\ nil) do
    if pending_save_manifest do
      write_manifest(pending_save_manifest)
    end

    try do
      :ets.delete(@table)
    rescue
      _ -> false
    end

    :ok
  end

  ## Helpers

  defp manifest(config) do
    Path.join(Mix.Project.manifest_path(config), @manifest)
  end

  defp build_manifest(config) do
    table = :ets.new(@table, [:public, :named_table, :set, read_concurrency: true])
    {all, _optional} = project_apps(config)

    %{}
    |> store_apps(table, all)
    |> store_apps(table, extra_apps(config))

    table
  end

  defp write_manifest(manifest) do
    :ok = :ets.tab2file(@table, String.to_charlist(manifest), sync: true)
  end

  defp extra_apps(config) do
    case Keyword.get(config, :language, :elixir) do
      :elixir ->
        Application.ensure_loaded(:ex_unit)
        Application.ensure_loaded(:iex)
        [:ex_unit, :iex, :mix]

      :erlang ->
        []
    end
  end

  # Applications will be nil for optional applications
  defp store_apps(seen, _table, nil), do: seen
  defp store_apps(seen, table, apps), do: Enum.reduce(apps, seen, &store_app(table, &1, &2))

  defp store_app(table, app, seen) do
    cond do
      Map.has_key?(seen, app) ->
        seen

      store_app(table, app) ->
        seen
        |> Map.put(app, true)
        |> store_apps(table, Application.spec(app, :applications))
        |> store_apps(table, Application.spec(app, :included_applications))

      true ->
        seen
    end
  end

  defp delete_app(table, app) do
    :ets.delete(table, {:application, app})
    :ets.match_delete(table, {:"$_", app})
  end

  defp store_app(table, app) do
    if modules = Application.spec(app, :modules) do
      :ets.insert(table, {{:application, app}, :ordsets.from_list(modules)})
      :ets.insert(table, Enum.map(modules, &{&1, app}))
      true
    else
      false
    end
  end

  defp project_apps(config) do
    project = Mix.Project.get!()

    properties =
      if function_exported?(project, :application, 0), do: project.application(), else: []

    extra =
      Keyword.get(properties, :included_applications, []) ++
        Keyword.get(properties, :extra_applications, [])

    Mix.Tasks.Compile.App.project_apps(properties, config, extra, fn ->
      Enum.map(config[:deps], &elem(&1, 0))
    end)
  end
end
