defmodule Mix.Compilers.ApplicationTracer do
  @moduledoc false
  @manifest "compile.app_tracer"
  @table __MODULE__
  @warnings_key 0

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

  def prepare(table, opts) do
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

    setup_warnings_table(config)
    {pending_save, Keyword.update(opts, :tracers, [__MODULE__], &[__MODULE__ | &1])}
  end

  # :erlang and other preloaded apps are not part of any module.
  # :code.which/1 will rule them out but, since :erlang is very
  # common, we rule it out upfront.
  def trace({_, _, :erlang, _, _}, _env) do
    :ok
  end

  # Skip protocol implementations too as the goal of protocols
  # is to invert the dependency graph. Perhaps it would be best
  # to skip tracing altogether if env.module is a protocol but
  # currently there is no cheap way to track this information.
  def trace({_, _, _, :__impl__, _}, _env) do
    :ok
  end

  def trace({type, meta, module, function, arity}, env)
      when type in [:remote_function, :remote_macro, :imported_function, :imported_macro] do
    # Unknown modules need to be looked up and filtered later
    unless :ets.member(@table, module) do
      :ets.insert(
        warnings_table(),
        {module, function, arity, env.module, env.function, env.file, meta[:line] || env.line}
      )
    end

    :ok
  end

  def trace(_, _) do
    :ok
  end

  def warnings(modules) do
    [{_, table, app, excludes}] = :ets.lookup(@table, @warnings_key)

    for module <- modules do
      :ets.delete(table, module)
    end

    warnings =
      :ets.foldl(
        fn {module, function, arity, env_module, env_function, env_file, env_line}, acc ->
          # If the module is preloaded, it is always available, so we skip it.
          # If the module is non existing, the compiler will warn, so we skip it.
          # If the module belongs to this application (from another compiler), we skip it.
          # If the module is excluded, we skip it.
          with path when is_list(path) <- :code.which(module),
               {:ok, module_app} <- app(path),
               true <- module_app != app,
               false <- module in excludes or {module, function, arity} in excludes do
            env_mfa =
              if env_function do
                {env_module, elem(env_function, 0), elem(env_function, 1)}
              else
                env_module
              end

            warning = {:undefined_app, String.to_atom(module_app), module, function, arity}
            [{__MODULE__, warning, {env_file, env_line, env_mfa}} | acc]
          else
            _ -> acc
          end
        end,
        [],
        table
      )

    warnings
    |> Module.ParallelChecker.group_warnings()
    |> Module.ParallelChecker.emit_warnings()
  end

  # ../elixir/ebin/elixir.beam -> elixir
  # ../ssl-9.6/ebin/ssl.beam -> ssl
  defp app(path) do
    case path |> Path.split() |> Enum.take(-3) do
      [dir, "ebin", _beam] -> {:ok, dir |> String.split("-") |> hd()}
      _ -> :error
    end
  end

  def stop(pending_save_manifest \\ nil) do
    try do
      :ets.delete(warnings_table())
    rescue
      _ -> false
    end

    if pending_save_manifest do
      :ets.delete(@table, @warnings_key)
      write_manifest(pending_save_manifest)
    end

    try do
      :ets.delete(@table)
    rescue
      _ -> false
    end

    :ok
  end

  def format_warning({:undefined_app, app, module, function, arity}) do
    """
    #{Exception.format_mfa(module, function, arity)} defined in application :#{app} \
    is used by the current application but the current application does not depend \
    on :#{app}. To fix this, you must do one of:

      1. #{mix_exs_undefined_app_fix(app)}

      2. In case you don't want to add a requirement to :#{app}, you may \
    optionally skip this warning by adding [xref: [exclude: [#{inspect(module)}]]] \
    to your "def project" in mix.exs
    """
  end

  defp mix_exs_undefined_app_fix(app) do
    application_key = application_key()

    if elixir_app?(app) or erlang_app?(app) or application_key == :applications do
      "You must include :#{app} under :#{application_key} inside \"def application\" in your mix.exs"
    else
      "Make sure :#{app} is listed as a dependency in \"def deps\" in your mix.exs"
    end
  end

  defp application_key do
    project = Mix.Project.get()

    with true <- function_exported?(project, :application, 0),
         [_ | _] = properties <- project.application(),
         Keyword.has_key?(properties, :applications) do
      :applications
    else
      _ -> :extra_applications
    end
  end

  defp elixir_app?(app), do: app in [:logger, :mix, :ex_unit, :iex, :eex]

  defp erlang_app?(app) do
    case :code.lib_dir(app) do
      [_ | _] = path -> List.starts_with?(path, :code.root_dir())
      _ -> false
    end
  end

  ## Helpers

  def setup_warnings_table(config) do
    table = :ets.new(:app_tracer_warnings, [:public, :duplicate_bag, write_concurrency: true])
    excludes = List.wrap(config[:xref][:exclude])
    :ets.insert(@table, [{@warnings_key, table, Atom.to_string(config[:app]), excludes}])
  end

  defp warnings_table() do
    :ets.lookup_element(@table, @warnings_key, 2)
  end

  defp manifest(config) do
    Path.join(Mix.Project.manifest_path(config), @manifest)
  end

  defp build_manifest(config) do
    table = :ets.new(@table, [:public, :named_table, :set, read_concurrency: true])
    {all, _optional} = Mix.Tasks.Compile.App.project_apps(config)

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
end
