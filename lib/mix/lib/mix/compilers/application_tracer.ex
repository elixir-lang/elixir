defmodule Mix.Compilers.ApplicationTracer do
  @moduledoc false
  @manifest "compile.app_tracer"
  @table __MODULE__
  @warnings_key 0

  def init(opts) do
    config = Mix.Project.config()
    manifest = manifest(config)

    if Mix.Utils.stale?([Mix.Project.config_mtime()], [manifest]) do
      build_manifest(config, manifest)
    else
      read_manifest(manifest) || build_manifest(config, manifest)
    end

    setup_warnings_table(config)
    Keyword.update(opts, :tracers, [__MODULE__], &[__MODULE__ | &1])
  end

  # :erlang and other preloaded apps are not part of any module.
  # :code.which/1 will rule them out but, since :erlang is very
  # common, we rule it out upfront.
  def trace({_, _, :erlang, _, _}, _env) do
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

  def warnings(modules_entries) do
    [{_, table, app, excludes}] = :ets.lookup(@table, @warnings_key)

    for module_entry <- modules_entries do
      :ets.delete(table, elem(module_entry, 1))
    end

    warnings =
      :ets.foldl(
        fn {module, function, arity, env_module, env_function, env_file, env_line}, acc ->
          # If the module is either preloaded, it is always available.
          # If the module is non existing, the compiler will warn, so we don't.
          # If the module belongs to this application (from another compiler), we skip it.
          # If the module is excluded, we skip it too.
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

            warning = {:undefined_app, module_app, module, function, arity}
            [{__MODULE__, warning, {env_file, env_line, env_mfa}} | acc]
          else
            _ -> acc
          end
        end,
        [],
        table
      )

    warnings
    |> Module.Checker.group_warnings()
    |> Module.Checker.emit_warnings()
  end

  # ../elixir/ebin/elixir.beam -> elixir
  # ../ssl-9.6/ebin/ssl.beam -> ssl
  defp app(path) do
    case path |> Path.split() |> Enum.take(-3) do
      [dir, "ebin", _beam] -> {:ok, dir |> String.split("-") |> hd()}
      _ -> :error
    end
  end

  def stop() do
    try do
      :ets.delete(warnings_table())
    rescue
      _ -> false
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
    is used by the current application but the current application does not directly \
    depend on :#{app}. To fix this, you must do one of:

      1. If :#{app} is part of Erlang/Elixir, you must include it under \
    :extra_applications inside "def application" in your mix.exs

      2. If :#{app} is a dependency, make sure it is listed under "def deps" \
    in your mix.exs

      3. In case you don't want to add a requirement to :#{app}, you may \
    optionally skip this warning by adding [xref: [exclude: #{inspect(module)}] \
    to your "def project" in mix.exs
    """
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

  defp read_manifest(manifest) do
    try do
      {:ok, table} = :ets.file2tab(String.to_charlist(manifest))
      table
    rescue
      _ -> nil
    end
  end

  defp build_manifest(config, manifest) do
    table = :ets.new(@table, [:public, :named_table, :set, read_concurrency: true])

    %{}
    |> store_apps(table, language_apps(config))
    |> store_apps(table, app_file_applications())
    |> store_deps(table, config)

    File.mkdir_p!(Path.dirname(manifest))
    :ok = :ets.tab2file(table, String.to_charlist(manifest), sync: true)
    table
  end

  defp language_apps(config) do
    case Keyword.get(config, :language, :elixir) do
      :elixir -> [:elixir, :ex_unit, :mix]
      :erlang -> [:stdlib, :kernel]
    end
  end

  defp app_file_applications() do
    project = Mix.Project.get!()

    if function_exported?(project, :application, 0) do
      properties = project.application()

      Keyword.get(properties, :applications, []) ++
        Keyword.get(properties, :extra_applications, []) ++
        Keyword.get(properties, :included_applications, [])
    else
      []
    end
  end

  defp store_deps(seen, table, config) do
    config
    |> Mix.Dep.compile_or_runtime_deps_mapping()
    |> Enum.reduce(seen, fn {app, _}, acc -> store_app(table, app, acc) end)
  end

  defp store_apps(seen, table, apps) do
    Enum.reduce(apps, seen, &store_app(table, &1, &2))
  end

  defp store_app(table, app, seen) do
    if Map.has_key?(seen, app) do
      seen
    else
      seen = Map.put(seen, app, true)
      result = Application.load(app)

      if result == :ok or result == {:error, {:already_loaded, app}} do
        modules = Application.spec(app, :modules)
        :ets.insert(table, Enum.map(modules, &{&1}))

        seen
        |> store_apps(table, Application.spec(app, :applications))
        |> store_apps(table, Application.spec(app, :included_applications))
      else
        seen
      end
    end
  end
end
