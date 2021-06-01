defmodule Mix.Tasks.Compile.All do
  use Mix.Task.Compiler

  @moduledoc false
  @compile {:no_warn_undefined, Logger}
  @recursive true

  # This is an internal task used by "mix compile" which
  # is meant to be recursive and be invoked for each child
  # project.

  @impl true
  def run(args) do
    Mix.Project.get!()
    config = Mix.Project.config()
    validate_compile_env? = "--no-validate-compile-env" not in args
    lib_path = Path.join(Mix.Project.build_path(config), "lib")

    # Make sure Mix.Dep is cached to avoid loading dependencies
    # during compilation. It is likely this will be invoked anyway,
    # as both Elixir and app compilers rely on it.
    Mix.Dep.cached()

    unless "--no-app-loading" in args do
      load_apps(config, lib_path, validate_compile_env?)
    end

    result =
      if "--no-compile" in args do
        Mix.Task.reenable("compile.all")
        {:noop, []}
      else
        # Build the project structure so we can write down compiled files.
        Mix.Project.build_structure(config)

        with_logger_app(config, fn ->
          config
          |> Mix.Tasks.Compile.compilers()
          |> compile(args, :noop, [])
        end)
      end

    app = config[:app]
    _ = Code.prepend_path(Mix.Project.compile_path())
    load_app(app, lib_path, validate_compile_env?)
    result
  end

  defp with_logger_app(config, fun) do
    app = Keyword.fetch!(config, :app)
    logger? = Process.whereis(Logger)
    logger_config_app = Application.get_env(:logger, :compile_time_application)

    try do
      if logger? do
        Logger.configure(compile_time_application: app)
      end

      fun.()
    after
      if logger? do
        Logger.configure(compile_time_application: logger_config_app)
      end
    end
  end

  defp compile([], _, status, diagnostics) do
    {status, diagnostics}
  end

  defp compile([compiler | rest], args, status, diagnostics) do
    {new_status, new_diagnostics} = run_compiler(compiler, args)
    diagnostics = diagnostics ++ new_diagnostics

    case new_status do
      :error ->
        if "--return-errors" not in args do
          exit({:shutdown, 1})
        end

        {:error, diagnostics}

      :ok ->
        compile(rest, args, :ok, diagnostics)

      :noop ->
        compile(rest, args, status, diagnostics)
    end
  end

  defp run_compiler(compiler, args) do
    result = Mix.Task.Compiler.normalize(Mix.Task.run("compile.#{compiler}", args), compiler)
    Enum.reduce(Mix.ProjectStack.pop_after_compiler(compiler), result, & &1.(&2))
  end

  ## App loading helpers

  defp load_apps(config, lib_path, validate_compile_env?) do
    {runtime, optional} = Mix.Tasks.Compile.App.project_apps(config)
    parent = self()
    opts = [ordered: false, timeout: :infinity]
    deps = for dep <- Mix.Dep.cached(), into: %{}, do: {dep.app, lib_path}

    stream_apps(runtime ++ optional, deps)
    |> Task.async_stream(&load_stream_app(&1, parent, validate_compile_env?), opts)
    |> Stream.run()
  end

  defp load_stream_app({app, lib_path}, parent, validate_compile_env?) do
    children =
      case load_app(app, lib_path, validate_compile_env?) do
        :ok ->
          Application.spec(app, :applications) ++ Application.spec(app, :included_applications)

        :error ->
          []
      end

    send(parent, {:done, app, children})
    :ok
  end

  defp stream_apps(initial, deps) do
    Stream.unfold({initial, %{}, %{}, deps}, &stream_app/1)
  end

  # We already processed this app, skip it.
  defp stream_app({[app | apps], seen, done, deps}) when is_map_key(seen, app) do
    stream_app({apps, seen, done, deps})
  end

  # We haven't processed this app, emit it.
  defp stream_app({[app | apps], seen, done, deps}) do
    {{app, deps[app]}, {apps, Map.put(seen, app, true), done, deps}}
  end

  # We have processed all apps and all seen have been done.
  defp stream_app({[], seen, done, _deps}) when map_size(seen) == map_size(done) do
    nil
  end

  # We have processed all apps but there is work being done.
  defp stream_app({[], seen, done, deps}) do
    receive do
      {:done, app, children} -> stream_app({children, seen, Map.put(done, app, true), deps})
    end
  end

  defp load_app(app, lib_path, validate_compile_env?) do
    if Application.spec(app, :vsn) do
      :ok
    else
      with {:ok, bin} <- read_app(app, lib_path),
           {:ok, {:application, _, properties} = application_data} <- consult_app_file(bin),
           :ok <- :application.load(application_data) do
        if compile_env = validate_compile_env? && properties[:compile_env] do
          Config.Provider.validate_compile_env(compile_env, false)
        end

        :ok
      else
        _ -> :error
      end
    end
  end

  # The app didn't come from a dep, go through the slow path (code/erl_prim_loader)
  defp read_app(app, nil) do
    name = Atom.to_charlist(app) ++ '.app'

    with [_ | _] = path <- :code.where_is_file(name),
         {:ok, bin, _full_name} <- :erl_prim_loader.get_file(path),
         do: {:ok, bin}
  end

  defp read_app(app, lib_path) do
    File.read("#{lib_path}/#{app}/ebin/#{app}.app")
  end

  defp consult_app_file(bin) do
    # The path could be located in an .ez archive, so we use the prim loader.
    with {:ok, tokens, _} <- :erl_scan.string(String.to_charlist(bin)) do
      :erl_parse.parse_term(tokens)
    end
  end
end
