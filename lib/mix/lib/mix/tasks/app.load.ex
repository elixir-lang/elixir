defmodule Mix.Tasks.App.Load do
  use Mix.Task

  @recursive true
  @shortdoc "Loads all registered apps"

  @moduledoc """
  Loads all apps this apps depends on.

  This also includes compile-time and optional apps.

  ## Command line options

    * `--no-validate-compile-env` - does not validate the application compile environment

  """
  def run(args) do
    config = Mix.Project.config()
    {runtime, optional} = Mix.Tasks.Compile.App.project_apps(config)
    validate_compile_env? = "--no-validate-compile-env" not in args

    %{}
    |> load_apps(runtime, validate_compile_env?)
    |> load_apps(optional, validate_compile_env?)

    :ok
  end

  @doc false
  def load(app, validate_compile_env?) do
    if Application.spec(app, :vsn) do
      :ok
    else
      name = Atom.to_charlist(app) ++ '.app'

      with [_ | _] = path <- :code.where_is_file(name),
           {:ok, {:application, _, properties} = application_data} <- consult_app_file(path),
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

  defp load_apps(seen, apps, validate_compile_env?) do
    Enum.reduce(apps, seen, fn app, seen ->
      if Map.has_key?(seen, app) do
        seen
      else
        seen = Map.put(seen, app, true)

        case load(app, validate_compile_env?) do
          :ok ->
            seen
            |> load_apps(Application.spec(app, :applications), validate_compile_env?)
            |> load_apps(Application.spec(app, :included_applications), validate_compile_env?)

          :error ->
            seen
        end
      end
    end)
  end

  defp consult_app_file(path) do
    # The path could be located in an .ez archive, so we use the prim loader.
    with {:ok, bin, _full_name} <- :erl_prim_loader.get_file(path),
         {:ok, tokens, _} <- :erl_scan.string(String.to_charlist(bin)) do
      :erl_parse.parse_term(tokens)
    end
  end
end
