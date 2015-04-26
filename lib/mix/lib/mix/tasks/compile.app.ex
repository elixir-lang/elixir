defmodule Mix.Tasks.Compile.App do
  use Mix.Task

  @recursive true

  @moduledoc """
  Writes an .app file.

  An `.app` file is a file containing Erlang terms that defines
  your application. Mix automatically generates this file based on
  your `mix.exs` configuration. You can learn more about OTP
  applications by seeing the documentation for the `Application`
  module.

  In order to generate the `.app` file, Mix expects your application
  to have both `:app` and `:version` keys. Furthermore, you can
  configure the generated application by defining an `application`
  function in your `mix.exs` with the following options:

    * `:applications` - all applications your application depends
      on at runtime. For example, if your application depends on
      Erlang's `:crypto`, it needs to be added to this list. Most
      of your dependencies must be added as well (unless they're
      a development or test dependency). Mix and other tools use this
      list in order to properly boot your application dependencies
      before starting the application itself.

    * `:registered` - the name of all registered processes in the
      application. If your application defines a local GenServer
      with name `MyServer`, it is recommended to add `MyServer`
      to this list. It is mostly useful to detect conflicts
      between applications that register the same names.

    * `:mod` - specify a module to invoke when the application
      is started, it must be in the format `{Mod, args}` where
      args is often an empty list. The module specified here must
      implement the callbacks defined by the `Application`
      module.

    * `:env` - default values for the application environment.
      The application environment is one of the most common ways
      to configure applications.

  Let's see an example `application` function:

      def application do
        [mod: {MyApp, []},
         env: [default: :value],
         applications: [:crypto]]
      end

  Besides the options above, `.app` files also expects other
  options like `:modules` and `:vsn`, but those are automatically
  filled by Mix.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.Project.get!
    config  = Mix.Project.config

    app     = Keyword.fetch!(config, :app)
    version = Keyword.fetch!(config, :version)

    validate_app(app)
    validate_version(version)

    path = Mix.Project.compile_path
    mods = modules_from(Path.wildcard("#{path}/*.beam")) |> Enum.sort

    target  = Path.join(path, "#{app}.app")
    sources = Mix.Project.config_files

    if opts[:force] || Mix.Utils.stale?(sources, [target]) || modules_changed?(mods, target) do
      best_guess = [
        vsn: to_char_list(version),
        modules: mods,
        applications: []
      ]

      properties = if function_exported?(project, :application, 0) do
        Keyword.merge(best_guess, project.application)
      else
        best_guess
      end

      # Ensure we always prepend the standard application dependencies
      core_apps = [:kernel, :stdlib] ++ language_app(config)
      properties = Keyword.update!(properties, :applications, fn apps ->
        core_apps ++ apps
      end)

      properties = ensure_correct_properties(app, config, properties)
      contents   = {:application, app, properties}

      # There are no modules, we assume no structure was built.
      # If it was built, it doesn't matter, we just rebuild it.
      if mods == [], do: Mix.Project.build_structure(config)
      File.write!(target, :io_lib.format("~p.", [contents]))

      Mix.shell.info "Generated #{app} app"
      :ok
    else
      :noop
    end
  end

  defp modules_changed?(mods, target) do
    case :file.consult(target) do
      {:ok, [{:application, _app, properties}]} ->
        properties[:modules] != mods
      _ ->
        false
    end
  end

  defp validate_app(app) when is_atom(app), do: :ok
  defp validate_app(_), do: raise(Mix.Error, message: "Expected :app to be an atom")

  defp validate_version(version) do
    unless is_binary(version) and match?({:ok, _}, Version.parse(version)) do
      raise(Mix.Error, message: "Expected :version to be a SemVer version")
    end
  end

  defp modules_from(beams) do
    Enum.map beams, &(&1 |> Path.basename |> Path.rootname(".beam") |> String.to_atom)
  end

  defp language_app(config) do
    case Keyword.fetch(config, :language) do
      {:ok, :elixir} -> [:elixir]
      {:ok, :erlang} -> []
      :error -> [:elixir]
    end
  end

  defp ensure_correct_properties(app, config, properties) do
    properties
    |> Keyword.put_new(:description, to_char_list(config[:description] || app))
    |> Keyword.put_new(:registered, [])
    |> validate_properties
  end

  defp validate_properties(properties) do
    Enum.each properties, fn
      {:description, value} ->
        unless is_list(value), do:
          invalid "Application description (:description) is not a character list (got #{inspect value})"
      {:id, value} ->
        unless is_list(value), do:
          invalid "Application id (:id) is not a character list (got #{inspect value} instead)"
      {:vsn, value} ->
        unless is_list(value), do:
          invalid "Application vsn (:vsn) is not a character list (got #{inspect value} instead)"
      {:maxT, value} ->
        unless value == :infinity or is_integer(value), do:
          invalid "Application maximum time (:maxT) is not an integer or :infinity (got #{inspect value} instead)"
      {:modules, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application modules (:modules) should be a list of atoms (got #{inspect value} instead)"
      {:registered, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application registered processes (:registered) should be a list of atoms (got #{inspect value} instead)"
      {:included_applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application included applications (:included_applications) should be a list of atoms (got #{inspect value} instead)"
      {:applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application dependencies (:applications) should be a list of atoms (got #{inspect value} instead)"
      {:env, value} ->
        unless Keyword.keyword?(value), do:
          invalid "Application dependencies (:env) should be a keyword list (got #{inspect value} instead)"
      {:start_phases, value} ->
        unless Keyword.keyword?(value), do:
          invalid "Application start phases (:start_phases) should be a keyword list (got #{inspect value} instead)"
      {:mod, []} ->
        :ok
      {:mod, {module, _args}} when is_atom(module) ->
        :ok
      {:mod, value} ->
        invalid "Application callback module (:mod) should be either [] or {module, start_args} (got #{inspect value} instead)"
      _ ->
        :ok
    end

    properties
  end

  defp invalid(message) do
    Mix.raise message
  end
end
