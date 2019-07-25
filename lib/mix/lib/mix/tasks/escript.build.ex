defmodule Mix.Tasks.Escript.Build do
  use Mix.Task
  use Bitwise, only_operators: true

  @shortdoc "Builds an escript for the project"
  @recursive true

  @moduledoc ~S"""
  Builds an escript for the project.

  An escript is an executable that can be invoked from the
  command line. An escript can run on any machine that has
  Erlang/OTP installed and by default does not require Elixir to
  be installed, as Elixir is embedded as part of the escript.

  This task guarantees the project and its dependencies are
  compiled and packages them inside an escript. Before invoking
  `mix escript.build`, it is only necessary to define a `:escript`
  key with a `:main_module` option in your `mix.exs` file:

      escript: [main_module: MyApp.CLI]

  Escripts should be used as a mechanism to share scripts between
  developers and not as a deployment mechanism. For running live
  systems, consider using `mix run` or building releases. See
  the `Application` module for more information on systems
  life-cycles.

  By default, this task starts the current application. If this
  is not desired, set the `:app` configuration to nil.

  This task also removes documentation and debugging chunks from
  the compiled `.beam` files to reduce the size of the escript.
  If this is not desired, check the `:strip_beams` option.

  > Note: escripts do not support projects and dependencies
  > that need to store or read artifacts from the priv directory.

  ## Command line options

  Expects the same command line options as `mix compile`.

  ## Configuration

  The following option must be specified in your `mix.exs` under `:escript`
  key:

    * `:main_module` - the module to be invoked once the escript starts.
      The module must contain a function named `main/1` that will receive the
      command line arguments as binaries.

  The remaining options can be specified to further customize the escript:

    * `:name` - the name of the generated escript.
      Defaults to app name.

    * `:path` - the path to write the escript to.
      Defaults to app name.

    * `:app` - the app that starts with the escript.
      Defaults to app name. Set it to `nil` if no application should
      be started.

    * `:strip_beams` - if `true` strips BEAM code in the escript to remove chunks
      unnecessary at runtime, such as debug information and documentation.
      Defaults to `true`.

    * `:embed_elixir` - if `true` embeds Elixir and its children apps
      (`ex_unit`, `mix`, etc.) mentioned in the `:applications` list inside the
      `application/0` function in `mix.exs`.

      Defaults to `true` for Elixir projects, `false` for Erlang projects.

      Note: if you set this to `false` for an Elixir project, you will have to add paths to Elixir's
      `ebin` directories to `ERL_LIBS` environment variable when running the resulting escript, in
      order for the code loader to be able to find `:elixir` application and its children
      applications (if they are used).

    * `:shebang` - shebang interpreter directive used to execute the escript.
      Defaults to `"#! /usr/bin/env escript\n"`.

    * `:comment` - comment line to follow shebang directive in the escript.
      Defaults to `""`.

    * `:emu_args` - emulator arguments to embed in the escript file.
      Defaults to `""`.

  There is one project-level option that affects how the escript is generated:

    * `language: :elixir | :erlang` - set it to `:erlang` for Erlang projects
      managed by Mix. Doing so will ensure Elixir is not embedded by default.
      Your app will still be started as part of escript loading, with the
      config used during build.

  ## Example

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "0.0.1",
            escript: escript()
          ]
        end

        def escript do
          [main_module: MyApp.CLI]
        end
      end

      defmodule MyApp.CLI do
        def main(_args) do
          IO.puts("Hello from MyApp!")
        end
      end

  """

  @impl true
  def run(args) do
    Mix.Project.get!()
    Mix.Task.run("loadpaths", args)

    unless "--no-compile" in args do
      Mix.Project.compile(args)
    end

    project = Mix.Project.config()
    language = Keyword.get(project, :language, :elixir)
    escriptize(project, language)
  end

  defp escriptize(project, language) do
    escript_opts = project[:escript] || []
    script_name = Mix.Local.name_for(:escript, project)
    filename = escript_opts[:path] || script_name
    main = escript_opts[:main_module]

    unless script_name do
      error_message =
        "Could not generate escript, no name given, " <>
          "set :name escript option or :app in the project settings"

      Mix.raise(error_message)
    end

    unless main do
      error_message =
        "Could not generate escript, please set :main_module " <>
          "in your project configuration (under :escript option) to a module that implements main/1"

      Mix.raise(error_message)
    end

    unless Code.ensure_loaded?(main) do
      error_message =
        "Could not generate escript, module #{main} defined as " <>
          ":main_module could not be loaded"

      Mix.raise(error_message)
    end

    app = Keyword.get(escript_opts, :app, project[:app])

    # Need to keep :strip_beam option for backward compatibility so
    # check for correct :strip_beams, then :strip_beam, then
    # use default true if neither are present.
    #
    # TODO: Deprecate :strip_beam option
    strip_beams? =
      Keyword.get_lazy(escript_opts, :strip_beams, fn ->
        Keyword.get(escript_opts, :strip_beam, true)
      end)

    escript_mod = String.to_atom(Atom.to_string(app) <> "_escript")

    beam_paths =
      [project_files(), deps_files(), core_files(escript_opts, language)]
      |> Stream.concat()
      |> prepare_beam_paths()
      |> Map.merge(consolidated_paths(project))

    tuples = gen_main(project, escript_mod, main, app, language) ++ read_beams(beam_paths)
    tuples = if strip_beams?, do: strip_beams(tuples), else: tuples

    case :zip.create('mem', tuples, [:memory]) do
      {:ok, {'mem', zip}} ->
        shebang = escript_opts[:shebang] || "#! /usr/bin/env escript\n"
        comment = build_comment(escript_opts[:comment])
        emu_args = build_emu_args(escript_opts[:emu_args], escript_mod)

        script = IO.iodata_to_binary([shebang, comment, emu_args, zip])
        File.mkdir_p!(Path.dirname(filename))
        File.write!(filename, script)
        set_perms(filename)

      {:error, error} ->
        Mix.raise("Error creating escript: #{error}")
    end

    Mix.shell().info("Generated escript #{filename} with MIX_ENV=#{Mix.env()}")
    :ok
  end

  defp project_files() do
    get_files(Mix.Project.app_path())
  end

  defp get_files(app) do
    Path.wildcard("#{app}/ebin/*.{app,beam}") ++
      (Path.wildcard("#{app}/priv/**/*") |> Enum.filter(&File.regular?/1))
  end

  defp set_perms(filename) do
    stat = File.stat!(filename)
    :ok = File.chmod(filename, stat.mode ||| 0o111)
  end

  defp deps_files() do
    deps = Mix.Dep.cached()
    Enum.flat_map(deps, fn dep -> get_files(dep.opts[:build]) end)
  end

  defp core_files(escript_opts, language) do
    if Keyword.get(escript_opts, :embed_elixir, language == :elixir) do
      Enum.flat_map([:elixir | extra_apps()], &app_files/1)
    else
      []
    end
  end

  defp extra_apps() do
    Mix.Project.config()[:app]
    |> extra_apps_in_app_tree()
    |> Enum.uniq()
  end

  defp extra_apps_in_app_tree(app) when app in [:kernel, :stdlib, :elixir] do
    []
  end

  defp extra_apps_in_app_tree(app) when app in [:eex, :ex_unit, :iex, :logger, :mix] do
    [app]
  end

  defp extra_apps_in_app_tree(app) do
    _ = Application.load(app)

    case Application.spec(app) do
      nil ->
        []

      spec ->
        applications =
          Keyword.get(spec, :applications, []) ++ Keyword.get(spec, :included_applications, [])

        Enum.flat_map(applications, &extra_apps_in_app_tree/1)
    end
  end

  defp app_files(app) do
    case :code.where_is_file('#{app}.app') do
      :non_existing -> Mix.raise("Could not find application #{app}")
      file -> get_files(Path.dirname(Path.dirname(file)))
    end
  end

  defp prepare_beam_paths(paths) do
    for path <- paths, into: %{}, do: {Path.basename(path), path}
  end

  defp read_beams(items) do
    Enum.map(items, fn {basename, beam_path} ->
      {String.to_charlist(basename), File.read!(beam_path)}
    end)
  end

  defp strip_beams(tuples) do
    for {basename, maybe_beam} <- tuples do
      with ".beam" <- Path.extname(basename),
           {:ok, binary} <- Mix.Release.strip_beam(maybe_beam) do
        {basename, binary}
      else
        _ -> {basename, maybe_beam}
      end
    end
  end

  defp consolidated_paths(config) do
    if config[:consolidate_protocols] do
      Mix.Project.consolidation_path(config)
      |> Path.join("*")
      |> Path.wildcard()
      |> prepare_beam_paths()
    else
      %{}
    end
  end

  defp build_comment(user_comment) do
    "%% #{user_comment}\n"
  end

  defp build_emu_args(user_args, escript_mod) do
    "%%! -escript main #{escript_mod} #{user_args}\n"
  end

  defp gen_main(project, name, module, app, language) do
    config =
      if File.regular?(project[:config_path]) do
        config = Config.Reader.read!(project[:config_path])
        Macro.escape(config)
      else
        []
      end

    module_body =
      quote do
        @module unquote(module)
        @config unquote(config)
        @app unquote(app)

        @spec main(OptionParser.argv()) :: any
        def main(args) do
          unquote(main_body_for(language))
        end

        defp load_config(config) do
          each_fun = fn {app, kw} ->
            set_env_fun = fn {k, v} -> :application.set_env(app, k, v, persistent: true) end
            :lists.foreach(set_env_fun, kw)
          end

          :lists.foreach(each_fun, config)
          :ok
        end

        defp start_app(nil) do
          :ok
        end

        defp start_app(app) do
          case :application.ensure_all_started(app) do
            {:ok, _} ->
              :ok

            {:error, {app, reason}} ->
              formatted_error =
                case :code.ensure_loaded(Application) do
                  {:module, Application} -> Application.format_error(reason)
                  {:error, _} -> :io_lib.format('~p', [reason])
                end

              error_message = [
                "ERROR! Could not start application ",
                :erlang.atom_to_binary(app, :utf8),
                ": ",
                formatted_error,
                ?\n
              ]

              io_error(error_message)
              :erlang.halt(1)
          end
        end

        defp io_error(message) do
          :io.put_chars(:standard_error, message)
        end
      end

    {:module, ^name, binary, _} = Module.create(name, module_body, Macro.Env.location(__ENV__))
    [{'#{name}.beam', binary}]
  end

  defp main_body_for(:elixir) do
    quote do
      load_config(@config)

      case :application.ensure_all_started(:elixir) do
        {:ok, _} ->
          start_app(@app)
          args = Enum.map(args, &List.to_string(&1))
          Kernel.CLI.run(fn _ -> @module.main(args) end)

        error ->
          io_error(["ERROR! Failed to start Elixir.\n", :io_lib.format('error: ~p~n', [error])])
          :erlang.halt(1)
      end
    end
  end

  defp main_body_for(:erlang) do
    quote do
      load_config(@config)
      start_app(@app)
      @module.main(args)
    end
  end
end
