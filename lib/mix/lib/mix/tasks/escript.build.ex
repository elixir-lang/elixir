defmodule Mix.Tasks.Escript.Build do
  use Mix.Task
  use Bitwise, only_operators: true

  @shortdoc "Builds an escript for the project"

  @moduledoc ~S"""
  Builds an escript for the project.

  An escript is an executable that can be invoked from the
  command line.  An escript can run on any machine that has
  Erlang installed and by default does not require Elixir to
  be installed, as Elixir is embedded as part of the escript.

  This task guarantees the project and its dependencies are
  compiled and packages them inside an escript.

  > Note: escripts do not support projects and dependencies
  > that need to store or read artifacts from the priv directory.

  ## Command line options

    * `--force`      - forces compilation regardless of modification times
    * `--no-compile` - skips compilation to .beam files

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

    * `:app` - the app to start with the escript.
      Defaults to app name. Set it to `nil` if no application should
      be started.

    * `:embed_elixir` - if `true` embed Elixir and its children apps
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
      managed by mix. Doing so will ensure Elixir is not embedded by default.
      Your app will still be started as part of escript loading, with the
      config used during build.

  ## Example

      defmodule MyApp.Mixfile do
        def project do
          [app: :my_app,
           version: "0.0.1",
           escript: escript]
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
  @switches [force: :boolean, compile: :boolean,
             deps_check: :boolean, archives_check: :boolean, elixir_version_check: :boolean]

  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    Mix.Project.get!
    {opts, _} = OptionParser.parse!(args, strict: @switches)

    if Keyword.get(opts, :compile, true) do
      Mix.Task.run :compile, args
    end

    project  = Mix.Project.config
    language = Keyword.get(project, :language, :elixir)

    escriptize(project, language, opts[:force])
  end

  defp escriptize(project, language, force) do
    escript_opts = project[:escript] || []

    script_name  = Mix.Local.name_for(:escript, project)
    filename     = escript_opts[:path] || script_name
    main         = escript_opts[:main_module]
    app          = Keyword.get(escript_opts, :app, project[:app])
    files        = project_files()

    escript_mod = String.to_atom(Atom.to_string(app) <> "_escript")

    cond do
      !script_name ->
        Mix.raise "Could not generate escript, no name given, " <>
          "set :name escript option or :app in the project settings"

      !main ->
        Mix.raise "Could not generate escript, please set :main_module " <>
          "in your project configuration (under :escript option) to a module that implements main/1"

      not Code.ensure_loaded?(main) ->
        Mix.raise "Could not generate escript, module #{main} defined as " <>
          ":main_module could not be loaded"

      force || Mix.Utils.stale?(files, [filename]) ->
        beam_paths =
          [files, deps_files(), core_files(escript_opts, language)]
          |> Stream.concat
          |> prepare_beam_paths()
          |> Map.merge(consolidated_paths(project))

        tuples = gen_main(project, escript_mod, main, app, language) ++
                 read_beams(beam_paths)

        case :zip.create 'mem', tuples, [:memory] do
          {:ok, {'mem', zip}} ->
            shebang  = escript_opts[:shebang] || "#! /usr/bin/env escript\n"
            comment  = build_comment(escript_opts[:comment])
            emu_args = build_emu_args(escript_opts[:emu_args], escript_mod)

            script = IO.iodata_to_binary([shebang, comment, emu_args, zip])
            File.mkdir_p!(Path.dirname(filename))
            File.write!(filename, script)
            set_perms(filename)
          {:error, error} ->
            Mix.raise "Error creating escript: #{error}"
        end

        Mix.shell.info "Generated escript #{filename} with MIX_ENV=#{Mix.env}"
        :ok
      true ->
        :noop
    end
  end

  defp project_files() do
    get_files(Mix.Project.app_path)
  end

  defp get_files(app) do
    Path.wildcard("#{app}/ebin/*.{app,beam}") ++
      (Path.wildcard("#{app}/priv/**/*") |> Enum.filter(&File.regular?/1))
  end

  defp set_perms(filename) do
    stat = File.stat!(filename)
    :ok  = File.chmod(filename, stat.mode ||| 0o111)
  end

  defp deps_files() do
    deps = Mix.Dep.cached()
    Enum.flat_map(deps, fn dep -> get_files(dep.opts[:build]) end)
  end

  defp core_files(escript_opts, language) do
    if Keyword.get(escript_opts, :embed_elixir, language == :elixir) do
      Enum.flat_map [:elixir | extra_apps()], &app_files/1
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
        applications = Keyword.get(spec, :applications, []) ++
                       Keyword.get(spec, :included_applications, [])
        Enum.flat_map(applications, &extra_apps_in_app_tree/1)
    end
  end

  defp app_files(app) do
    case :code.where_is_file('#{app}.app') do
      :non_existing -> Mix.raise "Could not find application #{app}"
      file -> get_files(Path.dirname(Path.dirname(file)))
    end
  end

  defp prepare_beam_paths(paths) do
    for path <- paths, into: %{}, do: {Path.basename(path), path}
  end

  defp read_beams(items) do
    items
    |> Enum.map(fn {basename, beam_path} ->
      {String.to_charlist(basename), File.read!(beam_path)}
    end)
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
        Macro.escape Mix.Config.read!(project[:config_path])
      else
        []
      end

    module_body = quote do
      @module unquote(module)
      @config unquote(config)
      @app unquote(app)

      @spec main(OptionParser.argv) :: any
      def main(args) do
        unquote(main_body_for(language))
      end

      defp load_config(config) do
        :lists.foreach(fn {app, kw} ->
          :lists.foreach(fn {k, v} ->
            :application.set_env(app, k, v, persistent: true)
          end, kw)
        end, config)
        :ok
      end

      defp start_app(nil) do
        :ok
      end

      defp start_app(app) do
        case :application.ensure_all_started(app) do
          {:ok, _} -> :ok
          {:error, {app, reason}} ->
            formatted_error = case :code.ensure_loaded(Application) do
              {:module, Application} -> Application.format_error(reason)
              {:error, _} -> :io_lib.format('~p', [reason])
            end
            io_error ["Could not start application ",
                      :erlang.atom_to_binary(app, :utf8),
                      ": ", formatted_error, ?\n]
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
      erl_version = :erlang.system_info(:otp_release)

      case :string.to_integer(erl_version) do
        {num, _} when num >= 18 -> nil
        _ ->
          io_error ["Incompatible Erlang/OTP release: ", erl_version,
                    ".\nThis escript requires at least Erlang/OTP 18.0\n"]
          :erlang.halt(1)
      end

      case :application.ensure_all_started(:elixir) do
        {:ok, _} ->
          load_config(@config)
          start_app(@app)
          args = Enum.map(args, &List.to_string(&1))
          Kernel.CLI.run fn _ -> @module.main(args) end, true
        error ->
          io_error ["Failed to start Elixir.\n", :io_lib.format('error: ~p~n', [error])]
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
