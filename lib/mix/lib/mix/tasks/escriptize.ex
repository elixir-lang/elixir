defmodule Mix.Tasks.Escriptize do
  use Mix.Task
  use Bitwise, only_operators: true

  @shortdoc "Generates an escript for the project"
  @recursive true

  @moduledoc %B"""
  Generates an escript for the project.

  ## Command line options

  * `--force` - forces compilation regardless of modification times

  * `--no-compile` - skips compilation to .beam files

  ## Configuration

  The following options can be specified in your `mix.exs` file:

  * `escript_name` - the name of the generated escript.
     Defaults to app name

  * `escript_path` - the path to write the escript to.
     Defaults to app name

  * `escript_app` - the app to start with the escript.
     Defaults to app name. Set it to `nil` if no application should
     be started.

  * `escript_main_module` - the module containing the `main/1` function.
     Defaults to `Project`

  * `escript_embed_elixir` - if `true` embed elixir in the escript file.
     Defaults to `true`

  * `escript_embed_extra_apps` - embed additional Elixir applications.
     if `escript_embed_elixir` is `true`.
     Defaults to `[]`

  * `escript_shebang` - shebang interpreter directive used to execute the escript.
     Defaults to "#! /usr/bin/env escript\n"

  * `escript_comment` - comment line to follow shebang directive in the escript.
     Defaults to "%%\n"

  * `escript_emu_args` - emulator arguments to embed in the escript file.
     Defaults to "%%!\n"

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean, no_compile: :boolean])

    # Require the project to be available
    Mix.Project.get!

    unless opts[:no_compile] do
      Mix.Task.run :compile, args
    end

    escriptize(Mix.project, opts[:force])
  end

  defp escriptize(project, force) do
    script_name  = project[:escript_name] || project[:app]
    filename     = project[:escript_path] || atom_to_binary(script_name)
    embed        = Keyword.get(project, :escript_embed_elixir, true)
    app          = Keyword.get(project, :escript_app, project[:app])
    files        = project_files()

    cond do
      !script_name ->
        raise Mix.Error, message: "Could not generate escript, no name given, " <>
          "set :escript_name or :app in the project settings"
      force || Mix.Utils.stale?(files, [filename]) ->
        tuples = gen_main(script_name, project[:escript_main_module], app) ++ to_tuples(files)
        tuples = tuples ++ deps_tuples()

        if embed do
          extra_apps = project[:escript_embed_extra_apps] || []
          tuples = Enum.reduce [:elixir|extra_apps], tuples, fn(app, acc) ->
            app_tuples(app) ++ acc
          end
        end

        # We might get duplicate tuples in umbrella projects from applications
        # sharing the same dependencies
        tuples = Enum.uniq(tuples, fn {name, _} -> name end)

        case :zip.create 'mem', tuples, [:memory] do
          { :ok, { 'mem', zip } } ->
            shebang  = project[:escript_shebang]  || "#! /usr/bin/env escript\n"
            comment  = project[:escript_comment]  || "%%\n"
            emu_args = project[:escript_emu_args] || "%%!\n"

            script = iolist_to_binary([shebang, comment, emu_args, zip])

            File.mkdir_p!(Path.dirname(filename))
            File.write!(filename, script)
          {:error, error} ->
            Mix.shell.error "Error creating escript: #{error}"
        end

        set_perms(filename)
        Mix.shell.info "Generated escript #{filename}"
        :ok
      true ->
        :noop
    end
  end

  defp project_files do
    get_files(".")
  end

  defp deps_tuples do
    Enum.reduce Mix.Deps.all || [], [], fn(dep, acc) ->
      dep_tuples(dep.app) ++ acc
    end
  end

  defp set_perms(filename) do
    stat = File.stat!(filename)
    :ok  = :file.change_mode(filename, stat.mode ||| 73)
  end

  defp dep_tuples(dep) do
    get_tuples(Path.join("deps", atom_to_binary(dep)))
  end

  defp app_tuples(app) do
    case :code.where_is_file('#{app}.app') do
      :non_existing -> raise Mix.Error, message: "Could not find application #{app}"
      file -> get_tuples(Path.dirname(Path.dirname(file)))
    end
  end

  defp get_files(app) do
    Path.wildcard("#{app}/ebin/*.{app,beam}") ++
      (Path.wildcard("#{app}/priv/**/*") |> Enum.filter(File.regular?(&1)))
  end

  defp get_tuples(app) do
    get_files(app) |> to_tuples
  end

  defp to_tuples(files) do
    lc f inlist files do
      { :unicode.characters_to_list(Path.basename(f)), File.read!(f) }
    end
  end

  defp gen_main(script_name, nil, app) do
    camelized = Mix.Utils.camelize(atom_to_binary(script_name))
    gen_main(script_name, Module.concat([camelized]), app)
  end

  defp gen_main(script_name, script_name, _app) do
    []
  end

  defp gen_main(name, module, app) do
    { :module, ^name, binary, _ } =
      defmodule name do
        @module module
        @app app

        def main(args) do
          case :application.start(:elixir) do
            :ok ->
              start_app
              args = Enum.map(args, :unicode.characters_to_binary(&1))
              Kernel.CLI.run fn -> @module.main(args) end, true
            _   ->
              IO.puts :stderr, IO.ANSI.escape("%{red, bright} Elixir is not in the code path, aborting.")
              System.halt(1)
          end
        end

        defp start_app do
          if app = @app do
            case Application.Behaviour.start(app) do
              :ok -> :ok
              { :error, reason } ->
                IO.puts :stderr, IO.ANSI.escape("%{red, bright} Could not start application #{app}: #{inspect reason}.")
                System.halt(1)
            end
          end
        end
      end

    [{ '#{name}.beam', binary }]
  end
end
