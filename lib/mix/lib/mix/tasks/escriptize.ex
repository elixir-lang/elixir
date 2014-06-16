defmodule Mix.Tasks.Escriptize do
  use Mix.Task
  use Bitwise, only_operators: true

  @shortdoc "Generates an escript for the project"
  @recursive true

  @moduledoc ~S"""
  Generates an escript for the project.

  ## Command line options

    * `--force`      - forces compilation regardless of modification times
    * `--no-compile` - skips compilation to .beam files

  ## Configuration

  The following option must be specified in your `mix.exs` under `:escript`
  key:

    * `:main_module` - the module to be invoked once the escript starts.
      The module must contain a function named `main/1` that will receive the
      command line arguments as binaries;

  The remaining options can be specified to further customize the escript:

    * `:name` - the name of the generated escript.
      Defaults to app name;

    * `:path` - the path to write the escript to.
      Defaults to app name;

    * `:app` - the app to start with the escript.
      Defaults to app name. Set it to `nil` if no application should
      be started.

    * `:embed_elixir` - if `true` embed elixir in the escript file.
      Defaults to `true`.

    * `:embed_extra_apps` - embed additional Elixir applications.
      if `:embed_elixir` is `true`.
      Defaults to `[]`.

    * `:shebang` - shebang interpreter directive used to execute the escript.
      Defaults to "#! /usr/bin/env escript\n".

    * `:comment` - comment line to follow shebang directive in the escript.
      Defaults to "%%\n"

    * `:emu_args` - emulator arguments to embed in the escript file.
      Defaults to "%%!\n".

  ## Example

      defmodule MyApp.Mixfile do
        def project do
          [ app: :myapp,
            version: "0.0.1",
            escript: escript ]
        end

        def escript do
          [ main_module: MyApp.CLI,
            embed_extra_apps: [:mix] ]
        end
      end

  """
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean, no_compile: :boolean])

    # Require the project to be available
    Mix.Project.get!

    unless opts[:no_compile] do
      Mix.Task.run :compile, args
    end

    escriptize(Mix.Project.config, opts[:force])
  end


  @deprecated_opts [
    :escript_main_module, :escript_name, :escript_path, :escript_app,
    :escript_embed_elixir, :escript_embed_extra_apps, :escript_shebang,
    :escript_comment, :escript_mu_args, ]

  @prefix_len String.length("escript_")

  defp collect_deprecated_opts(project) do
    Enum.reduce(@deprecated_opts, [], fn name, acc ->
      if Keyword.has_key?(project, name) do
        IO.puts :stderr, "Option #{inspect name} is deprecated. " <>
            "Use the new `:escript` option that takes a keyword list instead."
        new_name =
          Atom.to_string(name) |> String.slice(@prefix_len, 100) |> String.to_atom()
        [{new_name, project[name]}|acc]
      else
        acc
      end
    end)
  end

  defp escriptize(project, force) do
    escript_opts = project[:escript] || collect_deprecated_opts(project)

    script_name  = escript_opts[:name] || project[:app]
    filename     = escript_opts[:path] || Atom.to_string(script_name)
    main         = escript_opts[:main_module]
    embed        = Keyword.get(escript_opts, :embed_elixir, true)
    app          = Keyword.get(escript_opts, :app, project[:app])
    files        = project_files()

    cond do
      !script_name ->
        Mix.raise "Could not generate escript, no name given, " <>
          "set :name escript option or :app in the project settings"

      !main or !Code.ensure_loaded?(main)->
        Mix.raise "Could not generate escript, please set :main_module " <>
          "in your project configuration (under `:escript` option) to a module that implements main/1"

      force || Mix.Utils.stale?(files, [filename]) ->
        tuples = gen_main(script_name, main, app) ++ to_tuples(files)
        tuples = tuples ++ deps_tuples()

        if embed do
          extra_apps = escript_opts[:embed_extra_apps] || []
          tuples = Enum.reduce [:elixir|extra_apps], tuples, fn(app, acc) ->
            app_tuples(app) ++ acc
          end
        end

        # We might get duplicate tuples in umbrella projects from applications
        # sharing the same dependencies
        tuples = Enum.uniq(tuples, fn {name, _} -> name end)

        case :zip.create 'mem', tuples, [:memory] do
          {:ok, {'mem', zip}} ->
            shebang  = escript_opts[:shebang]  || "#! /usr/bin/env escript\n"
            comment  = escript_opts[:comment]  || "%%\n"
            emu_args = escript_opts[:emu_args] || "%%!\n"

            script = IO.iodata_to_binary([shebang, comment, emu_args, zip])

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
    get_files(Mix.Project.app_path)
  end

  defp deps_tuples do
    deps = Mix.Dep.loaded(env: Mix.env) || []
    Enum.reduce(deps, [], fn dep, acc ->
      get_tuples(dep.opts[:build]) ++ acc
    end)
  end

  defp set_perms(filename) do
    stat = File.stat!(filename)
    :ok  = :file.change_mode(filename, stat.mode ||| 73)
  end

  defp app_tuples(app) do
    case :code.where_is_file('#{app}.app') do
      :non_existing -> Mix.raise "Could not find application #{app}"
      file -> get_tuples(Path.dirname(Path.dirname(file)))
    end
  end

  defp get_files(app) do
    Path.wildcard("#{app}/ebin/*.{app,beam}") ++
      (Path.wildcard("#{app}/priv/**/*") |> Enum.filter(&File.regular?/1))
  end

  defp get_tuples(app) do
    get_files(app) |> to_tuples
  end

  defp to_tuples(files) do
    for f <- files do
      {String.to_char_list(Path.basename(f)), File.read!(f)}
    end
  end

  defp gen_main(name, module, app) do
    {:module, ^name, binary, _} =
      defmodule name do
        @module module
        @app app

        def main(args) do
          case :application.start(:elixir) do
            :ok ->
              start_app(@app)
              args = Enum.map(args, &List.to_string(&1))
              Kernel.CLI.run fn -> @module.main(args) end, true
            _   ->
              io_error "Elixir is not in the code path, aborting."
              System.halt(1)
          end
        end

        defp start_app(nil) do
          :ok
        end

        defp start_app(app) do
          case :application.ensure_all_started(app) do
            {:ok, _} -> :ok
            {:error, {app, reason}} ->
              io_error "Could not start application #{app}: " <>
                Application.format_error(reason)
              System.halt(1)
          end
        end

        defp io_error(message) do
           IO.puts :stderr, IO.ANSI.escape("%{red, bright} " <> message)
        end
      end

    [{'#{name}.beam', binary}]
  end
end
