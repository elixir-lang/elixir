defmodule Mix.Tasks.Escriptize do
  use Mix.Task
  use Bitwise, only_operators: true

  @shortdoc "Generates an escript for the project"

  @moduledoc %B"""
  Generates an escript for the project.

  ## Command line options

  * `--force` forces compilation regardless of mod times

  * `--no-compile` skips compilation to .beam

  ## Configuration

  The following options can be specified in your mix.exs file:

  * `escript_name` - the name of the generated escript
     Defaults to project name

  * `escript_path` - the path to write the escript to
     Defaults to project name

  * `escript_main_module` - the module containing the main/1 function.
     Defaults to `Project`

  * `escript_embed_elixir` - if true embed elixir in the escript file.
     Defaults to true

  * `escript_embed_extra_apps` - embed additional Elixir applications
     if `escript_embed_elixir` is true.
     Defaults to []

  * `escript_shebang`
     Defaults to "#! /usr/bin/env escript\n"

  * `escript_comment`
     Defaults to "%%\n"

  * `escript_emu_args` - emulator arguments to embed in the escript file
     Defaults to "%%!\n"

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean, no_compile: :boolean])

    unless opts[:no_compile] do
      Mix.Task.run :compile, args
    end

    escriptize(Mix.project, opts[:force])
  end

  defp escriptize(project, force) do
    script_name  = project[:escript_name] || project[:app]
    filename     = project[:escript_path] || atom_to_binary(script_name)
    compile_path = project[:compile_path] || "ebin"
    embed        = Keyword.get(project, :escript_embed_elixir, true)
    beams        = Mix.Project.config_files ++ Path.wildcard('#{compile_path}/*.beam')

    cond do
      beams == [] ->
        Mix.shell.error "Could not generate escript #{filename}, no beam files available"
        :noop
      force or Mix.Utils.stale?(beams, [filename]) ->
        files = gen_main(script_name, project[:escript_main_module])
        files = files ++ get_files(compile_path)

        if embed do
          extra_apps = project[:escript_embed_extra_apps] || []
          files = Enum.reduce [:elixir|extra_apps], files, fn(app, acc) ->
            app_files(app) ++ acc
          end
        end

        files = Enum.reduce Mix.Deps.all || [], files, fn(dep, acc) ->
          dep_files(dep.app) ++ acc
        end

        case :zip.create 'mem', files, [:memory] do
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
      true ->
        :noop
    end
  end

  defp set_perms(filename) do
    stat = File.stat!(filename)
    :ok  = :file.change_mode(filename, stat.mode ||| 73)
  end

  defp dep_files(dep) do
    get_files(Path.join(["deps", atom_to_binary(dep), "ebin"]))
  end

  defp app_files(app) do
    case :code.where_is_file('#{app}.app') do
      :non_existing -> raise Mix.Error, "Could not find application #{app}"
      file -> get_files(Path.dirname(file))
    end
  end

  defp get_files(dir) do
    lc f inlist Path.wildcard("#{dir}/**/*.{app,beam}") do
      { binary_to_list(Path.basename(f)), File.read!(f) }
    end
  end

  defp gen_main(script_name, nil) do
    camelized = Mix.Utils.camelize(atom_to_binary(script_name))
    gen_main(script_name, Module.concat([camelized]))
  end

  defp gen_main(script_name, script_name) do
    []
  end

  defp gen_main(name, module) do
    { :module, ^name, binary, _ } =
      defmodule name do
        @module module

        def main(args) do
          case :application.start(:elixir) do
            :ok ->
              Kernel.CLI.run fn -> @module.main(args) end, true
            _   ->
              IO.puts :stderr, "Elixir is not in the code path, aborting."
              System.halt(1)
          end
        end
      end

    [{ '#{name}.beam', binary }]
  end
end
