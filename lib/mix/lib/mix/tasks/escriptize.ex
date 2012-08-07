defmodule Mix.Tasks.Escriptize do
  use Mix.Task
  use Bitwise, only_operators: true

  @shortdoc "Generates an escript for the project"

  @moduledoc """
  Generates an escript for the project.

  The following options can be specified in your mix.exs file:

  * `escript_name` - the name of the generated escript
    Defaults to project name

  * `escript_main_module` - the module containing the main/1 function
     Defaults to 'Elixir-Projectname'

  * `escript_embed_elixir` - if true embed elixir in the escript file
    Defaults to false

  * `escript_shebang`
    Defaults to "#! /usr/bin/env escript\\n"

  * `escript_comment`
    Defaults to "%%\\n"

  * `escript_emu_args` - emulator arguments to embed in the escript file
    Defaults to "%%!\\n"

  """
  def run(args) do
    Mix.Task.run :compile, args
    project = Mix.project
    if project[:app] == :elixir do
      {:ok, {'mem', zip}} = :zip.create 'mem', elixir_files, [:memory]
      script = iolist_to_binary(["#! /usr/bin/env escript\n%%! -noshell\n", zip])
      :file.write_file('elixir', script)
      set_perms('elixir')
      Mix.Shell.info "Generated elixir escript"
    else
      escriptize(project)
    end
  end

  defp escriptize(project) do
    filename = project[:escript_name] || atom_to_list(project[:app])
    compile_path = project[:compile_path]  || "ebin"
    files = get_files compile_path
    files = [gen_main(filename, project[:escript_main_module])|files]
    embed_elixir = project[:escript_embed_elixir] || false
    if embed_elixir do
      files = files++elixir_files
    end
    case :zip.create 'mem', files, [:memory] do
      {:ok, {'mem', zip}} ->
        shebang = project[:escript_shebang] || "#! /usr/bin/env escript\n"
        comment = project[:escript_comment] || "%%\n"
        emu_args = project[:escript_emu_args] || "%%!\n"
        script = iolist_to_binary([shebang, comment, emu_args, zip])
        case :file.write_file(filename, script) do
          :ok -> :ok
          {:error, error} ->
            Mix.shell.error "Couldn't write #{filename}: #{error}"
        end
      {:error, error} -> Mix.shell.error "Error creating escript: #{error}"
    end
    set_perms(filename)
    Mix.Shell.info "Created escript #{filename}"
  end

  defp set_perms(filename) do
    {:ok, stat} = File.stat(filename)
    :ok = :file.change_mode(filename, stat.mode ||| 73)
  end

  defp elixir_files do
    {:file, e} = :code.is_loaded(:elixir)
    get_files(File.dirname(e))
  end

  defp gen_main([h|t]=name, nil) do
    gen_main(name, 'Elixir-'++[h-32|t])
  end
  defp gen_main(name, module) do
    forms = Enum.map [
      '-module(#{name}).',
      '-export([main/1]).',
      'main(Args) -> '++
      'case application:start(elixir) of '++
      'ok -> \'#{module}\':main(Args); '++
      '_ -> io:format("Elixir is not in the code path, aborting.\n") end.'],
      fn line ->
        {:ok, tokens, _} = :erl_scan.string line
        {:ok, forms} = :erl_parse.parse_form tokens
        forms
    end
    {:ok, _, bin} = :compile.forms forms
    {name++'.beam', bin}
  end

  defp get_files(d) do
    lc x inlist File.wildcard(File.join([d, "**"])), do: get_file x
  end

  defp get_file(f) do
    {:ok, bin} = File.read f
    {binary_to_list(:filename.basename(f)), bin}
  end

end
