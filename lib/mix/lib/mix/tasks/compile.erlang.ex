defmodule Mix.Tasks.Compile.Erlang do
  use Mix.Task

  @hidden true
  @shortdoc "Compile Erlang source files"

  @moduledoc """
  A task to compile Erlang source files.

  ## Command line options

  * `ERL_COMPILER_OPTIONS` - can be used to give default compile options.
     It's value must be a valid Erlang term. If the value is a list, it will
     be used as is. If it is not a list, it will be put into a list.

  ## Configuration

  * `:erlc_options` - compilation options that applies to Erlang compiler
     By default, the following options are on: `[:verbose, :report_errors, :report_warnings]`

  """
  def run(_) do
    project = Mix.project

    files        = Mix.Utils.extract_files(project[:erlc_paths], [:erl])
    compile_path = project[:compile_path]

    if files == [] do
      :noop
    else
      File.mkdir_p! compile_path
      compile_files project, files, compile_path
      :ok
    end
  end

  defp compile_files(project, files, compile_path) do
    erlc_options = project[:erlc_options] || []

    erlc_options = Enum.map erlc_options, fn(opt) ->
      case opt do
        { :i, dir } -> { :i, Path.expand(dir) |> binary_to_list }
        _           -> opt
      end
    end

    compile_path = compile_path |> Path.expand |> binary_to_list
    erlc_options = [{:outdir, compile_path}] ++ erlc_options
    File.mkdir_p!(compile_path)

    Enum.each files, fn(file) ->
      file = Path.rootname(file, ".erl") |> Path.expand |> binary_to_list

      case :compile.file(file, erlc_options) do
        { :ok, _ } -> Mix.shell.info  "Compiled #{file}.erl"
        :error     -> Mix.shell.error "== Compilation error on file #{file}.erl =="
      end
    end
  end
end
