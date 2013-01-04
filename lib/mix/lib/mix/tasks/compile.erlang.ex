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

  * `:erlangrc_options` - compilation options that applies
     to Erlangs' compiler, they are: `[:verbose,:report_errors,:report_warnings]`
     by default.
  """
  def run(_) do
    erlangrc_options = Mix.project[:erlangrc_options] || []

    # expands path for 'include' option
    erlangrc_options = Enum.map erlangrc_options, fn(opt) ->
      case opt do
        {:i, dir} -> {:i, File.expand_path(dir) /> binary_to_list}
        _         -> opt # as is
      end
    end

    # puts 'outdir' option
    compile_path = Mix.project[:compile_path] /> File.expand_path /> binary_to_list
    File.mkdir_p!(compile_path)

    # compiles erlang's files
    files = Mix.Utils.extract_files(Mix.project[:source_paths], [:erl])
    erlangrc_options = erlangrc_options ++ [{:outdir, compile_path}]

    Enum.each files, fn(file) ->
      file = String.replace(file, ".erl", "") /> File.expand_path /> binary_to_list
      case :compile.file(file, erlangrc_options) do
        {:ok, _} -> Mix.shell.info  "Compiled #{file}.erl"
        :error   -> Mix.shell.error "== Compilation error on file #{file}.erl"
      end
    end
  end

end
