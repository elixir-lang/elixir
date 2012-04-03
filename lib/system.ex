# Since Elixir does not (yet) support local macros,
# we need to wrap the step that extracts git information
# in this module.
defmodule System.GitCompiler do
  @moduledoc false

  defmacro generate do
    quote do
      @doc """
      Returns a tuple { Elixir version, commit sha-1, build date }
      """
      def build_version do
        { System.version,
          unquote(get_head_sha),
          unquote(get_date) }
      end
    end
  end

  defp get_head_sha do
    normalize :os.cmd 'git rev-parse HEAD'
  end

  defp get_date do
    normalize :os.cmd 'TZ=GMT date'
  end

  defp normalize(string) do
    Regex.replace_all %r(\n), to_binary(string), ""
  end
end

defmodule System do
  @moduledoc """
  The System module provides access to some variables used or
  maintained by the VM and to functions that interact strongly
  with the VM or the host system.
  """

  require System.GitCompiler
  System.GitCompiler.generate

  @doc """
  Returns Elixir's version as binary.
  """
  def version, do: "0.9.0.dev"

  @doc """
  Returns the list of command-line arguments passed to the program.
  """
  def argv do
    Erlang.gen_server.call(:elixir_code_server, :argv)
  end

  @doc """
  Registers a function that will be invoked
  at the end of program execution. Useful for
  invoking a hook on scripted mode.

  The function must expect the exit status code
  as argument.
  """
  def at_exit(fun) when is_function(fun, 1) do
    server_call { :at_exit, fun }
  end

  @doc """
  Get the stacktrace.
  """
  def stacktrace do
    filter_stacktrace Erlang.erlang.get_stacktrace
  end

  ## Helpers

  # Filter stacktrace by removing internal BOOTSTRAP calls.
  defp filter_stacktrace([{ Elixir.Builtin, :raise, _, _ }|t]), do: filter_stacktrace(t)
  defp filter_stacktrace([{ _mod, :BOOTSTRAP, _, _ }|t]), do: filter_stacktrace(t)
  defp filter_stacktrace([{ _mod, :BOOTSTRAP, _ }|t]), do: filter_stacktrace(t)
  defp filter_stacktrace([h|t]), do: [h|filter_stacktrace(t)]
  defp filter_stacktrace([]), do: []

  defp server_call(args) do
    Erlang.gen_server.call(:elixir_code_server, args)
  end
end
