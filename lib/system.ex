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
      def build_info do
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
  Executes `command` in a command shell of the target OS, captures the standard
  output of the command and returns this result as a string.

  `command` can be an atom or a charlist
  """
  def cmd(command), do: :os.cmd command

  @doc """
  Returns a list of all environment variables. Each environment variable is
  given as a single string on the format "VarName=Value", where VarName is the
  name of the variable and Value its value.
  """
  def get_env, do: :os.getenv

  @doc """
  Returns the Value of the environment variable `varname`, or nil if the
  environment variable is undefined.
  """
  def get_env(varname) do
    case :os.getenv(varname) do
    match: false
      nil
    match: other
      other
    end
  end

  @doc """
  Returns the process identifier of the current Erlang emulator in the format
  most commonly used by the operating system environment.

  See http://www.erlang.org/doc/man/os.html#getpid-0 for more info.
  """
  def get_pid, do: :os.getpid

  @doc """
  Sets a new `value` for the environment variable `varname`. Both arguments are
  expected to be of type charlist.
  """
  def put_env(varname, value), do: :os.putenv varname, value

  @doc """
  Returns the current working directory as a binary.
  """
  def pwd, do: :filename.absname("")

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
