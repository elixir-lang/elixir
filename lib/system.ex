# Since Elixir does not (yet) support local macros,
# we need to wrap the step that extracts git information
# in this module.
defmodule System.GitCompiler do
  @moduledoc false

  defmacro generate do
    quote do
      @doc """
      Returns a tuple { Elixir version, commit sha-1, build date }.

      The format of the return value may change in a future release. Please
      make sure your code doesn't depend on it.
      """
      def build_info do
        { System.version,
          unquote(get_head_sha),
          unquote(get_date) }
      end
    end
  end

  @doc """
  Tries to run `git rev-parse HEAD`. In case of success returns the
  commit sha, otherwise returns an empty string.
  """
  defp get_head_sha do
    # The following failures are possible:
    #
    #  1) there is no `git` command
    #  2) pwd is not a git repository
    #
    command = 'git rev-parse HEAD'
    opts = [:stream, :exit_status, :use_stdio,
            :stderr_to_stdout, :in, :eof]
    port = :erlang.open_port {:spawn, command}, opts

    output = read_port port
    case output do
    match: { 0, data }
      Regex.replace_all %r/\n/, to_binary(data), ""
    else:
      ""
    end
  end

  defp read_port(port, data // []) do
    receive do
    match: {^port, {:data, new_data}}
      read_port port, [new_data|data]
    match: {^port, :eof}
      :erlang.port_close port
      receive do
      match: {^port, {:exit_status, exit_status}}
        {exit_status, List.reverse data}
      end
    end
  end

  defp get_date do
    list_to_binary :httpd_util.rfc1123_date
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
  output of the command and returns this result.

  `command` can a charlist or a binary.
  """
  def cmd(command) when is_binary(command) do
      list_to_binary cmd(binary_to_list(command))
  end

  def cmd(command), do: :os.cmd command

  @doc """
  Returns a list of all environment variables. Each environment variable is
  given as a single string of the format "VarName=Value", where VarName is the
  name of the variable and Value its value.
  """
  def get_env do
    Enum.map :os.getenv, list_to_binary &1
  end

  @doc """
  Returns the Value of the environment variable `varname`, or nil if the
  environment variable is undefined.
  """
  def get_env(varname) when is_binary(varname) do
    list_to_binary get_env(binary_to_list(varname))
  end

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
  def get_pid, do: list_to_binary(:os.getpid)

  @doc """
  Sets a new `value` for the environment variable `varname`.
  """
  def put_env(varname, value) when is_list(varname) and is_list(value) do
   :os.putenv varname, value
  end

  def put_env(varname, value) do
   :os.putenv to_char_list(varname), to_char_list(value)
  end

  @doc """
  Sets a new value for each environment variable corresponding to each key in
  `dict`.
  """
  def put_env(dict) do
    Enum.each dict, fn({key, val}) -> put_env key, val end
  end

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
