defmodule System do
  @moduledoc """
  The System module provides access to some variables used or
  maintained by the VM and to functions that interact strongly
  with the VM or the host system.
  """

  # Tries to run `git rev-parse HEAD`. In case of success
  # returns the commit sha, otherwise returns an empty string.
  defmacrop get_head_sha do
    if :os.find_executable('git') do
      data = :os.cmd('git rev-parse HEAD')
      Regex.replace_all %r/\n/, to_binary(data), ""
    else
      ""
    end
  end

  # Get the date at compilation time.
  defmacrop get_date do
    list_to_binary :httpd_util.rfc1123_date
  end

  @doc """
  Returns Elixir's version as binary.
  """
  def version, do: "0.6.0"

  @doc """
  Returns a tuple { Elixir version, commit sha-1, build date }.

  The format of the return value may change in a future release. Please
  make sure your code doesn't depend on it.
  """
  def build_info do
    { version, get_head_sha, get_date }
  end

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
  Executes `command` in a command shell of the target OS,
  captures the standard output of the command and returns
  the result as a binary.

  If `command` is a char list, a char list is returned.
  Returns a binary otherwise.
  """
  def cmd(command) when is_list(command) do
    :os.cmd(command)
  end

  def cmd(command) do
    list_to_binary :os.cmd(to_char_list(command))
  end

  @doc """
  This functions looks up an executable program given
  its name using the environment variable PATH on Unix
  and Windows.

  If `command` is a char list, a char list is returned.
  Returns a binary otherwise.
  """
  def find_executable(command) when is_list(command) do
    :os.find_executable(command) || nil
  end

  def find_executable(command) do
    case :os.find_executable(to_char_list(command)) do
      false -> nil
      other -> list_to_binary(other)
    end
  end

  @doc """
  Returns a list of all environment variables. Each environment variable is
  given as a single string of the format "VarName=Value", where VarName is the
  name of the variable and Value its value.
  """
  def get_env do
    Enum.map :os.getenv, list_to_binary &1
  end

  @doc """
  Returns the value of the environment variable
  `varname` as a binary, or nil if the environment
  variable is undefined.
  """
  def get_env(varname) do
    case :os.getenv(to_char_list(varname)) do
      false -> nil
      other -> list_to_binary(other)
    end
  end

  @doc """
  Returns the process identifier of the current Erlang emulator
  in the format most commonly used by the operating system environment.

  See http://www.erlang.org/doc/man/os.html#getpid-0 for more info.
  """
  def get_pid, do: list_to_binary(:os.getpid)

  @doc """
  Sets a new `value` for the environment variable `varname`.
  """
  def put_env(varname, value) do
   :os.putenv to_char_list(varname), to_char_list(value)
  end

  @doc """
  Sets a new value for each environment variable corresponding
  to each key in `dict`.
  """
  def put_env(dict) do
    Enum.each dict, fn {key, val} -> put_env key, val end
  end

  @doc """
  Get the stacktrace.
  """
  def stacktrace do
    filter_stacktrace Erlang.erlang.get_stacktrace
  end

  ## Helpers

  # Filter stacktrace by removing internal BOOTSTRAP calls.
  defp filter_stacktrace([{ Kernel, :raise, _, _ }|t]), do: filter_stacktrace(t)
  defp filter_stacktrace([{ _mod, :BOOTSTRAP, _, info }|t]),
    do: filter_stacktrace([{ Kernel, :defmodule, 2, info }|t])
  defp filter_stacktrace([h|t]), do: [h|filter_stacktrace(t)]
  defp filter_stacktrace([]), do: []

  defp server_call(args) do
    Erlang.gen_server.call(:elixir_code_server, args)
  end
end
