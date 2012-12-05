defmodule System do
  @moduledoc """
  The System module provides access to some variables used or
  maintained by the VM and to functions that interact strongly
  with the VM or the host system.
  """

  # Tries to run `git describe --always --tags`. In case of success
  # returns the most recent tag, otherwise returns an empty string.
  defmacrop get_describe do
    dotgit = File.join(File.cwd!, ".git")
    if :os.find_executable('git') && File.exists?(dotgit) do
      data = :os.cmd('git describe --always --tags')
      Regex.replace %r/\n/, to_binary(data), ""
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
  @spec version() :: String.t
  def version, do: "0.7.3.dev"

  @doc """
  Returns a keywords list with version, git tag info and date.
  """
  @spec build_info() :: Keyword.t
  def build_info do
    [version: version, tag: get_describe, date: get_date]
  end

  @doc """
  Returns the list of command-line arguments passed to the program.
  """
  @spec argv() :: [String.t]
  def argv do
    :gen_server.call(:elixir_code_server, :argv)
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
  @spec cmd(char_list) :: char_list
  @spec cmd(String.t) :: String.t
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
  @spec find_executable(char_list) :: char_list | nil
  @spec find_executable(String.t) :: String.t | nil
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
  @spec get_env() :: [{String.t, String.t}]
  def get_env do
    Enum.map :os.getenv, :unicode.characters_to_binary &1
  end

  @doc """
  Returns the value of the environment variable
  `varname` as a binary, or nil if the environment
  variable is undefined.
  """
  @spec get_env(String.t) :: String.t | nil
  def get_env(varname) do
    case :os.getenv(to_char_list(varname)) do
      false -> nil
      other -> :unicode.characters_to_binary(other)
    end
  end

  @doc """
  Returns the process identifier of the current Erlang emulator
  in the format most commonly used by the operating system environment.

  See http://www.erlang.org/doc/man/os.html#getpid-0 for more info.
  """
  @spec get_pid() :: String.t
  def get_pid, do: list_to_binary(:os.getpid)

  @doc """
  Sets a new `value` for the environment variable `varname`.
  """
  @spec put_env(String.t, String.t | char_list) :: :ok
  def put_env(varname, value) when is_binary(value) or is_list(value) do
   :os.putenv to_char_list(varname), :unicode.characters_to_list(value)
   :ok
  end

  @doc """
  Sets a new value for each environment variable corresponding
  to each key in `dict`.
  """
  @spec put_env(Dict.t) :: :ok
  def put_env(dict) do
    Enum.each dict, fn {key, val} -> put_env key, val end
  end

  @doc """
  Get the stacktrace.
  """
  def stacktrace do
    filter_stacktrace :erlang.get_stacktrace
  end

  @doc """
  Halts the Erlang runtime system where the first argument status must be a
  non-negative integer, the atom `:abort` or any type that can be converted
  to a char list.

  * If an integer, the runtime system exits with the integer value which
    is returned to the Operating System;

  * If `:abort`, the runtime system aborts producing a core dump, if that is
    enabled in the operating system;

  * If a char list, an erlang crash dump is produced with status as slogan,
    and then the runtime system exits with status code 1;

  Note that on many platforms, only the status codes 0-255 are supported
  by the operating system.

  For integer status, Erlang runtime system closes all ports and allows async
  threads to finish their operations before exiting. To exit without such
  flushing, pass options [flush: false] instead.

  For more information, check: http://www.erlang.org/doc/man/erlang.html#halt-2

  ## Examples

      System.halt(0)
      System.halt(1, flush: false)
      System.halt(:abort)

  """
  @spec halt() :: no_return
  @spec halt(non_neg_integer | List.Chars.t | :abort) :: no_return
  @spec halt(non_neg_integer | List.Chars.t | :abort, [] | [flush: false]) :: no_return
  def halt(status // 0, options // [])

  def halt(status, options) when is_integer(status) or status == :abort do
    do_halt(status, options)
  end

  def halt(status, options) do
    do_halt(to_char_list(status), options)
  end

  # Support R15B
  if List.member?(:erlang.module_info(:exports), { :halt, 2 }) do
    defp do_halt(status, options),  do: :erlang.halt(status, options)
  else
    IO.puts "Using limited halt support. Upgrade to R15B01 or later is recommended."
    defp do_halt(status, _options), do: :erlang.halt(status)
  end

  ## Helpers

  # Filter stacktrace by removing internal BOOTSTRAP calls.
  defp filter_stacktrace([{ Kernel, :raise, _, _ }|t]), do: filter_stacktrace(t)
  defp filter_stacktrace([{ _mod, :BOOTSTRAP, _, info }|t]),
    do: filter_stacktrace([{ Kernel, :defmodule, 2, info }|t])
  defp filter_stacktrace([h|t]), do: [h|filter_stacktrace(t)]
  defp filter_stacktrace([]), do: []

  defp server_call(args) do
    :gen_server.call(:elixir_code_server, args)
  end
end
