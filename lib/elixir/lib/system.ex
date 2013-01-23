defmodule System do
  defexception NoHomeError,
    message: "could not find the user home, please set the HOME environment variable"

  defexception NoTmpDirError,
    message: "could not get a writable temporary directory, please set the TMPDIR environment variable"

  defexception NoAccessCwdError,
    message: "could not get a current working directory, the current location is not accessible"

  @moduledoc """
  The System module provides access to some variables used or
  maintained by the VM and to functions that interact strongly
  with the VM or the host system.
  """

  # Tries to run `git describe --always --tags`. In case of success
  # returns the most recent tag, otherwise returns an empty string.
  defmacrop get_describe do
    dotgit = Path.join(File.cwd!, ".git")
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
  def version, do: "0.8.0.dev"

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
    :gen_server.call(:elixir_code_server, :argv, System.services_timeout)
  end

  @doc """
  Returns the current working directory or nil if one
  is not available.
  """
  def cwd do
    case :file.get_cwd do
      { :ok, list } -> :unicode.characters_to_binary(list)
      _ -> nil
    end
  end

  @doc """
  Returns the current working directory or raises `System.NoAccessCwdError`.
  """
  def cwd! do
    cwd || raise NoAccessCwdError
  end

  @doc """
  Returns the user home (platform independent).
  It returns nil if no user home is set.
  """
  def user_home do
    get_unix_home || get_windows_home
  end

  @doc """
  Same as `user_home` but raises `System.NoHomeError`
  instead of returning nil if no user home is set.
  """
  def user_home! do
    user_home || raise NoHomeError
  end

  defp get_unix_home do
    get_env("HOME")
  end

  defp get_windows_home do
    get_env("USERPROFILE") || (
      hd = get_env("HOMEDRIVE")
      hp = get_env("HOMEPATH")
      hd && hp && hd <> hp
    )
  end

  @doc %B"""
  Returns a writable temporary directory.
  It searches for directories in the following order:

  1. The directory named by the TMPDIR environment variable
  2. The directory named by the TEMP environment variable
  3. The directory named by the TMP environment variable
  4. `C:\TMP` on Windows or `/tmp` on Unix
  5.  As a last resort, the current working directory

  Returns nil if none of the above are writable.
  """
  def tmp_dir do
    write_env_tmp_dir('TMPDIR') ||
      write_env_tmp_dir('TEMP')  ||
      write_env_tmp_dir('TMP') ||
      write_tmp_dir("/tmp")     ||
      ((cwd = cwd()) && write_tmp_dir(cwd))
  end

  @doc """
  Same as `tmp_dir` but raises `System.NoTmpDirError`
  instead of returning nil if no temp dir is set.
  """
  def tmp_dir! do
    tmp_dir || raise NoTmpDirError
  end

  defp write_env_tmp_dir(env) do
    case System.get_env(env) do
      nil -> nil
      tmp -> write_tmp_dir tmp
    end
  end

  defp write_tmp_dir(dir) do
    case File.stat(dir) do
      { :ok, File.Stat[type: :directory, access: access] } when access in [:read_write, :write] -> dir
      { :error, _ } -> nil
    end
  end

  @doc """
  Registers a function that will be invoked
  at the end of program execution. Useful for
  invoking a hook in a "script" mode.

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
  Gets Elixir's stacktrace.

  Notice the Erlang VM (and therefore this function) does not
  return the current stacktrace but rather the stacktrace of the
  latest exception.
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

  # services_timeout/0 is an internal function only to be used by Elixir.

  @doc false
  @spec services_timeout() :: non_neg_integer | :infinity
  def services_timeout() do
    :elixir_compiler.get_timeout
  end

  # Support R15B
  if List.member?(:erlang.module_info(:exports), { :halt, 2 }) do
    defp do_halt(status, options),  do: :erlang.halt(status, options)
  else
    IO.puts "Using limited halt support. Upgrade to R15B01 or later is recommended."
    defp do_halt(status, _options), do: :erlang.halt(status)
  end

  ## Helpers

  defp server_call(args) do
    :gen_server.call(:elixir_code_server, args, System.services_timeout)
  end

  defp filter_stacktrace([{ Kernel, :raise, _, _ }|t]), do: t
  defp filter_stacktrace(t), do: t
end
