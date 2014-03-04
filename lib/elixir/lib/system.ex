defmodule System do
  @moduledoc """
  The System module provides access to variables used or
  maintained by the VM and to functions that interact directly
  with the VM or the host system.
  """

  defp strip_re(iodata, pattern) do
    :re.replace(iodata, pattern, "", [return: :binary])
  end

  defp read_stripped(path) do
    case :file.read_file(path) do
      { :ok, binary } ->
        strip_re(binary, "^\s+|\s+$")
      _ -> ""
    end
  end

  # Read and strip the version from the `VERSION` file.
  defmacrop get_version do
    case read_stripped(:filename.join(__DIR__, "../../../VERSION")) do
      ""   -> raise RuntimeError, message: "could not read the version number from VERSION"
      data -> data
    end
  end

  # Tries to run `git describe --always --tags`. In the case of success returns
  # the most recent tag. If that is not available, tries to read the commit hash
  # from .git/HEAD. If that fails, returns an empty string.
  defmacrop get_describe do
    dirpath = :filename.join(__DIR__, "../../../.git")
    case :file.read_file_info(dirpath) do
      { :ok, _ } ->
        if :os.find_executable('git') do
          data = :os.cmd('git describe --always --tags')
          strip_re(data, "\n")
        else
          read_stripped(:filename.join(".git", "HEAD"))
        end
      _ -> ""
    end
  end

  # Get the date at compilation time.
  defmacrop get_date do
    iolist_to_binary :httpd_util.rfc1123_date
  end

  @doc """
  Elixir version information.

  Returns Elixir's version as binary.
  """
  @spec version() :: String.t
  def version, do: get_version

  @doc """
  Elixir build information.

  Returns a keyword list with Elixir version, git tag info and compilation date.
  """
  @spec build_info() :: Keyword.t
  def build_info do
    [version: version, tag: get_describe, date: get_date]
  end

  @doc """
  List command line arguments.

  Returns the list of command line arguments passed to the program.
  """
  @spec argv() :: [String.t]
  def argv do
    :elixir_code_server.call :argv
  end

  @doc """
  Modify command line arguments.

  Changes the list of command line arguments. Use it with caution,
  as it destroys any previous argv information.
  """
  @spec argv([String.t]) :: :ok
  def argv(args) do
    :elixir_code_server.cast({ :argv, args })
  end

  @doc """
  Current working directory.

  Returns the current working directory or `nil` if one
  is not available.
  """
  def cwd do
    case :file.get_cwd do
      { :ok, base } -> String.from_char_list!(base)
      _ -> nil
    end
  end

  @doc """
  Current working directory, exception on error.

  Returns the current working directory or raises `RuntimeError`.
  """
  def cwd! do
    cwd ||
      raise RuntimeError, message: "could not get a current working directory, the current location is not accessible"
  end

  @doc """
  User home directory.

  Returns the user home directory (platform independent).
  Returns `nil` if no user home is set.
  """
  def user_home do
    case :os.type() do
      { :win32, _ } -> get_windows_home
      _             -> get_unix_home
    end
  end

  @doc """
  User home directory, exception on error.

  Same as `user_home/0` but raises `RuntimeError`
  instead of returning `nil` if no user home is set.
  """
  def user_home! do
    user_home ||
      raise RuntimeError, message: "could not find the user home, please set the HOME environment variable"
  end

  defp get_unix_home do
    get_env("HOME")
  end

  defp get_windows_home do
    :filename.absname(
      get_env("USERPROFILE") || (
        hd = get_env("HOMEDRIVE")
        hp = get_env("HOMEPATH")
        hd && hp && hd <> hp
      )
    )
  end

  @doc ~S"""
  Writable temporary directory.

  Returns a writable temporary directory.
  Searches for directories in the following order:

  1. The directory named by the TMPDIR environment variable
  2. The directory named by the TEMP environment variable
  3. The directory named by the TMP environment variable
  4. `C:\TMP` on Windows or `/tmp` on Unix
  5.  As a last resort, the current working directory

  Returns `nil` if none of the above are writable.
  """
  def tmp_dir do
    write_env_tmp_dir('TMPDIR') ||
      write_env_tmp_dir('TEMP') ||
      write_env_tmp_dir('TMP')  ||
      write_tmp_dir('/tmp')     ||
      ((cwd = cwd()) && write_tmp_dir(cwd))
  end

  @doc """
  Writable temporary directory, exception on error.

  Same as `tmp_dir/0` but raises `RuntimeError`
  instead of returning `nil` if no temp dir is set.
  """
  def tmp_dir! do
    tmp_dir ||
      raise RuntimeError, message: "could not get a writable temporary directory, " <>
                                   "please set the TMPDIR environment variable"
  end

  defp write_env_tmp_dir(env) do
    case :os.getenv(env) do
      false -> nil
      tmp   -> write_tmp_dir(tmp)
    end
  end

  defp write_tmp_dir(dir) do
    case :file.read_file_info(dir) do
      {:ok, info} ->
        type_index = File.Stat.__record__(:index, :type)
        access_index = File.Stat.__record__(:index, :access)
        case { elem(info, type_index), elem(info, access_index) } do
          { :directory, access } when access in [:read_write, :write] ->
            String.from_char_list!(dir)
          _ ->
            nil
        end
      { :error, _ } -> nil
    end
  end

  @doc """
  Register a program exit handler function.

  Registers a function that will be invoked
  at the end of program execution. Useful for
  invoking a hook in "script" mode.

  The function must receive the exit status code
  as an argument.
  """
  def at_exit(fun) when is_function(fun, 1) do
    :elixir_code_server.cast { :at_exit, fun }
  end

  @doc """
  Execute a system command.

  Executes `command` in a command shell of the target OS,
  captures the standard output of the command and returns
  the result as a binary.

  If `command` is a char list, a char list is returned.
  Returns a binary otherwise.
  """
  @spec cmd(binary) :: binary
  @spec cmd(char_list) :: char_list

  def cmd(command) when is_list(command) do
    :os.cmd(command)
  end

  def cmd(command) when is_binary(command) do
    # Notice we don't use unicode for conversion
    # because the OS is expecting and returning raw bytes
    :binary.list_to_bin :os.cmd(:binary.bin_to_list(command))
  end

  @doc """
  Locate an executable on the system.

  This function looks up an executable program given
  its name using the environment variable PATH on Unix
  and Windows. It also considers the proper executable
  extension for each OS, so for Windows it will try to
  lookup files with `.com`, `.cmd` or similar extensions.

  If `program` is a char list, a char list is returned.
  Returns a binary otherwise.
  """
  @spec find_executable(binary) :: binary | nil
  @spec find_executable(char_list) :: char_list | nil

  def find_executable(program) when is_list(program) do
    :os.find_executable(program) || nil
  end

  def find_executable(program) when is_binary(program) do
    # Notice we don't use unicode for conversion
    # because the OS is expecting and returning raw bytes
    case :os.find_executable(:binary.bin_to_list(program)) do
      false -> nil
      other -> :binary.list_to_bin(other)
    end
  end

  @doc """
  System environment variables.

  Returns a list of all environment variables. Each variable is given as a
  `{name, value}` tuple where both `name` and `value` are strings.
  """
  @spec get_env() :: [{String.t, String.t}]
  def get_env do
    Enum.map(:os.getenv, fn var ->
        var = String.from_char_list! var
        [k, v] = String.split var, "=", global: false
        {k, v}
    end)
  end

  @doc """
  Environment variable value.

  Returns the value of the environment variable
  `varname` as a binary, or `nil` if the environment
  variable is undefined.
  """
  @spec get_env(binary) :: binary | nil
  def get_env(varname) when is_binary(varname) do
    case :os.getenv(String.to_char_list!(varname)) do
      false -> nil
      other -> String.from_char_list!(other)
    end
  end

  @doc """
  Erlang VM process identifier.

  Returns the process identifier of the current Erlang emulator
  in the format most commonly used by the operating system environment.

  See http://www.erlang.org/doc/man/os.html#getpid-0 for more info.
  """
  @spec get_pid() :: binary
  def get_pid, do: iolist_to_binary(:os.getpid)

  @doc """
  Set an environment variable value.

  Sets a new `value` for the environment variable `varname`.
  """
  @spec put_env(binary, binary) :: :ok
  def put_env(varname, value) when is_binary(varname) and is_binary(value) do
   :os.putenv :binary.bin_to_list(varname), String.to_char_list!(value)
   :ok
  end

  @doc """
  Set multiple environment variables.

  Sets a new value for each environment variable corresponding
  to each key in `dict`.
  """
  @spec put_env(Dict.t) :: :ok
  def put_env(dict) do
    Enum.each dict, fn {key, val} -> put_env key, val end
  end

  @doc """
  Deletes an environment variable.

  Removes the variable `varname` from the environment.
  """
  @spec delete_env(String.t) :: :ok
  def delete_env(varname) do
    :os.unsetenv(String.to_char_list!(varname))
    :ok
  end

  @doc """
  Last exception stacktrace.

  Note that the Erlang VM (and therefore this function) does not
  return the current stacktrace but rather the stacktrace of the
  latest exception.

  Inlined by the compiler into `:erlang.get_stacktrace/0`.
  """
  def stacktrace do
    :erlang.get_stacktrace
  end

  @doc """
  Halt the Erlang runtime system.

  Halts the Erlang runtime system where the argument `status` must be a
  non-negative integer, the atom `:abort` or a binary.

  * If an integer, the runtime system exits with the integer value which
    is returned to the operating system;

  * If `:abort`, the runtime system aborts producing a core dump, if that is
    enabled in the operating system;

  * If a char list, an erlang crash dump is produced with status as slogan,
    and then the runtime system exits with status code 1;

  Note that on many platforms, only the status codes 0-255 are supported
  by the operating system.

  For more information, check: http://www.erlang.org/doc/man/erlang.html#halt-1

  ## Examples

      System.halt(0)
      System.halt(1)
      System.halt(:abort)

  """
  @spec halt() :: no_return
  @spec halt(non_neg_integer | binary | :abort) :: no_return
  def halt(status \\ 0)

  def halt(status) when is_integer(status) or status == :abort do
    :erlang.halt(status)
  end

  def halt(status) when is_binary(status) do
    :erlang.halt(:binary.bin_to_list(status))
  end
end
