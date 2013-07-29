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
    case read_stripped("VERSION") do
      ""   -> raise CompileError, message: "could not read the version number from VERSION"
      data -> data
    end
  end

  # Tries to run `git describe --always --tags`. In the case of success returns
  # the most recent tag. If that is not available, tries to read the commit hash
  # from .git/HEAD. If that fails, returns an empty string.
  defmacrop get_describe do
    dirpath = ".git"
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
    list_to_binary :httpd_util.rfc1123_date
  end

  @doc """
  Returns Elixir's version as binary.
  """
  @spec version() :: String.t
  def version, do: get_version

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
    :elixir_code_server.call :argv
  end

  @doc """
  Changes the list of command-line arguments. Use it with caution,
  as it destory any previous argv information.
  """
  @spec argv([String.t]) :: :ok
  def argv(argv) do
    :elixir_code_server.cast({ :argv, argv })
  end

  @doc """
  Returns the current working directory or `nil` if one
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
  It returns `nil` if no user home is set.
  """
  def user_home do
    case :os.type() do
      { :win32, _ } -> get_windows_home
      _             -> get_unix_home
    end
  end

  @doc """
  Same as `user_home/0` but raises `System.NoHomeError`
  instead of returning `nil` if no user home is set.
  """
  def user_home! do
    user_home || raise NoHomeError
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

  @doc %B"""
  Returns a writable temporary directory.
  It searches for directories in the following order:

  1. The directory named by the TMPDIR environment variable
  2. The directory named by the TEMP environment variable
  3. The directory named by the TMP environment variable
  4. `C:\TMP` on Windows or `/tmp` on Unix
  5.  As a last resort, the current working directory

  Returns `nil` if none of the above are writable.
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
  instead of returning `nil` if no temp dir is set.
  """
  def tmp_dir! do
    tmp_dir || raise NoTmpDirError
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
            :unicode.characters_to_binary(dir)
          _ -> nil
        end
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
    :elixir_code_server.cast { :at_exit, fun }
  end

  @doc """
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
    list_to_binary :os.cmd(binary_to_list(command))
  end

  @doc """
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
    case :os.find_executable(binary_to_list(program)) do
      false -> nil
      other -> list_to_binary(other)
    end
  end

  @doc """
  Returns a list of all environment variables. Each environment variable is
  given as a single string of the format "VarName=Value", where VarName is the
  name of the variable and Value its value.
  """
  @spec get_env() :: [{binary, binary}]
  def get_env do
    Enum.map :os.getenv, :unicode.characters_to_binary(&1)
  end

  @doc """
  Returns the value of the environment variable
  `varname` as a binary, or `nil` if the environment
  variable is undefined.
  """
  @spec get_env(binary) :: binary | nil
  def get_env(varname) when is_binary(varname) do
    case :os.getenv(:unicode.characters_to_list(varname)) do
      false -> nil
      other -> :unicode.characters_to_binary(other)
    end
  end

  @doc """
  Returns the process identifier of the current Erlang emulator
  in the format most commonly used by the operating system environment.

  See http://www.erlang.org/doc/man/os.html#getpid-0 for more info.
  """
  @spec get_pid() :: binary
  def get_pid, do: list_to_binary(:os.getpid)

  @doc """
  Sets a new `value` for the environment variable `varname`.
  """
  @spec put_env(binary, binary) :: :ok
  def put_env(varname, value) when is_binary(varname) and is_binary(value) do
   :os.putenv binary_to_list(varname), :unicode.characters_to_list(value)
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

  For more information, check: http://www.erlang.org/doc/man/erlang.html#halt-1

  ## Examples

      System.halt(0)
      System.halt(1)
      System.halt(:abort)

  """
  @spec halt() :: no_return
  @spec halt(non_neg_integer | binary | :abort) :: no_return
  def halt(status // 0)

  def halt(status) when is_integer(status) or status == :abort do
    :erlang.halt(status)
  end

  def halt(status) when is_binary(status) do
    :erlang.halt(binary_to_list(status))
  end

  ## Helpers

  defp filter_stacktrace([{ Kernel, :raise, _, _ }|t]), do: t
  defp filter_stacktrace(t), do: t
end
