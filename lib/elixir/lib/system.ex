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
      {:ok, binary} ->
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
      {:ok, _} ->
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
    IO.iodata_to_binary :httpd_util.rfc1123_date
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
  @spec build_info() :: map
  def build_info do
    %{version: version, tag: get_describe, date: get_date}
  end

  @doc """
  Lists command line arguments.

  Returns the list of command line arguments passed to the program.
  """
  @spec argv() :: [String.t]
  def argv do
    :elixir_config.get(:argv)
  end

  @doc """
  Modifies command line arguments.

  Changes the list of command line arguments. Use it with caution,
  as it destroys any previous argv information.
  """
  @spec argv([String.t]) :: :ok
  def argv(args) do
    :elixir_config.put(:argv, args)
  end

  @doc """
  Current working directory.

  Returns the current working directory or `nil` if one
  is not available.
  """
  def cwd do
    case :file.get_cwd do
      {:ok, base} -> IO.chardata_to_string(fix_drive_letter(base))
      _ -> nil
    end
  end

  defp fix_drive_letter([l, ?:, ?/ | rest] = original) when l in ?A..?Z do
    case :os.type() do
      {:win32, _} -> [l+?a-?A, ?:, ?/ | rest]
      _ -> original
    end
  end

  defp fix_drive_letter(original), do: original

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
      {:win32, _} -> get_windows_home
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

    1. the directory named by the TMPDIR environment variable
    2. the directory named by the TEMP environment variable
    3. the directory named by the TMP environment variable
    4. `C:\TMP` on Windows or `/tmp` on Unix
    5. as a last resort, the current working directory

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
    case File.stat(dir) do
      {:ok, stat} ->
        case {stat.type, stat.access} do
          {:directory, access} when access in [:read_write, :write] ->
            IO.chardata_to_string(dir)
          _ ->
            nil
        end
      {:error, _} -> nil
    end
  end

  @doc """
  Registers a program exit handler function.

  Registers a function that will be invoked at the end of program execution.
  Useful for invoking a hook in "script" mode.

  The handler always executes in a different process from the one it was
  registered in. As a consequence, any resources managed by the calling process
  (ETS tables, open files, etc.) won't be available by the time the handler
  function is invoked.

  The function must receive the exit status code as an argument.
  """
  def at_exit(fun) when is_function(fun, 1) do
    :elixir_config.update :at_exit, &[fun|&1]
  end

  @doc """
  Locates an executable on the system.

  This function looks up an executable program given
  its name using the environment variable PATH on Unix
  and Windows. It also considers the proper executable
  extension for each OS, so for Windows it will try to
  lookup files with `.com`, `.cmd` or similar extensions.
  """
  @spec find_executable(binary) :: binary | nil
  def find_executable(program) when is_binary(program) do
    case :os.find_executable(String.to_char_list(program)) do
      false -> nil
      other -> List.to_string(other)
    end
  end

  @doc """
  System environment variables.

  Returns a list of all environment variables. Each variable is given as a
  `{name, value}` tuple where both `name` and `value` are strings.
  """
  @spec get_env() :: %{String.t => String.t}
  def get_env do
    Enum.into(:os.getenv, %{}, fn var ->
      var = IO.chardata_to_string var
      [k, v] = String.split var, "=", parts: 2
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
    case :os.getenv(String.to_char_list(varname)) do
      false -> nil
      other -> List.to_string(other)
    end
  end

  @doc """
  Erlang VM process identifier.

  Returns the process identifier of the current Erlang emulator
  in the format most commonly used by the operating system environment.

  See http://www.erlang.org/doc/man/os.html#getpid-0 for more info.
  """
  @spec get_pid() :: binary
  def get_pid, do: IO.iodata_to_binary(:os.getpid)

  @doc """
  Sets an environment variable value.

  Sets a new `value` for the environment variable `varname`.
  """
  @spec put_env(binary, binary) :: :ok
  def put_env(varname, value) when is_binary(varname) and is_binary(value) do
   :os.putenv String.to_char_list(varname), String.to_char_list(value)
   :ok
  end

  @doc """
  Sets multiple environment variables.

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
    :os.unsetenv(String.to_char_list(varname))
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
  Halts the Erlang runtime system.

  Halts the Erlang runtime system where the argument `status` must be a
  non-negative integer, the atom `:abort` or a binary.

    * If an integer, the runtime system exits with the integer value which
      is returned to the operating system.

    * If `:abort`, the runtime system aborts producing a core dump, if that is
      enabled in the operating system.

    * If a string, an erlang crash dump is produced with status as slogan,
      and then the runtime system exits with status code 1.

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
    :erlang.halt(String.to_char_list(status))
  end

  @doc ~S"""
  Executes the given `command` with `args`.

  `command` is expected to be an executable available in PATH
  unless an absolute path is given.

  `args` must be a list of strings which are not expanded
  in any way. For example, this means wildcard expansion will
  not happen unless `Path.wildcard/2` is used. On Windows though,
  wildcard expansion is up to the program.

  This function returns a tuple containing the collected result
  and the command exit status.

  ## Examples
  
      iex> System.cmd "echo", ["hello"]
      {"hello\n", 0}

      iex> System.cmd "echo", ["hello"], env: [{"MIX_ENV", "test"}]
      {"hello\n", 0}
      
      iex> System.cmd "echo", ["hello"], into: IO.stream(:stdio, :line)
      hello
      {%IO.Stream{}, 0}

  ## Options

    * `:into` - injects the result into the given collectable, defaults to `""`
    * `:cd` - the directory to run the command in
    * `:env` - an enumerable of tuples containing environment key-value as binary
    * `:arg0` - set the command arg0
    * `:stderr_to_stdout` - redirects stderr to stdout when `true`
    * `:parallelism` - when `true`, the VM will schedule port tasks to improve
      parallelism in the system. If set to `false`, the VM will try to perform
      commands immediately, improving latency at the expense of parallelism.
      The default can be set on system startup by passing the "+spp" argument
      to `--erl`.

  ## Error reasons

  If invalid arguments are given, `ArgumentError` is raised by
  `System.cmd/3`. `System.cmd/3` also expects a strict set of
  options and will raise if unknown or invalid options are given.

  Furthermore, `System.cmd/3` may fail with one of the POSIX reasons
  detailed below:

    * `:system_limit` - all available ports in the Erlang emulator are in use

    * `:enomem` - there was not enough memory to create the port

    * `:eagain` - there are no more available operating system processes

    * `:enametoolong` - the external command given was too long

    * `:emfile` - there are no more available file descriptors
      (for the operating system process that the Erlang emulator runs in)

    * `:enfile` - the file table is full (for the entire operating system)

    * `:eacces` - the command does not point to an executable file

    * `:enoent` - the command does not point to an existing file

  ## Shell commands

  If you desire to execute a trusted command inside a shell, with pipes,
  redirecting and so on, please check
  [Erlang's :os.cmd/1 function](http://www.erlang.org/doc/man/os.html#cmd-1).
  """
  @spec cmd(binary, [binary], Keyword.t) ::
        {Collectable.t, exit_status :: non_neg_integer}
  def cmd(command, args, opts \\ []) when is_binary(command) and is_list(args) do
    cmd = String.to_char_list(command)

    cmd =
      if Path.type(cmd) == :absolute do
        cmd
      else
        :os.find_executable(cmd) || :erlang.error(:enoent, [command, args, opts])
      end

    {into, opts} = cmd_opts(opts, [:use_stdio, :exit_status, :binary, :hide, args: args], "")
    {initial, fun} = Collectable.into(into)
    do_cmd Port.open({:spawn_executable, cmd}, opts), initial, fun
  end

  defp do_cmd(port, acc, fun) do
    receive do
      {^port, {:data, data}} ->
        do_cmd(port, fun.(acc, {:cont, data}), fun)
      {^port, {:exit_status, status}} ->
        {fun.(acc, :done), status}
    end
  end

  defp cmd_opts([{:into, any}|t], opts, _into),
    do: cmd_opts(t, opts, any)

  defp cmd_opts([{:cd, bin}|t], opts, into) when is_binary(bin),
    do: cmd_opts(t, [{:cd, bin}|opts], into)

  defp cmd_opts([{:arg0, bin}|t], opts, into) when is_binary(bin),
    do: cmd_opts(t, [{:arg0, bin}|opts], into)

  defp cmd_opts([{:stderr_to_stdout, true}|t], opts, into),
    do: cmd_opts(t, [:stderr_to_stdout|opts], into)

  defp cmd_opts([{:stderr_to_stdout, false}|t], opts, into),
    do: cmd_opts(t, opts, into)

  defp cmd_opts([{:parallelism, bool}|t], opts, into) when is_boolean(bool),
    do: cmd_opts(t, [{:parallelism, bool}|opts], into)

  defp cmd_opts([{:env, enum}|t], opts, into),
    do: cmd_opts(t, [{:env, validate_env(enum)}|opts], into)

  defp cmd_opts([{key, val}|_], _opts, _into),
    do: raise(ArgumentError, "invalid option #{inspect key} with value #{inspect val}")

  defp cmd_opts([], opts, into),
    do: {into, opts}

  defp validate_env(enum) do
    Enum.map enum, fn
      {k, v} ->
        {String.to_char_list(k), String.to_char_list(v)}
      other ->
        raise ArgumentError, "invalid environment key-value #{inspect other}"
    end
  end
end
