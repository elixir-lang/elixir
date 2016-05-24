defmodule System do
  @moduledoc """
  The `System` module provides functions that interact directly
  with the VM or the host system.

  ## Time

  The `System` module also provides functions that work with time,
  returning different times kept by the system with support for
  different time units.

  One of the complexities in relying on system times is that they
  may be adjusted. For example, when you enter and leave daylight
  saving time, the system clock will be adjusted, often adding
  or removing one hour. We call such changes "time warps". In
  order to understand how such changes may be harmful, imagine
  the following code:

      ## DO NOT DO THIS
      prev = System.os_time()
      # ... execute some code ...
      next = System.os_time()
      diff = next - prev

  If, while the code is executing, the system clock changes,
  some code that executed in 1 second may be reported as taking
  over 1 hour! To address such concerns, the VM provides a
  monotonic time via `System.monotonic_time/0` which never
  decreases and does not leap:

      ## DO THIS
      prev = System.monotonic_time()
      # ... execute some code ...
      next = System.monotonic_time()
      diff = next - prev


  Generally speaking, the VM provides three time measurements:

    * `os_time/0` - the time reported by the OS. This time may be
      adjusted forwards or backwards in time with no limitation;

    * `system_time/0` - the VM view of the `os_time/0`. The system time and OS
      time may not match in case of time warps although the VM works towards
      aligning them. This time is not monotonic (i.e., it may decrease)
      as its behaviour is configured [by the VM time warp
      mode](http://erlang.org/doc/apps/erts/time_correction.html#Time_Warp_Modes);

    * `monotonic_time/0` - a monotonically increasing time provided
      by the Erlang VM.

  The time functions in this module work in the `:native` unit
  (unless specified otherwise), which is OS dependent. Most of
  the time, all calculations are done in the `:native` unit, to
  avoid loss of precision, with `convert_time_unit/3` being
  invoked at the end to convert to a specific time unit like
  milliseconds or microseconds. See the `t:time_unit/0` type for
  more information.

  For a more complete rundown on the VM support for different
  times, see the [chapter on time and time
  correction](http://www.erlang.org/doc/apps/erts/time_correction.html)
  in the Erlang docs.
  """

  @typedoc """
  The time unit to be passed to functions like `monotonic_time/1` and others.

  The `:seconds`, `:milliseconds`, `:microseconds` and `:nanoseconds` time
  units controls the return value of the functions that accept a time unit.

  A time unit can also be a strictly positive integer. In this case, it
  represents the "parts per second": the time will be returned in `1 /
  parts_per_second` seconds. For example, using the `:milliseconds` time unit
  is equivalent to using `1000` as the time unit (as the time will be returned
  in 1/1000 seconds - milliseconds).

  Keep in mind the Erlang API will use `:milli_seconds`, `:micro_seconds`
  and `:nano_seconds` as time units although Elixir normalizes their spelling
  to match the SI convention.
  """
  @type time_unit ::
    :seconds
    | :milliseconds
    | :microseconds
    | :nanoseconds
    | pos_integer

  @base_dir     :filename.join(__DIR__, "../../..")
  @version_file :filename.join(@base_dir, "VERSION")

  defp strip(iodata) do
    :re.replace(iodata, "^[\s\r\n\t]+|[\s\r\n\t]+$", "", [:global, return: :binary])
  end

  defp read_stripped(path) do
    case :file.read_file(path) do
      {:ok, binary} ->
        strip(binary)
      _ ->
        ""
    end
  end

  # Read and strip the version from the VERSION file.
  defmacrop get_version do
    case read_stripped(@version_file) do
      ""   -> raise RuntimeError, message: "could not read the version number from VERSION"
      data -> data
    end
  end

  # Tries to run "git rev-parse --short HEAD". In the case of success returns
  # the short revision hash. If that fails, returns an empty string.
  defmacrop get_revision do
    :os.cmd('git rev-parse --short HEAD 2> /dev/null')
    |> strip
  end

  defp revision, do: get_revision

  # Get the date at compilation time.
  defmacrop get_date do
    IO.iodata_to_binary :httpd_util.rfc1123_date
  end

  @doc """
  Returns the endianness.
  """
  def endianness do
    :erlang.system_info(:endian)
  end

  @doc """
  Returns the endianness the system was compiled with.
  """
  @endianness :erlang.system_info(:endian)
  def compiled_endianness do
    @endianness
  end

  @doc """
  Elixir version information.

  Returns Elixir's version as binary.
  """
  @spec version() :: String.t
  def version, do: get_version

  @doc """
  Elixir build information.

  Returns a keyword list with Elixir version, git short revision hash and compilation date.
  """
  @spec build_info() :: map
  def build_info do
    %{build:    build,
      date:     get_date,
      revision: revision,
      version:  version,
      }
  end

  # Returns a string of the build info
  defp build do
    {:ok, v} = Version.parse(version)

    cond do
      ([] == v.pre) or ("" == revision) ->
        version
      true ->
        "#{version} (#{revision})"
    end
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
  """
  def user_home do
    :elixir_config.get(:home)
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
    :elixir_config.update :at_exit, &[fun | &1]
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
    case :os.find_executable(String.to_charlist(program)) do
      false -> nil
      other -> List.to_string(other)
    end
  end

  @doc """
  System environment variables.

  Returns a list of all environment variables. Each variable is given as a
  `{name, value}` tuple where both `name` and `value` are strings.
  """
  @spec get_env() :: %{optional(String.t) => String.t}
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
    case :os.getenv(String.to_charlist(varname)) do
      false -> nil
      other -> List.to_string(other)
    end
  end

  @doc """
  Erlang VM process identifier.

  Returns the process identifier of the current Erlang emulator
  in the format most commonly used by the operating system environment.

  For more information, see [`:os.getpid/0`](http://www.erlang.org/doc/man/os.html#getpid-0).
  """
  @spec get_pid() :: binary
  def get_pid, do: IO.iodata_to_binary(:os.getpid)

  @doc """
  Sets an environment variable value.

  Sets a new `value` for the environment variable `varname`.
  """
  @spec put_env(binary, binary) :: :ok
  def put_env(varname, value) when is_binary(varname) and is_binary(value) do
   :os.putenv String.to_charlist(varname), String.to_charlist(value)
   :ok
  end

  @doc """
  Sets multiple environment variables.

  Sets a new value for each environment variable corresponding
  to each key in `dict`.
  """
  @spec put_env(Enumerable.t) :: :ok
  def put_env(enum) do
    Enum.each enum, fn {key, val} -> put_env key, val end
  end

  @doc """
  Deletes an environment variable.

  Removes the variable `varname` from the environment.
  """
  @spec delete_env(String.t) :: :ok
  def delete_env(varname) do
    :os.unsetenv(String.to_charlist(varname))
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

    * If a string, an Erlang crash dump is produced with status as slogan,
      and then the runtime system exits with status code 1.

  Note that on many platforms, only the status codes 0-255 are supported
  by the operating system.

  For more information, see [`:erlang.halt/1`](http://www.erlang.org/doc/man/erlang.html#halt-1).

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
    :erlang.halt(String.to_charlist(status))
  end

  @doc ~S"""
  Executes the given `command` with `args`.

  `command` is expected to be an executable available in PATH
  unless an absolute path is given.

  `args` must be a list of binaries which the executable will receive
  as its arguments as is. This means that:

    * environment variables will not be interpolated
    * wildcard expansion will not happen (unless `Path.wildcard/2` is used
      explicitly)
    * arguments do not need to be escaped or quoted for shell safety

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
  [`:os.cmd/1`](http://www.erlang.org/doc/man/os.html#cmd-1).
  """
  @spec cmd(binary, [binary], Keyword.t) ::
        {Collectable.t, exit_status :: non_neg_integer}
  def cmd(command, args, opts \\ []) when is_binary(command) and is_list(args) do
    cmd = String.to_charlist(command)

    cmd =
      if Path.type(cmd) == :absolute do
        cmd
      else
        :os.find_executable(cmd) || :erlang.error(:enoent, [command, args, opts])
      end

    {into, opts} = cmd_opts(opts, [:use_stdio, :exit_status, :binary, :hide, args: args], "")
    {initial, fun} = Collectable.into(into)
    try do
      do_cmd Port.open({:spawn_executable, cmd}, opts), initial, fun
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        fun.(initial, :halt)
        :erlang.raise(kind, reason, stacktrace)
    else
      {acc, status} -> {fun.(acc, :done), status}
    end
  end

  defp do_cmd(port, acc, fun) do
    receive do
      {^port, {:data, data}} ->
        do_cmd(port, fun.(acc, {:cont, data}), fun)
      {^port, {:exit_status, status}} ->
        {acc, status}
    end
  end

  defp cmd_opts([{:into, any} | t], opts, _into),
    do: cmd_opts(t, opts, any)

  defp cmd_opts([{:cd, bin} | t], opts, into) when is_binary(bin),
    do: cmd_opts(t, [{:cd, bin} | opts], into)

  defp cmd_opts([{:arg0, bin} | t], opts, into) when is_binary(bin),
    do: cmd_opts(t, [{:arg0, bin} | opts], into)

  defp cmd_opts([{:stderr_to_stdout, true} | t], opts, into),
    do: cmd_opts(t, [:stderr_to_stdout | opts], into)

  defp cmd_opts([{:stderr_to_stdout, false} | t], opts, into),
    do: cmd_opts(t, opts, into)

  defp cmd_opts([{:parallelism, bool} | t], opts, into) when is_boolean(bool),
    do: cmd_opts(t, [{:parallelism, bool} | opts], into)

  defp cmd_opts([{:env, enum} | t], opts, into),
    do: cmd_opts(t, [{:env, validate_env(enum)} | opts], into)

  defp cmd_opts([{key, val} | _], _opts, _into),
    do: raise(ArgumentError, "invalid option #{inspect key} with value #{inspect val}")

  defp cmd_opts([], opts, into),
    do: {into, opts}

  defp validate_env(enum) do
    Enum.map enum, fn
      {k, nil} ->
        {String.to_charlist(k), false}
      {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}
      other ->
        raise ArgumentError, "invalid environment key-value #{inspect other}"
    end
  end

  @doc """
  Returns the current monotonic time in the `:native` time unit.

  This time is monotonically increasing and starts in an unspecified
  point in time.

  Inlined by the compiler into `:erlang.monotonic_time/0`.
  """
  @spec monotonic_time() :: integer
  def monotonic_time do
    :erlang.monotonic_time()
  end

  @doc """
  Returns the current monotonic time in the given time unit.

  This time is monotonically increasing and starts in an unspecified
  point in time.
  """
  @spec monotonic_time(time_unit) :: integer
  def monotonic_time(unit) do
    :erlang.monotonic_time(normalize_time_unit(unit))
  end

  @doc """
  Returns the current system time in the `:native` time unit.

  It is the VM view of the `os_time/0`. They may not match in
  case of time warps although the VM works towards aligning
  them. This time is not monotonic.

  Inlined by the compiler into `:erlang.system_time/0`.
  """
  @spec system_time() :: integer
  def system_time do
    :erlang.system_time()
  end

  @doc """
  Returns the current system time in the given time unit.

  It is the VM view of the `os_time/0`. They may not match in
  case of time warps although the VM works towards aligning
  them. This time is not monotonic.
  """
  @spec system_time(time_unit) :: integer
  def system_time(unit) do
    :erlang.system_time(normalize_time_unit(unit))
  end

  @doc """
  Converts `time` from time unit `from_unit` to time unit `to_unit`.

  The result is rounded via the floor function.

  `convert_time_unit/3` accepts an additional time unit (other than the
  ones in the `time_unit` type) called `:native`. `:native` is the time
  unit used by the Erlang runtime system. It's determined when the runtime
  starts and stays the same until the runtime is stopped. To determine what
  the `:native` unit amounts to in a system, you can call this function to
  convert 1 second to the `:native` time unit (i.e.,
  `System.convert_time_unit(1, :seconds, :native)`).
  """
  @spec convert_time_unit(integer, time_unit | :native, time_unit | :native) :: integer
  def convert_time_unit(time, from_unit, to_unit) do
    :erlang.convert_time_unit(time, normalize_time_unit(from_unit), normalize_time_unit(to_unit))
  end

  @doc """
  Returns the current time offset between the Erlang VM monotonic
  time and the Erlang VM system time.

  The result is returned in the `:native` time unit.

  See `time_offset/1` for more information.

  Inlined by the compiler into `:erlang.time_offset/0`.
  """
  @spec time_offset() :: integer
  def time_offset do
    :erlang.time_offset()
  end

  @doc """
  Returns the current time offset between the Erlang VM monotonic
  time and the Erlang VM system time.

  The result is returned in the given time unit `unit`. The returned
  offset, added to an Erlang monotonic time (e.g., obtained with
  `monotonic_time/1`), gives the Erlang system time that corresponds
  to that monotonic time.
  """
  @spec time_offset(time_unit) :: integer
  def time_offset(unit) do
    :erlang.time_offset(normalize_time_unit(unit))
  end

  @doc """
  Returns the current OS time.

  The result is returned in the `:native` time unit.

  This time may be adjusted forwards or backwards in time
  with no limitation and is not monotonic.

  Inlined by the compiler into `:os.system_time/0`.
  """
  @spec os_time() :: integer
  def os_time do
    :os.system_time()
  end

  @doc """
  Returns the current OS time in the given time `unit`.

  This time may be adjusted forwards or backwards in time
  with no limitation and is not monotonic.
  """
  @spec os_time(time_unit) :: integer
  def os_time(unit) do
    :os.system_time(normalize_time_unit(unit))
  end

  @doc """
  Generates and returns an integer that is unique in the current runtime
  instance.

  "Unique" means that this function, called with the same list of `modifiers`,
  will never return the same integer more than once on the current runtime
  instance.

  If `modifiers` is `[]`, then a unique integer (that can be positive or negative) is returned.
  Other modifiers can be passed to change the properties of the returned integer:

    * `:positive` - the returned integer is guaranteed to be positive.
    * `:monotonic` - the returned integer is monotonically increasing. This
      means that, on the same runtime instance (but even on different
      processes), integers returned using the `:monotonic` modifier will always
      be strictly less than integers returned by successive calls with the
      `:monotonic` modifier.

  All modifiers listed above can be combined; repeated modifiers in `modifiers`
  will be ignored.

  Inlined by the compiler into `:erlang.unique_integer/1`.
  """
  @spec unique_integer([:positive | :monotonic]) :: integer
  def unique_integer(modifiers \\ []) do
    :erlang.unique_integer(modifiers)
  end

  defp normalize_time_unit(:native),
    do: :native
  defp normalize_time_unit(:seconds),
    do: :seconds
  defp normalize_time_unit(:milliseconds),
    do: :milli_seconds
  defp normalize_time_unit(:microseconds),
    do: :micro_seconds
  defp normalize_time_unit(:nanoseconds),
    do: :nano_seconds
  defp normalize_time_unit(unit) when is_integer(unit) and unit > 0,
    do: unit

  # TODO: Warn on Elixir 1.4
  defp normalize_time_unit(erlang_unit)
      when erlang_unit in [:milli_seconds, :micro_seconds, :nano_seconds] do
    erlang_unit
  end

  defp normalize_time_unit(other) do
    raise ArgumentError,
      "unsupported time unit. Expected :seconds, :milliseconds, " <>
      ":microseconds, :nanoseconds, or a positive integer, " <>
      "got #{inspect other}"
  end
end
