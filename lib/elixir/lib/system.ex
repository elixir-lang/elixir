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
      mode](http://www.erlang.org/doc/apps/erts/time_correction.html#Time_Warp_Modes);

    * `monotonic_time/0` - a monotonically increasing time provided
      by the Erlang VM.

  The time functions in this module work in the `:native` unit
  (unless specified otherwise), which is OS dependent. Most of
  the time, all calculations are done in the `:native` unit, to
  avoid loss of precision, with `convert_time_unit/3` being
  invoked at the end to convert to a specific time unit like
  `:millisecond` or `:microsecond`. See the `t:time_unit/0` type for
  more information.

  For a more complete rundown on the VM support for different
  times, see the [chapter on time and time
  correction](http://www.erlang.org/doc/apps/erts/time_correction.html)
  in the Erlang docs.
  """

  @typedoc """
  The time unit to be passed to functions like `monotonic_time/1` and others.

  The `:second`, `:millisecond`, `:microsecond` and `:nanosecond` time
  units controls the return value of the functions that accept a time unit.

  A time unit can also be a strictly positive integer. In this case, it
  represents the "parts per second": the time will be returned in `1 /
  parts_per_second` seconds. For example, using the `:millisecond` time unit
  is equivalent to using `1000` as the time unit (as the time will be returned
  in 1/1000 seconds - milliseconds).
  """
  @type time_unit ::
          :second
          | :millisecond
          | :microsecond
          | :nanosecond
          | pos_integer

  @base_dir :filename.join(__DIR__, "../../..")
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
      "" -> raise RuntimeError, message: "could not read the version number from VERSION"
      data -> data
    end
  end

  # Returns OTP version that Elixir was compiled with.
  defmacrop get_otp_release do
    :erlang.list_to_binary(:erlang.system_info(:otp_release))
  end

  # Tries to run "git rev-parse --short HEAD". In the case of success returns
  # the short revision hash. If that fails, returns an empty string.
  defmacrop get_revision do
    null =
      case :os.type() do
        {:win32, _} -> 'NUL'
        _ -> '/dev/null'
      end

    'git rev-parse --short HEAD 2> '
    |> Kernel.++(null)
    |> :os.cmd()
    |> strip
  end

  defp revision, do: get_revision()

  # Get the date at compilation time.
  defmacrop get_date do
    {{year, month, day}, {hour, minute, second}} = :calendar.universal_time()

    "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0bZ"
    |> :io_lib.format([year, month, day, hour, minute, second])
    |> :erlang.iolist_to_binary()
  end

  @doc """
  Returns the endianness.
  """
  @spec endianness() :: :little | :big
  def endianness do
    :erlang.system_info(:endian)
  end

  @doc """
  Returns the endianness the system was compiled with.
  """
  @endianness :erlang.system_info(:endian)
  @spec compiled_endianness() :: :little | :big
  def compiled_endianness do
    @endianness
  end

  @doc """
  Elixir version information.

  Returns Elixir's version as binary.
  """
  @spec version() :: String.t()
  def version, do: get_version()

  @doc """
  Elixir build information.

  Returns a keyword list with Elixir version, Git short revision hash and compilation date.
  """
  @spec build_info() :: map
  def build_info do
    %{
      build: build(),
      date: get_date(),
      revision: revision(),
      version: version(),
      otp_release: get_otp_release()
    }
  end

  # Returns a string of the build info
  defp build do
    {:ok, v} = Version.parse(version())

    revision_string = if v.pre != [] and revision() != "", do: " (#{revision()})", else: ""
    otp_version_string = " (compiled with Erlang/OTP #{get_otp_release()})"

    version() <> revision_string <> otp_version_string
  end

  @doc """
  Lists command line arguments.

  Returns the list of command line arguments passed to the program.
  """
  @spec argv() :: [String.t()]
  def argv do
    :elixir_config.get(:argv)
  end

  @doc """
  Modifies command line arguments.

  Changes the list of command line arguments. Use it with caution,
  as it destroys any previous argv information.
  """
  @spec argv([String.t()]) :: :ok
  def argv(args) do
    :elixir_config.put(:argv, args)
  end

  @doc """
  Current working directory.

  Returns the current working directory or `nil` if one
  is not available.
  """
  @spec cwd() :: String.t() | nil
  def cwd do
    case :file.get_cwd() do
      {:ok, base} -> IO.chardata_to_string(fix_drive_letter(base))
      _ -> nil
    end
  end

  defp fix_drive_letter([l, ?:, ?/ | rest] = original) when l in ?A..?Z do
    case :os.type() do
      {:win32, _} -> [l + ?a - ?A, ?:, ?/ | rest]
      _ -> original
    end
  end

  defp fix_drive_letter(original), do: original

  @doc """
  Current working directory, exception on error.

  Returns the current working directory or raises `RuntimeError`.
  """
  @spec cwd!() :: String.t()
  def cwd! do
    cwd() ||
      raise RuntimeError,
        message:
          "could not get a current working directory, the current location is not accessible"
  end

  @doc """
  User home directory.

  Returns the user home directory (platform independent).
  """
  @spec user_home() :: String.t() | nil
  def user_home do
    :elixir_config.get(:home)
  end

  @doc """
  User home directory, exception on error.

  Same as `user_home/0` but raises `RuntimeError`
  instead of returning `nil` if no user home is set.
  """
  @spec user_home!() :: String.t()
  def user_home! do
    user_home() ||
      raise RuntimeError,
        message: "could not find the user home, please set the HOME environment variable"
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
  @spec tmp_dir() :: String.t() | nil
  def tmp_dir do
    write_env_tmp_dir('TMPDIR') || write_env_tmp_dir('TEMP') || write_env_tmp_dir('TMP') ||
      write_tmp_dir('/tmp') || ((cwd = cwd()) && write_tmp_dir(cwd))
  end

  @doc """
  Writable temporary directory, exception on error.

  Same as `tmp_dir/0` but raises `RuntimeError`
  instead of returning `nil` if no temp dir is set.
  """
  @spec tmp_dir!() :: String.t()
  def tmp_dir! do
    tmp_dir() ||
      raise RuntimeError,
        message:
          "could not get a writable temporary directory, " <>
            "please set the TMPDIR environment variable"
  end

  defp write_env_tmp_dir(env) do
    case :os.getenv(env) do
      false -> nil
      tmp -> write_tmp_dir(tmp)
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

      {:error, _} ->
        nil
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
  @spec at_exit((non_neg_integer -> any)) :: list(fun)
  def at_exit(fun) when is_function(fun, 1) do
    :elixir_config.update(:at_exit, &[fun | &1])
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
    assert_no_null_byte!(program, "System.find_executable/1")

    case :os.find_executable(String.to_charlist(program)) do
      false -> nil
      other -> List.to_string(other)
    end
  end

  @doc """
  Returns all system environment variables.

  The returned value is a map containing name-value pairs.
  Variable names and their values are strings.
  """
  @spec get_env() :: %{optional(String.t()) => String.t()}
  def get_env do
    Enum.into(:os.getenv(), %{}, fn var ->
      var = IO.chardata_to_string(var)
      [k, v] = String.split(var, "=", parts: 2)
      {k, v}
    end)
  end

  @doc """
  Returns the value of the given environment variable.

  The returned value of the environment variable
  `varname` is a string, or `nil` if the environment
  variable is undefined.
  """
  @spec get_env(String.t()) :: String.t() | nil
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

  For more information, see `:os.getpid/0`.
  """
  @spec get_pid() :: binary
  def get_pid, do: IO.iodata_to_binary(:os.getpid())

  @doc """
  Sets an environment variable value.

  Sets a new `value` for the environment variable `varname`.
  """
  @spec put_env(binary, binary) :: :ok
  def put_env(varname, value) when is_binary(varname) and is_binary(value) do
    case :binary.match(varname, "=") do
      {_, _} ->
        raise ArgumentError,
              "cannot execute System.put_env/2 for key with \"=\", got: #{inspect(varname)}"

      :nomatch ->
        :os.putenv(String.to_charlist(varname), String.to_charlist(value))
        :ok
    end
  end

  @doc """
  Sets multiple environment variables.

  Sets a new value for each environment variable corresponding
  to each `{key, value}` pair in `enum`.
  """
  @spec put_env(Enumerable.t()) :: :ok
  def put_env(enum) do
    Enum.each(enum, fn {key, val} -> put_env(key, val) end)
  end

  @doc """
  Deletes an environment variable.

  Removes the variable `varname` from the environment.
  """
  @spec delete_env(String.t()) :: :ok
  def delete_env(varname) do
    :os.unsetenv(String.to_charlist(varname))
    :ok
  end

  @doc """
  Deprecated mechanism to retrieve the last exception stacktrace.

  Accessing the stacktrace outside of a rescue/catch is deprecated.
  If you want to support only Elixir v1.7+, you must access
  `__STACKTRACE__/0` inside a rescue/catch. If you want to support
  earlier Elixir versions, move `System.stacktrace/0` inside a rescue/catch.

  Note that the Erlang VM (and therefore this function) does not
  return the current stacktrace but rather the stacktrace of the
  latest exception.
  """
  # TODO: Fully deprecate it on Elixir v1.9.
  # It is currently partially deprecated in elixir_dispatch.erl
  def stacktrace do
    apply(:erlang, :get_stacktrace, [])
  end

  @doc """
  Immediately halts the Erlang runtime system.

  Terminates the Erlang runtime system without properly shutting down
  applications and ports. Please see `stop/1` for a careful shutdown of the
  system.

  `status` must be a non-negative integer, the atom `:abort` or a binary.

    * If an integer, the runtime system exits with the integer value which
      is returned to the operating system.

    * If `:abort`, the runtime system aborts producing a core dump, if that is
      enabled in the operating system.

    * If a string, an Erlang crash dump is produced with status as slogan,
      and then the runtime system exits with status code 1.

  Note that on many platforms, only the status codes 0-255 are supported
  by the operating system.

  For more information, see `:erlang.halt/1`.

  ## Examples

      System.halt(0)
      System.halt(1)
      System.halt(:abort)

  """
  @spec halt(non_neg_integer | binary | :abort) :: no_return
  def halt(status \\ 0)

  def halt(status) when is_integer(status) or status == :abort do
    :erlang.halt(status)
  end

  def halt(status) when is_binary(status) do
    :erlang.halt(String.to_charlist(status))
  end

  @doc """
  Carefully stops the Erlang runtime system.

  All applications are taken down smoothly, all code is unloaded, and all ports
  are closed before the system terminates by calling `halt/1`.

  `status` must be a non-negative integer value which is returned by the
  runtime system to the operating system.

  Note that on many platforms, only the status codes 0-255 are supported
  by the operating system.

  For more information, see `:init.stop/1`.

  ## Examples

      System.stop(0)
      System.stop(1)

  """
  @doc since: "1.5.0"
  @spec stop(non_neg_integer | binary) :: no_return
  def stop(status \\ 0)

  def stop(status) when is_integer(status) do
    :init.stop(status)
  end

  def stop(status) when is_binary(status) do
    :init.stop(String.to_charlist(status))
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

  Internally, this function uses a `Port` for interacting with the
  outside world. However, if you plan to run a long-running program,
  ports guarantee stdin/stdout devices will be closed but it does not
  automatically terminate the program. The documentation for the
  `Port` module describes this problem and possible solutions under
  the "Zombie processes" section.

  ## Examples

      iex> System.cmd("echo", ["hello"])
      {"hello\n", 0}

      iex> System.cmd("echo", ["hello"], env: [{"MIX_ENV", "test"}])
      {"hello\n", 0}

      iex> System.cmd("echo", ["hello"], into: IO.stream(:stdio, :line))
      hello
      {%IO.Stream{}, 0}

  ## Options

    * `:into` - injects the result into the given collectable, defaults to `""`
    * `:cd` - the directory to run the command in
    * `:env` - an enumerable of tuples containing environment key-value as binary
    * `:arg0` - sets the command arg0
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
  redirecting and so on, please check `:os.cmd/1`.
  """
  @spec cmd(binary, [binary], keyword) :: {Collectable.t(), exit_status :: non_neg_integer}
  def cmd(command, args, opts \\ []) when is_binary(command) and is_list(args) do
    assert_no_null_byte!(command, "System.cmd/3")

    unless Enum.all?(args, &is_binary/1) do
      raise ArgumentError, "all arguments for System.cmd/3 must be binaries"
    end

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
      do_cmd(Port.open({:spawn_executable, cmd}, opts), initial, fun)
    catch
      kind, reason ->
        fun.(initial, :halt)
        :erlang.raise(kind, reason, __STACKTRACE__)
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

  defp cmd_opts([{:into, any} | t], opts, _into), do: cmd_opts(t, opts, any)

  defp cmd_opts([{:cd, bin} | t], opts, into) when is_binary(bin),
    do: cmd_opts(t, [{:cd, bin} | opts], into)

  defp cmd_opts([{:arg0, bin} | t], opts, into) when is_binary(bin),
    do: cmd_opts(t, [{:arg0, bin} | opts], into)

  defp cmd_opts([{:stderr_to_stdout, true} | t], opts, into),
    do: cmd_opts(t, [:stderr_to_stdout | opts], into)

  defp cmd_opts([{:stderr_to_stdout, false} | t], opts, into), do: cmd_opts(t, opts, into)

  defp cmd_opts([{:parallelism, bool} | t], opts, into) when is_boolean(bool),
    do: cmd_opts(t, [{:parallelism, bool} | opts], into)

  defp cmd_opts([{:env, enum} | t], opts, into),
    do: cmd_opts(t, [{:env, validate_env(enum)} | opts], into)

  defp cmd_opts([{key, val} | _], _opts, _into),
    do: raise(ArgumentError, "invalid option #{inspect(key)} with value #{inspect(val)}")

  defp cmd_opts([], opts, into), do: {into, opts}

  defp validate_env(enum) do
    Enum.map(enum, fn
      {k, nil} ->
        {String.to_charlist(k), false}

      {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}

      other ->
        raise ArgumentError, "invalid environment key-value #{inspect(other)}"
    end)
  end

  @doc """
  Returns the current monotonic time in the `:native` time unit.

  This time is monotonically increasing and starts in an unspecified
  point in time.

  Inlined by the compiler.
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

  Inlined by the compiler.
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
  ones in the `t:time_unit/0` type) called `:native`. `:native` is the time
  unit used by the Erlang runtime system. It's determined when the runtime
  starts and stays the same until the runtime is stopped. To determine what
  the `:native` unit amounts to in a system, you can call this function to
  convert 1 second to the `:native` time unit (i.e.,
  `System.convert_time_unit(1, :second, :native)`).
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

  Inlined by the compiler.
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

  Inlined by the compiler.
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
  Returns the Erlang/OTP release number.
  """
  @spec otp_release :: String.t()
  def otp_release do
    :erlang.list_to_binary(:erlang.system_info(:otp_release))
  end

  @doc """
  Returns the number of schedulers in the VM.
  """
  @spec schedulers :: pos_integer
  def schedulers do
    :erlang.system_info(:schedulers)
  end

  @doc """
  Returns the number of schedulers online in the VM.
  """
  @spec schedulers_online :: pos_integer
  def schedulers_online do
    :erlang.system_info(:schedulers_online)
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

  Inlined by the compiler.
  """
  @spec unique_integer([:positive | :monotonic]) :: integer
  def unique_integer(modifiers \\ []) do
    :erlang.unique_integer(modifiers)
  end

  defp assert_no_null_byte!(binary, operation) do
    case :binary.match(binary, "\0") do
      {_, _} ->
        raise ArgumentError,
              "cannot execute #{operation} for program with null byte, got: #{inspect(binary)}"

      :nomatch ->
        binary
    end
  end

  defp normalize_time_unit(:native), do: :native

  defp normalize_time_unit(:second), do: :second
  defp normalize_time_unit(:millisecond), do: :millisecond
  defp normalize_time_unit(:microsecond), do: :microsecond
  defp normalize_time_unit(:nanosecond), do: :nanosecond

  defp normalize_time_unit(:seconds), do: warn(:seconds, :second)
  defp normalize_time_unit(:milliseconds), do: warn(:milliseconds, :millisecond)
  defp normalize_time_unit(:microseconds), do: warn(:microseconds, :microsecond)
  defp normalize_time_unit(:nanoseconds), do: warn(:nanoseconds, :nanosecond)

  defp normalize_time_unit(:milli_seconds), do: warn(:milli_seconds, :millisecond)
  defp normalize_time_unit(:micro_seconds), do: warn(:micro_seconds, :microsecond)
  defp normalize_time_unit(:nano_seconds), do: warn(:nano_seconds, :nanosecond)

  defp normalize_time_unit(unit) when is_integer(unit) and unit > 0, do: unit

  defp normalize_time_unit(other) do
    raise ArgumentError,
          "unsupported time unit. Expected :second, :millisecond, " <>
            ":microsecond, :nanosecond, or a positive integer, " <> "got #{inspect(other)}"
  end

  defp warn(unit, replacement_unit) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    stacktrace = Enum.drop(stacktrace, 3)

    :elixir_config.warn({System, unit}, stacktrace) &&
      IO.warn(
        "deprecated time unit: #{inspect(unit)}. A time unit should be " <>
          ":second, :millisecond, :microsecond, :nanosecond, or a positive integer",
        stacktrace
      )

    replacement_unit
  end
end
