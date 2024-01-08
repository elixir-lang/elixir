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

    * `os_time/0` - the time reported by the operating system (OS). This time may be
      adjusted forwards or backwards in time with no limitation;

    * `system_time/0` - the VM view of the `os_time/0`. The system time and operating
      system time may not match in case of time warps although the VM works towards
      aligning them. This time is not monotonic (i.e., it may decrease)
      as its behavior is configured [by the VM time warp
      mode](https://www.erlang.org/doc/apps/erts/time_correction.html#Time_Warp_Modes);

    * `monotonic_time/0` - a monotonically increasing time provided
      by the Erlang VM. This is not strictly monotonically increasing. Multiple
      sequential calls of the function may return the same value.

  The time functions in this module work in the `:native` unit
  (unless specified otherwise), which is operating system dependent. Most of
  the time, all calculations are done in the `:native` unit, to
  avoid loss of precision, with `convert_time_unit/3` being
  invoked at the end to convert to a specific time unit like
  `:millisecond` or `:microsecond`. See the `t:time_unit/0` type for
  more information.

  For a more complete rundown on the VM support for different
  times, see the [chapter on time and time
  correction](https://www.erlang.org/doc/apps/erts/time_correction.html)
  in the Erlang docs.
  """

  defmodule EnvError do
    @moduledoc """
    An exception raised when a system environment variable is not set.

    For example, see `System.fetch_env!/1`.
    """

    defexception [:env]

    @impl true
    def message(%{env: env}) do
      "could not fetch environment variable #{inspect(env)} because it is not set"
    end
  end

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

  @type signal ::
          :sigabrt
          | :sigalrm
          | :sigchld
          | :sighup
          | :sigquit
          | :sigstop
          | :sigterm
          | :sigtstp
          | :sigusr1
          | :sigusr2

  @vm_signals [:sigquit, :sigterm, :sigusr1]
  @os_signals [:sighup, :sigabrt, :sigalrm, :sigusr2, :sigchld, :sigstop, :sigtstp]
  @signals @vm_signals ++ @os_signals

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
      "" -> raise "could not read the version number from VERSION"
      data -> data
    end
  end

  # Returns OTP version that Elixir was compiled with.
  defmacrop get_otp_release do
    :erlang.list_to_binary(:erlang.system_info(:otp_release))
  end

  # Tries to run "git rev-parse --short=7 HEAD". In the case of success returns
  # the short revision hash. If that fails, returns an empty string.
  defmacrop get_revision do
    null =
      case :os.type() do
        {:win32, _} -> ~c"NUL"
        _ -> ~c"/dev/null"
      end

    ~c"git rev-parse --short=7 HEAD 2> "
    |> Kernel.++(null)
    |> :os.cmd()
    |> strip
  end

  defp revision, do: get_revision()

  # Get the date at compilation time.
  # Follows https://reproducible-builds.org/specs/source-date-epoch/
  defmacrop get_date do
    unix_epoch =
      if source_date_epoch = :os.getenv(~c"SOURCE_DATE_EPOCH") do
        try do
          List.to_integer(source_date_epoch)
        rescue
          _ -> nil
        end
      end

    unix_epoch = unix_epoch || :os.system_time(:second)

    {{year, month, day}, {hour, minute, second}} =
      :calendar.gregorian_seconds_to_datetime(unix_epoch + 62_167_219_200)

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

  Returns a map with the Elixir version, the Erlang/OTP release it was compiled
  with, a short Git revision hash and the date and time it was built.

  Every value in the map is a string, and these are:

    * `:build` - the Elixir version, short Git revision hash and
      Erlang/OTP release it was compiled with
    * `:date` - a string representation of the ISO8601 date and time it was built
    * `:otp_release` - OTP release it was compiled with
    * `:revision` - short Git revision hash. If Git was not available at building
      time, it is set to `""`
    * `:version` - the Elixir version

  One should not rely on the specific formats returned by each of those fields.
  Instead one should use specialized functions, such as `version/0` to retrieve
  the Elixir version and `otp_release/0` to retrieve the Erlang/OTP release.

  ## Examples

      iex> System.build_info()
      %{
        build: "1.9.0-dev (772a00a0c) (compiled with Erlang/OTP 21)",
        date: "2018-12-24T01:09:21Z",
        otp_release: "21",
        revision: "772a00a0c",
        version: "1.9.0-dev"
      }

  """
  @spec build_info() :: %{
          build: String.t(),
          date: String.t(),
          revision: String.t(),
          version: String.t(),
          otp_release: String.t()
        }
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
  Marks if the system should halt or not at the end of ARGV processing.
  """
  @doc since: "1.9.0"
  @spec no_halt(boolean) :: :ok
  def no_halt(boolean) when is_boolean(boolean) do
    :elixir_config.put(:no_halt, boolean)
  end

  @doc """
  Checks if the system will halt or not at the end of ARGV processing.
  """
  @doc since: "1.9.0"
  @spec no_halt() :: boolean
  def no_halt() do
    :elixir_config.get(:no_halt)
  end

  @doc """
  Waits until the system boots.

  Calling this function blocks until all of ARGV is processed.
  Inside a release, this means the boot script and then ARGV
  have been processed. This is only useful for those implementing
  custom shells/consoles on top of Elixir.

  However, be careful to not invoke this command from within
  the process that is processing the command line arguments,
  as doing so would lead to a deadlock.
  """
  @doc since: "1.15.0"
  @spec wait_until_booted() :: :ok
  defdelegate wait_until_booted(), to: :elixir_config

  @doc """
  Current working directory.

  Returns the current working directory or `nil` if one
  is not available.
  """
  @deprecated "Use File.cwd/0 instead"
  @spec cwd() :: String.t() | nil
  def cwd do
    case File.cwd() do
      {:ok, cwd} -> cwd
      _ -> nil
    end
  end

  @doc """
  Current working directory, exception on error.

  Returns the current working directory or raises `RuntimeError`.
  """
  @deprecated "Use File.cwd!/0 instead"
  @spec cwd!() :: String.t()
  def cwd! do
    case File.cwd() do
      {:ok, cwd} ->
        cwd

      _ ->
        raise "could not get a current working directory, the current location is not accessible"
    end
  end

  @doc """
  User home directory.

  Returns the user home directory (platform independent).
  """
  @spec user_home() :: String.t() | nil
  def user_home do
    case :init.get_argument(:home) do
      {:ok, [[home] | _]} ->
        encoding = :file.native_name_encoding()
        :unicode.characters_to_binary(home, encoding, encoding)

      _ ->
        nil
    end
  end

  @doc """
  User home directory, exception on error.

  Same as `user_home/0` but raises `RuntimeError`
  instead of returning `nil` if no user home is set.
  """
  @spec user_home!() :: String.t()
  def user_home! do
    user_home() || raise "could not find the user home, please set the HOME environment variable"
  end

  @doc ~S"""
  Writable temporary directory.

  Returns a writable temporary directory.
  Searches for directories in the following order:

    1. the directory named by the TMPDIR environment variable
    2. the directory named by the TEMP environment variable
    3. the directory named by the TMP environment variable
    4. `C:\TMP` on Windows or `/tmp` on Unix-like operating systems
    5. as a last resort, the current working directory

  Returns `nil` if none of the above are writable.
  """
  @spec tmp_dir() :: String.t() | nil
  def tmp_dir do
    write_env_tmp_dir(~c"TMPDIR") || write_env_tmp_dir(~c"TEMP") || write_env_tmp_dir(~c"TMP") ||
      write_tmp_dir(~c"/tmp") || write_cwd_tmp_dir()
  end

  defp write_cwd_tmp_dir do
    case File.cwd() do
      {:ok, cwd} -> write_tmp_dir(cwd)
      _ -> nil
    end
  end

  @doc """
  Writable temporary directory, exception on error.

  Same as `tmp_dir/0` but raises `RuntimeError`
  instead of returning `nil` if no temp dir is set.
  """
  @spec tmp_dir!() :: String.t()
  def tmp_dir! do
    tmp_dir() ||
      raise "could not get a writable temporary directory, please set the TMPDIR environment variable"
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

  Registers a function that will be invoked at the end of an Elixir script.
  A script is typically started via the command line via the `elixir` and
  `mix` executables.

  The handler always executes in a different process from the one it was
  registered in. As a consequence, any resources managed by the calling process
  (ETS tables, open files, and others) won't be available by the time the handler
  function is invoked.

  The function must receive the exit status code as an argument.

  If the VM terminates programmatically, via `System.stop/1`, `System.halt/1`,
  or exit signals, the `at_exit/1` callbacks are not guaranteed to be executed.
  """
  @spec at_exit((non_neg_integer -> any)) :: :ok
  def at_exit(fun) when is_function(fun, 1) do
    :elixir_config.update(:at_exit, &[fun | &1])
    :ok
  end

  defmodule SignalHandler do
    @moduledoc false
    @behaviour :gen_event

    @impl true
    def init({event, fun}) do
      {:ok, {event, fun}}
    end

    @impl true
    def handle_call(_message, state) do
      {:ok, :ok, state}
    end

    @impl true
    def handle_event(signal, {event, fun}) do
      if signal == event, do: :ok = fun.()
      {:ok, {event, fun}}
    end

    @impl true
    def handle_info(_, {event, fun}) do
      {:ok, {event, fun}}
    end
  end

  @doc """
  Traps the given `signal` to execute the `fun`.

  > #### Avoid setting traps in libraries {: .warning}
  >
  > Trapping signals may have strong implications
  > on how a system shuts down and behaves in production and
  > therefore it is extremely discouraged for libraries to
  > set their own traps. Instead, they should redirect users
  > to configure them themselves. The only cases where it is
  > acceptable for libraries to set their own traps is when
  > using Elixir in script mode, such as in `.exs` files and
  > via Mix tasks.

  An optional `id` that uniquely identifies the function
  can be given, otherwise a unique one is automatically
  generated. If a previously registered `id` is given,
  this function returns an error tuple. The `id` can be
  used to remove a registered signal by calling
  `untrap_signal/2`.

  The given `fun` receives no arguments and it must return
  `:ok`.

  It returns `{:ok, id}` in case of success,
  `{:error, :already_registered}` in case the id has already
  been registered for the given signal, or `{:error, :not_sup}`
  in case trapping exists is not supported by the current OS.

  The first time a signal is trapped, it will override the
  default behavior from the operating system. If the same
  signal is trapped multiple times, subsequent functions
  given to `trap_signal` will execute *first*. In other
  words, you can consider each function is prepended to
  the signal handler.

  By default, the Erlang VM register traps to the three
  signals:

    * `:sigstop` - gracefully shuts down the VM with `stop/0`
    * `:sigquit` - halts the VM via `halt/0`
    * `:sigusr1` - halts the VM via status code of 1

  Therefore, if you add traps to the signals above, the
  default behavior above will be executed after all user
  signals.

  ## Implementation notes

  All signals run from a single process. Therefore, blocking the
  `fun` will block subsequent traps. It is also not possible to add
  or remove traps from within a trap itself.

  Internally, this functionality is built on top of `:os.set_signal/2`.
  When you register a trap, Elixir automatically sets it to `:handle`
  and it reverts it back to `:default` once all traps are removed
  (except for `:sigquit`, `:sigterm`, and `:sigusr1` which are always
  handled). If you or a library call `:os.set_signal/2` directly,
  it may disable Elixir traps (or Elixir may override your configuration).
  """
  @doc since: "1.12.0"
  @spec trap_signal(signal, (-> :ok)) :: {:ok, reference()} | {:error, :not_sup}
  @spec trap_signal(signal, id, (-> :ok)) ::
          {:ok, id} | {:error, :already_registered} | {:error, :not_sup}
        when id: term()
  def trap_signal(signal, id \\ make_ref(), fun)
      when signal in @signals and is_function(fun, 0) do
    :elixir_config.serial(fn ->
      gen_id = {signal, id}

      if {SignalHandler, gen_id} in signal_handlers() do
        {:error, :already_registered}
      else
        try do
          :os.set_signal(signal, :handle)
        rescue
          _ -> {:error, :not_sup}
        else
          :ok ->
            :ok =
              :gen_event.add_handler(:erl_signal_server, {SignalHandler, gen_id}, {signal, fun})

            {:ok, id}
        end
      end
    end)
  end

  @doc """
  Removes a previously registered `signal` with `id`.
  """
  @doc since: "1.12.0"
  @spec untrap_signal(signal, id) :: :ok | {:error, :not_found} when id: term
  def untrap_signal(signal, id) when signal in @signals do
    :elixir_config.serial(fn ->
      gen_id = {signal, id}

      case :gen_event.delete_handler(:erl_signal_server, {SignalHandler, gen_id}, :delete) do
        :ok ->
          if not trapping?(signal) do
            :os.set_signal(signal, :default)
          end

          :ok

        {:error, :module_not_found} ->
          {:error, :not_found}
      end
    end)
  end

  defp trapping?(signal) do
    signal in @vm_signals or
      Enum.any?(signal_handlers(), &match?({_, {^signal, _}}, &1))
  end

  defp signal_handlers do
    :gen_event.which_handlers(:erl_signal_server)
  end

  @doc """
  Locates an executable on the system.

  This function looks up an executable program given
  its name using the environment variable PATH on Windows and Unix-like
  operating systems. It also considers the proper executable
  extension for each operating system, so for Windows it will try to
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
    Map.new(:os.env(), fn {k, v} ->
      {IO.chardata_to_string(k), IO.chardata_to_string(v)}
    end)
  end

  @doc """
  Returns the value of the given environment variable.

  The returned value of the environment variable
  `varname` is a string. If the environment variable
  is not set, returns the string specified in `default` or
  `nil` if none is specified.

  ## Examples

      iex> System.get_env("PORT")
      "4000"

      iex> System.get_env("NOT_SET")
      nil

      iex> System.get_env("NOT_SET", "4001")
      "4001"

  """
  @doc since: "1.9.0"
  @spec get_env(String.t(), String.t()) :: String.t()
  @spec get_env(String.t(), nil) :: String.t() | nil
  def get_env(varname, default \\ nil)
      when is_binary(varname) and
             (is_binary(default) or is_nil(default)) do
    case :os.getenv(String.to_charlist(varname)) do
      false -> default
      other -> List.to_string(other)
    end
  end

  @doc """
  Returns the value of the given environment variable or `:error` if not found.

  If the environment variable `varname` is set, then `{:ok, value}` is returned
  where `value` is a string. If `varname` is not set, `:error` is returned.

  ## Examples

      iex> System.fetch_env("PORT")
      {:ok, "4000"}

      iex> System.fetch_env("NOT_SET")
      :error

  """
  @doc since: "1.9.0"
  @spec fetch_env(String.t()) :: {:ok, String.t()} | :error
  def fetch_env(varname) when is_binary(varname) do
    case :os.getenv(String.to_charlist(varname)) do
      false -> :error
      other -> {:ok, List.to_string(other)}
    end
  end

  @doc """
  Returns the value of the given environment variable or raises if not found.

  Same as `get_env/1` but raises instead of returning `nil` when the variable is
  not set.

  ## Examples

      iex> System.fetch_env!("PORT")
      "4000"

      iex> System.fetch_env!("NOT_SET")
      ** (System.EnvError) could not fetch environment variable "NOT_SET" because it is not set

  """
  @doc since: "1.9.0"
  @spec fetch_env!(String.t()) :: String.t()
  def fetch_env!(varname) when is_binary(varname) do
    get_env(varname) || raise(EnvError, env: varname)
  end

  @doc """
  Erlang VM process identifier.

  Returns the process identifier of the current Erlang emulator
  in the format most commonly used by the operating system environment.

  For more information, see `:os.getpid/0`.
  """
  @deprecated "Use System.pid/0 instead"
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
  to each `{key, value}` pair in `enum`. Keys and non-nil values
  are automatically converted to charlists. `nil` values erase
  the given keys.

  Overall, this is a convenience wrapper around `put_env/2` and
  `delete_env/2` with support for different key and value formats.
  """
  @spec put_env(Enumerable.t()) :: :ok
  def put_env(enum) do
    Enum.each(enum, fn
      {key, nil} ->
        :os.unsetenv(to_charlist(key))

      {key, val} ->
        key = to_charlist(key)

        case :string.find(key, "=") do
          :nomatch ->
            :os.putenv(key, to_charlist(val))

          _ ->
            raise ArgumentError,
                  "cannot execute System.put_env/1 for key with \"=\", got: #{inspect(key)}"
        end
    end)
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

  It always return an empty list.
  """
  @deprecated "Use __STACKTRACE__ instead"
  def stacktrace do
    []
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
  @spec halt() :: no_return
  @spec halt(non_neg_integer | binary | :abort) :: no_return
  def halt(status \\ 0)

  def halt(status) when is_integer(status) or status == :abort do
    :erlang.halt(status)
  end

  def halt(status) when is_binary(status) do
    :erlang.halt(String.to_charlist(status))
  end

  @doc """
  Returns the operating system PID for the current Erlang runtime system instance.

  Returns a string containing the (usually) numerical identifier for a process.
  On Unix-like operating systems, this is typically the return value of the `getpid()` system call.
  On Windows, the process ID as returned by the `GetCurrentProcessId()` system
  call is used.

  ## Examples

      System.pid()

  """
  @doc since: "1.9.0"
  @spec pid :: String.t()
  def pid do
    List.to_string(:os.getpid())
  end

  @doc """
  Restarts all applications in the Erlang runtime system.

  All applications are taken down smoothly, all code is unloaded, and all ports
  are closed before the system starts all applications once again.

  ## Examples

      System.restart()

  """
  @doc since: "1.9.0"
  @spec restart :: :ok
  defdelegate restart(), to: :init

  @doc """
  Asynchronously and carefully stops the Erlang runtime system.

  All applications are taken down smoothly, all code is unloaded, and all ports
  are closed before the system terminates by calling `halt/1`.

  `status` must be a non-negative integer or a binary.

    * If an integer, the runtime system exits with the integer value which is
      returned to the operating system. On many platforms, only the status codes
      0-255 are supported by the operating system.

    * If a binary, an Erlang crash dump is produced with status as slogan, and
      then the runtime system exits with status code 1.

  Note this function is asynchronous and the current process will continue
  executing after this function is invoked. In case you want to block the
  current process until the system effectively shuts down, you can invoke
  `Process.sleep(:infinity)`.

  ## Examples

      System.stop(0)
      System.stop(1)

  """
  @doc since: "1.5.0"
  @spec stop(non_neg_integer | binary) :: :ok
  def stop(status \\ 0)

  def stop(status) when is_integer(status) do
    at_exit(fn _ -> Process.sleep(:infinity) end)
    :init.stop(status)
  end

  def stop(status) when is_binary(status) do
    at_exit(fn _ -> Process.sleep(:infinity) end)
    :init.stop(String.to_charlist(status))
  end

  @doc ~S"""
  Executes the given `command` in the OS shell.

  It uses `sh` for Unix-like systems and `cmd` for Windows.

  > #### Watch out {: .warning}
  >
  > Use this function with care. In particular, **never
  > pass untrusted user input to this function**, as the user would be
  > able to perform "command injection attacks" by executing any code
  > directly on the machine. Generally speaking, prefer to use `cmd/3`
  > over this function.

  ## Examples

      iex> System.shell("echo hello")
      {"hello\n", 0}

  If you want to stream the output to Standard IO as it arrives:

      iex> System.shell("echo hello", into: IO.stream())
      hello
      {%IO.Stream{}, 0}

  ## Options

  It accepts the same options as `cmd/3` (except for `arg0`).
  It also accepts the following exclusive options:

    * `:close_stdin` (since v1.14.1) - if the stdin should be closed
      on Unix systems, forcing any command that waits on stdin to
      immediately terminate. Defaults to false.
  """
  @doc since: "1.12.0"
  @spec shell(binary, keyword) :: {Collectable.t(), exit_status :: non_neg_integer}
  def shell(command, opts \\ []) when is_binary(command) do
    assert_no_null_byte!(command, "System.shell/2")
    {close_stdin?, opts} = Keyword.pop(opts, :close_stdin, false)

    # Finding shell command logic from :os.cmd in OTP
    # https://github.com/erlang/otp/blob/8deb96fb1d017307e22d2ab88968b9ef9f1b71d0/lib/kernel/src/os.erl#L184
    case :os.type() do
      {:unix, _} ->
        shell_path = :os.find_executable(~c"sh") || :erlang.error(:enoent, [command, opts])
        close_stdin = if close_stdin?, do: " </dev/null", else: ""
        command = IO.iodata_to_binary(["(", command, "\n)", close_stdin])
        do_cmd({:spawn_executable, shell_path}, [args: ["-c", command]], opts)

      {:win32, osname} ->
        command = String.to_charlist(command)

        command =
          case {System.get_env("COMSPEC"), osname} do
            {nil, :windows} -> ~c"command.com /s /c " ++ command
            {nil, _} -> ~c"cmd /s /c " ++ command
            {cmd, _} -> ~c"#{cmd} /s /c " ++ command
          end

        do_cmd({:spawn, command}, [], opts)
    end
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

  If you want to stream the output to Standard IO as it arrives:

      iex> System.cmd("echo", ["hello"], into: IO.stream())
      hello
      {%IO.Stream{}, 0}

  If you want to read lines:

      iex> System.cmd("echo", ["hello\nworld"], into: [], lines: 1024)
      {["hello", "world"], 0}

  ## Options

    * `:into` - injects the result into the given collectable, defaults to `""`

    * `:lines` - (since v1.15.0) reads the output by lines instead of in bytes. It expects a
      number of maximum bytes to buffer internally (1024 is a reasonable default).
      The collectable will be called with each finished line (regardless of buffer
      size) and without the EOL character

    * `:cd` - the directory to run the command in

    * `:env` - an enumerable of tuples containing environment key-value as
      binary. The child process inherits all environment variables from its
      parent process, the Elixir application, except those overwritten or
      cleared using this option. Specify a value of `nil` to clear (unset) an
      environment variable, which is useful for preventing credentials passed
      to the application from leaking into child processes

    * `:arg0` - sets the command arg0

    * `:stderr_to_stdout` - redirects stderr to stdout when `true`

    * `:parallelism` - when `true`, the VM will schedule port tasks to improve
      parallelism in the system. If set to `false`, the VM will try to perform
      commands immediately, improving latency at the expense of parallelism.
      The default is `false`, and can be set on system startup by passing the
      [`+spp`](https://www.erlang.org/doc/man/erl.html#+spp) flag to `--erl`.
      Use `:erlang.system_info(:port_parallelism)` to check if enabled.

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
  redirecting and so on, please check `shell/2`.
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

    do_cmd({:spawn_executable, cmd}, [args: args], opts)
  end

  defp do_cmd(port_init, base_opts, opts) do
    {into, line, opts} =
      cmd_opts(opts, [:use_stdio, :exit_status, :binary, :hide] ++ base_opts, "", false)

    {initial, fun} = Collectable.into(into)

    try do
      case line do
        true -> do_port_line(Port.open(port_init, opts), initial, fun, [])
        false -> do_port_byte(Port.open(port_init, opts), initial, fun)
      end
    catch
      kind, reason ->
        fun.(initial, :halt)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      {acc, status} -> {fun.(acc, :done), status}
    end
  end

  defp do_port_byte(port, acc, fun) do
    receive do
      {^port, {:data, data}} ->
        do_port_byte(port, fun.(acc, {:cont, data}), fun)

      {^port, {:exit_status, status}} ->
        {acc, status}
    end
  end

  defp do_port_line(port, acc, fun, buffer) do
    receive do
      {^port, {:data, {:noeol, data}}} ->
        do_port_line(port, acc, fun, [data | buffer])

      {^port, {:data, {:eol, data}}} ->
        data = [data | buffer] |> Enum.reverse() |> IO.iodata_to_binary()
        do_port_line(port, fun.(acc, {:cont, data}), fun, [])

      {^port, {:exit_status, status}} ->
        # Data may arrive after exit status on line mode
        receive do
          {^port, {:data, {_, data}}} ->
            data = [data | buffer] |> Enum.reverse() |> IO.iodata_to_binary()
            {fun.(acc, {:cont, data}), status}
        after
          0 -> {acc, status}
        end
    end
  end

  defp cmd_opts([{:into, any} | t], opts, _into, line),
    do: cmd_opts(t, opts, any, line)

  defp cmd_opts([{:cd, bin} | t], opts, into, line) when is_binary(bin),
    do: cmd_opts(t, [{:cd, bin} | opts], into, line)

  defp cmd_opts([{:arg0, bin} | t], opts, into, line) when is_binary(bin),
    do: cmd_opts(t, [{:arg0, bin} | opts], into, line)

  defp cmd_opts([{:stderr_to_stdout, true} | t], opts, into, line),
    do: cmd_opts(t, [:stderr_to_stdout | opts], into, line)

  defp cmd_opts([{:stderr_to_stdout, false} | t], opts, into, line),
    do: cmd_opts(t, opts, into, line)

  defp cmd_opts([{:parallelism, bool} | t], opts, into, line) when is_boolean(bool),
    do: cmd_opts(t, [{:parallelism, bool} | opts], into, line)

  defp cmd_opts([{:env, enum} | t], opts, into, line),
    do: cmd_opts(t, [{:env, validate_env(enum)} | opts], into, line)

  defp cmd_opts([{:lines, max_line_length} | t], opts, into, _line)
       when is_integer(max_line_length) and max_line_length > 0,
       do: cmd_opts(t, [{:line, max_line_length} | opts], into, true)

  defp cmd_opts([{key, val} | _], _opts, _into, _line),
    do: raise(ArgumentError, "invalid option #{inspect(key)} with value #{inspect(val)}")

  defp cmd_opts([], opts, into, line),
    do: {into, line, opts}

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
  point in time. This is not strictly monotonically increasing. Multiple
  sequential calls of the function may return the same value.

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
  starts and stays the same until the runtime is stopped, but could differ
  the next time the runtime is started on the same machine. For this reason,
  you should use this function to convert `:native` time units to a predictable
  unit before you display them to humans.

  To determine how many seconds the `:native` unit represents in your current
  runtime, you can call this function to convert 1 second to the `:native`
  time unit: `System.convert_time_unit(1, :second, :native)`.
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
  offset, added to an Erlang monotonic time (for instance, one obtained with
  `monotonic_time/1`), gives the Erlang system time that corresponds
  to that monotonic time.
  """
  @spec time_offset(time_unit) :: integer
  def time_offset(unit) do
    :erlang.time_offset(normalize_time_unit(unit))
  end

  @doc """
  Returns the current operating system (OS) time.

  The result is returned in the `:native` time unit.

  This time may be adjusted forwards or backwards in time
  with no limitation and is not monotonic.

  Inlined by the compiler.
  """
  @spec os_time() :: integer
  @doc since: "1.3.0"
  def os_time do
    :os.system_time()
  end

  @doc """
  Returns the current operating system (OS) time in the given time `unit`.

  This time may be adjusted forwards or backwards in time
  with no limitation and is not monotonic.
  """
  @spec os_time(time_unit | :native) :: integer
  @doc since: "1.3.0"
  def os_time(unit) do
    :os.system_time(normalize_time_unit(unit))
  end

  @doc """
  Returns the Erlang/OTP release number.
  """
  @spec otp_release :: String.t()
  @doc since: "1.3.0"
  def otp_release do
    :erlang.list_to_binary(:erlang.system_info(:otp_release))
  end

  @doc """
  Returns the number of schedulers in the VM.
  """
  @spec schedulers :: pos_integer
  @doc since: "1.3.0"
  def schedulers do
    :erlang.system_info(:schedulers)
  end

  @doc """
  Returns the number of schedulers online in the VM.
  """
  @spec schedulers_online :: pos_integer
  @doc since: "1.3.0"
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
    IO.warn_once(
      {__MODULE__, unit},
      fn ->
        "deprecated time unit: #{inspect(unit)}. A time unit should be " <>
          ":second, :millisecond, :microsecond, :nanosecond, or a positive integer"
      end,
      _stacktrace_drop_levels = 4
    )

    replacement_unit
  end
end
