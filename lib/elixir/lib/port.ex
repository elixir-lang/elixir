defmodule Port do
  @moduledoc ~S"""
  Functions for interacting with the external world through ports.

  Ports provide a mechanism to start operating system processes external
  to the Erlang VM and communicate with them via message passing.

  ## Example

      iex> port = Port.open({:spawn, "cat"}, [:binary])
      iex> send(port, {self(), {:command, "hello"}})
      iex> send(port, {self(), {:command, "world"}})
      iex> flush()
      {#Port<0.1444>, {:data, "hello"}}
      {#Port<0.1444>, {:data, "world"}}
      iex> send(port, {self(), :close})
      :ok
      iex> flush()
      {#Port<0.1464>, :closed}
      :ok

  In the example above, we have created a new port that executes the
  program `cat`. `cat` is a program available on UNIX systems that
  receives data from multiple inputs and concatenates them in the output.

  After the port was created, we sent it two commands in the form of
  messages using `Kernel.send/2`. The first command has the binary payload
  of "hello" and the second has "world".

  After sending those two messages, we invoked the IEx helper `flush()`,
  which printed all messages received from the port, in this case we got
  "hello" and "world" back. Notice the messages are in binary because we
  passed the `:binary` option when opening the port in `Port.open/2`. Without
  such option, it would have yielded a list of bytes.

  Once everything was done, we closed the port.

  Elixir provides many conveniences for working with ports and some drawbacks.
  We will explore those below.

  ## Message and function APIs

  There are two APIs for working with ports. It can be either asynchronous via
  message passing, as in the example above, or by calling the functions on this
  module.

  The messages supported by ports and their counterpart function APIs are
  listed below:

    * `{pid, {:command, binary}}` - sends the given data to the port.
      See `command/3`.

    * `{pid, :close}` - closes the port. Unless the port is already closed,
      the port will reply with `{port, :closed}` message once it has flushed
      its buffers and effectively closed. See `close/1`.

    * `{pid, {:connect, new_pid}}` - sets the `new_pid` as the new owner of
      the port. Once a port is opened, the port is linked and connected to the
      caller process and communication to the port only happens through the
      connected process. This message makes `new_pid` the new connected processes.
      Unless the port is dead, the port will reply to the old owner with
      `{port, :connected}`. See `connect/2`.

  On its turn, the port will send the connected process the following messages:

    * `{port, {:data, data}}` - data sent by the port
    * `{port, :closed}` - reply to the `{pid, :close}` message
    * `{port, :connected}` - reply to the `{pid, {:connect, new_pid}}` message
    * `{:EXIT, port, reason}` - exit signals in case the port crashes. If reason
      is not `:normal`, this message will only be received if the owner process
      is trapping exits

  ## Open mechanisms

  The port can be opened through four main mechanisms.

  As a short summary, prefer to using the `:spawn` and `:spawn_executable`
  options mentioned below. The other two options, `:spawn_driver` and `:fd`
  are for advanced usage within the VM. Also consider using `System.cmd/3`
  if all you want is to execute a program and retrieve its return value.

  ### spawn

  The `:spawn` tuple receives a binary that is going to be executed as a
  full invocation. For example, we can use it to invoke "echo hello" directly:

      iex> port = Port.open({:spawn, "echo hello"}, [:binary])
      iex> flush()
      {#Port<0.1444>, {:data, "hello\n"}}

  `:spawn` will retrieve the program name from the argument and traverse your
  operating system `$PATH` environment variable looking for a matching program.

  Although the above is handy, it means it is impossible to invoke an executable
  that has whitespaces on its name or in any of its arguments. For those reasons,
  most times it is preferable to execute `:spawn_executable`.

  ### spawn_executable

  Spawn executable is a more restricted and explicit version of spawn. It expects
  full file paths to the executable you want to execute. If they are in your `$PATH`,
  they can be retrieved by calling `System.find_executable/1`:

      iex> path = System.find_executable("echo")
      iex> port = Port.open({:spawn_executable, path}, [:binary, args: ["hello world"]])
      iex> flush()
      {#Port<0.1380>, {:data, "hello world\n"}}

  When using `:spawn_executable`, the list of arguments can be passed via
  the `:args` option as done above. For the full list of options, see the
  documentation for the Erlang function `:erlang.open_port/2`.

  ### fd

  The `:fd` name option allows developers to access `in` and `out` file
  descriptors used by the Erlang VM. You would use those only if you are
  reimplementing core part of the Runtime System, such as the `:user` and
  `:shell` processes.

  ## Zombie operating system processes

  A port can be closed via the `close/1` function or by sending a `{pid, :close}`
  message. However, if the VM crashes, a long-running program started by the port
  will have its stdin and stdout channels closed but **it won't be automatically
  terminated**.

  While most UNIX command line tools will exit once its communication channels
  are closed, not all command line applications will do so. While we encourage
  graceful termination by detecting if stdin/stdout has been closed, we do not
  always have control over how third-party software terminates. In those cases,
  you can wrap the application in a script that checks for stdin. Here is such
  script in Bash:

      #!/bin/bash
      "$@" &
      pid=$!
      while read line ; do
        :
      done
      kill -KILL $pid

  Now instead of:

      Port.open(
        {:spawn_executable, "/path/to/program"},
        args: ["a", "b", "c"]
      )

  You may invoke:

      Port.open(
        {:spawn_executable, "/path/to/wrapper"},
        args: ["/path/to/program", "a", "b", "c"]
      )

  """

  @type name ::
          {:spawn, charlist | binary}
          | {:spawn_driver, charlist | binary}
          | {:spawn_executable, charlist | atom}
          | {:fd, non_neg_integer, non_neg_integer}

  @doc """
  Opens a port given a tuple `name` and a list of `options`.

  The module documentation above contains documentation and examples
  for the supported `name` values, summarized below:

    * `{:spawn, command}` - runs an external program. `command` must contain
      the program name and optionally a list of arguments separated by space.
      If passing programs or arguments with space in their name, use the next option.
    * `{:spawn_executable, filename}` - runs the executable given by the absolute
      file name `filename`. Arguments can be passed via the `:args` option.
    * `{:spawn_driver, command}` - spawns so-called port drivers.
    * `{:fd, fd_in, fd_out}` - accesses file descriptors, `fd_in` and `fd_out`
      opened by the VM.

  For more information and the list of options, see `:erlang.open_port/2`.

  Inlined by the compiler.
  """
  @spec open(name, list) :: port
  def open(name, options) do
    :erlang.open_port(name, options)
  end

  @doc """
  Closes the `port`.

  For more information, see `:erlang.port_close/1`.

  Inlined by the compiler.
  """
  @spec close(port) :: true
  def close(port) do
    :erlang.port_close(port)
  end

  @doc """
  Sends `data` to the port driver `port`.

  For more information, see `:erlang.port_command/2`.

  Inlined by the compiler.
  """
  @spec command(port, iodata, [:force | :nosuspend]) :: boolean
  def command(port, data, options \\ []) do
    :erlang.port_command(port, data, options)
  end

  @doc """
  Associates the `port` identifier with a `pid`.

  For more information, see `:erlang.port_connect/2`.

  Inlined by the compiler.
  """
  @spec connect(port, pid) :: true
  def connect(port, pid) do
    :erlang.port_connect(port, pid)
  end

  @doc """
  Returns information about the `port` or `nil` if the port is closed.

  For more information, see `:erlang.port_info/1`.
  """
  def info(port) do
    nillify(:erlang.port_info(port))
  end

  @doc """
  Returns information about the `port` or `nil` if the port is closed.

  For more information, see `:erlang.port_info/2`.
  """
  @spec info(port, atom) :: {atom, term} | nil
  def info(port, spec)

  def info(port, :registered_name) do
    case :erlang.port_info(port, :registered_name) do
      :undefined -> nil
      [] -> {:registered_name, []}
      other -> other
    end
  end

  def info(port, item) do
    nillify(:erlang.port_info(port, item))
  end

  @doc """
  Starts monitoring the given `port` from the calling process.

  Once the monitored port process dies, a message is delivered to the
  monitoring process in the shape of:

      {:DOWN, ref, :port, object, reason}

  where:

    * `ref` is a monitor reference returned by this function;
    * `object` is either the `port` being monitored (when monitoring by port ID)
    or `{name, node}` (when monitoring by a port name);
    * `reason` is the exit reason.

  See `:erlang.monitor/2` for more information.

  Inlined by the compiler.
  """
  @doc since: "1.6.0"
  @spec monitor(port | {name, node} | name) :: reference when name: atom
  def monitor(port) do
    :erlang.monitor(:port, port)
  end

  @doc """
  Demonitors the monitor identified by the given `reference`.

  If `monitor_ref` is a reference which the calling process
  obtained by calling `monitor/1`, that monitoring is turned off.
  If the monitoring is already turned off, nothing happens.

  See `:erlang.demonitor/2` for more information.

  Inlined by the compiler.
  """
  @doc since: "1.6.0"
  @spec demonitor(reference, options :: [:flush | :info]) :: boolean
  defdelegate demonitor(monitor_ref, options \\ []), to: :erlang

  @doc """
  Returns a list of all ports in the current node.

  Inlined by the compiler.
  """
  @spec list :: [port]
  def list do
    :erlang.ports()
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other), do: other
end
