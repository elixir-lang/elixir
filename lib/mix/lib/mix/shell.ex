defmodule Mix.Shell do
  @moduledoc """
  Defines `Mix.Shell` contract.
  """

  @doc """
  Prints the given ANSI message to the shell.
  """
  @callback info(message :: IO.ANSI.ansidata()) :: :ok

  @doc """
  Prints the given ANSI error to the shell.
  """
  @callback error(message :: IO.ANSI.ansidata()) :: :ok

  @doc """
  Executes the given command and returns its exit status.
  """
  @callback cmd(command :: String.t()) :: integer

  @doc """
  Executes the given command and returns its exit status.

  ## Options

    * `:print_app` - when `false`, does not print the app name
      when the command outputs something

    * `:stderr_to_stdout` - when `false`, does not redirect
      stderr to stdout

    * `:quiet` - when `true`, do not print the command output

    * `:env` - environment options to the executed command

  """
  @callback cmd(command :: String.t(), options :: keyword) :: integer

  @doc """
  Prompts the user for input.
  """
  @callback prompt(message :: binary) :: binary

  @doc """
  Prompts the user for confirmation.
  """
  @callback yes?(message :: binary, options :: keyword) :: boolean

  @doc """
  Prints the current application to the shell if
  it was not printed yet.
  """
  @callback print_app() :: :ok

  @doc """
  Returns the printable app name.

  This function returns the current application name,
  but only if the application name should be printed.

  Calling this function automatically toggles its value
  to `false` until the current project is re-entered. The
  goal is to avoid printing the application name
  multiple times.
  """
  def printable_app_name do
    Mix.ProjectStack.printable_app_name()
  end

  @doc """
  Executes the given `command` as a shell command and
  invokes the `callback` for the streamed response.

  This is most commonly used by shell implementations
  but can also be invoked directly.

  ## Options

    * `:cd` - (since v1.11.0) the directory to run the command in

    * `:stderr_to_stdout` - redirects stderr to stdout, defaults to true

    * `:env` - a list of environment variables, defaults to `[]`

    * `:quiet` - overrides the callback to no-op

  """
  def cmd(command, options \\ [], callback) when is_function(callback, 1) do
    callback =
      if Keyword.get(options, :quiet, false) do
        fn x -> x end
      else
        callback
      end

    env = validate_env(Keyword.get(options, :env, []))

    args =
      if Keyword.get(options, :stderr_to_stdout, true) do
        [:stderr_to_stdout]
      else
        []
      end

    opts =
      [:stream, :binary, :exit_status, :hide, :use_stdio, {:env, env}] ++
        args ++ Keyword.take(options, [:cd])

    port = Port.open({:spawn, shell_command(command)}, opts)
    port_read(port, callback)
  end

  defp port_read(port, callback) do
    receive do
      {^port, {:data, data}} ->
        _ = callback.(data)
        port_read(port, callback)

      {^port, {:exit_status, status}} ->
        status
    end
  end

  # Finding shell command logic from :os.cmd in OTP
  # https://github.com/erlang/otp/blob/8deb96fb1d017307e22d2ab88968b9ef9f1b71d0/lib/kernel/src/os.erl#L184
  defp shell_command(command) do
    case :os.type() do
      {:unix, _} ->
        command =
          command
          |> String.replace("\"", "\\\"")
          |> String.to_charlist()

        'sh -c "' ++ command ++ '"'

      {:win32, osname} ->
        command = '"' ++ String.to_charlist(command) ++ '"'

        case {System.get_env("COMSPEC"), osname} do
          {nil, :windows} -> 'command.com /s /c ' ++ command
          {nil, _} -> 'cmd /s /c ' ++ command
          {cmd, _} -> '#{cmd} /s /c ' ++ command
        end
    end
  end

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
end
