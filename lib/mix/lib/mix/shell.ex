defmodule Mix.Shell do
  @moduledoc """
  Defines `Mix.Shell` contract.
  """

  @doc """
  Prints the given ANSI message to the shell.
  """
  @callback info(message :: IO.ANSI.ansidata) :: :ok

  @doc """
  Prints the given ANSI error to the shell.
  """
  @callback error(message :: IO.ANSI.ansidata) :: :ok

  @doc """
  Writes data directly into the shell.
  """
  @callback write(message :: binary) :: :ok

  @doc """
  Prompts the user for input.
  """
  @callback prompt(message :: binary) :: binary

  @doc """
  Prompts the user for confirmation.
  """
  @callback yes?(message :: binary) :: boolean

  @doc """
  Prints the current application to the shell if
  it was not printed yet.
  """
  @callback print_app() :: :ok

  @doc """
  A collectable shell struct.
  """
  defstruct [print_app?: true]

  defimpl Collectable do
    def into(%Mix.Shell{print_app?: print_app?} = original) do
      fun = fn
        {:cont, shell}, {:cont, data} ->
          shell.write(data)
          {:cont, shell}
        {:print, shell}, {:cont, data} ->
          shell.print_app()
          shell.write(data)
          {:cont, shell}
        _, _ ->
          original
      end

      case print_app? do
        true -> {{:print, Mix.shell}, fun}
        false -> {{:cont, Mix.shell}, fun}
      end
    end
  end

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
    Mix.ProjectStack.printable_app_name
  end

  @doc false
  # TODO: Deprecate on Mix v1.8
  def cmd(command, options, callback) when is_function(callback, 1) do
    callback =
      if Keyword.get(options, :quiet, false) do
        fn x -> x end
      else
        callback
      end

    fun = fn
      _, {:cont, data} -> callback.(data)
      _, _ -> :ok
    end

    cmd(command, options, :ok, fun)
  end

  def cmd(command, callback) when is_function(callback, 1) do
    cmd(command, [], callback)
  end

  @doc """
  Executes the given `string` as a shell command.

    * `:into` - a collectable to print the result to, defaults to `""`
    * `:stderr_to_stdout` - redirects stderr to stdout, defaults to true
    * `:env` - a list of environment variables, defaults to `[]`

  """
  def cmd(command, options) when is_binary(command) and is_list(options) do
    collectable = Keyword.get(options, :into, "")
    {acc, callback} = Collectable.into(collectable)
    cmd(command, options, acc, callback)
  end

  defp cmd(command, options, acc, callback) do
    env = validate_env(Keyword.get(options, :env, []))

    args =
      if Keyword.get(options, :stderr_to_stdout, true) do
        [:stderr_to_stdout]
      else
        []
      end

    port = Port.open({:spawn, shell_command(command)},
                     [:stream, :binary, :exit_status, :hide, :use_stdio, {:env, env} | args])

    port_read(port, acc, callback)
  end

  defp port_read(port, acc, callback) do
    receive do
      {^port, {:data, data}} ->
        port_read(port, callback.(acc, {:cont, data}), callback)
      {^port, {:exit_status, status}} ->
        callback.(acc, :done)
        status
    end
  end

  # Finding shell command logic from :os.cmd in OTP
  # https://github.com/erlang/otp/blob/8deb96fb1d017307e22d2ab88968b9ef9f1b71d0/lib/kernel/src/os.erl#L184
  defp shell_command(command) do
    case :os.type do
      {:unix, _} ->
        command =
          command
          |> String.replace("\"", "\\\"")
          |> String.to_charlist
        'sh -c "' ++ command ++ '"'

      {:win32, osname} ->
        command = '"' ++ String.to_charlist(command) ++ '"'
        case {System.get_env("COMSPEC"), osname} do
          {nil, :windows} -> 'command.com /s /c ' ++ command
          {nil, _}        -> 'cmd /s /c ' ++ command
          {cmd, _}        -> '#{cmd} /s /c ' ++ command
        end
    end
  end

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
end
