defmodule Mix.Shell do
  @moduledoc """
  Defines Mix.Shell contract.
  """

  use Behaviour

  @doc """
  Informs the given message.
  """
  defcallback info(message :: IO.ANSI.ansidata) :: any

  @doc """
  Warns about the given error message.
  """
  defcallback error(message :: IO.ANSI.ansidata) :: any

  @doc """
  Prompts the user for input.
  """
  defcallback prompt(message :: String.t) :: String.t

  @doc """
  Asks the user for confirmation.
  """
  defcallback yes?(message :: String.t) :: boolean

  @doc """
  Executes the given command and returns its exit status.
  """
  defcallback cmd(command :: String.t) :: integer

  @doc """
  Executes the given command and returns its exit status.

  ## Options

    * `:print_app` - when false, does not print the app name
      when the command outputs something

    * `:stderr_to_stdout` - when false, does not redirect
      stderr to stdout

    * `:quiet` - when true, do not print the command output

  """
  defcallback cmd(command :: String.t, options :: Keyword.t) :: integer

  @doc """
  Prints the current application to shell if
  it was not printed yet.
  """
  defcallback print_app() :: any

  @doc """
  Returns the printable app name.

  This function returns the current application name
  but only if the application name should be printed.

  Calling this function automatically toggles its value
  to false until the current project is re-entered. The
  goal is to exactly avoid printing the application name
  multiple times.
  """
  def printable_app_name do
    Mix.ProjectStack.printable_app_name
  end

  @doc """
  An implementation of the command callback that
  is shared across different shells.
  """
  def cmd(command, options, callback) do
    args =
      if Keyword.get(options, :stderr_to_stdout, true) do
        [:stderr_to_stdout]
      else
        []
      end

    callback =
      if Keyword.get(options, :quiet, false) do
        fn x -> x end
      else
        callback
      end

    port = Port.open({:spawn, shell_command(command)},
                     [:stream, :binary, :exit_status, :hide, :use_stdio|args])

    do_cmd(port, callback)
  end

  defp do_cmd(port, callback) do
    receive do
      {^port, {:data, data}} ->
        callback.(data)
        do_cmd(port, callback)
      {^port, {:exit_status, status}} ->
        status
    end
  end

  # Finding shell command logic from :os.cmd in OTP
  # https://github.com/erlang/otp/blob/8deb96fb1d017307e22d2ab88968b9ef9f1b71d0/lib/kernel/src/os.erl#L184
  defp shell_command(command) do
    case :os.type do
      {:unix, _} ->
        command = command
          |> String.replace("\"", "\\\"")
          |> :binary.bin_to_list
        'sh -c "' ++ command ++ '"'

      {:win32, osname} ->
        command = :binary.bin_to_list(command)
        case {System.get_env("COMSPEC"), osname} do
          {nil, :windows} -> 'command.com /c ' ++ command
          {nil, _}        -> 'cmd /c ' ++ command
          {cmd, _}        -> '#{cmd} /c ' ++ command
        end
    end
  end
end
