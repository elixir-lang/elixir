defmodule Mix.Shell do
  @moduledoc """
  Defines Mix.Shell contract.
  """

  use Behaviour

  @doc """
  Informs the given message.
  """
  defcallback info(message :: binary) :: any

  @doc """
  Warns about the given error message.
  """
  defcallback error(message :: binary) :: any

  @doc """
  Asks the user for confirmation.
  """
  defcallback yes?(message :: binary) :: any

  @doc """
  Executes the given command and returns
  its exit status.
  """
  defcallback cmd(command :: binary) :: integer

  @doc """
  Returns if we should output application name to shell.
  """
  def output_app? do
    Mix.Server.call(:output_app?)
  end

  @doc """
  An implementation of the command callback that
  is shared across different shells.
  """
  def cmd(command, callback) do
    port = Port.open({ :spawn, shell_command(command) },
      [:stream, :binary, :exit_status, :hide, :use_stdio, :stderr_to_stdout])
    do_cmd(port, callback)
  end

  defp do_cmd(port, callback) do
    receive do
      { ^port, { :data, data } } ->
        callback.(data)
        do_cmd(port, callback)
      { ^port, { :exit_status, status } } ->
        status
    end
  end

  # Finding shell command logic from :os.cmd in OTP
  # https://github.com/erlang/otp/blob/8deb96fb1d017307e22d2ab88968b9ef9f1b71d0/lib/kernel/src/os.erl#L184
  defp shell_command(command) do
    case :os.type do
      { :unix, _ } ->
        command = command
          |> String.replace("\"", "\\\"")
          |> binary_to_list # We need to send bytes, not chars
        %c(sh -c "#{command}")

      { :win32, osname } ->
        command = to_char_list(command)
        case { System.get_env("COMSPEC"), osname } do
          { nil, :windows } -> 'command.com /c #{command}'
          { nil, _ }        -> 'cmd /c #{command}'
          { cmd, _ }        -> '#{cmd} /c #{command}'
        end
    end
  end
end
