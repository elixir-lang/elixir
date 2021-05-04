defmodule Mix.Shell do
  @moduledoc """
  Defines `Mix.Shell` contract.
  """

  @doc false
  defstruct [:callback]

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
      if options[:quiet] do
        fn x -> x end
      else
        callback
      end

    options =
      options
      |> Keyword.take([:cd, :stderr_to_stdout, :env])
      |> Keyword.put(:into, %Mix.Shell{callback: callback})
      |> Keyword.put_new(:stderr_to_stdout, true)

    {_, status} = System.shell(command, options)
    status
  end

  defimpl Collectable do
    def into(%Mix.Shell{callback: fun}) do
      {:ok,
       fn
         _, {:cont, data} -> fun.(data)
         _, _ -> :ok
       end}
    end
  end
end
