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

  Shortcut for `c:cmd/2` with empty options.
  """
  @callback cmd(command :: String.t()) :: integer

  @doc """
  Executes the given command and returns its exit status.

  ## Options

  This callback should support the following options:

    * `:print_app` - when `false`, does not print the app name
      when the command outputs something

    * `:stderr_to_stdout` - when `false`, does not redirect
      stderr to stdout

    * `:quiet` - when `true`, do not print the command output

    * `:env` - environment options to the executed command

    * `:cd` *(since v1.11.0)* - the directory to run the command in

  All the built-in shells support these.
  """
  @callback cmd(command :: String.t(), options :: keyword) :: integer

  @doc """
  Prompts the user for input.
  """
  @callback prompt(message :: binary) :: binary

  @doc """
  Prompts the user for confirmation.

  Shortcut for `yes?/2` with empty options.
  """
  @callback yes?(message :: binary) :: boolean

  @doc """
  Prompts the user for confirmation.

  ## Options

    * `:default` - `:yes` or `:no` (the default is `:yes`)
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
  @spec printable_app_name() :: atom | nil
  def printable_app_name do
    Mix.ProjectStack.printable_app_name()
  end

  @doc """
  Executes the given `command` and invokes the `callback` for the streamed response.

  `command` is either a string, to be executed as a `System.shell/2` command,
  or a `{executable, args}` to be executed via `System.cmd/3`.

  `callback` takes the output data of the command. Its return value is ignored.

  This function is most commonly used by `Mix.Shell` implementations but can
  also be invoked directly.

  ## Options

    * `:cd` *(since v1.11.0)* - the directory to run the command in

    * `:stderr_to_stdout` - redirects stderr to stdout, defaults to true, unless use_stdio is set to false

    * `:use_stdio` - controls whether the command should use stdin / stdout / stdrr, defaults to true

    * `:env` - a list of environment variables, defaults to `[]`

    * `:quiet` - overrides the callback to no-op

  """
  @spec cmd(String.t() | {String.t(), [String.t()]}, keyword, (binary -> term)) ::
          exit_status :: non_neg_integer
  def cmd(command, options \\ [], callback) when is_function(callback, 1) do
    callback =
      if options[:quiet] do
        fn x -> x end
      else
        callback
      end

    use_stdio = Keyword.get(options, :use_stdio, true)

    options =
      options
      |> Keyword.take([:cd, :stderr_to_stdout, :env, :use_stdio])
      |> Keyword.put(:into, %Mix.Shell{callback: callback})
      |> Keyword.put_new(:stderr_to_stdout, use_stdio)

    {_, status} =
      case command do
        {command, args} -> System.cmd(command, args, options)
        command when is_binary(command) -> System.shell(command, options)
      end

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
