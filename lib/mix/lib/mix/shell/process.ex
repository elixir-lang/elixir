defmodule Mix.Shell.Process do
  @moduledoc """
  Mix shell that uses the current process mailbox for communication.

  This module provides a Mix shell implementation that uses
  the current process mailbox for communication instead of IO.

  As an example, when `Mix.shell.info("hello")` is called,
  the following message will be sent to the calling process:

      {:mix_shell, :info, ["hello"]}

  This is mainly useful in tests, allowing us to assert
  if given messages were received or not instead of performing
  checks on some captured IO. Since we need to guarantee a clean
  slate between tests, there is also a `flush/1` function
  responsible for flushing all `:mix_shell` related messages
  from the process inbox.

  ## Examples

      Mix.shell().info("hello")

      receive do
        {:mix_shell, :info, [msg]} -> msg
      end
      #=> "hello"

      send(self(), {:mix_shell_input, :prompt, "Pretty cool"})
      Mix.shell().prompt?("How cool was that?!")
      #=> "Pretty cool"

  """

  @behaviour Mix.Shell

  @doc """
  Flushes all `:mix_shell` and `:mix_shell_input` messages from the current process.

  If a callback is given, it is invoked for each received message.

  ## Examples

      flush(&IO.inspect(&1))

  """
  def flush(callback \\ fn x -> x end) do
    receive do
      {:mix_shell, _, _} = message ->
        callback.(message)
        flush(callback)

      {:mix_shell_input, _, _} = message ->
        callback.(message)
        flush(callback)
    after
      0 -> :done
    end
  end

  @doc """
  Prints the current application if it
  was not printed yet.
  """
  def print_app do
    if name = Mix.Shell.printable_app_name() do
      send(self(), {:mix_shell, :info, ["==> #{name}"]})
    end
  end

  @doc """
  Forwards the message to the current process.
  """
  def info(message) do
    print_app()
    send(self(), {:mix_shell, :info, [format(message)]})
    :ok
  end

  @doc """
  Forwards the error to the current process.
  """
  def error(message) do
    print_app()
    send(self(), {:mix_shell, :error, [format(message)]})
    :ok
  end

  defp format(message) do
    message |> IO.ANSI.format(false) |> IO.iodata_to_binary()
  end

  @doc """
  Forwards the message to the current process.

  It also checks the inbox for an input message matching:

      {:mix_shell_input, :prompt, value}

  If one does not exist, it will abort since there was no shell
  process inputs given. `value` must be a string.

  ## Examples

  The following will answer with `"Meg"` to the prompt
  `"What's your name?"`:

      # The response is sent before calling prompt/1 so that prompt/1 can read it
      send(self(), {:mix_shell_input, :prompt, "Meg"})
      Mix.shell().prompt("What's your name?")

  """
  def prompt(message) do
    print_app()
    send(self(), {:mix_shell, :prompt, [message]})

    receive do
      {:mix_shell_input, :prompt, response} -> response
    after
      0 -> raise "no shell process input given for prompt/1"
    end
  end

  @doc """
  Forwards the message to the current process.

  It also checks the inbox for an input message matching:

      {:mix_shell_input, :yes?, value}

  If one does not exist, it will abort since there was no shell
  process inputs given. `value` must be `true` or `false`.

  ## Example

      # Send the response to self() first so that yes?/1 will be able to read it
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.shell().yes?("Are you sure you want to continue?")

  """
  def yes?(message) do
    print_app()
    send(self(), {:mix_shell, :yes?, [message]})

    receive do
      {:mix_shell_input, :yes?, response} -> response
    after
      0 -> raise "no shell process input given for yes?/1"
    end
  end

  @doc """
  Executes the given command and forwards its messages to
  the current process.
  """
  def cmd(command, opts \\ []) do
    print_app? = Keyword.get(opts, :print_app, true)

    Mix.Shell.cmd(command, opts, fn data ->
      if print_app?, do: print_app()
      send(self(), {:mix_shell, :run, [data]})
    end)
  end
end
