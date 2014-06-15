defmodule Mix do
  @moduledoc """
  Mix is a build tool that provides tasks for creating, compiling and
  testing Elixir projects. Mix is inspired by the Leiningen
  build tool for Clojure and was written by one of its contributors.

  This module works as a facade for accessing the most common functionality
  in Elixir, such as the shell and the current project configuration.

  For getting started with Elixir, checkout out the guide available on
  [Elixir's website](http://elixir-lang.org).
  """

  use Application

  @doc false
  def start do
    Application.start(:elixir)
    Application.start(:mix)
  end

  @doc false
  def start(_type, []) do
    import Supervisor.Spec

    children = [
      worker(Mix.TasksServer, []),
      worker(Mix.ProjectStack, [])
    ]

    opts = [strategy: :one_for_one, name: Mix.Supervisor]
    stat = Supervisor.start_link(children, opts)

    if env = System.get_env("MIX_ENV") do
      env(String.to_atom env)
    end

    stat
  end

  @doc """
  Returns the mix environment.
  """
  def env do
    # env is not available on bootstrapping, so set a :dev default
    Application.get_env(:mix, :env, :dev)
  end

  @doc """
  Changes the current mix env.

  Be careful when invoking this function as any project
  configuration won't be reloaded.
  """
  def env(env) when is_atom(env) do
    Application.put_env(:mix, :env, env)
  end

  @doc """
  The shell is a wrapper for doing IO.

  It contains conveniences for asking the user information,
  printing status and so forth. It is also swappable,
  allowing developers to use a test shell that simply sends the
  messages to the current process.
  """
  def shell do
    Application.get_env(:mix, :shell, Mix.Shell.IO)
  end

  @doc """
  Sets the current shell.
  """
  def shell(shell) do
    Application.put_env(:mix, :shell, shell)
  end

  @doc """
  Raises a mix error that is nicely formatted.
  """
  def raise(message) when is_binary(message) do
    Kernel.raise Mix.Error, mix: mix_info, message: message
  end

  @doc """
  Raises a mix compatible exception.

  A mix compatible exception has a `mix_error` field which mix
  uses to store the project or application name which is
  automatically by the formatting tools.
  """
  def raise(exception, opts) when is_atom(exception) do
    Kernel.raise %{exception.exception(opts) | mix: mix_info}
  end

  defp mix_info do
    case Mix.ProjectStack.peek do
      %{name: name, config: config, pos: pos} when pos > 0 ->
        if app = config[:app] do
          {:app, app}
        else
          {:project, name}
        end
      _ ->
        :none
    end
  end
end
