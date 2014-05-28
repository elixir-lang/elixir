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
  def start(_, []) do
    res = Mix.Sup.start_link
    if env = System.get_env("MIX_ENV") do
      env(String.to_atom env)
    end
    res
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
end
