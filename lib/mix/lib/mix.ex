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

  use Application.Behaviour

  @doc false
  def start do
    :application.start(:elixir)
    :application.start(:mix)
  end

  @doc false
  def start(_, []) do
    res = Mix.Sup.start_link
    if env = System.get_env("MIX_ENV") do
      env(binary_to_atom env)
    end
    res
  end

  @doc """
  Returns the mix environment.
  """
  def env do
    # env is not available on bootstrapping
    case :application.get_env(:mix, :env) do
      {:ok, env} -> env
      :undefined -> :dev
    end
  end

  @doc """
  Changes the current mix env.

  Be careful when invoking this function as any project
  configuration won't be reloaded.
  """
  def env(env) when is_atom(env) do
    :application.set_env(:mix, :env, env)
  end

  @doc """
  The shell is a wrapper for doing IO.

  It contains conveniences for asking the user information,
  printing status and so forth. It is also swappable,
  allowing developers to use a test shell that simply sends the
  messages to the current process.
  """
  def shell do
    case :application.get_env(:mix, :shell) do
      {:ok, shell} -> shell
      :undefined -> Mix.Shell.IO
    end
  end

  @doc """
  Sets the current shell.
  """
  def shell(shell) do
    :application.set_env(:mix, :shell, shell)
  end

  @doc false
  def project do
    # IO.write :stderr, "Mix.project/0 is deprecated, please use Mix.Project.config/0 instead\n#{Exception.format_stacktrace}"
    Mix.Project.config
  end
end
