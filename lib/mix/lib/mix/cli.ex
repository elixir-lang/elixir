defmodule Mix.CLI do
  @moduledoc false

  defmacrop exceptions do
    [Mix.Error, Mix.NoTaskError, Mix.InvalidTaskError, Mix.NoProjectError]
  end

  @doc """
  Runs Mix according to the command line arguments.
  """
  def run(args // System.argv) do
    do_run do_load(args)
  end

  defp do_load(args) do
    file = "mix.exs"

    if File.regular?(file) do
      Code.require_file file
    end

    args
  end

  defp do_run([h|t]) do
    do_task h, t
  end

  defp do_run([]) do
    do_task Mix.Project.config[:default] || "test", []
  end

  defp do_task(name, args) do
    try do
      Mix.Task.run(name, args)
    rescue
      # We only rescue exceptions in the mix namespace, all
      # others pass through and will explode on the users face
      exception in exceptions -> Mix.shell.error exception.message
    end
  end  
end