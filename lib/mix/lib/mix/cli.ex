defmodule Mix.CLI do
  @moduledoc false

  defmacrop exceptions do
    [Mix.Error, Mix.NoTaskError, Mix.InvalidTaskError, Mix.NoProjectError, Mix.OutOfDateDepsError]
  end

  @doc """
  Runs Mix according to the command line arguments.
  """
  def run(args // System.argv) do
    Mix.Local.append_tasks
    do_run do_load(args)
  end

  defp do_load(args) do
    file = "mix.exs"

    if File.regular?(file) do
      Code.load_file file
      Mix.Task.run "loadpaths", ["--no-check"]
      Mix.Task.reenable "loadpaths"
      Mix.Task.reenable "deps.loadpaths"
    end

    args
  end

  defp do_run([h|t]) do
    do_task h, t
  end

  defp do_run([]) do
    do_task Mix.project[:default] || "test", []
  end

  defp do_task(name, args) do
    try do
      Mix.Task.run(name, args)
    rescue
      # We only rescue exceptions in the mix namespace, all
      # others pass through and will explode on the users face
      exception in exceptions ->
        Mix.shell.error exception.message
        exit(1)
    end
  end  
end