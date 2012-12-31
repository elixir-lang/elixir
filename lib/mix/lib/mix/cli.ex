defmodule Mix.CLI do
  @moduledoc false

  @doc """
  Runs Mix according to the command line arguments.
  """
  def run(args // System.argv) do
    Mix.Local.append_tasks

    args = load_mixfile(args)
    { task, args } = get_task(args)

    if Mix.Project.get do
      Mix.Task.run "loadpaths", ["--no-check"]
      Mix.Task.reenable "loadpaths"
      Mix.Task.reenable "deps.loadpaths"
    end

    run_task task, args
  end

  defp load_mixfile(args) do
    file = "mix.exs"

    if File.regular?(file) do
      Code.load_file file
    end

    args
  end

  defp get_task([h|t]) do
    { h, t }
  end

  defp get_task([]) do
    { Mix.project[:default] || "test", [] }
  end

  defp run_task(name, args) do
    try do
      Mix.Task.run(name, args)
    rescue
      # We only rescue exceptions in the mix namespace, all
      # others pass through and will explode on the users face
      exception ->
        stacktrace = System.stacktrace

        if function_exported?(exception, :mix_error, 0) do
          if msg = exception.message, do: Mix.shell.error "** (Mix) #{msg}"
          exit(1)
        else
          raise exception, [], stacktrace
        end
    end
  end
end