defmodule Mix.CLI do
  @moduledoc false

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
    do_task Mix.Mixfile.default_task(Mix.Mixfile.get_project), []
  end

  defp do_task(name, args) do
    try do
      Mix.Task.run(name, args)
    rescue
      exception -> IO.puts :stderr, exception.message
    end
  end  
end