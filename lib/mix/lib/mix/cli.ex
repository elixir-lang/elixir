defmodule Mix.CLI do
  @moduledoc false

  @doc """
  Runs Mix according to the command line arguments.
  """
  def run(args // System.argv) do
    case check_for_shortcuts(args) do
      :help ->
        display_banner()
      :version ->
        display_version()
      nil ->
        proceed(args)
    end
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
    { Mix.project[:default_task], [] }
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

  defp proceed(args) do
    Mix.Local.append_archives
    Mix.Local.append_paths

    args = load_mixfile(args)
    { task, args } = get_task(args)
    change_env(task)

    if Mix.Project.get do
      Mix.Task.run "loadpaths", ["--no-deps-check", "--no-elixir-version-check"]
      Mix.Task.reenable "loadpaths"
      Mix.Task.reenable "deps.loadpaths"
    end

    run_task task, args
  end

  defp change_env(task) do
    if nil?(System.get_env("MIX_ENV")) && (env = Mix.project[:preferred_cli_env][task]) do
      Mix.env(env)
      Mix.Project.push Mix.Project.pop
    end
  end

  defp display_banner() do
    run_task "help", []
  end

  defp display_version() do
    IO.puts "Elixir #{System.version}"
  end

  # Check for --help or --version in the args
  defp check_for_shortcuts([first_arg|_]) when first_arg in
      ["--help", "-h", "-help"], do: :help

  defp check_for_shortcuts([first_arg|_]) when first_arg in
      ["--version", "-v"], do: :version

  defp check_for_shortcuts(_), do: nil
end
