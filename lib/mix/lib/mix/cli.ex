defmodule Mix.CLI do
  @moduledoc false

  @doc """
  Runs Mix according to the command line arguments.
  """
  def main(args \\ System.argv) do
    Mix.Local.append_archives
    Mix.Local.append_paths

    if env_variable_activated?("MIX_QUIET"), do: Mix.shell(Mix.Shell.Quiet)
    if env_variable_activated?("MIX_DEBUG"), do: Mix.debug(true)

    case check_for_shortcuts(args) do
      :help ->
        proceed(["help"])
      :version ->
        display_version()
      nil ->
        proceed(args)
    end
  end

  defp proceed(args) do
    load_dot_config()
    load_mixfile()
    {task, args} = get_task(args)
    ensure_hex(task)
    change_env(task)
    run_task(task, args)
  end

  defp load_mixfile() do
    file = System.get_env("MIX_EXS") || "mix.exs"
    _ = if File.regular?(file) do
      Code.load_file(file)
    end
  end

  defp get_task(["-" <> _ | _]) do
    Mix.shell.error "** (Mix) Mix requires a task name when passing flags, " <>
                    "try invoking \"mix #{Mix.Project.config[:default_task]}\" instead"
    exit({:shutdown, 1})
  end

  defp get_task([h | t]) do
    {h, t}
  end

  defp get_task([]) do
    case Mix.Project.get do
      nil ->
        Mix.shell.error "** (Mix) \"mix\" with no arguments must be executed on a directory with a mix.exs file"
        Mix.shell.info """

        Usage: mix [task]

        Examples:

            mix             - Invokes the default task (current: "mix run")
            mix new         - Creates a new Elixir project
            mix help        - Lists all available tasks
            mix help TASK   - Prints documentation for a given task
        """
        exit({:shutdown, 1})
      _ ->
        {Mix.Project.config[:default_task], []}
    end
  end

  defp run_task(name, args) do
    try do
      ensure_no_slashes(name)
      Mix.Task.run "loadconfig"
      Mix.Task.run name, args
    rescue
      # We only rescue exceptions in the Mix namespace, all
      # others pass through and will explode on the users face
      exception ->
        stacktrace = System.stacktrace

        if Map.get(exception, :mix) && not Mix.debug? do
          mod = exception.__struct__ |> Module.split() |> Enum.at(0, "Mix")
          Mix.shell.error "** (#{mod}) #{Exception.message(exception)}"
          exit({:shutdown, 1})
        else
          reraise exception, stacktrace
        end
    end
  end

  defp env_variable_activated?(name) do
    System.get_env(name) in ~w(1 true)
  end

  defp ensure_hex("local.hex"),
    do: :ok
  defp ensure_hex(_task),
    do: Mix.Hex.ensure_updated?()

  defp ensure_no_slashes(task) do
    if String.contains?(task, "/") do
      raise Mix.NoTaskError, task: task
    end
  end

  defp change_env(task) do
    if env = preferred_cli_env(task) do
      Mix.env(env)
      if project = Mix.Project.pop do
        %{name: name, file: file} = project
        Mix.Project.push name, file
      end
    end
  end

  defp preferred_cli_env(task) do
    if System.get_env("MIX_ENV") do
      nil
    else
      task = String.to_atom(task)
      Mix.Project.config[:preferred_cli_env][task] || Mix.Task.preferred_cli_env(task)
    end
  end

  defp load_dot_config do
    path = Path.join(Mix.Utils.mix_home, "config.exs")
    if File.regular?(path) do
      Mix.Task.run "loadconfig", [path]
    end
  end

  defp display_version() do
    IO.puts :erlang.system_info(:system_version)
    IO.puts "Mix " <> System.build_info[:build]
  end

  # Check for --help or --version in the args
  defp check_for_shortcuts([first_arg | _]) when first_arg in ["--help", "-h"],
    do: :help

  defp check_for_shortcuts([first_arg | _]) when first_arg in ["--version", "-v"],
    do: :version

  defp check_for_shortcuts(_), do: nil
end
