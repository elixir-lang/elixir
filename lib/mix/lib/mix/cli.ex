defmodule Mix.CLI do
  @moduledoc false

  @doc """
  Runs Mix according to the command line arguments.
  """
  def main(args \\ System.argv()) do
    Mix.Local.append_archives()
    Mix.Local.append_paths()

    if env_variable_activated?("MIX_QUIET"), do: Mix.shell(Mix.Shell.Quiet)
    if env_variable_activated?("MIX_DEBUG"), do: Mix.debug(true)

    case check_for_shortcuts(args) do
      :help ->
        Mix.shell().info("Mix is a build tool for Elixir")
        display_usage()

      :version ->
        display_version()

      nil ->
        proceed(args)
    end
  end

  defp proceed(args) do
    load_dot_config()
    load_mix_exs()
    {task, args} = get_task(args)
    ensure_hex(task)
    maybe_change_env_and_target(task)
    run_task(task, args)
  end

  defp load_mix_exs() do
    file = System.get_env("MIX_EXS") || "mix.exs"

    if File.regular?(file) do
      Code.compile_file(file)
    end
  end

  defp get_task(["-" <> _ | _]) do
    task = "mix #{Mix.Project.config()[:default_task]}"

    Mix.shell().error(
      "** (Mix) Mix only recognizes the options --help and --version.\n" <>
        "You may have wanted to invoke a task instead, such as #{inspect(task)}"
    )

    display_usage()
    exit({:shutdown, 1})
  end

  defp get_task([h | t]) do
    {h, t}
  end

  defp get_task([]) do
    case Mix.Project.get() do
      nil ->
        Mix.shell().error(
          "** (Mix) \"mix\" with no arguments must be executed in a directory with a mix.exs file"
        )

        display_usage()
        exit({:shutdown, 1})

      _ ->
        {Mix.Project.config()[:default_task], []}
    end
  end

  defp run_task(name, args) do
    try do
      ensure_no_slashes(name)
      Mix.Task.run("loadconfig")
      Mix.Task.run(name, args)
    rescue
      # We only rescue exceptions in the Mix namespace, all
      # others pass through and will explode on the users face
      exception ->
        if Map.get(exception, :mix) && not Mix.debug?() do
          mod = exception.__struct__ |> Module.split() |> Enum.at(0, "Mix")
          Mix.shell().error("** (#{mod}) #{Exception.message(exception)}")
          exit({:shutdown, 1})
        else
          reraise exception, __STACKTRACE__
        end
    end
  end

  defp env_variable_activated?(name) do
    System.get_env(name) in ~w(1 true)
  end

  defp ensure_hex("local.hex"), do: :ok
  defp ensure_hex(_task), do: Mix.Hex.ensure_updated?()

  defp ensure_no_slashes(task) do
    if String.contains?(task, "/") do
      raise Mix.NoTaskError, task: task
    end
  end

  defp maybe_change_env_and_target(task) do
    task = String.to_atom(task)
    config = Mix.Project.config()

    env = preferred_cli_env(task, config)
    target = preferred_cli_target(task, config)
    env && Mix.env(env)
    target && Mix.target(target)

    if env || target do
      reload_project()
    end
  end

  defp reload_project() do
    if project = Mix.Project.pop() do
      %{name: name, file: file} = project
      Mix.Project.push(name, file)
    end
  end

  defp preferred_cli_env(task, config) do
    if System.get_env("MIX_ENV") do
      nil
    else
      config[:preferred_cli_env][task] || Mix.Task.preferred_cli_env(task)
    end
  end

  defp preferred_cli_target(task, config) do
    config[:preferred_cli_target][task]
  end

  defp load_dot_config do
    path = Path.join(Mix.Utils.mix_config(), "config.exs")

    if File.regular?(path) do
      Mix.Task.run("loadconfig", [path])
    end
  end

  defp display_version do
    IO.puts(:erlang.system_info(:system_version))
    IO.puts("Mix " <> System.build_info()[:build])
  end

  defp display_usage do
    Mix.shell().info("""

    Usage: mix [task]

    Examples:

        mix             - Invokes the default task (mix run) in a project
        mix new PATH    - Creates a new Elixir project at the given path
        mix help        - Lists all available tasks
        mix help TASK   - Prints documentation for a given task

    The --help and --version options can be given instead of a task for usage and versioning information.
    """)
  end

  # Check for --help or --version in the args
  defp check_for_shortcuts([arg]) when arg in ["--help", "-h"], do: :help

  defp check_for_shortcuts([arg]) when arg in ["--version", "-v"], do: :version

  defp check_for_shortcuts(_), do: nil
end
