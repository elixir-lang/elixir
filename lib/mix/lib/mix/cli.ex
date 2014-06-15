defmodule Mix.CLI do
  @moduledoc false

  @doc """
  Runs Mix according to the command line arguments.
  """
  def main(args \\ System.argv) do
    Mix.Local.append_archives
    Mix.Local.append_paths

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
    Mix.Tasks.Local.Hex.maybe_update()
    load_dot_config()
    args = load_mixfile(args)
    {task, args} = get_task(args)
    change_env(task)
    run_task(task, args)
  end

  defp load_mixfile(args) do
    file = System.get_env("MIX_EXS") || "mix.exs"
    if File.regular?(file) do
      Code.load_file(file)
    end
    args
  end

  defp get_task(["-" <> _|_]) do
    Mix.shell.error "** (Mix) Cannot implicitly pass flags to default mix task, " <>
                    "please invoke instead: mix #{Mix.Project.config[:default_task]}"
    exit(1)
  end

  defp get_task([h|t]) do
    {h, t}
  end

  defp get_task([]) do
    {Mix.Project.config[:default_task], []}
  end

  defp run_task(name, args) do
    try do
      if Mix.Project.get do
        Mix.Task.run "loadconfig"
        Mix.Task.run "deps.loadpaths", ["--no-deps-check"]
        Mix.Task.run "loadpaths", ["--no-elixir-version-check"]
        Mix.Task.reenable "deps.loadpaths"
        Mix.Task.reenable "loadpaths"
      end

      # If the task is not available, let's try to
      # compile the repository and then run it again.
      cond do
        Mix.Task.get(name) ->
          Mix.Task.run(name, args)
        Mix.Project.get ->
          Mix.Task.run("compile")
          Mix.Task.run(name, args)
        true ->
          # Raise no task error
          Mix.Task.get!(name)
      end
    rescue
      # We only rescue exceptions in the mix namespace, all
      # others pass through and will explode on the users face
      exception ->
        stacktrace = System.stacktrace

        cond do
          Map.get(exception, :mix_error, false) ->
            IO.write :stderr, "warning: setting mix_error: true in Mix exceptions is deprecated, " <>
                              "please define a `:mix` field and use Mix.raise to raise them"
            Mix.shell.error "** (Mix) #{Exception.message(exception)}"

          info = Map.get(exception, :mix) ->
            mod = exception.__struct__ |> Module.split() |> Enum.at(0, "Mix")
            Mix.shell.error "** (#{mod})#{show_mix_info(info)} #{Exception.message(exception)}"

          true ->
            reraise exception, stacktrace
        end

        exit(1)
    end
  end

  defp show_mix_info({:project, proj}), do: " [#{inspect proj}]"
  defp show_mix_info({:app, app}),      do: " [#{app}]"
  defp show_mix_info(:none),            do: ""

  defp change_env(task) do
    if nil?(System.get_env("MIX_ENV")) &&
       (env = Mix.Project.config[:preferred_cli_env][task]) do
      Mix.env(env)
      if project = Mix.Project.pop do
        %{name: name, file: file} = project
        Mix.Project.push name, file
      end
    end
  end

  defp load_dot_config do
    path = Path.expand("~/.mix/config.exs")
    if File.regular?(path) do
      Mix.Task.run "loadconfig", [path]
    end
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
