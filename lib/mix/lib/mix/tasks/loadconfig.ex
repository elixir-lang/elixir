defmodule Mix.Tasks.Loadconfig do
  use Mix.Task

  @moduledoc """
  Loads and persists the project configuration.

  In case the application is an umbrella application, the
  configuration for all children app will be merged together
  and, in case there are any conflicts, they need to be resolved
  in the umbrella application.
  """

  @doc """
  Runs this task.
  """
  def run(_) do
    Mix.Config.persist load()
  end

  @doc """
  Loads the configuration for the current project.
  """
  def load() do
    if Mix.Project.get do
      project = Mix.Project.config
      project[:config_path]
      |> eval()
      |> merge(Mix.Project.umbrella?)
    else
      []
    end
  end

  defp eval(nil) do
    if File.regular?("config/config.exs") do
      eval("config/config.exs")
    else
      []
    end
  end

  defp eval(file) do
    try do
      Mix.Config.read(file)
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        Mix.shell.error "Could not load config #{file} from project #{inspect Mix.Project.get}"
        :erlang.raise(kind, reason, stacktrace)
    end
  end

  defp merge(base, false), do: base
  defp merge(base, true) do
    Mix.Dep.Umbrella.unloaded
    |> Enum.reduce([], fn dep, acc ->
         Mix.Dep.in_dependency dep, fn _ ->
           config = eval(Mix.Project.config[:config_path])
           merge dep, acc, config, base
         end
       end)
    |> Mix.Config.merge(base)
  end

  defp merge(dep, acc, config, base) do
    Mix.Config.merge(acc, config, fn app, k, v1, v2 ->
      if Keyword.has_key?(base, app) and Keyword.has_key?(base[app], k) do
        v1
      else
        raise Mix.Error, message: "umbrella child #{inspect dep.app} has set the configuration for " <>
          "key #{inspect k} in app #{inspect app} to #{inspect v2} but another umbrella child has " <>
          "already set it to #{inspect v1}. You need to remove the configuration or resolve " <>
          "the conflict by defining a config file and setting a value in your umbrella project"
      end
    end)
  end
end
