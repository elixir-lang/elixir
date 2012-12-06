defmodule Mix.Tasks.Deps.Start do
  use Mix.Task

  import Mix.Deps, only: [all: 0, ok?: 1]

  @hidden true
  @shortdoc "Start all dependencies"

  @moduledoc """
  Starts all dependencies. This must be invoked
  by compilation tasks that requires dependencies
  to be started.
  """
  def run(_) do
    lc dep inlist all, ok?(Mix.Dep[] = dep), dep.opts do
      start_app(dep.app, dep.opts[:app])
    end
  end

  defp start_app(_, false), do: :ok

  defp start_app(dep, app) when is_atom(app) and app != nil do
    case Application.Behaviour.start(app) do
      :ok -> :ok
      { :error, reason } ->
        Mix.shell.error "Could not start application #{app} for dependency #{dep}: #{inspect reason}." <>
          "You may skip start the application for this dependency by settng :app to false"
    end
  end

  defp start_app(dep, _), do: start_app(dep, dep)
end
