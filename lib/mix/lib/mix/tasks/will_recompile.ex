defmodule Mix.Tasks.WillRecompile do
  use Mix.Task
  @moduledoc false

  @doc """
  Annotates the current project will recompile.
  """
  def run(_) do
    config = Mix.Project.config()

    # Slight reimplementation of `manifest_path` because we need to
    # get manifests for root and children in umbrella case.
    paths =
      if Mix.Project.umbrella?(config) do
        build = Mix.Project.build_path(config)

        children =
          Enum.map(Mix.Project.apps_paths(config), fn {app, _} ->
            Path.join([build, "lib", Atom.to_string(app)])
          end)

        [build | children]
      else
        [Mix.Project.app_path(config)]
      end

    filename = Mix.ProjectStack.reset_config_mtime()

    for path <- paths do
      path = Path.join(path, ".mix")
      File.mkdir_p!(path)
      File.touch!(Path.join(path, filename))
    end

    :ok
  end
end
