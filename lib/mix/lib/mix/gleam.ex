defmodule Mix.Gleam do
  def load_config(dir) do
    config_path = Path.join(dir, "gleam.toml")

    case File.read(config_path) do
      {:ok, config} ->
        parse_config(config)

      {:error, :enoent} ->
        []

      {:error, error} ->
        raise("Error reading gleam.toml config #{inspect(config_path)}: #{error}")
    end
  end

  defp parse_config(config) do
    dependencies =
      config
      |> table("dependencies")
      |> parse_deps()

    dev_dependencies =
      config
      |> table("dev-dependencies")
      |> parse_deps
      |> Enum.map(&Tuple.insert_at(&1, 2, only: :dev))

    %{
      deps: dependencies ++ dev_dependencies
    }
  end

  # Given a string of deps, returns them as a list of tuples
  defp parse_deps(string) do
    ~r/^([a-z][a-z0-9_]+)\s*=\s*"([^"]+)"\s*/m
    |> Regex.scan(string)
    |> Enum.map(fn dep ->
      [_, name, version] = dep
      {String.to_atom(name), version}
    end)
  end

  # Grabs a TOML table by name and returns its contents as a string
  def table(config, name) do
    ~r/^\[#{name}\]$[\r\n]+((?:[a-z][a-z0-9_]*\s*=\s*"[^"]+"\r?\n?)+)/m
    |> Regex.run(config)
    |> List.last()
  end
end
