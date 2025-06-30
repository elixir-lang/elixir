# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Gleam do
  # Version that introduced `gleam export package-information` command
  @required_gleam_version ">= 1.10.0"

  def load_config(dir) do
    File.cd!(dir, fn ->
      gleam!(~W(export package-information --out /dev/stdout))
      |> JSON.decode!()
      |> Map.fetch!("gleam.toml")
      |> parse_config()
    end)
  end

  def parse_config(json) do
    deps =
      Map.get(json, "dependencies", %{})
      |> Enum.map(&parse_dep/1)

    dev_deps =
      Map.get(json, "dev-dependencies", %{})
      |> Enum.map(&parse_dep(&1, only: :dev))

    %{
      name: Map.fetch!(json, "name"),
      version: Map.fetch!(json, "version"),
      deps: deps ++ dev_deps
    }
    |> maybe_gleam_version(json)
    |> maybe_erlang_opts(json)
  rescue
    KeyError ->
      Mix.raise("Command \"gleam export package-information\" unexpected format: \n" <> json)
  end

  defp parse_dep({dep, requirement}, opts \\ []) do
    dep = String.to_atom(dep)

    spec =
      case requirement do
        %{"version" => version} ->
          {dep, version, opts}

        %{"path" => path} ->
          {dep, Keyword.merge(opts, path: Path.expand(path))}

        %{"git" => git, "ref" => ref} ->
          {dep, git: git, ref: ref}

        _ ->
          Mix.raise("Gleam package #{dep} has unsupported requirement: #{inspect(requirement)}")
      end

    case spec do
      {dep, version, []} -> {dep, version}
      spec -> spec
    end
  end

  defp maybe_gleam_version(config, json) do
    case json["gleam"] do
      nil -> config
      version -> Map.put(config, :gleam, version)
    end
  end

  defp maybe_erlang_opts(config, json) do
    config =
      case get_in(json, ["erlang", "application_start_module"]) do
        nil -> config
        mod -> Map.put(config, :mod, mod)
      end

    case get_in(json, ["erlang", "extra_applications"]) do
      nil -> config
      extra_applications -> Map.put(config, :extra_applications, extra_applications)
    end
  end

  def require!() do
    available_version()
    |> Version.match?(@required_gleam_version)
  end

  defp available_version do
    case gleam!(["--version"]) do
      "gleam " <> version -> Version.parse!(version) |> Version.to_string()
      output -> Mix.raise("Command \"gleam --version\" unexpected format: #{output}")
    end
  rescue
    e in Version.InvalidVersionError ->
      Mix.raise("Command \"gleam --version\" invalid version format: #{e.version}")
  end

  defp gleam!(args) do
    System.cmd("gleam", args)
  catch
    :error, :enoent ->
      Mix.raise(
        "The \"gleam\" executable is not available in your PATH. " <>
          "Please install it, as one of your dependencies requires it. "
      )
  else
    {response, 0} ->
      String.trim(response)

    {response, _} when is_binary(response) ->
      Mix.raise("Command \"gleam #{Enum.join(args, " ")}\" failed with reason: #{response}")

    {_, _} ->
      Mix.raise("Command \"gleam #{Enum.join(args, " ")}\" failed")
  end
end
