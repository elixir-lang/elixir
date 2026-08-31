# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule Mix.Gleam do
  # Version that introduced generating an .app file 
  @gleam_version_requirement ">= 1.19.0"

  @spec load_config(Path.t()) :: config :: map()
  def load_config(dir) do
    File.cd!(dir, fn ->
      with {:ok, output} <-
             gleam(~W(export package-information --out /dev/stdout)),
           json <- JSON.decode!(output),
           {:ok, gleam_toml} <- Map.fetch(json, "gleam.toml") do
        parse_config(gleam_toml)
      else
        :error ->
          {:error, "\"gleam.toml\" key not found in \"gleam export package-information\" output"}

        {:error, message} ->
          {:error, message}
      end
      |> assert_ok_value!()
    end)
  end

  @spec parse_config(map()) :: {:ok, config :: map()} | {:error, message :: binary()}
  def parse_config(json) when is_map(json) do
    deps =
      Map.get(json, "dependencies", %{})
      |> Enum.map(&parse_dep!/1)

    with {:ok, name} <- Map.fetch(json, "name"),
         {:ok, version} <- Map.fetch(json, "version") do
      config =
        %{
          name: name,
          version: version,
          deps: deps
        }
        |> maybe_gleam_version(json)

      {:ok, config}
    else
      :error ->
        {:error,
         "Command \"gleam export package-information\" unexpected format: \n" <>
           inspect(json, pretty: true, limit: :infinity)}
    end
  end

  defp parse_dep!({dep, requirement}, opts \\ []) do
    String.to_atom(dep)
    |> build_dep_spec(requirement, opts)
    |> assert_ok_value!()
  end

  defp build_dep_spec(dep, %{"version" => version}, []),
    do: {:ok, {dep, version}}

  defp build_dep_spec(dep, %{"version" => version}, opts),
    do: {:ok, {dep, version, opts}}

  defp build_dep_spec(dep, %{"path" => path}, opts),
    do: {:ok, {dep, Keyword.merge(opts, path: Path.expand(path))}}

  defp build_dep_spec(dep, %{"git" => git, "ref" => ref}, _opts),
    do: {:ok, {dep, git: git, ref: ref}}

  defp build_dep_spec(dep, requirement, _opts),
    do: {:error, "Gleam package #{dep} has unsupported requirement: #{inspect(requirement)}"}

  defp maybe_gleam_version(config, json) do
    case json["gleam"] do
      nil -> config
      version -> Map.put(config, :gleam, version)
    end
  end

  @spec requirements!() :: :ok
  def requirements!() do
    case fetch_gleam_version() do
      {:ok, gleam_version} ->
        if Version.match?(gleam_version, @gleam_version_requirement) do
          :ok
        else
          {:error,
           "Current Gleam version does not meet minimum requirements " <>
             "#{@gleam_version_requirement}),  got: #{gleam_version}"}
        end

      {:error, message} ->
        {:error, message}
    end
    |> assert_ok_value!()
  end

  defp fetch_gleam_version() do
    case gleam(["--version"]) do
      {:ok, "gleam " <> version} ->
        case Version.parse(version) do
          {:ok, parsed_version} ->
            {:ok, Version.to_string(parsed_version)}

          :error ->
            {:error, "Command \"gleam --version\" invalid version format: #{version}"}
        end

      {:error, output} ->
        {:error, "Command \"gleam --version\" unexpected format: #{output}"}
    end
  end

  defp gleam(args) do
    System.cmd("gleam", args)
  catch
    :error, :enoent ->
      {:error,
       "The \"gleam\" executable is not available in your PATH. " <>
         "Please install it, as one of your dependencies requires it"}
  else
    {response, 0} ->
      {:ok, String.trim(response)}

    {response, _} when is_binary(response) ->
      {:error, "Command \"gleam #{Enum.join(args, " ")}\" failed with reason: #{response}"}

    {_, _} ->
      {:error, "Command \"gleam #{Enum.join(args, " ")}\" failed"}
  end

  defp assert_ok_value!(:ok), do: :ok
  defp assert_ok_value!({:ok, term}), do: term
  defp assert_ok_value!({:error, message}) when is_binary(message), do: Mix.raise(message)
end
