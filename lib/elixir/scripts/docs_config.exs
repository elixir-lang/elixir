# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

# Generate docs_config.js for version chooser in ExDoc
[app] = System.argv()
skipped = Version.parse!("1.0.3")
root_dir = Path.expand("../../../", __DIR__)

git_repo? =
  root_dir
  |> Path.join(".git")
  |> File.dir?()

versions =
  if git_repo? do
    {text_tags, 0} = System.cmd("git", ["tag"])

    for(
      "v" <> rest <- String.split(text_tags),
      not String.ends_with?(rest, "-latest"),
      version = Version.parse!(rest),
      Version.compare(version, skipped) == :gt,
      do: version
    )
    |> Enum.sort({:desc, Version})
  else
    []
  end

latest =
  if git_repo? do
    versions
    |> Stream.filter(&(&1.pre == []))
    |> Enum.fetch!(0)
    |> Version.to_string()
  else
    nil
  end

version_nodes =
  if git_repo? do
    for version <- versions do
      version_string = Version.to_string(version)
      map = %{version: "v#{version_string}", url: "https://hexdocs.pm/#{app}/#{version_string}"}

      if version_string == latest do
        Map.put(map, :latest, true)
      else
        map
      end
    end
  else
    []
  end

search_nodes =
  if git_repo? do
    for app <- ~w(eex elixir ex_unit iex logger mix)s do
      %{name: app, version: latest}
    end
  else
    []
  end

File.mkdir_p!("doc/#{app}")

File.write!("doc/#{app}/docs_config.js", """
var versionNodes = #{JSON.encode_to_iodata!(version_nodes)};
var searchNodes = #{JSON.encode_to_iodata!(search_nodes)};
""")
