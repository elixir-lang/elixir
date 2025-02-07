# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

# Generate docs_config.js for version chooser in ExDoc
[app] = System.argv()

{text_tags, 0} = System.cmd("git", ["tag"])
skipped = Version.parse!("1.0.3")

versions =
  for(
    "v" <> rest <- String.split(text_tags),
    not String.ends_with?(rest, "-latest"),
    version = Version.parse!(rest),
    Version.compare(version, skipped) == :gt,
    do: version
  )
  |> Enum.sort({:desc, Version})

latest =
  versions
  |> Stream.filter(&(&1.pre == []))
  |> Enum.fetch!(0)
  |> Version.to_string()

version_nodes =
  for version <- versions do
    version_string = Version.to_string(version)
    map = %{version: "v#{version_string}", url: "https://hexdocs.pm/#{app}/#{version_string}"}

    if version_string == latest do
      Map.put(map, :latest, true)
    else
      map
    end
  end

search_nodes =
  for app <- ~w(eex elixir ex_unit iex logger mix)s do
    %{name: app, version: latest}
  end

File.mkdir_p!("doc/#{app}")

File.write!("doc/#{app}/docs_config.js", """
var versionNodes = #{JSON.encode_to_iodata!(version_nodes)};
var searchNodes = #{JSON.encode_to_iodata!(search_nodes)};
""")
