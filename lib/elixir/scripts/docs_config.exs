# Generate docs_config.js for version chooser in ExDoc
[app] = System.argv()

{text_tags, 0} = System.cmd("git", ["tag"])
skipped = Version.parse!("1.0.3")

list_contents =
  for(
    "v" <> rest <- String.split(text_tags),
    not String.ends_with?(rest, "-latest"),
    version = Version.parse!(rest),
    Version.compare(version, skipped) == :gt,
    do: version
  )
  |> Enum.sort({:desc, Version})
  |> Enum.map_intersperse(", ", fn version ->
    version_string = Version.to_string(version)
    ~s[{"version":"v#{version_string}", "url":"https://hexdocs.pm/#{app}/#{version_string}"}]
  end)

File.mkdir_p!("doc/#{app}")
File.write!("doc/#{app}/docs_config.js", ["var versionNodes = [", list_contents, "];\n"])
