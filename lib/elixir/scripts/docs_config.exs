# Generate docs_config.js for version chooser in ExDoc
[app] = System.argv()

{text_tags, 0} = System.cmd("git", ["tag"])
skipped = Version.parse!("1.0.3")

list_contents =
  text_tags
  |> String.split()
  |> Enum.map(fn "v" <> rest -> Version.parse!(rest) end)
  |> Enum.filter(&(Version.compare(&1, skipped) == :gt))
  |> Enum.sort({:desc, Version})
  |> Enum.map_intersperse(", ", fn version ->
    version_string = Version.to_string(version)
    ~s[{"version":"v#{version_string}", "url":"https://hexdocs.pm/#{app}/#{version_string}"}]
  end)

File.mkdir_p!("doc/#{app}")
File.write!("doc/#{app}/docs_config.js", ["var versionNodes = [", list_contents, "];\n"])
