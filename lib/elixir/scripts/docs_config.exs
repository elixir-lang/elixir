app =
  case System.argv() do
    [app] -> app
    _ -> raise "Expected a single argument for the app name"
  end

{text_tags, 0} = System.cmd("git", ["tag"])

skipped = Version.parse!("1.0.3")

list_contents =
  text_tags
  |> String.split()
  |> Enum.filter(fn tag ->
    String.starts_with?(tag, "v") and not String.ends_with?(tag, "-latest")
  end)
  |> Enum.map(fn "v" <> rest -> Version.parse!(rest) end)
  |> Enum.filter(fn version -> Version.compare(version, skipped) == :gt end)
  |> Enum.sort({:desc, Version})
  |> Enum.map_intersperse(", ", fn version ->
    version_string = Version.to_string(version)
    ~s[{"version":"v#{version_string}", "url":"https://hexdocs.pm/#{app}/#{version_string}"}]
  end)

File.mkdir_p!("doc/#{app}")
File.write!("doc/#{app}/docs_config.js", ["var versionNodes = [", list_contents, "];\n"])
 
