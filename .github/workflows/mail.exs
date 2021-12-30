# #!/usr/bin/env elixir
[tag] = System.argv()
Mix.install([:req, :jason])

%{status: 200, body: body} =
  Req.get!("https://api.github.com/repos/elixir-lang/elixir/releases/tags/#{tag}")

names_and_checksums =
  for asset <- body["assets"],
      name = asset["name"],
      name =~ ~r/.sha\d+sum$/,
      do: {name, Req.get!(asset["browser_download_url"]).body}

IO.puts("https://github.com/elixir-lang/elixir/releases/tag/#{tag}")
IO.puts("")

for {name, checksum} <- Enum.sort(names_and_checksums) do
  root = Path.rootname(name)
  "." <> type = Path.extname(name)
  IO.puts("  * #{root} - #{type} - #{checksum}")
end
