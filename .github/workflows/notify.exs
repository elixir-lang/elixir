# #!/usr/bin/env elixir
[tag] = System.argv()

Mix.install([
  {:req, "~> 0.2.1"},
  {:jason, "~> 1.0"}
])

%{status: 200, body: body} =
  Req.get!("https://api.github.com/repos/elixir-lang/elixir/releases/tags/#{tag}")

if body["draft"] do
  raise "cannot notify a draft release"
end

## Notify on elixir-lang-ann

names_and_checksums =
  for asset <- body["assets"],
      name = asset["name"],
      name =~ ~r/.sha\d+sum$/,
      do: {name, Req.get!(asset["browser_download_url"]).body}

line_items =
  for {name, checksum} <- Enum.sort(names_and_checksums) do
    root = Path.rootname(name)
    "." <> type = Path.extname(name)
    "  * #{root} - #{type} - #{checksum}\n"
  end

headers = %{
  "X-Postmark-Server-Token" => System.fetch_env!("ELIXIR_LANG_ANN_TOKEN")
}

body = %{
  "From" => "jose.valim@dashbit.co",
  "To" => "elixir-lang-ann@googlegroups.com",
  "Subject" => "Elixir #{tag} released",
  "HtmlBody" => "https://github.com/elixir-lang/elixir/releases/tag/#{tag}\n\n#{line_items}",
  "MessageStream": "outbound"
}

resp = Req.post!("https://api.postmarkapp.com/email", {:json, body}, headers: headers)
IO.puts("#{resp.status} elixir-lang-ann\n#{inspect(resp.body)}")

## Notify on Elixir Forum

headers = %{
  "api-key" => System.fetch_env!("ELIXIR_FORUM_TOKEN"),
  "api-username" => "Elixir"
}

body = %{
  "title" => "Elixir #{tag} released",
  "raw" => "https://github.com/elixir-lang/elixir/releases/tag/#{tag}\n\n#{body["body"]}",
  # Elixir News
  "category" => 28
}

resp = Req.post!("https://elixirforum.com/posts.json", {:json, body}, headers: headers))
IO.puts("#{resp.status} Elixir Forum\n#{inspect(resp.body)}")
