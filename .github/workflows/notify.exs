# #!/usr/bin/env elixir

# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

[tag] = System.argv()

Mix.install([
  {:req, "~> 0.2.1"},
  {:jason, "~> 1.0"}
])

%{status: 200, body: release} =
  Req.get!("https://api.github.com/repos/elixir-lang/elixir/releases/tags/#{tag}")

if release["draft"] do
  raise "cannot notify a draft release"
end

## Notify on elixir-lang-ann

names_and_checksums =
  for asset <- release["assets"],
      name = asset["name"],
      name =~ ~r/.sha\d+sum$/,
      do: {name, Req.get!(asset["browser_download_url"]).body}

line_items =
  for {name, checksum_and_name} <- Enum.sort(names_and_checksums) do
    [checksum | _] = String.split(checksum_and_name, " ")
    root = Path.rootname(name)
    "." <> type = Path.extname(name)
    "  * #{root} - #{type} - #{checksum}\n"
  end

body = "https://github.com/elixir-lang/elixir/releases/tag/#{tag}\n\n#{line_items}"

IO.puts([
  "========================================\n",
  body,
  "\n========================================"
])

mail = %{
  # The email must have access to post
  "From" => "jose.valim@dashbit.co",
  "To" => "elixir-lang-ann@googlegroups.com",
  "Subject" => "Elixir #{tag} released",
  "HtmlBody" => body,
  "MessageStream" => "outbound"
}

unless System.get_env("DRYRUN") do
  headers = %{
    "X-Postmark-Server-Token" => System.fetch_env!("ELIXIR_LANG_ANN_TOKEN")
  }

  resp = Req.post!("https://api.postmarkapp.com/email", {:json, mail}, headers: headers)
  IO.puts("#{resp.status} elixir-lang-ann\n#{inspect(resp.body)}")
end

## Notify on Elixir Forum

post = %{
  "title" => "Elixir #{tag} released",
  "raw" => "https://github.com/elixir-lang/elixir/releases/tag/#{tag}\n\n#{release["body"]}",
  # Elixir News
  "category" => 28
}

unless System.get_env("DRYRUN") do
  headers = %{
    "api-key" => System.fetch_env!("ELIXIR_FORUM_TOKEN"),
    "api-username" => "Elixir"
  }

  resp = Req.post!("https://elixirforum.com/posts.json", {:json, post}, headers: headers)
  IO.puts("#{resp.status} Elixir Forum\n#{inspect(resp.body)}")
end
