watch("lib/.*") { system("make test_elixir") }
watch("src/.*") { system("make test") }
watch("test/erlang/.*") { system("make test_erlang") }
watch("test/elixir/.*") { system("make test_elixir") }