watch("(lib|src)/.*") { system("make test") }
watch("test/erlang/.*") { system("make test_erlang") }
watch("test/elixir/.*") { system("make test_elixir") }