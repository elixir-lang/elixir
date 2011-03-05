watch("(lib|src)/.*") { system("make test") }
watch("test/erlang/.*") { system("make test_erlang") }
watch("test/elixir/.*_test\.ex") do |md|
  puts "Running Elixir tests ..."
  command = "time bin/exunit #{md[0]}"
  puts command
  system command
  puts
end