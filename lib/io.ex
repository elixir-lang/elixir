module IO

def print(device, list) when is_list(list) do
  print(device, list_to_binary(list))
end

def print(device // :standard_io, item) do
  Erlang.io.format device, item, []
end

def puts(device, list) when is_list(list) do
  puts(device, list_to_binary(list))
end

def puts(device // :standard_io, item) do
  print(device, item)
  Erlang.io.format("~n")
end