ns Orddict

# TODO: Write in Elixir once we have guards and comparison operators
def fetch: [orddict, key, default] do
  Erlang.elixir_helpers.orddict_find(key, orddict, default)
end