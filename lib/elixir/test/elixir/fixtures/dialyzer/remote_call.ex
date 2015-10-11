defmodule Dialyzer.RemoteCall do
  def map_var() do
    map = %{a: 1}
    map.key
  end

  def map_var(map) when is_map(map) do
    map.key
  end

  def mod_var() do
    module = Hello
    module.fun
  end

  def mod_var(module) when is_atom(module) or is_atom(elem(module, 0)) do
    module.fun
  end
end
