defmodule Dialyzer.RemoteCall do
  _ = Application.load(:dialyzer)
  case Application.spec(:dialyzer, :vsn) do
    ~c(3.) ++ _ ->
      @dialyzer {:no_return, [map_var: 0]}
      @dialyzer {:no_match, [map_var: 0, mod_var: 0, mod_var: 1]}
    _ ->
      :ok
  end

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
