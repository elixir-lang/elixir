defmodule Dialyzer.RemoteCall do
  _ = Application.load(:dialyzer)

  case Application.spec(:dialyzer, :vsn) do
    ~c(2.) ++ _ ->
      @dialyzer {:no_fail_call, [map_var: 0]}

    three when three < ~c(3.0.2) ->
      # regression introduced in 3.0 for map warnings fixed in 3.0.2
      @dialyzer {:no_match, [map_var: 0, mod_var: 0, mod_var: 1]}

    _ ->
      :ok
  end

  def map_var() do
    map = %{key: 1}
    map.key
  end

  def map_var(map) when is_map(map) do
    map.key
  end

  def mod_var() do
    module = Hello
    module.fun()
  end
end
