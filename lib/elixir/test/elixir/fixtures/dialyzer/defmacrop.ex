defmodule Dialyzer.Defmacrop do
  defmacrop good_macro(id) do
    quote do
      {:good, {:good_macro, unquote(id)}}
    end
  end

  def run() do
    good_macro("Not So Bad")
  end
end
