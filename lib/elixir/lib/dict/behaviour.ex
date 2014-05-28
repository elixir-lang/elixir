defmodule Dict.Behaviour do
  @moduledoc false

  defmacro __using__(_) do
    quote do
      use Dict
    end
  end
end
