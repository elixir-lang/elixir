Mix.start
ExUnit.start []

defmodule MixTest.Case do
  defmacro __using__(opts // []) do
    quote do
      use ExUnit.Case, unquote(opts)

      def setup(test) do
        Mix.mixfile(nil)
      end

      defoverridable [setup: 1]
    end
  end
end