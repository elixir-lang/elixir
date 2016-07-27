defmodule Dialyzer.StructUpdate do
  defstruct [:foo]

  def update(%__MODULE__{} = struct) do
    %__MODULE__{struct | foo: :bar}
  end
end
