defmodule Dialyzer.ProtocolOpaque do
  def circus() do
    duck = Dialyzer.ProtocolOpaque.Duck.new()
    Dialyzer.ProtocolOpaque.Entity.speak(duck)
  end
end

defprotocol Dialyzer.ProtocolOpaque.Entity do
  @fallback_to_any true
  def speak(entity)
end

defmodule Dialyzer.ProtocolOpaque.Duck do
  @opaque t :: %__MODULE__{feathers: :white_and_grey}
  defstruct feathers: :white_and_grey

  @spec new :: t
  def new(), do: %__MODULE__{}

  defimpl Dialyzer.ProtocolOpaque.Entity do
    def speak(%Dialyzer.ProtocolOpaque.Duck{}), do: "Quack!"
  end
end

defimpl Dialyzer.ProtocolOpaque.Entity, for: Any do
  def speak(_any) do
    "I can be anything"
  end
end
