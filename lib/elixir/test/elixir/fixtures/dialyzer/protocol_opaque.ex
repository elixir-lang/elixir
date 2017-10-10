defmodule Dialyzer.ProtocolOpaque do
  def circus() do
    duck = Dialyzer.ProtocolOpaque.Duck.new()
    Dialyzer.ProtocolOpaque.Entity.speak(duck)
  end
end

defprotocol Dialyzer.ProtocolOpaque.Entity do
  def speak(entity)
end

defmodule Dialyzer.ProtocolOpaque.Duck do
  @opaque t :: %__MODULE__{}
  defstruct feathers: :white_and_grey

  @spec new :: t
  def new(), do: %__MODULE__{}

  defimpl Dialyzer.ProtocolOpaque.Entity do
    def speak(%Dialyzer.ProtocolOpaque.Duck{}), do: "Quack!"
  end
end
