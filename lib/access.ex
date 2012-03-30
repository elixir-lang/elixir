import Elixir.Builtin, except: [access: 2]

defprotocol Access, [access(element, qualifier)],
  only: [List, BitString, Record, Tuple, Atom, PID, Function]

defimpl Access, for: Tuple do
  def access(tuple, integer) when is_integer(integer) do
    :erlang.element(integer, tuple)
  end
end