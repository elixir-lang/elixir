Enum.map([], fn _ -> BadReferencer.no_func4() end)

defprotocol BadProtocol do
  def func(arg)
end

defmodule BadReferencer do
  import Record
  require Integer

  def reference do
    _ = extract(1, 2)
    _ = is_record({:record})
    _ = Integer.is_even(2)

    NotAModule
    BadReferencer.no_func()
  end
end

defmodule BadReferencer2 do
  def reference(_) do
    NotAModule2
    MissingModule2.call()
    BadReferencer.reference()
    BadReferencer.reference(1)
    BadReferencer.no_func2()
  end
end

defimpl BadProtocol, for: BadReferencer do
  def func(_) do
    BadReferencer.reference()
    BadReferencer.no_func3()
  end
end
