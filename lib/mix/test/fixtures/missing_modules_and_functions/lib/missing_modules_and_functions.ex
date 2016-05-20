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

defmodule BadReferencer3 do
  if function_exported?(List, :flatten, 1) do
    List.flatten([1, 2, 3])
  else
    List.old_flatten([1, 2, 3])
  end

  if function_exported?(List, :flatten, 1) do
    def flatten(arg), do: List.flatten(arg)
  else
    def flatten(arg), do: List.old_flatten(arg)
  end

  if function_exported?(List, :flatten, 1) do
    def flatten2(arg), do: List.old_flatten(arg)
  else
    def flatten2(arg), do: List.flatten(arg)
  end

  def erlang do
    :missing_module
    :missing_module.no_func()
    :lists.not_a_real_func()
    :lists.all(1, 2)
    :lists.all(1, 2, 3)
  end
end

if function_exported?(List, :flatten, 1) do
  List.flatten([1, 2, 3])
else
  List.old_flatten([1, 2, 3])
end
