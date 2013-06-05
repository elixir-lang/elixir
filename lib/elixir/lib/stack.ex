defmodule Stack do
  @moduledoc """
  A simple stack.
  """

  @opaque t :: record
  @type   v :: any

  defexception Empty, message: "the stack is empty"

  defrecordp :stack, list: []

  @doc """
  Creates an empty stack.
  """
  @spec new :: t
  def new do
    stack()
  end

  @doc """
  Creates a new stack from the given enumerable.

  ## Examples

      iex> Stack.new(1 .. 4)
      #Stack<[1,2,3,4]>

  """
  @spec new(Enum.t) :: t
  def new(enum) do
    stack(list: Enum.to_list(enum))
  end

  @doc """
  Push a value in the stack.

  ## Examples

      iex> Stack.new |> Stack.push(42) |> Stack.push(23) |> Stack.push(1337)
      #Stack<[1337,23,42]>

  """
  @spec push(t, v) :: t
  def push(stack(list: list), value) do
    stack(list: [value | list])
  end

  @doc """
  Pop a value from the stack.

  ## Examples

      iex> Stack.new |> Stack.push(42) |> Stack.push(23) |> Stack.pop
      {23,#Stack<[42]>}
      iex> Stack.new |> Stack.pop(:empty)
      {:empty,#Stack<[]>}

  """
  @spec pop(t)    :: { v, t }
  @spec pop(t, v) :: { v, t }
  def pop(stack, default // nil)

  def pop(stack(list: []), default) do
    { default, stack() }
  end

  def pop(stack(list: [head | rest]), _) do
    { head, stack(list: rest) }
  end

  @doc """
  Pop a value from the stack, raising if its empty.

  ## Examples

      iex> Stack.new |> Stack.push(42) |> Stack.pop
      {42,#Stack<[]>}
      iex> Stack.new |> Stack.pop!
      ** (Stack.Empty) the queue is empty

  """
  @spec pop!(t) :: { v, t } | no_return
  def pop!(stack(list: [])) do
    raise Empty
  end

  def pop!(stack() = self) do
    pop(self)
  end

  @doc """
  Peek the element that would be popped.

  ## Examples

      iex> Stack.new |> Stack.push(42) |> Stack.peek
      42
      iex> Stack.new |> Stack.peek(:empty)
      :empty

  """
  @spec peek(t)    :: v
  @spec peek(t, v) :: v
  def peek(stack, default // nil)

  def peek(stack(list: []), default) do
    default
  end

  def peek(stack(list: [head | _]), _) do
    head
  end

  @doc """
  Peek the element that should be popped, raising if it's empty.

  ## Examples

      iex> Stack.new |> Stack.push(42) |> Stack.push(23) |> Stack.peek!
      23
      iex> Stack.new |> Stack.peek!
      ** (Stack.Empty) the stack is empty

  """
  @spec peek!(t) :: v | no_return
  def peek!(stack(list: [])) do
    raise Empty
  end

  def peek!(stack) do
    peek(stack)
  end

  @doc """
  Reverse the stack.

  ## Examples

      iex> Stack.new(1 .. 4) |> Stack.reverse
      #Stack<[4,3,2,1]>

  """
  @spec reverse(t) :: t
  def reverse(stack(list: list)) do
    stack(list: Enum.reverse(list))
  end

  @doc """
  Check if the stack is empty.
  """
  @spec empty?(t) :: boolean
  def empty?(stack(list: [])) do
    true
  end

  def empty?(stack()) do
    false
  end

  @doc """
  Check if the the value is present in the stack.
  """
  @spec member?(t, v) :: boolean
  def member?(stack(list: []), _) do
    false
  end

  def member?(stack(list: list), value) do
    Enum.member?(list, value)
  end

  @doc """
  Get the size of the stack.
  """
  @spec size(t) :: non_neg_integer
  def size(stack(list: list)) do
    length(list)
  end

  @doc """
  Fold the stack from the left.
  """
  @spec foldl(t, any, ((v, any) -> any)) :: any
  def foldl(stack(list: list), acc, fun) do
    List.foldl(list, acc, fun)
  end

  @doc """
  Fold the stack from the right.
  """
  @spec foldr(t, any, ((v, any) -> any)) :: any
  def foldr(stack(list: list), acc, fun) do
    List.foldr(list, acc, fun)
  end

  @doc """
  Convert the stack to a list.
  """
  @spec to_list(t) :: [v]
  def to_list(stack(list: list)) do
    list
  end
end

defimpl Enumerable, for: Stack do
  def reduce(stack, acc, fun) do
    Stack.foldl(stack, acc, fun)
  end

  def count(stack) do
    Stack.size(stack)
  end

  def member?(stack, value) do
    Stack.member?(stack, value)
  end
end

defimpl Binary.Inspect, for: Stack do
  def inspect(stack, opts) do
    "#Stack<" <> Kernel.inspect(Stack.to_list(stack), opts) <> ">"
  end
end
