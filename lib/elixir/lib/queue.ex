defmodule Queue do
  @moduledoc """
  A simple and performant queue.
  """

  @opaque t :: record
  @type   v :: any

  defexception Empty, message: "the queue is empty"

  defrecordp :queue, enqueue: [], dequeue: []

  @doc """
  Creates an empty queue.
  """
  @spec new :: t
  def new do
    queue()
  end

  @doc """
  Creates a new queue from the given enumerable.

  ## Examples

      iex> Queue.new(1 .. 4)
      #Queue<[1,2,3,4]>

  """
  @spec new(Enum.t) :: t
  def new(enum) do
    queue(dequeue: Enum.to_list(enum))
  end

  @doc """
  Enqueue a value in the queue.

  ## Examples

      iex> Queue.new |> Queue.enq(42) |> Queue.enq(23) |> Queue.enq(1337)
      #Queue<[42,23,1337]>

  """
  @spec enq(t, v) :: t
  def enq(queue(enqueue: [], dequeue: []), value) do
    queue(dequeue: [value])
  end

  # minor amortization in case of two enqs
  def enq(queue(enqueue: enq, dequeue: [deq]), value) do
    queue(enqueue: enq, dequeue: [deq, value])
  end

  def enq(queue(enqueue: enq, dequeue: deq), value) do
    queue(enqueue: [value | enq], dequeue: deq)
  end

  @doc """
  Dequeue a value from the queue.

  ## Examples

      iex> Queue.new |> Queue.enq(42) |> Queue.enq(23) |> Queue.deq
      {42,#Queue<[23]>}
      iex> Queue.new |> Queue.deq(:empty)
      {:empty,#Queue<[]>}

  """
  @spec deq(t)    :: { v, t }
  @spec deq(t, v) :: { v, t }
  def deq(queue, default // nil)

  def deq(queue(enqueue: [], dequeue: []), default) do
    { default, queue() }
  end

  def deq(queue(enqueue: [], dequeue: [deq]), _) do
    { deq, queue() }
  end

  def deq(queue(enqueue: [enq], dequeue: [deq]), _) do
    { deq, queue(dequeue: [enq]) }
  end

  def deq(queue(enqueue: enq, dequeue: [value]), _) do
    { value, queue(dequeue: Enum.reverse(enq)) }
  end

  def deq(queue(enqueue: enq, dequeue: [head | rest]), _) do
    { head, queue(enqueue: enq, dequeue: rest) }
  end

  @doc """
  Dequeue a value from the queue, raising if its empty.

  ## Examples

      iex> Queue.new |> Queue.enq(42) |> Queue.deq!
      {42,#Queue<[]>}
      iex> Queue.new |> Queue.deq!
      ** (Queue.Empty) the queue is empty

  """
  @spec deq!(t) :: { v, t } | no_return
  def deq!(queue(enqueue: [], dequeue: [])) do
    raise Empty
  end

  def deq!(queue) do
    deq(queue)
  end

  @doc """
  Peek the element that would be dequeued.

  ## Examples

      iex> Queue.new |> Queue.enq(42) |> Queue.peek
      42
      iex> Queue.new |> Queue.peek(:empty)
      :empty

  """
  @spec peek(t)    :: v
  @spec peek(t, v) :: v
  def peek(queue, default // nil)

  def peek(queue(enqueue: [], dequeue: []), default) do
    default
  end

  def peek(queue(dequeue: [value | _]), _) do
    value
  end

  @doc """
  Peek the element that should be dequeued, raising if it's empty.

  ## Examples

      iex> Queue.new |> Queue.enq(42) |> Queue.enq(23) |> Queue.peek!
      42
      iex> Queue.new |> Queue.peek!
      ** (Queue.Empty) the queue is empty

  """
  @spec peek!(t) :: v | no_return
  def peek!(queue(enqueue: [], dequeue: [])) do
    raise Empty
  end

  def peek!(queue) do
    peek(queue)
  end

  @doc """
  Reverse the queue.

  ## Examples

      iex> Queue.new(1 .. 4) |> Queue.reverse
      #Queue<[4,3,2,1]>

  """
  @spec reverse(t) :: t
  def reverse(queue(enqueue: enq, dequeue: deq)) do
    queue(enqueue: deq, dequeue: enq)
  end

  @doc """
  Check if the queue is empty.
  """
  @spec empty?(t) :: boolean
  def empty?(queue(enqueue: [], dequeue: [])) do
    true
  end

  def empty?(queue()) do
    false
  end

  @doc """
  Check if the the value is present in the queue.
  """
  @spec member?(t, v) :: boolean
  def member?(queue(enqueue: [], dequeue: [])) do
    false
  end

  def member?(queue(enqueue: enq, dequeue: deq), value) do
    Enum.member?(enq, value) or Enum.member?(deq, value)
  end

  @doc """
  Get the size of the queue.
  """
  @spec size(t) :: non_neg_integer
  def size(queue(enqueue: enq, dequeue: deq)) do
    length(enq) + length(deq)
  end

  @doc """
  Fold the queue from the left.
  """
  @spec foldl(t, any, ((v, any) -> any)) :: any
  def foldl(queue(enqueue: enq, dequeue: deq), acc, fun) do
    List.foldr(enq, List.foldl(deq, acc, fun), fun)
  end

  @doc """
  Fold the queue from the right.
  """
  @spec foldr(t, any, ((v, any) -> any)) :: any
  def foldr(queue(enqueue: enq, dequeue: deq), acc, fun) do
    List.foldr(deq, List.foldl(enq, acc, fun), fun)
  end

  @doc """
  Convert the queue to a list.
  """
  @spec to_list(t) :: [v]
  def to_list(queue(enqueue: enq, dequeue: deq)) do
    deq ++ Enum.reverse(enq)
  end
end

defimpl Enumerable, for: Queue do
  def reduce(queue, acc, fun) do
    Queue.foldl(queue, acc, fun)
  end

  def count(queue) do
    Queue.size(queue)
  end

  def member?(queue, value) do
    Queue.member?(queue, value)
  end
end

defimpl Binary.Inspect, for: Queue do
  def inspect(queue, opts) do
    "#Queue<" <> Kernel.inspect(Queue.to_list(queue), opts) <> ">"
  end
end
