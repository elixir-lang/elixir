defmodule Queue do

  @moduledoc ~S"""
  Elixir wrapper around Erlang's 'queue' module, following the convention of pipe operator
  This module implements (double ended) FIFO queues in an efficient manner.
  All functions fail with reason badarg if arguments are of wrong type, for example queue arguments are not queues, indexes are not integers, list arguments are not lists. Improper lists cause internal crashes. An index out of range for a queue also causes a failure with reason badarg.
  Some functions, where noted, fail with reason empty for an empty queue.
  The data representing a queue as used by this module should be regarded as opaque by other modules. Any code assuming knowledge of the format is running on thin ice.
  All operations has an amortized O(1) running time, except len/1, join/2, split/2, filter/2 and member/2 that have O(n). To minimize the size of a queue minimizing the amount of garbage built by queue operations, the queues do not contain explicit length information, and that is why len/1 is O(n). If better performance for this particular operation is essential, it is easy for the caller to keep track of the length.
  Queues are double ended. The mental picture of a queue is a line of people (items) waiting for their turn. The queue front is the end with the item that has waited the longest. The queue rear is the end an item enters when it starts to wait. If instead using the mental picture of a list, the front is called head and the rear is called tail.
  Entering at the front and exiting at the rear are reverse operations on the queue.
  """
  @functions :queue.module_info(:exports)

  Enum.map @functions, fn
    {:module_info, _} -> 0
    {name, 0} -> def unquote(name)(), do: apply(:queue, unquote(name), [])
    {name, 1} -> def unquote(name)(x), do: apply(:queue, unquote(name), [x])
    {name, 2} -> def unquote(name)(x,y), do: apply(:queue, unquote(name), [y, x])
  end
end
