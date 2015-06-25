# Use elixir_bootstrap module to be able to bootstrap Kernel.
# The bootstrap module provides simpler implementations of the
# functions removed, simple enough to bootstrap.
import Kernel, except: [@: 1, defmodule: 2, def: 1, def: 2, defp: 2,
                        defmacro: 1, defmacro: 2, defmacrop: 2]
import :elixir_bootstrap

defmodule Kernel do
  @moduledoc """
  `Kernel` provides the default macros and functions
  Elixir imports into your environment. These macros and functions
  can be skipped or cherry-picked via the `import` macro. For
  instance, if you want to tell Elixir not to import the `if`
  macro, you can do:

      import Kernel, except: [if: 2]

  Elixir also has special forms that are always imported and
  cannot be skipped. These are described in `Kernel.SpecialForms`.

  Some of the functions described in this module are inlined by
  the Elixir compiler into their Erlang counterparts in the `:erlang`
  module. Those functions are called BIFs (builtin internal functions)
  in Erlang-land and they exhibit interesting properties, as some of
  them are allowed in guards and others are used for compiler
  optimizations.

  Most of the inlined functions can be seen in effect when capturing
  the function:

      iex> &Kernel.is_atom/1
      &:erlang.is_atom/1

  Those functions will be explicitly marked in their docs as
  "inlined by the compiler".
  """

  ## Delegations to Erlang with inlining (macros)

  @doc """
  Returns an integer or float which is the arithmetical absolute value of `number`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> abs(-3.33)
      3.33

      iex> abs(-3)
      3

  """
  @spec abs(number) :: number
  def abs(number) do
    :erlang.abs(number)
  end

  @doc """
  Invokes the given `fun` with the array of arguments `args`.

  Inlined by the compiler.

  ## Examples

      iex> apply(fn x -> x * 2 end, [2])
      4

  """
  @spec apply(fun, [any]) :: any
  def apply(fun, args) do
    :erlang.apply(fun, args)
  end

  @doc """
  Invokes the given `fun` from `module` with the array of arguments `args`.

  Inlined by the compiler.

  ## Examples

      iex> apply(Enum, :reverse, [[1, 2, 3]])
      [3, 2, 1]

  """
  @spec apply(module, atom, [any]) :: any
  def apply(module, fun, args) do
    :erlang.apply(module, fun, args)
  end

  @doc """
  Extracts the part of the binary starting at `start` with length `length`.
  Binaries are zero-indexed.

  If `start` or `length` reference in any way outside the binary, an
  `ArgumentError` exception is raised.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> binary_part("foo", 1, 2)
      "oo"

  A negative `length` can be used to extract bytes that come *before* the byte
  at `start`:

      iex> binary_part("Hello", 5, -3)
      "llo"

  """
  @spec binary_part(binary, pos_integer, integer) :: binary
  def binary_part(binary, start, length) do
    :erlang.binary_part(binary, start, length)
  end

  @doc """
  Returns an integer which is the size in bits of `bitstring`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> bit_size(<<433::16, 3::3>>)
      19

      iex> bit_size(<<1, 2, 3>>)
      24

  """
  @spec bit_size(bitstring) :: non_neg_integer
  def bit_size(bitstring) do
    :erlang.bit_size(bitstring)
  end

  @doc """
  Returns the number of bytes needed to contain `bitstring`.

  That is, if the number of bits in `bitstring` is not divisible by 8, the
  resulting number of bytes will be rounded up (by excess). This operation
  happens in constant time.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> byte_size(<<433::16, 3::3>>)
      3

      iex> byte_size(<<1, 2, 3>>)
      3

  """
  @spec byte_size(binary) :: non_neg_integer
  def byte_size(binary) do
    :erlang.byte_size(binary)
  end

  @doc """
  Performs an integer division.

  Raises an `ArithmeticError` exception if one of the arguments is not an
  integer.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> div(5, 2)
      2

  """
  @spec div(integer, integer) :: integer
  def div(left, right) do
    :erlang.div(left, right)
  end

  @doc """
  Stops the execution of the calling process with the given reason.

  Since evaluating this function causes the process to terminate,
  it has no return value.

  Inlined by the compiler.

  ## Examples

  When a process reaches its end, by default it exits with
  reason `:normal`. You can also call `exit/1` explicitly if you
  want to terminate a process but not signal any failure:

      exit(:normal)

  In case something goes wrong, you can also use `exit/1` with
  a different reason:

      exit(:seems_bad)

  If the exit reason is not `:normal`, all the processes linked to the process
  that exited will crash (unless they are trapping exits).

  ## OTP exits

  Exits are used by the OTP to determine if a process exited abnormally
  or not. The following exits are considered "normal":

    * `exit(:normal)`
    * `exit(:shutdown)`
    * `exit({:shutdown, term})`

  Exiting with any other reason is considered abnormal and treated
  as a crash. This means the default supervisor behaviour kicks in,
  error reports are emitted, etc.

  This behaviour is relied on in many different places. For example,
  `ExUnit` uses `exit(:shutdown)` when exiting the test process to
  signal linked processes, supervision trees and so on to politely
  shutdown too.

  ## CLI exits

  Building on top of the exit signals mentioned above, if the
  process started by the command line exits with any of the three
  reasons above, its exit is considered normal and the Operating
  System process will exit with status 0.

  It is, however, possible to customize the Operating System exit
  signal by invoking:

      exit({:shutdown, integer})

  This will cause the OS process to exit with the status given by
  `integer` while signaling all linked OTP processes to politely
  shutdown.

  Any other exit reason will cause the OS process to exit with
  status `1` and linked OTP processes to crash.
  """
  @spec exit(term) :: no_return
  def exit(reason) do
    :erlang.exit(reason)
  end

  @doc """
  Returns the head of a list; raises `ArgumentError` if the list is empty.

  Inlined by the compiler.

  ## Examples

      iex> hd([1, 2, 3, 4])
      1

  """
  @spec hd(list) :: term
  def hd(list) do
    :erlang.hd(list)
  end

  @doc """
  Returns `true` if `term` is an atom; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_atom(term) :: boolean
  def is_atom(term) do
    :erlang.is_atom(term)
  end

  @doc """
  Returns `true` if `term` is a binary; otherwise returns `false`.

  A binary always contains a complete number of bytes.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_binary "foo"
      true
      iex> is_binary <<1::3>>
      false

  """
  @spec is_binary(term) :: boolean
  def is_binary(term) do
    :erlang.is_binary(term)
  end

  @doc """
  Returns `true` if `term` is a bitstring (including a binary); otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_bitstring "foo"
      true
      iex> is_bitstring <<1::3>>
      true

  """
  @spec is_bitstring(term) :: boolean
  def is_bitstring(term) do
    :erlang.is_bitstring(term)
  end

  @doc """
  Returns `true` if `term` is either the atom `true` or the atom `false` (i.e.,
  a boolean); otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_boolean(term) :: boolean
  def is_boolean(term) do
    :erlang.is_boolean(term)
  end

  @doc """
  Returns `true` if `term` is a floating point number; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_float(term) :: boolean
  def is_float(term) do
    :erlang.is_float(term)
  end

  @doc """
  Returns `true` if `term` is a function; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_function(term) :: boolean
  def is_function(term) do
    :erlang.is_function(term)
  end

  @doc """
  Returns `true` if `term` is a function that can be applied with `arity` number of arguments;
  otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_function(fn(x) -> x * 2 end, 1)
      true
      iex> is_function(fn(x) -> x * 2 end, 2)
      false

  """
  @spec is_function(term, non_neg_integer) :: boolean
  def is_function(term, arity) do
    :erlang.is_function(term, arity)
  end

  @doc """
  Returns `true` if `term` is an integer; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_integer(term) :: boolean
  def is_integer(term) do
    :erlang.is_integer(term)
  end

  @doc """
  Returns `true` if `term` is a list with zero or more elements; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_list(term) :: boolean
  def is_list(term) do
    :erlang.is_list(term)
  end

  @doc """
  Returns `true` if `term` is either an integer or a floating point number;
  otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_number(term) :: boolean
  def is_number(term) do
    :erlang.is_number(term)
  end

  @doc """
  Returns `true` if `term` is a pid (process identifier); otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_pid(term) :: boolean
  def is_pid(term) do
    :erlang.is_pid(term)
  end

  @doc """
  Returns `true` if `term` is a port identifier; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_port(term) :: boolean
  def is_port(term) do
    :erlang.is_port(term)
  end

  @doc """
  Returns `true` if `term` is a reference; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_reference(term) :: boolean
  def is_reference(term) do
    :erlang.is_reference(term)
  end

  @doc """
  Returns `true` if `term` is a tuple; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_tuple(term) :: boolean
  def is_tuple(term) do
    :erlang.is_tuple(term)
  end

  @doc """
  Returns `true` if `term` is a map; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_map(term) :: boolean
  def is_map(term) do
    :erlang.is_map(term)
  end

  @doc """
  Returns the length of `list`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> length([1, 2, 3, 4, 5, 6, 7, 8, 9])
      9

  """
  @spec length(list) :: non_neg_integer
  def length(list) do
    :erlang.length(list)
  end

  @doc """
  Returns an almost unique reference.

  The returned reference will re-occur after approximately 2^82 calls;
  therefore it is unique enough for practical purposes.

  Inlined by the compiler.

  ## Examples

      make_ref() #=> #Reference<0.0.0.135>

  """
  @spec make_ref() :: reference
  def make_ref() do
    :erlang.make_ref()
  end

  @doc """
  Returns the size of a map.

  The size of a map is the number of key-value pairs that the map contains.

  This operation happens in constant time.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> map_size(%{a: "foo", b: "bar"})
      2

  """
  @spec map_size(map) :: non_neg_integer
  def map_size(map) do
    :erlang.map_size(map)
  end

  @doc """
  Returns the biggest of the two given terms according to
  Erlang's term ordering. If the terms compare equal, the
  first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> max(1, 2)
      2
      iex> max(:a, :b)
      :b

  """
  @spec max(term, term) :: term
  def max(first, second) do
    :erlang.max(first, second)
  end

  @doc """
  Returns the smallest of the two given terms according to
  Erlang's term ordering. If the terms compare equal, the
  first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> min(1, 2)
      1
      iex> min("foo", "bar")
      "bar"

  """
  @spec min(term, term) :: term
  def min(first, second) do
    :erlang.min(first, second)
  end

  @doc """
  Returns an atom representing the name of the local node.
  If the node is not alive, `:nonode@nohost` is returned instead.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec node() :: node
  def node do
    :erlang.node
  end

  @doc """
  Returns the node where the given argument is located.
  The argument can be a pid, a reference, or a port.
  If the local node is not alive, `:nonode@nohost` is returned.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec node(pid | reference | port) :: node
  def node(arg) do
    :erlang.node(arg)
  end

  @doc """
  Computes the remainder of an integer division.

  Raises an `ArithmeticError` exception if one of the arguments is not an
  integer.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> rem(5, 2)
      1

  """
  @spec rem(integer, integer) :: integer
  def rem(left, right) do
    :erlang.rem(left, right)
  end

  @doc """
  Rounds a number to the nearest integer.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> round(5.6)
      6
      iex> round(5.2)
      5
      iex> round(-9.9)
      -10

  """
  @spec round(number) :: integer
  def round(number) do
    :erlang.round(number)
  end

  @doc """
  Sends a message to the given `dest` and returns the message.

  `dest` may be a remote or local pid, a (local) port, a locally
  registered name, or a tuple `{registered_name, node}` for a registered
  name at another node.

  Inlined by the compiler.

  ## Examples

      iex> send self(), :hello
      :hello

  """
  @spec send(dest :: pid | port | atom | {atom, node}, msg) :: msg when msg: any
  def send(dest, msg) do
    :erlang.send(dest, msg)
  end

  @doc """
  Returns the pid (process identifier) of the calling process.

  Allowed in guard clauses. Inlined by the compiler.
  """
  @spec self() :: pid
  def self() do
    :erlang.self()
  end

  @doc """
  Spawns the given function and returns its pid.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      current = self()
      child   = spawn(fn -> send current, {self(), 1 + 2} end)

      receive do
        {^child, 3} -> IO.puts "Received 3 back"
      end

  """
  @spec spawn((() -> any)) :: pid
  def spawn(fun) do
    :erlang.spawn(fun)
  end

  @doc """
  Spawns the given module and function passing the given args
  and returns its pid.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      spawn(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn(module, atom, list) :: pid
  def spawn(module, fun, args) do
    :erlang.spawn(module, fun, args)
  end

  @doc """
  Spawns the given function, links it to the current process and returns its pid.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      current = self()
      child   = spawn_link(fn -> send current, {self(), 1 + 2} end)

      receive do
        {^child, 3} -> IO.puts "Received 3 back"
      end

  """
  @spec spawn_link((() -> any)) :: pid
  def spawn_link(fun) do
    :erlang.spawn_link(fun)
  end

  @doc """
  Spawns the given module and function passing the given args,
  links it to the current process and returns its pid.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      spawn_link(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn_link(module, atom, list) :: pid
  def spawn_link(module, fun, args) do
    :erlang.spawn_link(module, fun, args)
  end

  @doc """
  Spawns the given function, monitors it and returns its pid
  and monitoring reference.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      current = self()
      spawn_monitor(fn -> send current, {self(), 1 + 2} end)

  """
  @spec spawn_monitor((() -> any)) :: {pid, reference}
  def spawn_monitor(fun) do
    :erlang.spawn_monitor(fun)
  end

  @doc """
  Spawns the given module and function passing the given args,
  monitors it and returns its pid and monitoring reference.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      spawn_monitor(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn_monitor(module, atom, list) :: {pid, reference}
  def spawn_monitor(module, fun, args) do
    :erlang.spawn_monitor(module, fun, args)
  end

  @doc """
  A non-local return from a function. Check `Kernel.SpecialForms.try/1` for more information.

  Inlined by the compiler.
  """
  @spec throw(term) :: no_return
  def throw(term) do
    :erlang.throw(term)
  end

  @doc """
  Returns the tail of a list. Raises `ArgumentError` if the list is empty.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> tl([1, 2, 3, :go])
      [2, 3, :go]
  """
  @spec tl(maybe_improper_list) :: maybe_improper_list
  def tl(list) do
    :erlang.tl(list)
  end

  @doc """
  Returns the integer part of `number`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> trunc(5.4)
      5
      iex> trunc(5.99)
      5

  """
  @spec trunc(number) :: integer
  def trunc(number) do
    :erlang.trunc(number)
  end

  @doc """
  Returns the size of a tuple.

  This operation happens in constant time.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> tuple_size {:a, :b, :c}
      3

  """
  @spec tuple_size(tuple) :: non_neg_integer
  def tuple_size(tuple) do
    :erlang.tuple_size(tuple)
  end

  @doc """
  Arithmetic addition.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 + 2
      3

  """
  @spec (number + number) :: number
  def left + right do
    :erlang.+(left, right)
  end

  @doc """
  Arithmetic subtraction.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 - 2
      -1

  """
  @spec (number - number) :: number
  def left - right do
    :erlang.-(left, right)
  end

  @doc """
  Arithmetic unary plus.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> +1
      1

  """
  @spec (+number) :: number
  def (+value) do
    :erlang.+(value)
  end

  @doc """
  Arithmetic unary minus.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> -2
      -2

  """
  @spec (-number) :: number
  def (-value) do
    :erlang.-(value)
  end

  @doc """
  Arithmetic multiplication.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 * 2
      2

  """
  @spec (number * number) :: number
  def left * right do
    :erlang.*(left, right)
  end

  @doc """
  Arithmetic division.

  The result is always a float. Use `div/2` and `rem/2` if you want
  an integer division or the remainder.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 / 2
      0.5

      iex> 2 / 1
      2.0

  """
  @spec (number / number) :: float
  def left / right do
    :erlang./(left, right)
  end

  @doc """
  Concatenates two lists.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> [1] ++ [2, 3]
      [1, 2, 3]

      iex> 'foo' ++ 'bar'
      'foobar'

  """
  @spec (list ++ term) :: maybe_improper_list
  def left ++ right do
    :erlang.++(left, right)
  end

  @doc """
  Removes the first occurrence of an item on the left list
  for each item on the right.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> [1, 2, 3] -- [1, 2]
      [3]

      iex> [1, 2, 3, 2, 1] -- [1, 2, 2]
      [3, 1]

  """
  @spec (list -- list) :: list
  def left -- right do
    :erlang.--(left, right)
  end

  @doc """
  Boolean not.

  `arg` must be a boolean; if it's not, an `ArgumentError` exception is raised.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> not false
      true

  """
  @spec not(boolean) :: boolean
  def not(arg) do
    :erlang.not(arg)
  end

  @doc """
  Returns `true` if left is less than right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 < 2
      true

  """
  @spec (term < term) :: boolean
  def left < right do
    :erlang.<(left, right)
  end

  @doc """
  Returns `true` if left is more than right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 > 2
      false

  """
  @spec (term > term) :: boolean
  def left > right do
    :erlang.>(left, right)
  end

  @doc """
  Returns `true` if left is less than or equal to right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 <= 2
      true

  """
  @spec (term <= term) :: boolean
  def left <= right do
    :erlang."=<"(left, right)
  end

  @doc """
  Returns `true` if left is more than or equal to right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 >= 2
      false

  """
  @spec (term >= term) :: boolean
  def left >= right do
    :erlang.>=(left, right)
  end

  @doc """
  Returns `true` if the two items are equal.

  This operator considers 1 and 1.0 to be equal. For match
  semantics, use `===` instead.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 == 2
      false

      iex> 1 == 1.0
      true

  """
  @spec (term == term) :: boolean
  def left == right do
    :erlang.==(left, right)
  end

  @doc """
  Returns `true` if the two items are not equal.

  This operator considers 1 and 1.0 to be equal. For match
  comparison, use `!==` instead.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 != 2
      true

      iex> 1 != 1.0
      false

  """
  @spec (term != term) :: boolean
  def left != right do
    :erlang."/="(left, right)
  end

  @doc """
  Returns `true` if the two items are match.

  This operator gives the same semantics as the one existing in
  pattern matching, i.e., `1` and `1.0` are equal, but they do
  not match.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 === 2
      false

      iex> 1 === 1.0
      false

  """
  @spec (term === term) :: boolean
  def left === right do
    :erlang."=:="(left, right)
  end

  @doc """
  Returns `true` if the two items do not match.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 !== 2
      true

      iex> 1 !== 1.0
      true

  """
  @spec (term !== term) :: boolean
  def left !== right do
    :erlang."=/="(left, right)
  end

  @doc """
  Gets the element at the zero-based `index` in `tuple`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, 3}
      iex> elem(tuple, 1)
      :bar

  """
  @spec elem(tuple, non_neg_integer) :: term
  def elem(tuple, index) do
    :erlang.element(index + 1, tuple)
  end

  @doc """
  Inserts `value` at the given zero-based `index` in `tuple`.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, 3}
      iex> put_elem(tuple, 0, :baz)
      {:baz, :bar, 3}

  """
  @spec put_elem(tuple, non_neg_integer, term) :: tuple
  def put_elem(tuple, index, value) do
    :erlang.setelement(index + 1, tuple, value)
  end

  ## Implemented in Elixir

  @doc """
  Boolean or.

  If the first argument is `true`, `true` is returned; otherwise, the second
  argument is returned.

  Requires only the first argument to be a boolean since it short-circuits.
  If the first argument is not a boolean, an `ArgumentError` exception is
  raised.

  Allowed in guard tests.

  ## Examples

      iex> true or false
      true
      iex> false or 42
      42

  """
  defmacro left or right do
    quote do: __op__(:orelse, unquote(left), unquote(right))
  end

  @doc """
  Boolean and.

  If the first argument is `false`, `false` is returned; otherwise, the second
  argument is returned.

  Requires only the first argument to be a boolean since it short-circuits. If
  the first argument is not a boolean, an `ArgumentError` exception is raised.

  Allowed in guard tests.

  ## Examples

      iex> true and false
      false
      iex> true and "yay!"
      "yay!"

  """
  defmacro left and right do
    quote do: __op__(:andalso, unquote(left), unquote(right))
  end

  @doc """
  Boolean not.

  Receives any argument (not just booleans) and returns `true` if the argument
  is `false` or `nil`; returns `false` otherwise.

  Not allowed in guard clauses.

  ## Examples

      iex> !Enum.empty?([])
      false

      iex> !List.first([])
      true

  """
  defmacro !(arg)

  defmacro !({:!, _, [arg]}) do
    optimize_boolean(quote do
      case unquote(arg) do
        x when x in [false, nil] -> false
        _ -> true
      end
    end)
  end

  defmacro !(arg) do
    optimize_boolean(quote do
      case unquote(arg) do
        x when x in [false, nil] -> true
        _ -> false
      end
    end)
  end

  @doc """
  Concatenates two binaries.

  ## Examples

      iex> "foo" <> "bar"
      "foobar"

  The `<>` operator can also be used in pattern matching (and guard clauses) as
  long as the first part is a literal binary:

      iex> "foo" <> x = "foobar"
      iex> x
      "bar"

  `x <> "bar" = "foobar"` would have resulted in a `CompileError` exception.

  """
  defmacro left <> right do
    concats = extract_concatenations({:<>, [], [left, right]})
    quote do: <<unquote_splicing(concats)>>
  end

  # Extracts concatenations in order to optimize many
  # concatenations into one single clause.
  defp extract_concatenations({:<>, _, [left, right]}) do
    [wrap_concatenation(left)|extract_concatenations(right)]
  end

  defp extract_concatenations(other) do
    [wrap_concatenation(other)]
  end

  defp wrap_concatenation(binary) when is_binary(binary) do
    binary
  end

  defp wrap_concatenation(other) do
    {:::, [], [other, {:binary, [], nil}]}
  end

  @doc """
  Raises an exception.

  If the argument `msg` is a binary, it raises a `RuntimeError` exception
  using the given argument as message.

  If `msg` is an atom, it just calls `raise/2` with the atom as the first
  argument and `[]` as the second argument.

  If `msg` is anything else, raises an `ArgumentError` exception.

  ## Examples

      iex> raise "Oops"
      ** (RuntimeError) Oops

      try do
        1 + :foo
      rescue
        x in [ArithmeticError] ->
          IO.puts "that was expected"
          raise x
      end

  """
  defmacro raise(msg) do
    # Try to figure out the type at compilation time
    # to avoid dead code and make dialyzer happy.
    msg = case not is_binary(msg) and bootstraped?(Macro) do
      true  -> Macro.expand(msg, __CALLER__)
      false -> msg
    end

    case msg do
      msg when is_binary(msg) ->
        quote do
          :erlang.error RuntimeError.exception(unquote(msg))
        end
      {:<<>>, _, _} = msg ->
        quote do
          :erlang.error RuntimeError.exception(unquote(msg))
        end
      alias when is_atom(alias) ->
        quote do
          :erlang.error unquote(alias).exception([])
        end
      _ ->
        quote do
          case unquote(msg) do
            msg when is_binary(msg) ->
              :erlang.error RuntimeError.exception(msg)
            atom when is_atom(atom) ->
              :erlang.error atom.exception([])
            %{__struct__: struct, __exception__: true} = other when is_atom(struct) ->
              :erlang.error other
            _ ->
              :erlang.error ArgumentError.exception("raise/1 expects an alias or a string as the first argument")
          end
        end
    end
  end

  @doc """
  Raises an exception.

  Calls the `exception/1` function on the given argument (which has to be a
  module name like `ArgumentError` or `RuntimeError`) passing `attrs` as the
  attributes in order to retrieve the exception struct.

  Any module that contains a call to the `defexception/1` macro automatically
  implements the `exception/1` callback expected by `raise/2`. See the docs for
  `defexception/1` for more information.

  ## Examples

      iex> raise(ArgumentError, message: "Sample")
      ** (ArgumentError) Sample

  """
  defmacro raise(exception, attrs) do
    quote do
      :erlang.error unquote(exception).exception(unquote(attrs))
    end
  end

  @doc """
  Raises an exception preserving a previous stacktrace.

  Works like `raise/1` but does not generate a new stacktrace.

  Notice that `System.stacktrace/0` returns the stacktrace
  of the last exception. That said, it is common to assign
  the stacktrace as the first expression inside a `rescue`
  clause as any other exception potentially raised (and
  rescued) between the rescue clause and the raise call
  may change the `System.stacktrace/0` value.

  ## Examples

      try do
        raise "Oops"
      rescue
        exception ->
          stacktrace = System.stacktrace
          if Exception.message(exception) == "Oops" do
            reraise exception, stacktrace
          end
      end
  """
  defmacro reraise(msg, stacktrace) do
    # Try to figure out the type at compilation time
    # to avoid dead code and make dialyzer happy.

    case Macro.expand(msg, __CALLER__) do
      msg when is_binary(msg) ->
        quote do
          :erlang.raise :error, RuntimeError.exception(unquote(msg)), unquote(stacktrace)
        end
      {:<<>>, _, _} = msg ->
        quote do
          :erlang.raise :error, RuntimeError.exception(unquote(msg)), unquote(stacktrace)
        end
      alias when is_atom(alias) ->
        quote do
          :erlang.raise :error, unquote(alias).exception([]), unquote(stacktrace)
        end
      msg ->
        quote do
          stacktrace = unquote(stacktrace)
          case unquote(msg) do
            msg when is_binary(msg) ->
              :erlang.raise :error, RuntimeError.exception(msg), stacktrace
            atom when is_atom(atom) ->
              :erlang.raise :error, atom.exception([]), stacktrace
            %{__struct__: struct, __exception__: true} = other when is_atom(struct) ->
              :erlang.raise :error, other, stacktrace
            _ ->
              :erlang.error ArgumentError.exception("reraise/2 expects an alias or a string as the first argument")
          end
        end
    end
  end

  @doc """
  Raises an exception preserving a previous stacktrace.

  `reraise/3` works like `reraise/2`, except it passes arguments to the
  `exception/1` function like explained in `raise/2`.

  ## Examples

      try do
        raise "Oops"
      rescue
        exception ->
          stacktrace = System.stacktrace
          reraise WrapperError, [exception: exception], stacktrace
      end
  """
  defmacro reraise(exception, attrs, stacktrace) do
    quote do
      :erlang.raise :error, unquote(exception).exception(unquote(attrs)), unquote(stacktrace)
    end
  end

  @doc """
  Matches the term on the left against the regular expression or string on the
  right. Returns `true` if `left` matches `right` (if it's a regular expression)
  or contains `right` (if it's a string).

  ## Examples

      iex> "abcd" =~ ~r/c(d)/
      true

      iex> "abcd" =~ ~r/e/
      false

      iex> "abcd" =~ "bc"
      true

      iex> "abcd" =~ "ad"
      false

      iex> "abcd" =~ ""
      true

  """

  @spec (String.t =~ (String.t | Regex.t)) :: boolean
  def left =~ "" when is_binary(left), do: true

  def left =~ right when is_binary(left) and is_binary(right) do
    :binary.match(left, right) != :nomatch
  end

  def left =~ right when is_binary(left) do
    Regex.match?(right, left)
  end

  @doc ~S"""
  Inspects the given argument according to the `Inspect` protocol.
  The second argument is a keyword list with options to control
  inspection.

  ## Options

  `inspect/2` accepts a list of options that are internally
  translated to an `Inspect.Opts` struct. Check the docs for
  `Inspect.Opts` to see the supported options.

  ## Examples

      iex> inspect(:foo)
      ":foo"

      iex> inspect [1, 2, 3, 4, 5], limit: 3
      "[1, 2, 3, ...]"

      iex> inspect("olá" <> <<0>>)
      "<<111, 108, 195, 161, 0>>"

      iex> inspect("olá" <> <<0>>, binaries: :as_strings)
      "\"olá\\0\""

      iex> inspect("olá", binaries: :as_binaries)
      "<<111, 108, 195, 161>>"

      iex> inspect('bar')
      "'bar'"

      iex> inspect([0|'bar'])
      "[0, 98, 97, 114]"

      iex> inspect(100, base: :octal)
      "0o144"

      iex> inspect(100, base: :hex)
      "0x64"

  Note that the `Inspect` protocol does not necessarily return a valid
  representation of an Elixir term. In such cases, the inspected result
  must start with `#`. For example, inspecting a function will return:

      inspect fn a, b -> a + b end
      #=> #Function<...>

  """
  @spec inspect(Inspect.t, Keyword.t) :: String.t
  def inspect(arg, opts \\ []) when is_list(opts) do
    opts  = struct(Inspect.Opts, opts)
    limit = case opts.pretty do
      true  -> opts.width
      false -> :infinity
    end
    IO.iodata_to_binary(
      Inspect.Algebra.format(Inspect.Algebra.to_doc(arg, opts), limit)
    )
  end

  @doc """
  Creates and updates structs.

  The `struct` argument may be an atom (which defines `defstruct`)
  or a `struct` itself. The second argument is any `Enumerable` that
  emits two-item tuples (key-value pairs) during enumeration.

  Keys in the `Enumerable` that don't exist in the struct are automatically
  discarded.

  This function is useful for dynamically creating and updating
  structs, as well as for converting maps to structs; in the latter case, just
  inserting the appropriate `:__struct__` field into the map may not be enough
  and `struct/2` should be used instead.

  ## Examples

      defmodule User do
        defstruct name: "john"
      end

      struct(User)
      #=> %User{name: "john"}

      opts = [name: "meg"]
      user = struct(User, opts)
      #=> %User{name: "meg"}

      struct(user, unknown: "value")
      #=> %User{name: "meg"}

      struct(User, %{name: "meg"})
      #=> %User{name: "meg"}

  """
  @spec struct(module | map, Enum.t) :: map
  def struct(struct, kv \\ [])

  def struct(struct, []) when is_atom(struct) or is_tuple(struct) do
    apply(struct, :__struct__, [])
  end

  def struct(struct, kv) when is_atom(struct) or is_tuple(struct) do
    struct(apply(struct, :__struct__, []), kv)
  end

  def struct(%{__struct__: _} = struct, kv) do
    Enum.reduce(kv, struct, fn {k, v}, acc ->
      case :maps.is_key(k, acc) and k != :__struct__ do
        true  -> :maps.put(k, v, acc)
        false -> acc
      end
    end)
  end

  @doc """
  Gets a value from a nested structure.

  Uses the `Access` protocol to traverse the structures
  according to the given `keys`, unless the `key` is a
  function.

  If a key is a function, the function will be invoked
  passing three arguments, the operation (`:get`), the
  data to be accessed, and a function to be invoked next.

  This means `get_in/2` can be extended to provide
  custom lookups. The downside is that functions cannot be
  stored as keys in the accessed data structures.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_in(users, ["john", :age])
      27

  In case any of entries in the middle returns `nil`, `nil` will be returned
  as per the Access protocol:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_in(users, ["unknown", :age])
      nil

  When one of the keys is a function, the function is invoked.
  In the example below, we use a function to get all the maps
  inside a list:

      iex> users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
      iex> all = fn :get, data, next -> Enum.map(data, next) end
      iex> get_in(users, [all, :age])
      [27, 23]

  If the previous value before invoking the function is `nil`,
  the function *will* receive nil as a value and must handle it
  accordingly.
  """
  @spec get_in(Access.t, nonempty_list(term)) :: term
  def get_in(data, keys)

  def get_in(data, [h]) when is_function(h),
    do: h.(:get, data, &(&1))
  def get_in(data, [h|t]) when is_function(h),
    do: h.(:get, data, &get_in(&1, t))

  def get_in(nil, [_]),
    do: nil
  def get_in(nil, [_|t]),
    do: get_in(nil, t)

  def get_in(data, [h]),
    do: Access.get(data, h)
  def get_in(data, [h|t]),
    do: get_in(Access.get(data, h), t)

  @doc """
  Puts a value in a nested structure.

  Uses the `Access` protocol to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users, ["john", :age], 28)
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  In case any of entries in the middle returns `nil`,
  an error will be raised when trying to access it next.
  """
  @spec put_in(Access.t, nonempty_list(term), term) :: Access.t
  def put_in(data, keys, value) do
    elem(get_and_update_in(data, keys, fn _ -> {nil, value} end), 1)
  end

  @doc """
  Updates a key in a nested structure.

  Uses the `Access` protocol to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users, ["john", :age], &(&1 + 1))
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  In case any of entries in the middle returns `nil`,
  an error will be raised when trying to access it next.
  """
  @spec update_in(Access.t, nonempty_list(term), (term -> term)) :: Access.t
  def update_in(data, keys, fun) do
    elem(get_and_update_in(data, keys, fn x -> {nil, fun.(x)} end), 1)
  end

  @doc """
  Gets a value and updates a nested structure.

  It expects a tuple to be returned, containing the value
  retrieved and the update one.

  Uses the `Access` protocol to traverse the structures
  according to the given `keys`, unless the `key` is a
  function.

  If a key is a function, the function will be invoked
  passing three arguments, the operation (`:get_and_update`),
  the data to be accessed, and a function to be invoked next.

  This means `get_and_update_in/3` can be extended to provide
  custom lookups. The downside is that functions cannot be stored
  as keys in the accessed data structures.

  ## Examples

  This function is useful when there is a need to retrieve the current
  value (or something calculated in function of the current value) and
  update it at the same time. For example, it could be used to increase
  the age of a user by one and return the previous age in one pass:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_and_update_in(users, ["john", :age], &{&1, &1 + 1})
      {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

  When one of the keys is a function, the function is invoked.
  In the example below, we use a function to get and increment all
  ages inside a list:

      iex> users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
      iex> all = fn :get_and_update, data, next ->
      ...>   Enum.map(data, next) |> :lists.unzip
      ...> end
      iex> get_and_update_in(users, [all, :age], &{&1, &1 + 1})
      {[27, 23], [%{name: "john", age: 28}, %{name: "meg", age: 24}]}

  If the previous value before invoking the function is `nil`,
  the function *will* receive `nil` as a value and must handle it
  accordingly (be it by failing or providing a sane default).
  """
  @spec get_and_update_in(Access.t, nonempty_list(term),
                          (term -> {get, term})) :: {get, Access.t} when get: var
  def get_and_update_in(data, keys, fun)

  def get_and_update_in(data, [h], fun) when is_function(h),
    do: h.(:get_and_update, data, fun)
  def get_and_update_in(data, [h|t], fun) when is_function(h),
    do: h.(:get_and_update, data, &get_and_update_in(&1, t, fun))

  def get_and_update_in(data, [h], fun),
    do: Access.get_and_update(data, h, fun)
  def get_and_update_in(data, [h|t], fun),
    do: Access.get_and_update(data, h, &get_and_update_in(&1, t, fun))

  @doc """
  Puts a value in a nested structure via the given `path`.

  This is similar to `put_in/3`, except the path is extracted via
  a macro rather than passing a list. For example:

      put_in(opts[:foo][:bar], :baz)

  Is equivalent to:

      put_in(opts, [:foo, :bar], :baz)

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. For more information about the supported path
  expressions, please check `get_and_update_in/2` docs.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users["john"][:age], 28)
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users["john"].age, 28)
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  """
  defmacro put_in(path, value) do
    [h|t] = unnest(path, [], "put_in/2")
    expr  = nest_get_and_update_in(h, t, quote(do: fn _ -> {nil, unquote(value)} end))
    quote do: :erlang.element(2, unquote(expr))
  end

  @doc """
  Updates a nested structure via the given `path`.

  This is similar to `update_in/3`, except the path is extracted via
  a macro rather than passing a list. For example:

      update_in(opts[:foo][:bar], &(&1 + 1))

  Is equivalent to:

      update_in(opts, [:foo, :bar], &(&1 + 1))

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. For more information about the supported path
  expressions, please check `get_and_update_in/2` docs.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users["john"][:age], &(&1 + 1))
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users["john"].age, &(&1 + 1))
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  """
  defmacro update_in(path, fun) do
    [h|t] = unnest(path, [], "update_in/2")
    expr = nest_get_and_update_in(h, t, quote(do: fn x -> {nil, unquote(fun).(x)} end))
    quote do: :erlang.element(2, unquote(expr))
  end

  @doc """
  Gets a value and updates a nested data structure via the given `path`.

  This is similar to `get_and_update_in/3`, except the path is extracted
  via a macro rather than passing a list. For example:

      get_and_update_in(opts[:foo][:bar], &{&1, &1 + 1})

  Is equivalent to:

      get_and_update_in(opts, [:foo, :bar], &{&1, &1 + 1})

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. See the Paths section below.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_and_update_in(users["john"].age, &{&1, &1 + 1})
      {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

  ## Paths

  A path may start with a variable, local or remote call, and must be
  followed by one or more:

    * `foo[bar]` - access a field; in case an intermediate field is not
      present or returns `nil`, an empty map is used

    * `foo.bar` - access a map/struct field; in case the field is not
      present, an error is raised

  Here are some valid paths:

      users["john"][:age]
      users["john"].age
      User.all["john"].age
      all_users()["john"].age

  Here are some invalid ones:

      # Does a remote call after the initial value
      users["john"].do_something(arg1, arg2)

      # Does not access any field
      users

  """
  defmacro get_and_update_in(path, fun) do
    [h|t] = unnest(path, [], "get_and_update_in/2")
    nest_get_and_update_in(h, t, fun)
  end

  defp nest_get_and_update_in([], fun),  do: fun
  defp nest_get_and_update_in(list, fun) do
    quote do
      fn x -> unquote(nest_get_and_update_in(quote(do: x), list, fun)) end
    end
  end

  defp nest_get_and_update_in(h, [{:access, key}|t], fun) do
    quote do
      Access.get_and_update(
        unquote(h),
        unquote(key),
        unquote(nest_get_and_update_in(t, fun))
      )
    end
  end

  defp nest_get_and_update_in(h, [{:map, key}|t], fun) do
    quote do
      Access.Map.get_and_update!(unquote(h), unquote(key), unquote(nest_get_and_update_in(t, fun)))
    end
  end

  defp unnest({{:., _, [Access, :get]}, _, [expr, key]}, acc, kind) do
    unnest(expr, [{:access, key}|acc], kind)
  end

  defp unnest({{:., _, [expr, key]}, _, []}, acc, kind)
      when is_tuple(expr) and
           :erlang.element(1, expr) != :__aliases__ and
           :erlang.element(1, expr) != :__MODULE__ do
    unnest(expr, [{:map, key}|acc], kind)
  end

  defp unnest(other, [], kind) do
    raise ArgumentError,
      "expected expression given to #{kind} to access at least one element, got: #{Macro.to_string other}"
  end

  defp unnest(other, acc, kind) do
    case proper_start?(other) do
      true -> [other|acc]
      false ->
        raise ArgumentError,
          "expression given to #{kind} must start with a variable, local or remote call " <>
          "and be followed by an element access, got: #{Macro.to_string other}"
    end
  end

  defp proper_start?({{:., _, [expr, _]}, _, _args})
    when is_atom(expr)
    when :erlang.element(1, expr) == :__aliases__
    when :erlang.element(1, expr) == :__MODULE__, do: true

  defp proper_start?({atom, _, _args})
    when is_atom(atom), do: true

  defp proper_start?(other),
    do: not is_tuple(other)

  @doc """
  Converts the argument to a string according to the
  `String.Chars` protocol.

  This is the function invoked when there is string interpolation.

  ## Examples

      iex> to_string(:foo)
      "foo"

  """
  # If it is a binary at compilation time, simply return it.
  defmacro to_string(arg) when is_binary(arg), do: arg

  defmacro to_string(arg) do
    quote do: String.Chars.to_string(unquote(arg))
  end

  @doc """
  Converts the argument to a char list according to the `List.Chars` protocol.

  ## Examples

      iex> to_char_list(:foo)
      'foo'

  """
  defmacro to_char_list(arg) do
    quote do: List.Chars.to_char_list(unquote(arg))
  end

  @doc """
  Returns `true` if `term` is `nil`, `false` otherwise.

  Allowed in guard clauses.

  ## Examples

      iex> is_nil(1)
      false

      iex> is_nil(nil)
      true

  """
  defmacro is_nil(term) do
    quote do: unquote(term) == nil
  end

  @doc """
  A convenience macro that checks if the right side (an expression) matches the
  left side (a pattern).

  ## Examples

      iex> match?(1, 1)
      true

      iex> match?(1, 2)
      false

      iex> match?({1, _}, {1, 2})
      true

      iex> map = %{a: 1, b: 2}
      iex> match?(%{a: _}, map)
      true

      iex> a = 1
      iex> match?(^a, 1)
      true

  `match?/2` is very useful when filtering of finding a value in an enumerable:

      list = [{:a, 1}, {:b, 2}, {:a, 3}]
      Enum.filter list, &match?({:a, _}, &1)
      #=> [{:a, 1}, {:a, 3}]

  Guard clauses can also be given to the match:

      list = [{:a, 1}, {:b, 2}, {:a, 3}]
      Enum.filter list, &match?({:a, x} when x < 2, &1)
      #=> [{:a, 1}]

  However, variables assigned in the match will not be available
  outside of the function call (unlike regular pattern matching with the `=`
  operator):

      iex> match?(x, 1)
      true
      iex> binding()
      []

  """
  defmacro match?(pattern, expr)

  # Special case where underscore, which always matches, is passed as the first
  # argument.
  defmacro match?({:_, _, atom}, _right) when is_atom(atom) do
    true
  end

  defmacro match?(left, right) do
    quote do
      case unquote(right) do
        unquote(left) ->
          true
        _ ->
          false
      end
    end
  end

  @doc """
  Reads and writes attributes of the current module.

  The canonical example for attributes is annotating that a module
  implements the OTP behaviour called `gen_server`:

      defmodule MyServer do
        @behaviour :gen_server
        # ... callbacks ...
      end

  By default Elixir supports all the module attributes supported by Erlang, but
  custom attributes can be used as well:

      defmodule MyServer do
        @my_data 13
        IO.inspect @my_data #=> 13
      end

  Unlike Erlang, such attributes are not stored in the module by default since
  it is common in Elixir to use custom attributes to store temporary data that
  will be available at compile-time. Custom attributes may be configured to
  behave closer to Erlang by using `Module.register_attribute/3`.

  Finally, notice that attributes can also be read inside functions:

      defmodule MyServer do
        @my_data 11
        def first_data, do: @my_data
        @my_data 13
        def second_data, do: @my_data
      end

      MyServer.first_data #=> 11
      MyServer.second_data #=> 13

  It is important to note that reading an attribute takes a snapshot of
  its current value. In other words, the value is read at compilation
  time and not at runtime. Check the `Module` module for other functions
  to manipulate module attributes.
  """
  defmacro @(expr)

  # Typespecs attributes are special cased by the compiler so far
  defmacro @({name, _, args}) do
    # Check for Macro as it is compiled later than Module
    case bootstraped?(Module) do
      false -> nil
      true  ->
        assert_module_scope(__CALLER__, :@, 1)
        function? = __CALLER__.function != nil

        case not function? and __CALLER__.context == :match do
          false -> nil
          true  ->
            raise ArgumentError, "invalid write attribute syntax, you probably meant to use: @#{name} expression"
        end

        case is_list(args) and length(args) == 1 and typespec(name) do
          false ->
            do_at(args, name, function?, __CALLER__)
          macro ->
            case bootstraped?(Kernel.Typespec) do
              false -> nil
              true  -> quote do: Kernel.Typespec.unquote(macro)(unquote(hd(args)))
            end
        end
    end
  end

  # @attribute value
  defp do_at([arg], name, function?, env) do
    case function? do
      true ->
        raise ArgumentError, "cannot set attribute @#{name} inside function/macro"
      false ->
        case name do
          :behavior ->
            :elixir_errors.warn env.line, env.file,
                                "@behavior attribute is not supported, please use @behaviour instead"
          _ ->
            :ok
        end

        quote do: Module.put_attribute(__MODULE__, unquote(name), unquote(arg))
    end
  end

  # @attribute or @attribute()
  defp do_at(args, name, function?, env) when is_atom(args) or args == [] do
    stack = env_stacktrace(env)

    case function? do
      true ->
        attr = Module.get_attribute(env.module, name, stack)
        try do
          :elixir_quote.escape(attr, false)
        rescue
          e in [ArgumentError] ->
            raise ArgumentError, "cannot inject attribute @#{name} into function/macro because " <> Exception.message(e)
        else
          {val, _} -> val
        end
      false ->
        escaped = case stack do
          [] -> []
          _  -> Macro.escape(stack)
        end
        quote do: Module.get_attribute(__MODULE__, unquote(name), unquote(escaped))
    end
  end

  # All other cases
  defp do_at(args, name, _function?, _env) do
    raise ArgumentError, "expected 0 or 1 argument for @#{name}, got: #{length(args)}"
  end

  defp typespec(:type),     do: :deftype
  defp typespec(:typep),    do: :deftypep
  defp typespec(:opaque),   do: :defopaque
  defp typespec(:spec),     do: :defspec
  defp typespec(:callback), do: :defcallback
  defp typespec(_),         do: false

  @doc """
  Returns the binding for the given context as a keyword list.

  In the returned result, keys are variable names and values are the
  corresponding variable values.

  If the given `context` is `nil` (by default it is), the binding for the
  current context is returned.

  ## Examples

      iex> x = 1
      iex> binding()
      [x: 1]
      iex> x = 2
      iex> binding()
      [x: 2]

      iex> binding(:foo)
      []
      iex> var!(x, :foo) = 1
      1
      iex> binding(:foo)
      [x: 1]

  """
  defmacro binding(context \\ nil) do
    in_match? = Macro.Env.in_match?(__CALLER__)
    for {v, c} <- __CALLER__.vars, c == context do
      {v, wrap_binding(in_match?, {v, [], c})}
    end
  end

  defp wrap_binding(true, var) do
    quote do: ^(unquote(var))
  end

  defp wrap_binding(_, var) do
    var
  end

  @doc """
  Provides an `if` macro.

  This macro expects the first argument to be a condition and the second
  argument to be a keyword list.

  ## One-liner examples

      if(foo, do: bar)

  In the example above, `bar` will be returned if `foo` evaluates to
  `true` (i.e., it is neither `false` nor `nil`). Otherwise, `nil` will be
  returned.

  An `else` option can be given to specify the opposite:

      if(foo, do: bar, else: baz)

  ## Blocks examples

  It's also possible to pass a block to the `if` macro. The first
  example above would be translated to:

      if foo do
        bar
      end

  Note that `do/end` become delimiters. The second example would
  translate to:

      if foo do
        bar
      else
        baz
      end

  In order to compare more than two clauses, the `cond/1` macro has to be used.
  """
  defmacro if(condition, clauses) do
    do_clause = Keyword.get(clauses, :do, nil)
    else_clause = Keyword.get(clauses, :else, nil)

    optimize_boolean(quote do
      case unquote(condition) do
        x when x in [false, nil] -> unquote(else_clause)
        _ -> unquote(do_clause)
      end
    end)
  end

  @doc """
  Provides an `unless` macro.

  This macro evaluates and returns the `do` block passed in as the second
  argument unless `clause` evaluates to `true`. Otherwise, it returns the value
  of the `else` block if present or `nil` if not.

  See also `if/2`.

  ## Examples

      iex> unless(Enum.empty?([]), do: "Hello")
      nil

      iex> unless(Enum.empty?([1, 2, 3]), do: "Hello")
      "Hello"

      iex> unless Enum.sum([2, 2]) == 5 do
      ...>   "Math still works"
      ...> else
      ...>   "Math is broken"
      ...> end
      "Math still works"

  """
  defmacro unless(clause, options) do
    do_clause   = Keyword.get(options, :do, nil)
    else_clause = Keyword.get(options, :else, nil)
    quote do
      if(unquote(clause), do: unquote(else_clause), else: unquote(do_clause))
    end
  end

  @doc """
  Destructures two lists, assigning each term in the
  right one to the matching term in the left one.

  Unlike pattern matching via `=`, if the sizes of the left
  and right lists don't match, destructuring simply stops
  instead of raising an error.

  ## Examples

      iex> destructure([x, y, z], [1, 2, 3, 4, 5])
      iex> {x, y, z}
      {1, 2, 3}

  In the example above, even though the right list has more entries than the
  left one, destructuring works fine. If the right list is smaller, the
  remaining items are simply set to `nil`:

      iex> destructure([x, y, z], [1])
      iex> {x, y, z}
      {1, nil, nil}

  The left-hand side supports any expression you would use
  on the left-hand side of a match:

      x = 1
      destructure([^x, y, z], [1, 2, 3])

  The example above will only work if `x` matches the first value in the right
  list. Otherwise, it will raise a `MatchError` (like the `=` operator would
  do).
  """
  defmacro destructure(left, right) when is_list(left) do
    Enum.reduce left, right, fn item, acc ->
      {:case, meta, args} =
        quote do
          case unquote(acc) do
            [h|t] ->
              unquote(item) = h
              t
            other when other == [] or other == nil ->
              unquote(item) = nil
              []
          end
        end
      {:case, meta, args}
    end
  end

  @doc """
  Returns a range with the specified start and end.

  Both ends are included.

  ## Examples

      iex> 0 in 1..3
      false

      iex> 1 in 1..3
      true

      iex> 2 in 1..3
      true

      iex> 3 in 1..3
      true

  """
  defmacro first .. last do
    {:%{}, [], [__struct__: Elixir.Range, first: first, last: last]}
  end

  @doc """
  Provides a short-circuit operator that evaluates and returns
  the second expression only if the first one evaluates to `true`
  (i.e., it is not `nil` nor `false`). Returns the first expression
  otherwise.

  Not allowed in guard clauses.

  ## Examples

      iex> Enum.empty?([]) && Enum.empty?([])
      true

      iex> List.first([]) && true
      nil

      iex> Enum.empty?([]) && List.first([1])
      1

      iex> false && throw(:bad)
      false


  Note that, unlike Erlang's `and` operator,
  this operator accepts any expression as the first argument,
  not only booleans.
  """
  defmacro left && right do
    quote do
      case unquote(left) do
        x when x in [false, nil] ->
          x
        _ ->
          unquote(right)
      end
    end
  end

  @doc """
  Provides a short-circuit operator that evaluates and returns the second
  expression only if the first one does not evaluate to `true` (i.e., it
  is either `nil` or `false`). Returns the first expression otherwise.

  Not allowed in guard clauses.

  ## Examples

      iex> Enum.empty?([1]) || Enum.empty?([1])
      false

      iex> List.first([]) || true
      true

      iex> Enum.empty?([1]) || 1
      1

      iex> Enum.empty?([]) || throw(:bad)
      true

  Note that, unlike Erlang's `or` operator,
  this operator accepts any expression as the first argument,
  not only booleans.
  """
  defmacro left || right do
    quote do
      case unquote(left) do
        x when x in [false, nil] ->
          unquote(right)
        x ->
          x
      end
    end
  end

  @doc """
  Pipe operator.

  This operator introduces the expression on the left-hand side as
  the first argument to the function call on the right-hand side.

  ## Examples

      iex> [1, [2], 3] |> List.flatten()
      [1, 2, 3]

  The example above is the same as calling `List.flatten([1, [2], 3])`.

  The `|>` operator is mostly useful when there is a desire to execute a series
  of operations resembling a pipeline:

      iex> [1, [2], 3] |> List.flatten |> Enum.map(fn x -> x * 2 end)
      [2, 4, 6]

  In the example above, the list `[1, [2], 3]` is passed as the first argument
  to the `List.flatten/1` function, then the flattened list is passed as the
  first argument to the `Enum.map/2` function which doubles each element of the
  list.

  In other words, the expression above simply translates to:

      Enum.map(List.flatten([1, [2], 3]), fn x -> x * 2 end)

  Beware of operator precedence when using the pipe operator.
  For example, the following expression:

      String.graphemes "Hello" |> Enum.reverse

  Translates to:

      String.graphemes("Hello" |> Enum.reverse)

  which results in an error as the `Enumerable` protocol is not defined
  for binaries. Adding explicit parentheses resolves the ambiguity:

      String.graphemes("Hello") |> Enum.reverse

  Or, even better:

      "Hello" |> String.graphemes |> Enum.reverse

  """
  defmacro left |> right do
    [{h, _}|t] = Macro.unpipe({:|>, [], [left, right]})
    :lists.foldl fn {x, pos}, acc -> Macro.pipe(acc, x, pos) end, h, t
  end

  @doc """
  Returns `true` if `module` is loaded and contains a
  public `function` with the given `arity`, otherwise `false`.

  Note that this function does not load the module in case
  it is not loaded. Check `Code.ensure_loaded/1` for more
  information.

  ## Examples

      iex> function_exported?(Enum, :member?, 2)
      true

  """
  @spec function_exported?(atom | tuple, atom, arity) :: boolean
  def function_exported?(module, function, arity) do
    :erlang.function_exported(module, function, arity)
  end

  @doc """
  Returns `true` if `module` is loaded and contains a
  public `macro` with the given `arity`, otherwise `false`.

  Note that this function does not load the module in case
  it is not loaded. Check `Code.ensure_loaded/1` for more
  information.

  ## Examples

      iex> macro_exported?(Kernel, :use, 2)
      true

  """
  @spec macro_exported?(atom, atom, integer) :: boolean
  def macro_exported?(module, macro, arity) do
    case :code.is_loaded(module) do
      {:file, _} -> :lists.member({macro, arity}, module.__info__(:macros))
      _ -> false
    end
  end

  @doc """
  Checks if the element on the left-hand side is a member of the
  collection on the right-hand side.

  ## Examples

      iex> x = 1
      iex> x in [1, 2, 3]
      true

  This operator (which is a macro) simply translates to a call to
  `Enum.member?/2`. The example above would translate to:

      Enum.member?([1, 2, 3], x)

  ## Guards

  The `in` operator can be used in guard clauses as long as the
  right-hand side is a range or a list. In such cases, Elixir will expand the
  operator to a valid guard expression. For example:

      when x in [1, 2, 3]

  translates to:

      when x === 1 or x === 2 or x === 3

  When using ranges:

      when x in 1..3

  translates to:

      when x >= 1 and x <= 3

  """
  defmacro left in right do
    in_module? = (__CALLER__.context == nil)

    right = case bootstraped?(Macro) and not in_module? do
      true  -> Macro.expand(right, __CALLER__)
      false -> right
    end

    case right do
      _ when in_module? ->
        quote do: Elixir.Enum.member?(unquote(right), unquote(left))
      [] ->
        false
      [h|t] ->
        :lists.foldr(fn x, acc ->
          quote do
            unquote(comp(left, x)) or unquote(acc)
          end
        end, comp(left, h), t)
      {:%{}, [], [__struct__: Elixir.Range, first: first, last: last]} ->
        in_range(left, Macro.expand(first, __CALLER__), Macro.expand(last, __CALLER__))
      _ ->
        raise ArgumentError, <<"invalid args for operator in, it expects a compile time list ",
                                        "or range on the right side when used in guard expressions, got: ",
                                        Macro.to_string(right) :: binary>>
    end
  end

  defp in_range(left, first, last) do
    case opt_in?(first) and opt_in?(last) do
      true  ->
        case first <= last do
          true  -> increasing_compare(left, first, last)
          false -> decreasing_compare(left, first, last)
        end
      false ->
        quote do
          (:erlang."=<"(unquote(first), unquote(last)) and
           unquote(increasing_compare(left, first, last)))
          or
          (:erlang."<"(unquote(last), unquote(first)) and
           unquote(decreasing_compare(left, first, last)))
        end
    end
  end

  defp opt_in?(x), do: is_integer(x) or is_float(x) or is_atom(x)

  defp comp(left, right) do
    quote(do: :erlang."=:="(unquote(left), unquote(right)))
  end

  defp increasing_compare(var, first, last) do
    quote do
      :erlang.">="(unquote(var), unquote(first)) and
      :erlang."=<"(unquote(var), unquote(last))
    end
  end

  defp decreasing_compare(var, first, last) do
    quote do
      :erlang."=<"(unquote(var), unquote(first)) and
      :erlang.">="(unquote(var), unquote(last))
    end
  end

  @doc """
  When used inside quoting, marks that the given variable should
  not be hygienized.

  The argument can be either a variable unquoted or in standard tuple form
  `{name, meta, context}`.

  Check `Kernel.SpecialForms.quote/2` for more information.
  """
  defmacro var!(var, context \\ nil)

  defmacro var!({name, meta, atom}, context) when is_atom(name) and is_atom(atom) do
    do_var!(name, meta, context, __CALLER__)
  end

  defmacro var!(x, _context) do
    raise ArgumentError, "expected a var to be given to var!, got: #{Macro.to_string(x)}"
  end

  defp do_var!(name, meta, context, env) do
    # Remove counter and force them to be vars
    meta = :lists.keydelete(:counter, 1, meta)
    meta = :lists.keystore(:var, 1, meta, {:var, true})

    case Macro.expand(context, env) do
      x when is_atom(x) ->
        {name, meta, x}
      x ->
        raise ArgumentError, "expected var! context to expand to an atom, got: #{Macro.to_string(x)}"
    end
  end

  @doc """
  When used inside quoting, marks that the given alias should not
  be hygienized. This means the alias will be expanded when
  the macro is expanded.

  Check `Kernel.SpecialForms.quote/2` for more information.
  """
  defmacro alias!(alias)

  defmacro alias!(alias) when is_atom(alias) do
    alias
  end

  defmacro alias!({:__aliases__, meta, args}) do
    # Simply remove the alias metadata from the node
    # so it does not affect expansion.
    {:__aliases__, :lists.keydelete(:alias, 1, meta), args}
  end

  ## Definitions implemented in Elixir

  @doc ~S"""
  Defines a module given by name with the given contents.

  This macro defines a module with the given `alias` as its name and with the
  given contents. It returns a tuple with four elements:

    * `:module`
    * the module name
    * the binary contents of the module
    * the result of evaluating the contents block

  ## Examples

      iex> defmodule Foo do
      ...>   def bar, do: :baz
      ...> end
      iex> Foo.bar
      :baz

  ## Nesting

  Nesting a module inside another module affects the name of the nested module:

      defmodule Foo do
        defmodule Bar do
        end
      end

  In the example above, two modules - `Foo` and `Foo.Bar` - are created.
  When nesting, Elixir automatically creates an alias to the inner module,
  allowing the second module `Foo.Bar` to be accessed as `Bar` in the same
  lexical scope where it's defined (the `Foo` module).

  If the `Foo.Bar` module is moved somewhere else, the references to `Bar` in
  the `Foo` module need to be updated to the fully-qualified name (`Foo.Bar`) or
  an alias has to be explicitly set in the `Foo` module with the help of
  `Kernel.SpecialForms.alias/2`.

      defmodule Foo.Bar do
        # code
      end

      defmodule Foo do
        alias Foo.Bar
        # code here can refer to `Foo.Bar` as just `Bar`
      end

  ## Dynamic names

  Elixir module names can be dynamically generated. This is very
  useful when working with macros. For instance, one could write:

      defmodule String.to_atom("Foo#{1}") do
        # contents ...
      end

  Elixir will accept any module name as long as the expression passed as the
  first argument to `defmodule/2` evaluates to an atom.
  Note that, when a dynamic name is used, Elixir won't nest the name under the
  current module nor automatically set up an alias.

  """
  defmacro defmodule(alias, do: block) do
    env   = __CALLER__
    boot? = bootstraped?(Macro)

    expanded =
      case boot? do
        true  -> Macro.expand(alias, env)
        false -> alias
      end

    {expanded, with_alias} =
      case boot? and is_atom(expanded) do
        true ->
          # Expand the module considering the current environment/nesting
          full = expand_module(alias, expanded, env)

          # Generate the alias for this module definition
          {new, old} = module_nesting(env.module, full)
          meta = [defined: full, context: env.module] ++ alias_meta(alias)

          {full, {:alias, meta, [old, [as: new, warn: false]]}}
        false ->
          {expanded, nil}
      end

    {escaped, _} = :elixir_quote.escape(block, false)
    module_vars  = module_vars(env.vars, 0)

    quote do
      unquote(with_alias)
      :elixir_module.compile(unquote(expanded), unquote(escaped),
                             unquote(module_vars), __ENV__)
    end
  end

  defp alias_meta({:__aliases__, meta, _}), do: meta
  defp alias_meta(_), do: []

  # defmodule :foo
  defp expand_module(raw, _module, _env) when is_atom(raw),
    do: raw

  # defmodule Elixir.Alias
  defp expand_module({:__aliases__, _, [:Elixir|t]}, module, _env) when t != [],
    do: module

  # defmodule Alias in root
  defp expand_module({:__aliases__, _, _}, module, %{module: nil}),
    do: module

  # defmodule Alias nested
  defp expand_module({:__aliases__, _, t}, _module, env),
    do: :elixir_aliases.concat([env.module|t])

  # defmodule _
  defp expand_module(_raw, module, env),
    do: :elixir_aliases.concat([env.module, module])

  # quote vars to be injected into the module definition
  defp module_vars([{key, kind}|vars], counter) do
    var =
      case is_atom(kind) do
        true  -> {key, [], kind}
        false -> {key, [counter: kind], nil}
      end

    under = String.to_atom(<<"_@", :erlang.integer_to_binary(counter)::binary>>)
    args  = [key, kind, under, var]
    [{:{}, [], args}|module_vars(vars, counter+1)]
  end

  defp module_vars([], _counter) do
    []
  end

  # Gets two modules names and return an alias
  # which can be passed down to the alias directive
  # and it will create a proper shortcut representing
  # the given nesting.
  #
  # Examples:
  #
  #     module_nesting('Elixir.Foo.Bar', 'Elixir.Foo.Bar.Baz.Bat')
  #     {'Elixir.Baz', 'Elixir.Foo.Bar.Baz'}
  #
  # In case there is no nesting/no module:
  #
  #     module_nesting(nil, 'Elixir.Foo.Bar.Baz.Bat')
  #     {false, 'Elixir.Foo.Bar.Baz.Bat'}
  #
  defp module_nesting(nil, full),
    do: {false, full}

  defp module_nesting(prefix, full) do
    case split_module(prefix) do
      [] -> {false, full}
      prefix -> module_nesting(prefix, split_module(full), [], full)
    end
  end

  defp module_nesting([x|t1], [x|t2], acc, full),
    do: module_nesting(t1, t2, [x|acc], full)
  defp module_nesting([], [h|_], acc, _full),
    do: {String.to_atom(<<"Elixir.", h::binary>>),
          :elixir_aliases.concat(:lists.reverse([h|acc]))}
  defp module_nesting(_, _, _acc, full),
    do: {false, full}

  defp split_module(atom) do
    case :binary.split(Atom.to_string(atom), ".", [:global]) do
      ["Elixir"|t] -> t
      _ -> []
    end
  end

  @doc """
  Defines a function with the given name and body.

  ## Examples

      defmodule Foo do
        def bar, do: :baz
      end

      Foo.bar #=> :baz

  A function that expects arguments can be defined as follows:

      defmodule Foo do
        def sum(a, b) do
          a + b
        end
      end

  In the example above, a `sum/2` function is defined; this function receives
  two arguments and returns their sum.

  """
  defmacro def(call, expr \\ nil) do
    define(:def, call, expr, __CALLER__)
  end

  @doc """
  Defines a private function with the given name and body.

  Private functions are only accessible from within the module in which they are
  defined. Trying to access a private function from outside the module it's
  defined in results in an `UndefinedFunctionError` exception.

  Check `def/2` for more information.

  ## Examples

      defmodule Foo do
        def bar do
          sum(1, 2)
        end

        defp sum(a, b), do: a + b
      end

      Foo.bar #=> 3
      Foo.sum(1, 2) #=> ** (UndefinedFunctionError) undefined function: Foo.sum/2

  """
  defmacro defp(call, expr \\ nil) do
    define(:defp, call, expr, __CALLER__)
  end

  @doc """
  Defines a macro with the given name and body.

  ## Examples

      defmodule MyLogic do
        defmacro unless(expr, opts) do
          quote do
            if !unquote(expr), unquote(opts)
          end
        end
      end

      require MyLogic
      MyLogic.unless false do
        IO.puts "It works"
      end

  """
  defmacro defmacro(call, expr \\ nil) do
    define(:defmacro, call, expr, __CALLER__)
  end

  @doc """
  Defines a private macro with the given name and body.

  Private macros are only accessible from the same module in which they are
  defined.

  Check `defmacro/2` for more information.

  """
  defmacro defmacrop(call, expr \\ nil) do
    define(:defmacrop, call, expr, __CALLER__)
  end

  defp define(kind, call, expr, env) do
    assert_module_scope(env, kind, 2)
    assert_no_function_scope(env, kind, 2)
    line = env.line

    {call, uc} = :elixir_quote.escape(call, true)
    {expr, ue} = :elixir_quote.escape(expr, true)

    # Do not check clauses if any expression was unquoted
    check_clauses = not(ue or uc)
    pos = :elixir_locals.cache_env(env)

    quote do
      :elixir_def.store_definition(unquote(line), unquote(kind), unquote(check_clauses),
                                   unquote(call), unquote(expr), unquote(pos))
    end
  end

  @doc """
  Defines a struct.

  A struct is a tagged map that allows developers to provide
  default values for keys, tags to be used in polymorphic
  dispatches and compile time assertions.

  The only thing needed to define a struct is a `__struct__/0` function that
  returns a map with the struct fields and their default values. `defstruct/1`
  is a convenience macro which defines such a function (as well as a `t` type
  and deriving conveniences).

  When using `defstruct/1`, a struct named like the enclosing module is defined.

  For more information about structs, please check `Kernel.SpecialForms.%/2`.

  ## Examples

      defmodule User do
        defstruct name: nil, age: nil
      end

  Struct fields are evaluated at compile-time, which allows
  them to be dynamic. In the example below, `10 + 11` is
  evaluated at compile-time and the age field is stored
  with value `21`:

      defmodule User do
        defstruct name: nil, age: 10 + 11
      end

  The `fields` argument is usually a keyword list with fields as keys and
  default values as corresponding values. `defstruct/1` also supports a list of
  atoms as its argument: in that case, the atoms in the list will be used as
  the struct's fields and they will all default to `nil`.

      defmodule Post do
        defstruct [:title, :content, :author]
      end

  ## Deriving

  Although structs are maps, by default structs do not implement
  any of the protocols implemented for maps. For example, attempting
  to use a protocol with the `User` struct leads to an error:

      john = %User{name: "John"}
      MyProtocol.call(john)
      ** (Protocol.UndefinedError) protocol MyProtocol not implemented for %User{...}

  `defstruct/1`, however, allows protocol implementations to be
  *derived*. This can be done by defining a `@derive` attribute as a
  list before invoking `defstruct/1`:

      defmodule User do
        @derive [MyProtocol]
        defstruct name: nil, age: 10 + 11
      end

      MyProtocol.call(john) #=> works

  For each protocol in the `@derive` list, Elixir will assert there is an
  implementation of that protocol for any (regardless if fallback to any
  is true) and check if the any implementation defines a `__deriving__/3`
  callback. If so, the callback is invoked, otherwise an implementation
  that simply points to the any implementation is automatically derived.

  ## Types

  It is recommended to define types for structs. By convention such type
  is called `t`. To define a struct inside a type, the struct literal syntax
  is used:

      defmodule User do
        defstruct name: "John", age: 25
        @type t :: %User{name: String.t, age: non_neg_integer}
      end

  It is recommended to only use the struct syntax when defining the struct's
  type. When referring to another struct it's better to use `User.t`instead of
  `%User{}`.

  The types of the struct fields that are not included in the struct's type
  default to `term`.

  Structs whose internal structure is private to the local module (pattern
  matching them or directly accessing their fields should not be allowed) should
  use the `@opaque` attribute. Structs whose internal structure is public should
  use `@type`. See `Kernel.Typespec` for more information on opaque types.

  """
  defmacro defstruct(fields) do
    quote bind_quoted: [fields: fields] do
      fields = Kernel.Def.struct(__MODULE__, fields)
      @struct fields

      case Module.get_attribute(__MODULE__, :derive) do
        [] -> :ok
        derive -> Protocol.__derive__(derive, __MODULE__, __ENV__)
      end

      def __struct__() do
        @struct
      end

      fields
    end
  end

  @doc ~S"""
  Defines an exception.

  Exceptions are structs backed by a module that implements
  the `Exception` behaviour. The `Exception` behaviour requires
  two functions to be implemented:

    * `exception/1` - receives the arguments given to `raise/2`
       and returns the exception struct. The default implementation
       accepts either a set of keyword arguments that is merged into
       the struct or a string to be used as the exception's message.

    * `message/1` - receives the exception struct and must return its
      message. Most commonly exceptions have a message field which
      by default is accessed by this function. However, if an exception
      does not have a message field, this function must be explicitly
      implemented.

  Since exceptions are structs, the API supported by `defstruct/1`
  is also available in `defexception/1`.

  ## Raising exceptions

  The most common way to raise an exception is via `raise/2`:

      defmodule MyAppError do
        defexception [:message]
      end

      value = [:hello]

      raise MyAppError,
        message: "did not get what was expected, got: #{inspect value}"

  In many cases it is more convenient to pass the expected value to
  `raise/2` and generate the message in the `exception/1` callback:

      defmodule MyAppError do
        defexception [:message]

        def exception(value) do
          msg = "did not get what was expected, got: #{inspect value}"
          %MyAppError{message: msg}
        end
      end

      raise MyAppError, value

  The example above shows the preferred strategy for customizing
  exception messages.

  """
  defmacro defexception(fields) do
    fields = case is_list(fields) do
      true  -> [{:__exception__, true}|fields]
      false -> quote(do: [{:__exception__, true}] ++ unquote(fields))
    end

    quote do
      @behaviour Exception
      fields = defstruct unquote(fields)

      @spec exception(String.t) :: Exception.t
      def exception(msg) when is_binary(msg) do
        exception(message: msg)
      end

      @spec exception(Keyword.t) :: Exception.t
      def exception(args) when is_list(args) do
        Kernel.struct(__struct__, args)
      end

      defoverridable exception: 1

      if Map.has_key?(fields, :message) do
        @spec message(Exception.t) :: String.t
        def message(exception) do
          exception.message
        end

        defoverridable message: 1
      end
    end
  end

  @doc """
  Defines a protocol.

  A protocol specifies an API that should be defined by its
  implementations.

  ## Examples

  In Elixir, only `false` and `nil` are considered falsy values.
  Everything else evaluates to `true` in `if` clauses. Depending
  on the application, it may be important to specify a `blank?`
  protocol that returns a boolean for other data types that should
  be considered "blank". For instance, an empty list or an empty
  binary could be considered blank.

  Such protocol could be implemented as follows:

      defprotocol Blank do
        @doc "Returns `true` if `data` is considered blank/empty"
        def blank?(data)
      end

  Now that the protocol is defined it can be implemented. It needs to be
  implemented for each Elixir type; for example:

      # Integers are never blank
      defimpl Blank, for: Integer do
        def blank?(number), do: false
      end

      # The only blank list is the empty one
      defimpl Blank, for: List do
        def blank?([]), do: true
        def blank?(_),  do: false
      end

      # The only blank atoms are `false` and `nil`
      defimpl Blank, for: Atom do
        def blank?(false), do: true
        def blank?(nil),   do: true
        def blank?(_),     do: false
      end

  The implementation of the `Blank` protocol would need to be defined for all
  Elixir types. The available types are:

    * Structs (see below)
    * `Tuple`
    * `Atom`
    * `List`
    * `BitString`
    * `Integer`
    * `Float`
    * `Function`
    * `PID`
    * `Map`
    * `Port`
    * `Reference`
    * `Any` (see below)

  ## Protocols and Structs

  The real benefit of protocols comes when mixed with structs.
  For instance, Elixir ships with many data types implemented as
  structs, like `HashDict` and `HashSet`. We can implement the
  `Blank` protocol for those types as well:

      defimpl Blank, for: [HashDict, HashSet] do
        def blank?(enum_like), do: Enum.empty?(enum_like)
      end

  When implementing a protocol for a struct, the `:for` option can be omitted if
  the `defimpl` call is inside the module that defines the struct:

      defmodule User do
        defstruct [:email, :name]

        defimpl Blank do
          def blank?(%User{}), do: false
        end
      end

  If a protocol is not found for a given type, it will fallback to
  `Any`.

  ## Fallback to any

  In some cases, it may be convenient to provide a default
  implementation for all types. This can be achieved by
  setting the `@fallback_to_any` attribute to `true` in the protocol
  definition:

      defprotocol Blank do
        @fallback_to_any true
        def blank?(data)
      end

  The `Blank` protocol can now be implemented for `Any`:

      defimpl Blank, for: Any do
        def blank?(_), do: true
      end

  One may wonder why such behaviour (fallback to any) is not the default one.

  It is two-fold: first, the majority of protocols cannot
  implement an action in a generic way for all types; in fact,
  providing a default implementation may be harmful, because users
  may rely on the default implementation instead of providing a
  specialized one.

  Second, falling back to `Any` adds an extra lookup to all types,
  which is unnecessary overhead unless an implementation for `Any` is
  required.

  ## Types

  Defining a protocol automatically defines a type named `t`, which
  can be used as follows:

      @spec present?(Blank.t) :: boolean
      def present?(blank) do
        not Blank.blank?(blank)
      end

  The `@spec` above expresses that all types allowed to implement the
  given protocol are valid argument types for the given function.

  ## Reflection

  Any protocol module contains three extra functions:

    * `__protocol__/1` - returns the protocol name when `:name` is given, and a
      keyword list with the protocol functions and their arities when
      `:functions` is given

    * `impl_for/1` - receives a structure and returns the module that
      implements the protocol for the structure, `nil` otherwise

    * `impl_for!/1` - same as above but raises an error if an implementation is
      not found

      Enumerable.__protocol__(:functions)
      #=> [count: 1, member?: 2, reduce: 3]

      Enumerable.impl_for([])
      #=> Enumerable.List

      Enumerable.impl_for(42)
      #=> nil

  ## Consolidation

  In order to cope with code loading in development, protocols in
  Elixir provide a slow implementation of protocol dispatching specific
  to development.

  In order to speed up dispatching in production environments, where
  all implementations are known up-front, Elixir provides a feature
  called protocol consolidation. For this reason, all protocols are
  compiled with `debug_info` set to `true`, regardless of the option
  set by `elixirc` compiler. The debug info though may be removed
  after consolidation.

  For more information on how to apply protocol consolidation to
  a given project, please check the functions in the `Protocol`
  module or the `mix compile.protocols` task.
  """
  defmacro defprotocol(name, do: block) do
    Protocol.__protocol__(name, do: block)
  end

  @doc """
  Defines an implementation for the given protocol.

  See `defprotocol/2` for more information and examples on protocols.

  Inside an implementation, the name of the protocol can be accessed
  via `@protocol` and the current target as `@for`.
  """
  defmacro defimpl(name, opts, do_block \\ []) do
    merged = Keyword.merge(opts, do_block)
    merged = Keyword.put_new(merged, :for, __CALLER__.module)
    Protocol.__impl__(name, merged)
  end

  @doc """
  Makes the given functions in the current module overridable.

  An overridable function is lazily defined, allowing a developer to override
  it.

  ## Example

      defmodule DefaultMod do
        defmacro __using__(_opts) do
          quote do
            def test(x, y) do
              x + y
            end

            defoverridable [test: 2]
          end
        end
      end

      defmodule InheritMod do
        use DefaultMod

        def test(x, y) do
          x * y + super(x, y)
        end
      end

  As seen as in the example above, `super` can be used to call the default
  implementation.

  """
  defmacro defoverridable(keywords) do
    quote do
      Module.make_overridable(__MODULE__, unquote(keywords))
    end
  end

  @doc """
  `use` is a simple mechanism for using a given module into
  the current context.

  ## Examples

  For example, in order to write tests using the ExUnit framework,
  a developer should use the `ExUnit.Case` module:

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        test "always pass" do
          assert true
        end
      end

  By calling `use`, a hook called `__using__` will be invoked in
  `ExUnit.Case` which will then do the proper setup.

  Simply put, `use` translates to to:

      defmodule AssertionTest do
        require ExUnit.Case
        ExUnit.Case.__using__([async: true])

        test "always pass" do
          assert true
        end
      end

  `__using__/1` is just a regular macro that can be defined in any module:

      defmodule MyModule do
        defmacro __using__(opts) do
          quote do
            # code that will run in the module that uses MyModule
          end
        end
      end

  """
  defmacro use(module, opts \\ []) do
    expanded = Macro.expand(module, __CALLER__)

    case is_atom(expanded) do
      false ->
        raise ArgumentError, "invalid arguments for use, expected an atom or alias as argument"
      true ->
        quote do
          require unquote(expanded)
          unquote(expanded).__using__(unquote(opts))
        end
    end
  end

  @doc """
  Define a function that delegates to another module.

  Functions defined with `defdelegate/2` are public and can be invoked from
  outside the module they're defined in (like if they were defined using
  `def/2`). When the desire is to delegate as private functions, `import` should
  be used.

  Delegation only works with functions; delegating macros is not supported.

  ## Options

    * `:to` - the expression to delegate to. Any expression
      is allowed and its results will be evaluated at runtime. Usually
      evaluates to the name of a module.

    * `:as` - the function to call on the target given in `:to`.
      This parameter is optional and defaults to the name being
      delegated (`funs`).

    * `:append_first` - if `true`, when delegated, the first argument
      passed to the delegated function will be relocated to the end of the
      arguments when dispatched to the target.

      The motivation behind this is because Elixir normalizes
      the "handle" as the first argument while some Erlang modules
      expect it as the last argument.

  ## Examples

      defmodule MyList do
        defdelegate reverse(list), to: :lists
        defdelegate other_reverse(list), to: :lists, as: :reverse
        defdelegate [reverse(list), map(list, callback)], to: :lists, append_first: true
      end

      MyList.reverse([1, 2, 3])
      #=> [3, 2, 1]

      MyList.other_reverse([1, 2, 3])
      #=> [3, 2, 1]

      MyList.map([1, 2, 3], &(&1 * 2))
      #=> [2, 4, 6]

  """
  defmacro defdelegate(funs, opts) do
    funs = Macro.escape(funs, unquote: true)
    quote bind_quoted: [funs: funs, opts: opts] do
      target = Keyword.get(opts, :to) ||
        raise ArgumentError, "expected to: to be given as argument"

      for fun <- List.wrap(funs) do
        {name, args, as, as_args} = Kernel.Def.delegate(fun, opts)
        def unquote(name)(unquote_splicing(args)) do
          unquote(target).unquote(as)(unquote_splicing(as_args))
        end
      end
    end
  end

  ## Sigils

  @doc ~S"""
  Handles the sigil `~S`.

  It simply returns a string without escaping characters and without
  interpolations.

  ## Examples

      iex> ~S(foo)
      "foo"

      iex> ~S(f#{o}o)
      "f\#{o}o"

  """
  defmacro sigil_S(term, modifiers)
  defmacro sigil_S(string, []), do: string

  @doc ~S"""
  Handles the sigil `~s`.

  It returns a string as if it was a double quoted string, unescaping characters
  and replacing interpolations.

  ## Examples

      iex> ~s(foo)
      "foo"

      iex> ~s(f#{:o}o)
      "foo"

      iex> ~s(f\#{:o}o)
      "f\#{:o}o"

  """
  defmacro sigil_s(term, modifiers)
  defmacro sigil_s({:<<>>, line, pieces}, []) do
    {:<<>>, line, Macro.unescape_tokens(pieces)}
  end

  @doc ~S"""
  Handles the sigil `~C`.

  It simply returns a char list without escaping characters and without
  interpolations.

  ## Examples

      iex> ~C(foo)
      'foo'

      iex> ~C(f#{o}o)
      'f\#{o}o'

  """
  defmacro sigil_C(term, modifiers)
  defmacro sigil_C({:<<>>, _line, [string]}, []) when is_binary(string) do
    String.to_char_list(string)
  end

  @doc ~S"""
  Handles the sigil `~c`.

  It returns a char list as if it were a single quoted string, unescaping
  characters and replacing interpolations.

  ## Examples

      iex> ~c(foo)
      'foo'

      iex> ~c(f#{:o}o)
      'foo'

      iex> ~c(f\#{:o}o)
      'f\#{:o}o'

  """
  defmacro sigil_c(term, modifiers)

  # We can skip the runtime conversion if we are
  # creating a binary made solely of series of chars.
  defmacro sigil_c({:<<>>, _line, [string]}, []) when is_binary(string) do
    String.to_char_list(Macro.unescape_string(string))
  end

  defmacro sigil_c({:<<>>, line, pieces}, []) do
    binary = {:<<>>, line, Macro.unescape_tokens(pieces)}
    quote do: String.to_char_list(unquote(binary))
  end

  @doc """
  Handles the sigil `~r`.

  It returns a regular expression pattern, unescaping characters and replacing
  interpolations.

  More information on regexes can be found in the `Regex` module.

  ## Examples

      iex> Regex.match?(~r(foo), "foo")
      true

      iex> Regex.match?(~r/a#{:b}c/, "abc")
      true

  """
  defmacro sigil_r(term, modifiers)
  defmacro sigil_r({:<<>>, _line, [string]}, options) when is_binary(string) do
    binary = Macro.unescape_string(string, fn(x) -> Regex.unescape_map(x) end)
    regex  = Regex.compile!(binary, :binary.list_to_bin(options))
    Macro.escape(regex)
  end

  defmacro sigil_r({:<<>>, line, pieces}, options) do
    binary = {:<<>>, line, Macro.unescape_tokens(pieces, fn(x) -> Regex.unescape_map(x) end)}
    quote do: Regex.compile!(unquote(binary), unquote(:binary.list_to_bin(options)))
  end

  @doc ~S"""
  Handles the sigil `~R`.

  It returns a regular expression pattern without escaping
  nor interpreting interpolations.

  More information on regexes can be found in the `Regex` module.

  ## Examples

      iex> Regex.match?(~R(f#{1,3}o), "f#o")
      true

  """
  defmacro sigil_R(term, modifiers)
  defmacro sigil_R({:<<>>, _line, [string]}, options) when is_binary(string) do
    regex = Regex.compile!(string, :binary.list_to_bin(options))
    Macro.escape(regex)
  end

  @doc ~S"""
  Handles the sigil `~w`.

  It returns a list of "words" split by whitespace. Character unescaping and
  interpolation happens for each word.

  ## Modifiers

    * `s`: words in the list are strings (default)
    * `a`: words in the list are atoms
    * `c`: words in the list are char lists

  ## Examples

      iex> ~w(foo #{:bar} baz)
      ["foo", "bar", "baz"]

      iex> ~w(--source test/enum_test.exs)
      ["--source", "test/enum_test.exs"]

      iex> ~w(foo bar baz)a
      [:foo, :bar, :baz]

  """
  defmacro sigil_w(term, modifiers)
  defmacro sigil_w({:<<>>, _line, [string]}, modifiers) when is_binary(string) do
    split_words(Macro.unescape_string(string), modifiers)
  end

  defmacro sigil_w({:<<>>, line, pieces}, modifiers) do
    binary = {:<<>>, line, Macro.unescape_tokens(pieces)}
    split_words(binary, modifiers)
  end

  @doc ~S"""
  Handles the sigil `~W`.

  It returns a list of "words" split by whitespace without escaping nor
  interpreting interpolations.

  ## Modifiers

    * `s`: words in the list are strings (default)
    * `a`: words in the list are atoms
    * `c`: words in the list are char lists

  ## Examples

      iex> ~W(foo #{bar} baz)
      ["foo", "\#{bar}", "baz"]

  """
  defmacro sigil_W(term, modifiers)
  defmacro sigil_W({:<<>>, _line, [string]}, modifiers) when is_binary(string) do
    split_words(string, modifiers)
  end

  defp split_words("", _modifiers), do: []

  defp split_words(string, modifiers) do
    mod =
      case modifiers do
        [] -> ?s
        [mod] when mod == ?s or mod == ?a or mod == ?c -> mod
        _else -> raise ArgumentError, "modifier must be one of: s, a, c"
      end

    case is_binary(string) do
      true ->
        case mod do
          ?s -> String.split(string)
          ?a -> for p <- String.split(string), do: String.to_atom(p)
          ?c -> for p <- String.split(string), do: String.to_char_list(p)
        end
      false ->
        case mod do
          ?s -> quote do: String.split(unquote(string))
          ?a -> quote do: for(p <- String.split(unquote(string)), do: String.to_atom(p))
          ?c -> quote do: for(p <- String.split(unquote(string)), do: String.to_char_list(p))
        end
    end
  end

  ## Shared functions

  defp optimize_boolean({:case, meta, args}) do
    {:case, [{:optimize_boolean, true}|meta], args}
  end

  # We need this check only for bootstrap purposes.
  # Once Kernel is loaded and we recompile, it is a no-op.
  case :code.ensure_loaded(Kernel) do
    {:module, _} ->
      defp bootstraped?(_), do: true
    {:error, _} ->
      defp bootstraped?(module), do: :code.ensure_loaded(module) == {:module, module}
  end

  defp assert_module_scope(env, fun, arity) do
    case env.module do
      nil -> raise ArgumentError, "cannot invoke #{fun}/#{arity} outside module"
      _   -> :ok
    end
  end

  defp assert_no_function_scope(env, fun, arity) do
    case env.function do
      nil -> :ok
      _   -> raise ArgumentError, "cannot invoke #{fun}/#{arity} inside function/macro"
    end
  end

  defp env_stacktrace(env) do
    case bootstraped?(Path) do
      true  -> Macro.Env.stacktrace(env)
      false -> []
    end
  end
end
