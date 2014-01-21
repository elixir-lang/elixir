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
  instance, if you want to tell Elixir not to import the `case`
  macro, you can do:

      import Kernel, except: [case: 2]

  Elixir also has special forms that are always imported and
  cannot be skipped. These are described in `Kernel.SpecialForms`.

  Some of the functions described in this module are simply
  a proxy to their Erlang counterpart. Although they are documented
  here for convenience, you can access their original documentation
  at http://www.erlang.org/doc/man/erlang.html.
  """

  ## Delegations to Erlang with inlining (macros)

  @doc """
  Returns an integer or float which is the arithmetical absolute value of `number`.

  Allowed in guard tests.

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
  Inlines to `:erlang.apply/2`.

  ## Examples

      iex> apply(fn x -> x * 2 end, [2])
      4

  """
  defmacro apply(fun, args) do
    quote do
      :erlang.apply(unquote(fun), unquote(args))
    end
  end

  @doc """
  Invokes the given `fun` from `module` with the array of arguments `args`.
  Inlines to `:erlang.apply/3`.

  ## Examples

      iex> apply(Enum, :reverse, [[1, 2, 3]])
      [3,2,1]

  """
  defmacro apply(module, fun, args) do
    quote do
      :erlang.apply(unquote(module), unquote(fun), unquote(args))
    end
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_atom` in UTF8 encoding.

  ## Examples

      iex> atom_to_binary(:my_atom)
      "my_atom"

  """
  defmacro atom_to_binary(some_atom) do
    quote do
      :erlang.atom_to_binary(unquote(some_atom), :utf8)
    end
  end

  @doc """
  Returns a string which corresponds to the text representation of `atom`.

  ## Examples

      iex> atom_to_list(:elixir)
      'elixir'

  """
  @spec atom_to_list(atom) :: list
  def atom_to_list(atom) do
    :erlang.atom_to_list(atom)
  end

  @doc """
  Extracts the part of the binary starting at `start` with length `length`.
  Binaries are zero-indexed.

  If start or length references in any way outside the binary, an
  `ArgumentError` exception is raised.

  Allowed in guard tests.

  ## Examples

      iex> binary_part("foo", 1, 2)
      "oo"

  A negative length can be used to extract bytes at the end of a binary:

      iex> binary_part("foo", 3, -1)
      "o"

  """
  @spec binary_part(binary, pos_integer, integer) :: binary
  def binary_part(binary, start, length) do
    :erlang.binary_part(binary, start, length)
  end


  @doc """
  Returns the atom whose text representation is `some_binary` in
  UTF8 encoding.

  Currently Elixir does not support conversions for binaries which
  contains Unicode characters greater than 16#FF.

  ## Examples

      iex> binary_to_atom("my_atom")
      :my_atom

  """
  defmacro binary_to_atom(some_binary) do
    quote do
      :erlang.binary_to_atom(unquote(some_binary), :utf8)
    end
  end

  @doc """
  Works like `binary_to_atom/1` but the atom must exist.

  Currently Elixir does not support conversions for binaries which
  contains Unicode characters greater than 16#FF.

  ## Examples

      iex> :my_atom
      ...> binary_to_existing_atom("my_atom")
      :my_atom

      iex> binary_to_existing_atom("this_atom_will_never_exist")
      ** (ArgumentError) argument error

  """
  defmacro binary_to_existing_atom(some_binary) do
    quote do
      :erlang.binary_to_existing_atom(unquote(some_binary), :utf8)
    end
  end

  @doc """
  Returns a integer whose text representation is `some_binary`.

  ## Examples

      iex> binary_to_integer("123")
      123

  """
  def binary_to_integer(some_binary) do
    :erlang.binary_to_integer(some_binary)
  end

  @doc """
  Returns an integer whose text representation in base `base`
  is `some_binary`.

  ## Examples

      iex> binary_to_integer("3FF", 16)
      1023

  """
  def binary_to_integer(some_binary, base) do
    :erlang.binary_to_integer(some_binary, base)
  end

  @doc """
  Returns a float whose text representation is `some_binary`.

  ## Examples

      iex> binary_to_float("2.2017764e+0")
      2.2017764

  """
  def binary_to_float(some_binary) do
    :erlang.binary_to_float(some_binary)
  end

  @doc false
  def binary_to_term(binary) do
    IO.write "binary_to_term/1 is deprecated, please use :erlang.binary_to_term/1 instead\n#{Exception.format_stacktrace}"
    :erlang.binary_to_term(binary)
  end

  @doc false
  def binary_to_term(binary, options) do
    IO.write "binary_to_term/2 is deprecated, please use :erlang.binary_to_term/2 instead\n#{Exception.format_stacktrace}"
    :erlang.binary_to_term(binary, options)
  end

  @doc """
  Returns an integer which is the size in bits of `bitstring`.

  Allowed in guard tests.

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
  Returns a list of integers which correspond to the bytes of `bitstring`. If the
  number of bits in the binary is not divisible by 8, the last element of the list will
  be a bitstring containing the remaining bits (1 up to 7 bits).
  """
  @spec bitstring_to_list(bitstring) :: list
  def bitstring_to_list(bitstring) do
    :erlang.bitstring_to_list(bitstring)
  end

  @doc """
  Returns an integer which is the number of bytes needed to contain `bitstring`.
  (That is, if the number of bits in `bitstring` is not divisible by 8, the resulting
  number of bytes will be rounded up.)

  Allowed in guard tests.

  ## Examples

      iex> byte_size(<<433::16, 3::3>>)
      3
      iex> byte_size(<<1, 2, 3>>)
      3

  """
  @spec byte_size(bitstring) :: non_neg_integer
  def byte_size(bitstring) do
    :erlang.byte_size(bitstring)
  end

  @doc """
  Provides an integer division macro according to Erlang semantics.
  Raises an error if one of the arguments is not an integer.
  Can be used in guard tests.

  ## Examples

      iex> div(5, 2)
      2

  """
  defmacro div(left, right) do
    quote do: __op__(:div, unquote(left), unquote(right))
  end

  @doc """
  Stops the execution of the calling process with the given reason.
  Since evaluating this function causes the process to terminate,
  it has no return value.

  ## Examples

      exit(:normal)
      exit(:seems_bad)

  """
  @spec exit(term) :: no_return
  def exit(reason) do
    :erlang.exit(reason)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the given float.

  ## Examples

      iex> float_to_list(7.0)
      '7.00000000000000000000e+00'

  """
  @spec float_to_list(number) :: list
  def float_to_list(number) do
    :erlang.float_to_list(number)
  end


  @doc """
  Returns a binary which corresponds to the text representation
  of `some_float`.

  ## Examples

      iex> float_to_binary(7.0)
      "7.00000000000000000000e+00"

  """
  def float_to_binary(some_float) do
    :erlang.float_to_binary(some_float)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `float`.

  ## Options

  * `:decimals` — number of decimal points to show
  * `:scientific` — number of decimal points to show, in scientific format
  * `:compact` — If true, use the most compact representation (ignored with the `scientific` option)

  ## Examples

      float_to_binary 7.1, [decimals: 2, compact: true] #=> "7.1"

  """
  def float_to_binary(float, options) do
    :erlang.float_to_binary(float, expand_compact(options))
  end

  @doc """
  Returns a list which corresponds to the text representation
  of `float`.

  ## Options

  * `:decimals` — number of decimal points to show
  * `:scientific` — number of decimal points to show, in scientific format
  * `:compact` — If true, use the most compact representation (ignored with the `scientific` option)

  ## Examples

      float_to_list 7.1, [decimals: 2, compact: true] #=> '7.1'

  """
  def float_to_list(float, options) do
    :erlang.float_to_list(float, expand_compact(options))
  end

  @doc """
  Returns the head of a list, raises `badarg` if the list is empty.
  """
  @spec hd(list) :: term
  def hd(list) do
    :erlang.hd(list)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_integer`.

  ## Examples

      iex> integer_to_binary(123)
      "123"

  """
  def integer_to_binary(some_integer) do
    :erlang.integer_to_binary(some_integer)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_integer` in base `base`.

  ## Examples

      iex> integer_to_binary(100, 16)
      "64"

  """
  def integer_to_binary(some_integer, base) do
    :erlang.integer_to_binary(some_integer, base)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the given integer.

  ## Examples

      iex> integer_to_list(7)
      '7'

  """
  @spec integer_to_list(integer) :: list
  def integer_to_list(number) do
    :erlang.integer_to_list(number)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the
  given integer in the given case.

  ## Examples

      iex> integer_to_list(1023, 16)
      '3FF'

  """
  @spec integer_to_list(integer, pos_integer) :: list
  def integer_to_list(number, base) do
    :erlang.integer_to_list(number, base)
  end

  @doc """
  Returns the size of an iolist.

  ## Examples

      iex> iolist_size([1, 2|<<3, 4>>])
      4

  """
  @spec iolist_size(iolist) :: non_neg_integer
  def iolist_size(item) do
    :erlang.iolist_size(item)
  end

  @doc """
  Returns a binary which is made from the integers and binaries in iolist.

  Notice that this function treats lists of integers as raw bytes
  and does not perform any kind of encoding conversion. If you want to convert
  from a char list to a string (both utf-8 encoded), please use
  `String.from_char_list!/1` instead.

  If this function receives a binary, the same binary is returned.

  ## Examples

      iex> bin1 = <<1, 2, 3>>
      ...> bin2 = <<4, 5>>
      ...> bin3 = <<6>>
      ...> iolist_to_binary([bin1, 1, [2, 3, bin2], 4|bin3])
      <<1,2,3,1,2,3,4,5,4,6>>

      iex> bin = <<1, 2, 3>>
      ...> iolist_to_binary(bin)
      <<1,2,3>>

  """
  @spec iolist_to_binary(iolist | binary) :: binary
  def iolist_to_binary(item) do
    :erlang.iolist_to_binary(item)
  end

  @doc """
  Returns `true` if `term` is an atom; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_atom(term) :: boolean
  def is_atom(term) do
    :erlang.is_atom(term)
  end

  @doc """
  Returns `true` if `term` is a binary; otherwise returns `false`.

  A binary always contains a complete number of bytes.

  Allowed in guard tests.
  """
  @spec is_binary(term) :: boolean
  def is_binary(term) do
    :erlang.is_binary(term)
  end

  @doc """
  Returns `true` if `term` is a bitstring (including a binary); otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_bitstring(term) :: boolean
  def is_bitstring(term) do
    :erlang.is_bitstring(term)
  end

  @doc """
  Returns `true` if `term` is either the atom `true` or the atom `false` (i.e. a boolean);
  otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_boolean(term) :: boolean
  def is_boolean(term) do
    :erlang.is_boolean(term)
  end

  @doc """
  Returns `true` if `term` is a floating point number; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_float(term) :: boolean
  def is_float(term) do
    :erlang.is_float(term)
  end

  @doc """
  Returns `true` if `term` is a function; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_function(term) :: boolean
  def is_function(term) do
    :erlang.is_function(term)
  end

  @doc """
  Returns `true` if `term` is a function that can be applied with `arity` number of arguments;
  otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_function(term, non_neg_integer) :: boolean
  def is_function(term, arity) do
    :erlang.is_function(term, arity)
  end

  @doc """
  Returns `true` if `term` is an integer; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_integer(term) :: boolean
  def is_integer(term) do
    :erlang.is_integer(term)
  end

  @doc """
  Returns `true` if `term` is a list with zero or more elements; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_list(term) :: boolean
  def is_list(term) do
    :erlang.is_list(term)
  end

  @doc """
  Returns `true` if `term` is either an integer or a floating point number;
  otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_number(term) :: boolean
  def is_number(term) do
    :erlang.is_number(term)
  end

  @doc """
  Returns `true` if `term` is a pid (process identifier); otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_pid(term) :: boolean
  def is_pid(term) do
    :erlang.is_pid(term)
  end

  @doc """
  Returns `true` if `term` is a port identifier; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_port(term) :: boolean
  def is_port(term) do
    :erlang.is_port(term)
  end

  @doc """
  Returns `true` if `term` is a reference; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_reference(term) :: boolean
  def is_reference(term) do
    :erlang.is_reference(term)
  end

  @doc """
  Returns `true` if `term` is a tuple; otherwise returns `false`.

  Allowed in guard tests.
  """
  @spec is_tuple(term) :: boolean
  def is_tuple(term) do
    :erlang.is_tuple(term)
  end

  @doc """
  Returns the length of `list`.

  Allowed in guard tests.

  ## Examples

      iex> length([1, 2, 3, 4, 5, 6, 7, 8, 9])
      9
  """
  @spec length(list) :: non_neg_integer
  def length(list) do
    :erlang.length(list)
  end

  @doc """
  Returns the atom whose text representation is `list`.

  ## Examples

      iex> list_to_atom('elixir')
      :elixir
  """
  @spec list_to_atom(list) :: atom
  def list_to_atom(list) do
    :erlang.list_to_atom(list)
  end

  @doc """
  Returns a bitstring which is made from the integers and bitstrings in `bitstring_list`.
  (the last tail in `bitstring_list` is allowed to be a bitstring.)

  ## Examples

      iex> bin1 = <<1, 2, 3>>
      ...> bin2 = <<4, 5>>
      ...> bin3 = <<6, 7::4>>
      ...> list_to_bitstring([bin1, 1, [2, 3, bin2], 4|bin3])
      <<1,2,3,1,2,3,4,5,4,6,7::size(4)>>

  """
  @spec list_to_bitstring(maybe_improper_list(char | binary | iolist | bitstring, binary | bitstring | [])) :: bitstring
  def list_to_bitstring(bitstring_list) do
    :erlang.list_to_bitstring(bitstring_list)
  end

  @doc """
  Returns the atom whose text representation is `list`,
  but only if there already exists such atom.
  """
  @spec list_to_existing_atom(list) :: atom
  def list_to_existing_atom(list) do
    :erlang.list_to_existing_atom(list)
  end

  @doc """
  Returns the float whose text representation is `list`.

  ## Examples

      iex> list_to_float('2.2017764e+0')
      2.2017764
  """
  @spec list_to_float(list) :: float
  def list_to_float(list) do
    :erlang.list_to_float(list)
  end

  @doc """
  Returns an integer whose text representation is `list`.

  ## Examples

      iex> list_to_integer('123')
      123
  """
  @spec list_to_integer(list) :: integer
  def list_to_integer(list) do
    :erlang.list_to_integer(list)
  end

  @doc """
  Returns an integer whose text representation in base `base` is `list`.

  ## Examples

      iex> list_to_integer('3FF', 16)
      1023
  """
  @spec list_to_integer(list, non_neg_integer) :: integer
  def list_to_integer(list, base) do
    :erlang.list_to_integer(list, base)
  end

  @doc """
  Returns a tuple which corresponds to `list`. `list` can contain any Erlang terms.

  ## Examples

      iex> list_to_tuple([:share, [:elixir, 163]])
      {:share, [:elixir, 163]}
  """
  @spec list_to_tuple(list) :: tuple
  def list_to_tuple(list) do
    :erlang.list_to_tuple(list)
  end

  @doc """
  Returns an almost unique reference.

  The returned reference will re-occur after approximately 2^82 calls;
  therefore it is unique enough for practical purposes.

  ## Examples

      make_ref() #=> #Reference<0.0.0.135>

  """
  @spec make_ref() :: reference
  def make_ref() do
    :erlang.make_ref()
  end

  @doc """
  Return the biggest of the two given terms according to
  Erlang's term ordering. If the terms compare equal, the
  first one is returned.

  ## Examples

      iex> max(1, 2)
      2

  """
  @spec max(term, term) :: term
  def max(first, second) do
    :erlang.max(first, second)
  end

  @doc """
  Return the smallest of the two given terms according to
  Erlang's term ordering. If the terms compare equal, the
  first one is returned.

  ## Examples

      iex> min(1, 2)
      1

  """
  @spec min(term, term) :: term
  def min(first, second) do
    :erlang.min(first, second)
  end

  @doc """
  Returns an atom representing the name of the local node.
  If the node is not alive, `nonode@nohost` is returned instead.

  Allowed in guard tests.
  """
  @spec node() :: node
  def node do
    :erlang.node
  end

  @doc """
  Returns the node where the given argument is located.
  The argument can be a pid, a reference, or a port.
  If the local node is not alive, `nonode@nohost` is returned.

  Allowed in guard tests.
  """
  @spec node(pid|reference|port) :: node
  def node(arg) do
    :erlang.node(arg)
  end

  @doc """
  Provides an integer remainder macro according to Erlang semantics.
  Raises an error if one of the arguments is not an integer.
  Can be used in guard tests.

  ## Examples

      iex> rem(5, 2)
      1

  """
  defmacro rem(left, right) do
    quote do: __op__(:rem, unquote(left), unquote(right))
  end

  @doc """
  Returns an integer by rounding the given number.
  Allowed in guard tests.

  ## Examples

      iex> round(5.5)
      6

  """
  @spec round(number) :: integer
  def round(number) do
    :erlang.round(number)
  end

  @doc """
  Sends a message to the given `dest` and returns the message.

  `dest` may be a remote or local pid, a (local) port, a locally
  registered name, or a tuple `{registed_name, node}` for a registered
  name at another node.

  ## Examples

      iex> send self(), :hello
      :hello

  """
  @spec send(dest :: pid | port | atom | { atom, node }, msg) :: msg when msg: any
  def send(dest, msg) do
    :erlang.send(dest, msg)
  end

  @doc """
  Returns the pid (process identifier) of the calling process.
  Allowed in guard clauses.
  """
  @spec self() :: pid
  def self() do
    :erlang.self()
  end

  @doc """
  Returns the size of the given argument, which must be a tuple
  or a binary. If possible, please use `tuple_size` or `byte_size`.
  """
  @spec size(tuple|binary) :: non_neg_integer
  def size(arg) do
    :erlang.size(arg)
  end

  @doc """
  Spawns the given function and returns its pid.

  Check the modules `Process` and `Node` for other functions
  to handle processes, including spawning functions in nodes.

  ## Examples

      current = Kernel.self
      child   = spawn(fn -> send current, { Kernel.self, 1 + 2 } end)

      receive do
        { ^child, 3 } -> IO.puts "Received 3 back"
      end

  """
  @spec spawn((() -> any)) :: pid
  def spawn(fun) do
    :erlang.spawn(fun)
  end

  @doc """
  Spawns the given module and function passing the given args
  and returns its pid.

  Check the modules `Process` and `Node` for other functions
  to handle processes, including spawning functions in nodes.

  ## Examples

      spawn(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn(module, atom, list) :: pid
  def spawn(module, fun, args) do
    :erlang.spawn(module, fun, args)
  end

  @doc """
  Spawns the given function, links it to the current process and returns its pid.

  Check the modules `Process` and `Node` for other functions
  to handle processes, including spawning functions in nodes.

  ## Examples

      current = Kernel.self
      child   = spawn_link(fn -> send current, { Kernel.self, 1 + 2 } end)

      receive do
        { ^child, 3 } -> IO.puts "Received 3 back"
      end

  """
  @spec spawn_link((() -> any)) :: pid
  def spawn_link(fun) do
    :erlang.spawn_link(fun)
  end

  @doc """
  Spawns the given module and function passing the given args,
  links it to the current process and returns its pid.

  Check the modules `Process` and `Node` for other functions
  to handle processes, including spawning functions in nodes.

  ## Examples

      spawn_link(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn_link(module, atom, list) :: pid
  def spawn_link(module, fun, args) do
    :erlang.spawn_link(module, fun, args)
  end

  @doc false
  def term_to_binary(term) do
    IO.write "term_to_binary/1 is deprecated, please use :erlang.term_to_binary/1 instead\n#{Exception.format_stacktrace}"
    :erlang.term_to_binary(term)
  end

  @doc false
  def term_to_binary(term, opts) do
    IO.write "term_to_binary/2 is deprecated, please use :erlang.term_to_binary/2 instead\n#{Exception.format_stacktrace}"
    :erlang.term_to_binary(term, opts)
  end

  @doc """
  A non-local return from a function. Check `try/2` for more information.
  """
  @spec throw(term) :: no_return
  def throw(term) do
    :erlang.throw(term)
  end

  @doc """
  Returns the tail of a list. Raises `ArgumentError` if the list is empty.
  """
  @spec tl(maybe_improper_list) :: maybe_improper_list
  def tl(list) do
    :erlang.tl(list)
  end

  @doc """
  Returns an integer by truncating the given number.
  Allowed in guard clauses.

  ## Examples

      iex> trunc(5.5)
      5

  """
  @spec trunc(number) :: integer
  def trunc(number) do
    :erlang.trunc(number)
  end

  @doc """
  Returns the size of a tuple.
  """
  @spec tuple_size(tuple) :: non_neg_integer
  def tuple_size(tuple) do
    :erlang.tuple_size(tuple)
  end

  @doc """
  Converts a tuple to a list.
  """
  @spec tuple_to_list(tuple) :: list
  def tuple_to_list(tuple) do
    :erlang.tuple_to_list(tuple)
  end

  @doc """
  Arithmetic plus. Allowed in guard clauses.

  ## Examples

      iex> 1 + 2
      3

  """
  defmacro left + right do
    quote do: __op__(:+, unquote(left), unquote(right))
  end

  @doc """
  Arithmetic minus. Allowed in guard clauses.

  ## Examples

      iex> 1 - 2
      -1

  """
  defmacro left - right do
    quote do: __op__(:-, unquote(left), unquote(right))
  end

  @doc """
  Arithmetic unary plus. Allowed in guard clauses.

  ## Examples

      iex> +1
      1

  """
  defmacro (+value) when is_number(value), do: value
  defmacro (+value) do
    quote do: :erlang.+(unquote(value))
  end

  @doc """
  Arithmetic unary minus. Allowed in guard clauses.

  ## Examples

      iex> -2
      -2

  """
  defmacro (-value) when is_number(value), do: :erlang.-(value)
  defmacro (-value) do
    quote do: :erlang.-(unquote(value))
  end

  @doc """
  Arithmetic multiplication. Allowed in guard clauses.

  ## Examples

      iex> 1 * 2
      2

  """
  defmacro left * right do
    quote do: __op__(:*, unquote(left), unquote(right))
  end

  @doc """
  Arithmetic division. Unlike other languages,
  the result is always a float. Use `div` and `rem` if you want
  a natural division or the remainder. Allowed in guard clauses.

  ## Examples

      iex> 1 / 2
      0.5
      iex> 2 / 1
      2.0

  """
  defmacro left / right do
    quote do: __op__(:/, unquote(left), unquote(right))
  end

  @doc """
  Concatenates two lists. Allowed in guard clauses.

  ## Examples

      iex> [1] ++ [2, 3]
      [1,2,3]

      iex> 'foo' ++ 'bar'
      'foobar'

  """
  # We compile down to the operator because it is
  # automatically expanded when the left side is known.
  defmacro left ++ right do
    quote do: __op__(:++, unquote(left), unquote(right))
  end

  @doc """
  Removes the first occurrence of an item on the left
  for each item on the right. Allowed in guard clauses.

  ## Examples

      iex> [1, 2, 3] -- [1, 2]
      [3]

      iex> [1, 2, 3, 2, 1] -- [1, 2, 2]
      [3,1]

  """
  defmacro left -- right do
    quote do: __op__(:--, unquote(left), unquote(right))
  end

  @doc """
  Boolean or. Requires only the first argument to be a
  boolean since it short-circuits. Allowed in guard clauses.

  ## Examples

      iex> true or false
      true

  """
  defmacro left or right do
    quote do: __op__(:orelse, unquote(left), unquote(right))
  end

  @doc """
  Boolean and. Requires only the first argument to be a
  boolean since it short-circuits. Allowed in guard clauses.

  ## Examples

      iex> true and false
      false

  """
  defmacro left and right do
    quote do: __op__(:andalso, unquote(left), unquote(right))
  end

  @doc """
  Boolean exclusive-or. Arguments must be booleans.
  Returns `true` if and only if both arguments are different.
  Allowed in guard clauses.

  ## Examples

      iex> true xor false
      true
      iex> true xor true
      false

  """
  defmacro left xor right do
    quote do: __op__(:xor, unquote(left), unquote(right))
  end

  @doc """
  Boolean not. Argument must be a boolean.
  Allowed in guard clauses.

  ## Examples

      iex> not false
      true

  """
  defmacro not(arg) do
    quote do: __op__(:not, unquote(arg))
  end

  @doc """
  Returns `true` if left is less than right.
  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 < 2
      true

  """
  defmacro left < right do
    quote do: __op__(:<, unquote(left), unquote(right))
  end

  @doc """
  Returns `true` if left is more than right.
  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 > 2
      false

  """
  defmacro left > right do
    quote do: __op__(:>, unquote(left), unquote(right))
  end

  @doc """
  Returns `true` if left is less than or equal to right.
  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 <= 2
      true

  """
  defmacro left <= right do
    quote do: __op__(:"=<", unquote(left), unquote(right))
  end

  @doc """
  Returns `true` if left is more than or equal to right.
  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 >= 2
      false

  """
  defmacro left >= right do
    quote do: __op__(:>=, unquote(left), unquote(right))
  end

  @doc """
  Returns `true` if the two items are equal.

  This operator considers 1 and 1.0 to be equal. For strict
  comparison, use `===` instead.

  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 == 2
      false

      iex> 1 == 1.0
      true

  """
  defmacro left == right do
    quote do: __op__(:==, unquote(left), unquote(right))
  end

  @doc """
  Returns `true` if the two items are not equal.

  This operator considers 1 and 1.0 to be equal. For strict
  comparison, use `!==` instead.

  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 != 2
      true
      iex> 1 != 1.0
      false

  """
  defmacro left != right do
    quote do: __op__(:"/=", unquote(left), unquote(right))
  end

  @doc """
  Returns `true` if the two items are strictly equal.
  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 === 2
      false

      iex> 1 === 1.0
      false

  """
  defmacro left === right do
    quote do: __op__(:"=:=", unquote(left), unquote(right))
  end

  @doc """
  Returns `true` if the two items are strictly not equal.
  Like Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      iex> 1 !== 2
      true

      iex> 1 !== 1.0
      true

  """
  defmacro left !== right do
    quote do: __op__(:"=/=", unquote(left), unquote(right))
  end

  @doc """
  Receives any argument and returns `true` if it is `false`
  or `nil`. Returns `false` otherwise. Not allowed in guard
  clauses.

  ## Examples

      iex> !1
      false
      iex> ![1, 2, 3]
      false
      iex> !false
      true
      iex> !nil
      true

  """
  defmacro !(arg)

  defmacro !({ :!, _, [arg] }) do
    quote do
      case unquote(arg) do
        unquote(cond_var) when unquote(cond_var) in [false, nil] -> false
        _ -> true
      end
    end
  end

  defmacro !(arg) do
    quote do
      case unquote(arg) do
        unquote(cond_var) when unquote(cond_var) in [false, nil] -> true
        _ -> false
      end
    end
  end

  ## Implemented in Elixir

  @doc """
  Concatenates two binaries.

  ## Examples

      iex> "foo" <> "bar"
      "foobar"

  The `<>` operator can also be used in guard clauses as
  long as the first part is a literal binary:

      iex> "foo" <> x = "foobar"
      ...> x
      "bar"

  """
  defmacro left <> right do
    concats = extract_concatenations({ :<>, [], [left, right] })
    quote do: << unquote_splicing(concats) >>
  end

  # Extracts concatenations in order to optimize many
  # concatenations into one single clause.
  defp extract_concatenations({ :<>, _, [left, right] }) do
    [wrap_concatenation(left)|extract_concatenations(right)]
  end

  defp extract_concatenations(other) do
    [wrap_concatenation(other)]
  end

  defp wrap_concatenation(binary) when is_binary(binary) do
    binary
  end

  defp wrap_concatenation(other) do
    { :::, [], [other, { :binary, [], nil }] }
  end

  @doc """
  Raises an error.

  If the argument is a binary, it raises `RuntimeError`
  using the given argument as message.

  If anything else, becomes a call to `raise(argument, [])`.

  ## Examples

      raise "Given values do not match"

      try do
        1 + :foo
      rescue
        x in [ArithmeticError] ->
          IO.puts "that was expected"
          raise x
      end

  """
  @spec raise(binary | atom | tuple) :: no_return
  defmacro raise(msg) do
    # Try to figure out the type at compilation time
    # to avoid dead code and make dialyzer happy.
    msg = case not is_binary(msg) and bootstraped?(Macro.Env) do
      true  -> Macro.expand(msg, __CALLER__)
      false -> msg
    end

    case msg do
      msg when is_binary(msg) ->
        quote do
          :erlang.error RuntimeError.exception(message: unquote(msg))
        end
      { :<<>>, _, _ } = msg ->
        quote do
          :erlang.error RuntimeError.exception(message: unquote(msg))
        end
      alias when is_atom(alias) ->
        quote do
          :erlang.error unquote(alias).exception([])
        end
      _ ->
        quote do
          case unquote(msg) do
            msg when is_binary(msg) -> :erlang.error RuntimeError.exception(message: msg)
            msg -> :erlang.error msg.exception([])
          end
        end
    end
  end

  @doc """
  Raises an error.

  Calls `.exception` on the given argument passing
  the args in order to retrieve the appropriate exception
  structure.

  Any module defined via `defexception` automatically
  implements `exception(args)` callback expected by `raise/2`.

  ## Examples

      iex> raise(ArgumentError, message: "Sample")
      ** (ArgumentError) Sample

  """
  @spec raise(tuple | atom, list) :: no_return
  defmacro raise(exception, args) do
    quote do
      :erlang.error unquote(exception).exception(unquote(args))
    end
  end

  @doc """
  Re-raises an exception with the given stacktrace.

  ## Examples

      try do
        raise "Oops"
      rescue
        exception ->
          stacktrace = System.stacktrace
          if exception.message == "Oops" do
            raise exception, [], stacktrace
          end
      end

  Notice that `System.stacktrace` returns the stacktrace
  of the last exception. That said, it is common to assign
  the stacktrace as the first expression inside a `rescue`
  clause as any other exception potentially raised (and
  rescued) in between the rescue clause and the raise call
  may change the `System.stacktrace` value.
  """
  @spec raise(tuple | atom, list, list) :: no_return
  defmacro raise(exception, args, stacktrace) do
    quote do
      :erlang.raise :error, unquote(exception).exception(unquote(args)), unquote(stacktrace)
    end
  end

  @doc """
  Get the element at the zero-based `index` in `tuple`.

  Implemented as a macro so it can be used in guards.

  ## Example

      iex> tuple = { :foo, :bar, 3 }
      ...> elem(tuple, 1)
      :bar

  """
  defmacro elem(tuple, index) when is_integer(index) do
    quote do: :erlang.element(unquote(index + 1), unquote(tuple))
  end

  defmacro elem(tuple, index) do
    quote do: :erlang.element(unquote(index) + 1, unquote(tuple))
  end

  @doc """
  Sets the element in `tuple` at the zero-based `index` to the given `value`.

  ## Example

      iex> tuple = { :foo, :bar, 3 }
      ...> set_elem(tuple, 0, :baz)
      { :baz, :bar, 3 }

  """
  defmacro set_elem(tuple, index, value) when is_integer(index) do
    quote do: :erlang.setelement(unquote(index + 1), unquote(tuple), unquote(value))
  end

  defmacro set_elem(tuple, index, value) do
    quote do: :erlang.setelement(unquote(index) + 1, unquote(tuple), unquote(value))
  end

  @doc """
  Checks if the given structure is an exception.

  ## Examples

      iex> is_exception((fn -> ArithmeticError.new end).())
      true
      iex> is_exception((fn -> 1 end).())
      false

  """
  defmacro is_exception(thing) do
    case __CALLER__.in_guard? do
      true ->
        quote do
          is_tuple(unquote(thing)) and tuple_size(unquote(thing)) > 1 and
            :erlang.element(2, unquote(thing)) == :__exception__
        end
      false ->
        quote do
          result = unquote(thing)
          is_tuple(result) and tuple_size(result) > 1 and
            :erlang.element(2, result) == :__exception__
        end
    end
  end

  @doc """
  Checks if the given structure is a record. It is basically
  a convenient macro that checks the structure is a tuple and
  the first element matches the given kind.

  ## Examples

      defrecord Config, sample: nil

      is_record(Config.new, Config) #=> true
      is_record(Config.new, List)   #=> false

  """
  defmacro is_record(thing, kind) do
    case __CALLER__.in_guard? do
      true ->
        quote do
          is_tuple(unquote(thing)) and tuple_size(unquote(thing)) > 0
            and :erlang.element(1, unquote(thing)) == unquote(kind)
        end
      false ->
        quote do
          result = unquote(thing)
          is_tuple(result) and tuple_size(result) > 0
            and :erlang.element(1, result) == unquote(kind)
        end
    end
  end

  @doc """
  Checks if the given argument is a record.
  """
  defmacro is_record(thing) do
    case __CALLER__.in_guard? do
      true ->
        quote do
          is_tuple(unquote(thing)) and tuple_size(unquote(thing)) > 0
            and is_atom(:erlang.element(1, unquote(thing)))
        end
      false ->
        quote do
          result = unquote(thing)
          is_tuple(result) and tuple_size(result) > 0
            and is_atom(:erlang.element(1, result))
        end
    end
  end

  @doc """
  Checks if the given argument is a regex.
  """
  defmacro is_regex(thing) do
    quote do
      is_record(unquote(thing), Regex)
    end
  end

  @doc """
  Checks if the given argument is a range.
  """
  defmacro is_range(thing) do
    quote do
      is_record(unquote(thing), Range)
    end
  end

  @doc """
  Matches the term on the left against the regular expression or string on the
  right. Returns true if `left` matches `right` (if it's a regular expression)
  or contains `right` (if it's a string).

  ## Examples

      iex> "abcd" =~ %r/c(d)/
      true

      iex> "abcd" =~ %r/e/
      false

      iex> "abcd" =~ "bc"
      true

      iex> "abcd" =~ "ad"
      false

  """
  def left =~ right when is_binary(left) and is_binary(right) do
    :binary.match(left, right) != :nomatch
  end

  def left =~ right when is_binary(left) and is_tuple(right) and
      tuple_size(right) > 0 and elem(right, 0) == Regex do
    Regex.match?(right, left)
  end

  @doc %S"""
  Inspect the given argument according to the `Inspect` protocol.
  The second argument is a keywords list with options to control
  inspection.

  ## Options

  The following options are supported:

  * `:records` - when false, records are not formatted by the inspect protocol,
                 they are instead printed as just tuples, defaults to true;

  * `:binaries` - when `:as_strings` all binaries will be printed as strings,
                  non-printable bytes will be escaped; when `:as_binaries` all
                  binaries will be printed in bit syntax; when the default
                  `:infer`, the binary will be printed as a string if it is
                  printable, otherwise in bit syntax;

  * `:char_lists` - when `:as_char_lists` all lists will be printed as char lists,
                    non-printable elements will be escaped; when `:as_lists` all
                    lists will be printed as lists; when the default `:infer`, the
                    list will be printed as a char list if it is printable,
                    otherwise as list;

  * `:limit` - limits the number of items that are printed for tuples, bitstrings,
               and lists, does not apply to strings nor char lists, defaults to 50;

  * `:pretty` - if set to true enables pretty printing, defaults to false;

  * `:width` - the width available for inspect to layout the data structure
               representation. Defaults to the smaller of 80 or the terminal width;

  ## Examples

      iex> inspect(:foo)
      ":foo"

      iex> inspect [1, 2, 3, 4, 5], limit: 3
      "[1, 2, 3, ...]"

      iex> inspect(ArgumentError[])
      "ArgumentError[message: \"argument error\"]"

      iex> inspect(ArgumentError[], records: false)
      "{ArgumentError, :__exception__, \"argument error\"}"

      iex> inspect("josé" <> <<0>>)
      "<<106, 111, 115, 195, 169, 0>>"

      iex> inspect("josé" <> <<0>>, binaries: :as_strings)
      "\"josé\\000\""

      iex> inspect("josé", binaries: :as_binaries)
      "<<106, 111, 115, 195, 169>>"

  Note that the inspect protocol does not necessarily return a valid
  representation of an Elixir term. In such cases, the inspected result
  must start with `#`. For example, inspecting a function will return:

      inspect &(&1 + &2)
      #=> #Function<...>

  """
  @spec inspect(Inspect.t, Keyword.t) :: String.t
  def inspect(arg, opts // []) when is_list(opts) do
    opts  = Inspect.Opts.new(opts)
    limit = case opts.pretty do
      true  -> opts.width
      false -> :infinity
    end
    Inspect.Algebra.pretty(Inspect.Algebra.to_doc(arg, opts), limit)
  end

  @doc """
  Converts the argument to a string according to the String.Chars protocol.
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
  Convert the argument to a list according to the List.Chars protocol.

  ## Examples

      iex> to_char_list(:foo)
      'foo'

  """
  defmacro to_char_list(arg) do
    quote do: List.Chars.to_char_list(unquote(arg))
  end

  @doc """
  Checks if the given argument is nil or not.
  Allowed in guard clauses.

  ## Examples

      iex> nil?(1)
      false
      iex> nil?(nil)
      true

  """
  defmacro nil?(x) do
    quote do: unquote(x) == nil
  end

  @doc """
  A convenient macro that checks if the right side matches
  the left side. The left side is allowed to be a match pattern.

  ## Examples

      iex> match?(1, 1)
      true
      iex> match?(1, 2)
      false
      iex> match?({1, _}, {1, 2})
      true

  Match can also be used to filter or find a value in an enumerable:

      list = [{:a, 1}, {:b, 2}, {:a, 3}]
      Enum.filter list, &match?({:a, _}, &1)

  Guard clauses can also be given to the match:

      list = [{:a, 1}, {:b, 2}, {:a, 3}]
      Enum.filter list, &match?({:a, x } when x < 2, &1)

  However, variables assigned in the match will not be available
  outside of the function call:

      iex> match?(x, 1)
      true
      iex> binding([:x]) == []
      true

  """
  defmacro match?(pattern, expr)

  # Special case underscore since it always matches
  defmacro match?({ :_, _, atom }, _right) when is_atom(atom) do
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
  This macro is a shortcut to read and add attributes to the module
  being compiled. Elixir module attributes are similar to Erlang's with
  some differences. The canonical example for attributes is annotating
  that a module implements the OTP behavior called `gen_server`:

      defmodule MyServer do
        @behavior :gen_server
        # ... callbacks ...
      end

  By default Elixir supports all Erlang module attributes, but any developer
  can also add custom attributes:

      defmodule MyServer do
        @my_data 13
        IO.inspect @my_data #=> 13
      end

  Unlike Erlang, such attributes are not stored in the module by
  default since it is common in Elixir to use such attributes to store
  temporary data. A developer can configure an attribute to behave closer
  to Erlang by calling `Module.register_attribute/3`.

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
  time and not at runtime. Check the module `Module` for other functions
  to manipulate module attributes.
  """
  defmacro @(expr)

  # Typespecs attributes are special cased by the compiler so far
  defmacro @({ name, _, args }) do
    # Check for Macro as it is compiled later than Module
    case bootstraped?(Module) do
      false -> nil
      true  ->
        assert_module_scope(__CALLER__, :@, 1)
        function? = env_function(__CALLER__) != nil

        case is_list(args) and length(args) == 1 and typespec(name) do
          false ->
            case name == :typedoc and internal? do
              true  -> nil
              false -> do_at(args, name, function?, __CALLER__)
            end
          macro ->
            # Do not compile typespecs during internal compilation
            case internal? do
              true  -> nil
              false -> quote do: Kernel.Typespec.unquote(macro)(unquote(hd(args)))
            end
        end
    end
  end

  # @attribute value
  defp do_at([arg], name, function?, _env) do
    case function? do
      true ->
        raise ArgumentError, message: "cannot dynamically set attribute @#{name} inside function"
      false ->
        quote do: Module.put_attribute(__MODULE__, unquote(name), unquote(arg))
    end
  end

  # @attribute or @attribute()
  defp do_at(args, name, function?, env) when is_atom(args) or args == [] do
    case function? do
      true ->
        stack =
          case bootstraped?(Path) do
            true  -> env.stacktrace
            false -> []
          end
        attr = Module.get_attribute(env_module(env), name, stack)
        :erlang.element(1, :elixir_quote.escape(attr, false))
      false ->
        quote do: Module.get_attribute(__MODULE__, unquote(name), true)
    end
  end

  # All other cases
  defp do_at(args, name, _function?, _env) do
    raise ArgumentError, message: "expected 0 or 1 argument for @#{name}, got: #{length(args)}"
  end

  defp typespec(:type),     do: :deftype
  defp typespec(:typep),    do: :deftypep
  defp typespec(:opaque),   do: :defopaque
  defp typespec(:spec),     do: :defspec
  defp typespec(:callback), do: :defcallback
  defp typespec(_),         do: false

  @doc """
  Returns the binding as a keyword list where the variable name
  is the key and the variable value is the value.

  ## Examples

      iex> x = 1
      iex> binding()
      [x: 1]
      iex> x = 2
      iex> binding()
      [x: 2]

  """
  defmacro binding() do
    do_binding(nil, __CALLER__.vars, __CALLER__.in_match?)
  end

  @doc """
  Receives a list of atoms at compilation time and returns the
  binding of the given variables as a keyword list where the
  variable name is the key and the variable value is the value.

  In case a variable in the list does not exist in the binding,
  it is not included in the returned result.

  ## Examples

      iex> x = 1
      iex> binding([:x, :y])
      [x: 1]

  """
  defmacro binding(list) when is_list(list) do
    do_binding(list, nil, __CALLER__.vars, __CALLER__.in_match?)
  end

  defmacro binding(context) when is_atom(context) do
    do_binding(context, __CALLER__.vars, __CALLER__.in_match?)
  end

  @doc """
  Receives a list of atoms at compilation time and returns the
  binding of the given variables in the given context as a keyword
  list where the variable name is the key and the variable value
  is the value.

  In case a variable in the list does not exist in the binding,
  it is not included in the returned result.

  ## Examples

      iex> var!(x, :foo) = 1
      iex> binding([:x, :y])
      []
      iex> binding([:x, :y], :foo)
      [x: 1]

  """
  defmacro binding(list, context) when is_list(list) and is_atom(context) do
    do_binding(list, context, __CALLER__.vars, __CALLER__.in_match?)
  end

  defp do_binding(context, vars, in_match) do
    lc { v, c } inlist vars, c == context, v != :_@CALLER do
      { v, wrap_binding(in_match, { v, [], c }) }
    end
  end

  defp do_binding(list, context, vars, in_match) do
    lc { v, c } inlist vars, c == context, :lists.member(v, list) do
      { v, wrap_binding(in_match, { v, [], c }) }
    end
  end

  defp wrap_binding(true, var) do
    quote do: ^(unquote(var))
  end

  defp wrap_binding(_, var) do
    var
  end

  @doc """
  Provides an `if` macro. This macro expects the first argument to
  be a condition and the rest are keyword arguments.

  ## One-liner examples

      if(foo, do: bar)

  In the example above, `bar` will be returned if `foo` evaluates to
  `true` (i.e. it is neither `false` nor `nil`). Otherwise, `nil` will be returned.

  An `else` option can be given to specify the opposite:

      if(foo, do: bar, else: baz)

  ## Blocks examples

  Elixir also allows you to pass a block to the `if` macro. The first
  example above would be translated to:

      if foo do
        bar
      end

  Notice that `do/end` becomes delimiters. The second example would
  then translate to:

      if foo do
        bar
      else
        baz
      end

  If you want to compare more than two clauses, you can use the `cond/1`
  macro.
  """
  defmacro if(condition, clauses) do
    do_clause = Keyword.get(clauses, :do, nil)
    else_clause = Keyword.get(clauses, :else, nil)

    quote do
      case unquote(condition) do
        unquote(cond_var) when unquote(cond_var) in [false, nil] -> unquote(else_clause)
        _ -> unquote(do_clause)
      end
    end
  end

  @doc """
  Evaluates the expression corresponding to the first clause that
  evaluates to true. Raises an error if all conditions evaluate to
  to nil or false.

  ## Examples

      cond do
        1 + 1 == 1 ->
          "This will never match"
        2 * 2 != 4 ->
          "Nor this"
        true ->
          "This will"
      end

  """
  defmacro cond([do: pairs]) do
    [{ :->, meta, [[condition], clause] }|t] = :lists.reverse pairs

    new_acc =
      case condition do
        { :_, _, atom } when is_atom(atom) ->
          raise ArgumentError, message: <<"unbound variable _ inside cond. ",
            "If you want the last clause to match, you probably meant to use true ->">>
        x when is_atom(x) and x != false and x != nil ->
          clause
        _ ->
          quote line: get_line(meta) do
            case !unquote(condition) do
              false -> unquote(clause)
            end
          end
      end

    build_cond_clauses(t, new_acc, meta)
  end

  # Builds cond clauses by nesting them recursively.
  #
  #     case !foo do
  #       false -> 1
  #       true ->
  #         case !bar do
  #           false -> 2
  #           true -> 3
  #         end
  #     end
  #
  defp build_cond_clauses([{ :->, new, [[condition], clause] }|t], acc, old) do
    clauses = [falsy_clause(old, acc), truthy_clause(new, clause)]
    acc = quote do: (case unquote(condition), do: unquote(clauses))
    build_cond_clauses(t, acc, new)
  end

  defp build_cond_clauses([], acc, _), do: acc

  defp falsy_clause(meta, acc) do
    { :->, meta, [[quote(do: unquote(cond_var) when unquote(cond_var) in [false, nil])], acc] }
  end

  defp truthy_clause(meta, clause) do
    { :->, meta, [[quote(do: _)], clause] }
  end

  # Setting cond: true in metadata turns on a small optimization
  # in Elixir compiler. In the long run, we want to bring this
  # optimization to Elixir land, but not right now.
  defp cond_var do
    { :x, [cond: true], Kernel }
  end

  defp get_line(meta) do
    case :lists.keyfind(:line, 1, meta) do
      { :line, line } -> line
      false -> 0
    end
  end

  @doc """
  Evaluates and returns the do-block passed in as a second argument
  unless clause evaluates to true.
  Returns nil otherwise.
  See also `if`.

  ## Examples

      iex> unless(1, do: "Hello")
      nil
      iex> unless(false, do: "Hello")
      "Hello"

  """
  defmacro unless(clause, options) do
    do_clause   = Keyword.get(options, :do, nil)
    else_clause = Keyword.get(options, :else, nil)
    quote do
      if(unquote(clause), do: unquote(else_clause), else: unquote(do_clause))
    end
  end

  @doc """
  Allows you to destructure two lists, assigning each term in the right to the
  matching term in the left. Unlike pattern matching via `=`, if the sizes of
  the left and right lists don't match, destructuring simply stops instead of
  raising an error.

  ## Examples

      iex> destructure([x, y, z], [1, 2, 3, 4, 5])
      ...> {x, y, z}
      {1, 2, 3}

  Notice in the example above, even though the right
  size has more entries than the left, destructuring works
  fine. If the right size is smaller, the remaining items
  are simply assigned to nil:

      iex> destructure([x, y, z], [1])
      ...> {x, y, z}
      {1, nil, nil}

  The left side supports any expression you would use
  on the left side of a match:

      x = 1
      destructure([^x, y, z], [1, 2, 3])

  The example above will only work if x matches
  the first value from the right side. Otherwise,
  it will raise a CaseClauseError.
  """
  defmacro destructure(left, right) when is_list(left) do
    Enum.reduce left, right, fn item, acc ->
      { :case, meta, args } =
        quote do
          case unquote(acc) do
            [unquote(item)|t] ->
              t
            other when other == [] or other == nil ->
              unquote(item) = nil
          end
        end
      { :case, [{:export_all,true}|meta], args }
    end
  end

  @doc """
  Returns a range with the specified start and end.
  Includes both ends.

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
    { :{}, [], [Elixir.Range, first, last] }
  end

  @doc """
  Provides a short-circuit operator that evaluates and returns
  the second expression only if the first one evaluates to true
  (i.e. it is not nil nor false). Returns the first expression
  otherwise.

  ## Examples

      iex> true && true
      true
      iex> nil && true
      nil
      iex> true && 1
      1
      iex> false && throw(:bad)
      false

  Notice that, unlike Erlang's `and` operator,
  this operator accepts any expression as an argument,
  not only booleans, however it is not allowed in guards.
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
  expression only if the first one does not evaluate to true (i.e. it
  is either nil or false). Returns the first expression otherwise.

  ## Examples

      iex> false || false
      false
      iex> nil || true
      true
      iex> false || 1
      1
      iex> true || throw(:bad)
      true

  Notice that, unlike Erlang's `or` operator,
  this operator accepts any expression as an argument,
  not only booleans, however it is not allowed in guards.
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
  `|>` is called the pipeline operator as it is useful
  to write pipeline style expressions. This operator
  introduces the expression on the left as the first
  argument to the function call on the right.

  ## Examples

      iex> [1, [2], 3] |> List.flatten |> Enum.map(&(&1 * 2))
      [2,4,6]

  The expression above is simply translated to:

      Enum.map(List.flatten([1, [2], 3]), &(&1 * 2))

  Be aware of operator precedence when using this operator.
  For example, the following expression:

      String.graphemes "Hello" |> Enum.reverse

  Is translated to:

      String.graphemes("Hello" |> Enum.reverse)

  Which will result in an error as Enumerable protocol
  is not defined for binaries. Adding explicit parenthesis
  resolves the ambiguity:

      String.graphemes("Hello") |> Enum.reverse

  """
  defmacro left |> right do
    :lists.foldl fn x, acc -> Macro.pipe(acc, x) end, left, Macro.unpipe(right)
  end

  @doc """
  Returns true if the `module` is loaded and contains a
  public `function` with the given `arity`, otherwise false.

  In case a tuple module is given, the `arity` is automatically
  increased by one.

  Notice that this function does not load the module in case
  it is not loaded. Check `Code.ensure_loaded/1` for more
  information.
  """
  @spec function_exported?(atom | tuple, atom, integer) :: boolean
  def function_exported?(module, function, arity) do
    case is_tuple(module) do
      true  ->
        :erlang.function_exported(elem(module, 0), function, arity + 1)
      false ->
        :erlang.function_exported(module, function, arity)
    end
  end

  @doc """
  Returns true if the `module` is loaded and contains a
  public `macro` with the given `arity`, otherwise false.

  Notice that this function does not load the module in case
  it is not loaded. Check `Code.ensure_loaded/1` for more
  information.
  """
  @spec macro_exported?(atom, atom, integer) :: boolean
  def macro_exported?(module, macro, arity) do
    :lists.member({macro, arity}, module.__info__(:macros))
  end

  @doc """
  Access the given element using the qualifier according
  to the `Access` protocol. All calls in the form `foo[bar]`
  are translated to `access(foo, bar)`.

  The usage of this protocol is to access a raw value in a
  keyword list.

      sample = [a: 1, b: 2, c: 3]
      sample[:b] #=> 2

  ## Aliases

  Whenever invoked on an alias or an atom, the access protocol is
  expanded at compilation time rather than on runtime. This feature
  is used by records to allow a developer to match against an specific
  part of a record:

      def increment(State[counter: counter, other: 13] = state) do
        state.counter(counter + 1)
      end

  In the example above, we use the Access protocol to match the
  counter field in the record `State`. Considering the record
  definition is as follows:

      defrecord State, counter: 0, other: nil

  The clause above is translated to:

      def increment({ State, counter, 13 } = state) do
        state.counter(counter + 1)
      end

  The same pattern can be used to create a new record:

      def new_state(counter) do
        State[counter: counter]
      end

  The example above is faster than `State.new(counter: :counter)` because
  the record is expanded at compilation time and not at runtime. If a field
  is not specified on creation, it will have its default value.

  Finally, as in Erlang, Elixir also allows the following syntax:

      new_uri = State[_: 1]

  In this case **all** fields will be set to `1`. Notice that,
  as in Erlang, in case an expression is given, it will be
  evaluated multiple times:

      new_uri = State[_: IO.puts "Hello"]

  In this case, `"Hello"` will be printed twice (one per each field).
  """
  defmacro access(element, args) when is_list(args) do
    caller = __CALLER__
    atom   = Macro.expand(element, caller)

    case is_atom(atom) and atom != nil do
      true ->
        fields =
          try do
            case :lists.member(atom, caller.context_modules) and Module.open?(atom) do
              true  -> Module.get_attribute(atom, :record_fields)
              false -> atom.__record__(:fields)
            end
          rescue
            UndefinedFunctionError ->
              # We first try to call __record__ and just then check if
              # it is loaded so we allow the ParallelCompiler to solve
              # conflicts.
              case :code.ensure_loaded(atom) do
                { :error, _ } ->
                  :elixir_aliases.ensure_loaded(caller.line, atom, :elixir_env.ex_to_env(caller))
                _ ->
                  raise ArgumentError, message: "cannot access module #{inspect atom} because it is not a record"
              end
          end

        Record.access(atom, fields, args, caller)
      false ->
        case caller.in_match? or caller.in_guard? do
          true  -> raise ArgumentError, message: "dynamic access cannot be invoked inside match and guard clauses"
          false -> :ok
        end

        case args do
          [h] -> quote do: Access.access(unquote(element), unquote(h))
          _   -> raise ArgumentError, message: "expected one argument in access"
        end
    end
  end

  @doc """
  Checks if the element on the left side is member of the
  collection on the right side.

  ## Examples

      iex> x = 1
      ...> x in [1, 2, 3]
      true

  This macro simply translates the expression above to:

      Enum.member?([1,2,3], x)

  ## Guards

  The in operator can be used on guard clauses as long as the
  right side is a range or a list. Elixir will then expand the
  operator to a valid guard expression. For example:

      when x in [1,2,3]

  Translates to:

      when x === 1 or x === 2 or x === 3

  When using ranges:

      when x in 1..3

  Translates to:

      when x >= 1 and x <= 3

  """
  defmacro left in right do
    cache = (env_context(__CALLER__) == nil)

    right = case bootstraped?(Macro.Env) do
      true  -> Macro.expand(right, __CALLER__)
      false ->
        case right do
          # For bootstrapping we special case @attributes
          { :@, _, [{ name, _, atom }] } when is_atom(name) and is_atom(atom) ->
            Module.get_attribute(env_module(__CALLER__), name, true)
          _ ->
            right
        end
    end

    case right do
      _ when cache ->
        quote do: Elixir.Enum.member?(unquote(right), unquote(left))
      [] ->
        false
      [h|t] ->
        :lists.foldr(fn x, acc ->
          quote do
            unquote(comp(left, x)) or unquote(acc)
          end
        end, comp(left, h), t)
      { :{}, _, [Elixir.Range, first, last] } ->
        in_range(left, Macro.expand(first, __CALLER__), Macro.expand(last, __CALLER__))
      first .. last ->
        # This range came from a module attribute, so it is a
        # literal value and we need to escape it.
        in_range(left, Macro.escape(first), Macro.escape(last))
      _ ->
        raise ArgumentError, message: <<"invalid args for operator in, it expects a compile time list ",
                                        "or range on the right side when used in guard expressions">>
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
  When used inside quoting, marks that the variable should
  not be hygienized. The argument can be either a variable
  unquoted or an atom representing the variable name.

  Check `Kernel.SpecialForms.quote/2` for more information.
  """
  defmacro var!(var, context // nil)

  defmacro var!(var, context) when is_atom(var) do
    do_var!(var, [], context, __CALLER__)
  end

  defmacro var!({ name, meta, atom }, context) when is_atom(name) and is_atom(atom) do
    do_var!(name, meta, context, __CALLER__)
  end

  defmacro var!(x, _context) do
    raise ArgumentError, message: "expected a var to be given to var!, got: #{Macro.to_string(x)}"
  end

  defp do_var!(name, meta, context, env) do
    # Remove counter and force them to be vars
    meta = :lists.keydelete(:counter, 1, meta)
    meta = :lists.keystore(:var, 1, meta, { :var, true })

    case Macro.expand(context, env) do
      x when is_atom(x) ->
        { name, meta, x }
      x ->
        raise ArgumentError, message: "expected var! context to expand to an atom, got: #{Macro.to_string(x)}"
    end
  end

  @doc """
  When used inside quoting, marks that the alias should not
  be hygienezed. This means the alias will be expanded when
  the macro is expanded.

  Check `Kernel.SpecialForms.quote/2` for more information.
  """
  defmacro alias!(alias)

  defmacro alias!(alias) when is_atom(alias) do
    alias
  end

  defmacro alias!({ :__aliases__, meta, args }) do
    # Simply remove the alias metadata from the node
    # so it does not affect expansion.
    { :__aliases__, :lists.keydelete(:alias, 1, meta), args }
  end

  ## Definitions implemented in Elixir


  @doc """
  Defines a module given by name with the given contents.

  It returns the module name, the module binary and the
  block contents result.

  ## Examples

      defmodule Foo do
        def bar, do: :baz
      end

      Foo.bar #=> :baz

  ## Nesting

  Nesting a module inside another module affects its name:

      defmodule Foo do
        defmodule Bar do
        end
      end

  In the example above, two modules `Foo` and `Foo.Bar` are created.
  When nesting, Elixir automatically creates an alias, allowing the
  second module `Foo.Bar` to be accessed as `Bar` in the same lexical
  scope.

  This means that, if the module `Bar` is moved to another file,
  the references to `Bar` needs to be updated or an alias needs to
  be explicitly set with the help of `Kernel.SpecialForms.alias/2`.

  ## Dynamic names

  Elixir module names can be dynamically generated. This is very
  useful for macros. For instance, one could write:

      defmodule binary_to_atom("Foo\#{1}") do
        # contents ...
      end

  Elixir will accept any module name as long as the expression
  returns an atom. Note that, when a dynamic name is used, Elixir
  won't nest the name under the current module nor automatically
  set up an alias.
  """
  defmacro defmodule(alias, do: block) do
    env   = __CALLER__
    boot? = bootstraped?(Macro.Env)

    expanded =
      case boot? do
        true  -> Macro.expand(alias, env)
        false -> alias
      end

    { expanded, with_alias } =
      case boot? and is_atom(expanded) do
        true ->
          # Expand the module considering the current environment/nesting
          full = expand_module(alias, expanded, env)

          # Generate the alias for this module definition
          { new, old } = module_nesting(env_module(env), full)
          meta = [defined: full, context: true] ++ alias_meta(alias)

          { full, { :alias, meta, [old, [as: new, warn: false]] } }
        false ->
          { expanded, nil }
      end

    { escaped, _ } = :elixir_quote.escape(block, false)
    module_vars    = module_vars(env_vars(env), 0)

    quote do
      unquote(with_alias)
      :elixir_module.compile(unquote(expanded), unquote(escaped),
                             unquote(module_vars), __ENV__)
    end
  end

  defp alias_meta({ :__aliases__, meta, _ }), do: meta
  defp alias_meta(_), do: []

  # defmodule :foo
  defp expand_module(raw, _module, _env) when is_atom(raw),
    do: raw

  # defmodule Hello
  defp expand_module({ :__aliases__, _, [h] }, _module, env),
    do: :elixir_aliases.concat([env.module, h])

  # defmodule Hello.World
  defp expand_module({ :__aliases__, _, _ } = alias, module, env) do
    case :elixir_aliases.expand(alias, env.aliases, env.macro_aliases, env.lexical_tracker) do
      atom when is_atom(atom) ->
        module
      aliases when is_list(aliases) ->
        :elixir_aliases.concat([env.module, module])
    end
  end

  # defmodule Elixir.Hello.World
  defp expand_module(_raw, module, env),
    do: :elixir_aliases.concat([env.module, module])

  # quote vars to be injected into the module definition
  defp module_vars([{ key, kind }|vars], counter) do
    var =
      case is_atom(kind) do
        true  -> { key, [], kind }
        false -> { key, [counter: kind], nil }
      end

    args = [key, kind, binary_to_atom(<<"_@", integer_to_binary(counter)::binary>>), var]
    [{ :{}, [], args }|module_vars(vars, counter+1)]
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
  #     { 'Elixir.Baz', 'Elixir.Foo.Bar.Baz' }
  #
  # In case there is no nesting/no module:
  #
  #     module_nesting(nil, 'Elixir.Foo.Bar.Baz.Bat')
  #     { false, 'Elixir.Foo.Bar.Baz.Bat' }
  #
  defp module_nesting(nil, full),
    do: { false, full }

  defp module_nesting(prefix, full) do
    case split_module(prefix) do
      [] -> { false, full }
      prefix -> module_nesting(prefix, split_module(full), [], full)
    end
  end

  defp module_nesting([x|t1], [x|t2], acc, full),
    do: module_nesting(t1, t2, [x|acc], full)
  defp module_nesting([], [h|_], acc, _full),
    do: { binary_to_atom(<<"Elixir.", h::binary>>),
          :elixir_aliases.concat(:lists.reverse([h|acc])) }
  defp module_nesting(_, _, _acc, full),
    do: { false, full }

  defp split_module(atom) do
    case :binary.split(atom_to_binary(atom), ".", [:global]) do
      ["Elixir"|t] -> t
      _ -> []
    end
  end

  @doc """
  Defines a function with the given name and contents.

  ## Examples

      defmodule Foo do
        def bar, do: :baz
      end

      Foo.bar #=> :baz

  A function that expects arguments can be defined as follow:

      defmodule Foo do
        def sum(a, b) do
          a + b
        end
      end

  In the example above, we defined a function `sum` that receives
  two arguments and sums them.

  """
  defmacro def(call, expr // nil) do
    define(:def, call, expr, __CALLER__)
  end

  @doc """
  Defines a function that is private. Private functions are
  only accessible from within the module in which they are defined.

  Check `def/2` for more information

  ## Examples

      defmodule Foo do
        def bar do
          sum(1, 2)
        end

        defp sum(a, b), do: a + b
      end

  In the example above, `sum` is private and accessing it
  through `Foo.sum` will raise an error.
  """
  defmacro defp(call, expr // nil) do
    define(:defp, call, expr, __CALLER__)
  end

  @doc """
  Defines a macro with the given name and contents.

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
  defmacro defmacro(call, expr // nil) do
    define(:defmacro, call, expr, __CALLER__)
  end

  @doc """
  Defines a macro that is private. Private macros are
  only accessible from the same module in which they are defined.

  Check `defmacro/2` for more information
  """
  defmacro defmacrop(call, expr // nil) do
    define(:defmacrop, call, expr, __CALLER__)
  end

  defp define(kind, call, expr, env) do
    assert_module_scope(env, kind, 2)
    assert_no_function_scope(env, kind, 2)
    line = env_line(env)

    { call, uc } = :elixir_quote.escape(call, true)
    { expr, ue } = :elixir_quote.escape(expr, true)

    # Do not check clauses if any expression was unquoted
    check_clauses = not(ue or uc)
    pos = :elixir_locals.cache_env(env)

    quote do
      :elixir_def.store_definition(unquote(line), unquote(kind), unquote(check_clauses),
                                   unquote(call), unquote(expr), unquote(pos))
    end
  end

  @doc %S"""
  Exports a module with a record definition and runtime operations.

  Please see the `Record` module's documentation for an introduction
  to records in Elixir. The following sections are going into details
  specific to `defrecord`.

  ## Examples

      defrecord User, name: nil, age: 0

  The following line defines a module that exports information
  about a record. The definition above provides a shortcut
  syntax for creating and updating the record at compilation
  time:

      user = User[]
      #=> User[name: nil, age: 0]

      User[name: "José", age: 25]
      #=> User[name: "José", age: 25]

  And also a set of functions for working with the record
  at runtime:

      user = User.new(age: 25)
      user.name          #=> Returns the value of name
      user.name("José")  #=> Updates the value of name

      # Update multiple attributes at once:
      user.update(name: "Other", age: 25)

      # Obtain the keywords representation of a record:
      user.to_keywords #=> [name: "José", age: 25]

  Since a record is simply a tuple where the first element is
  the record name, we can get the raw record representation as
  follows:

      inspect User.new, records: false
      #=> { User, nil, 0 }

  In addition to defining readers and writers for each attribute, Elixir also
  defines an `update_#{attribute}` function to update the value. Such
  functions expect a function as an argument that receives the current
  value and must return the new one. For example, every time the file
  is accessed, the accesses counter can be incremented with:

      user.update_age(fn(old) -> old + 1 end)

  ## Types

  Every record defines a type named `t` that can be accessed in typespecs.
  Those types can be specified inside the record definition:

      defrecord User do
        record_type name: string, age: integer
      end

  All fields without a specified type are assumed to have type `term`.

  Assuming the `User` record defined above, it could be used in typespecs
  as follow:

      @spec handle_user(User.t) :: boolean()

  ## Runtime introspection

  At runtime, developers can use `__record__` to get information
  about the given record:

      User.__record__(:name)
      #=> User

      User.__record__(:fields)
      #=> [name: nil, age: 0]

  In order to quickly access the index of a field, one can use
  the `__record__` function with `:index` as the first argument:

      User.__record__(:index, :age)
      #=> 2

      User.__record__(:index, :unknown)
      #=> nil

  ## Compile-time introspection

  At compile time, one can access the following information about the record
  from within the record module:

  * `@record_fields` — a keyword list of record fields with defaults
  * `@record_types` — a keyword list of record fields with types

  For example:

      defrecord Foo, bar: nil do
        record_type bar: nil | integer
        IO.inspect @record_fields
        IO.inspect @record_types
      end

  Prints out:

       [bar: nil]
       [bar: {:|,[line: ...],[nil,{:integer,[line: ...],nil}]}]

  Where the last line is a quoted representation of

       [bar: nil | integer]

  """
  defmacro defrecord(name, fields, do_block // [])

  defmacro defrecord(name, fields, do_block) do
    case is_list(fields) and Keyword.get(fields, :do, false) do
      false -> Record.defrecord(name, fields, do_block)
      other -> Record.defrecord(name, Keyword.delete(fields, :do), do: other)
    end
  end

  @doc %S"""
  Defines a set of private macros to manipulate a record definition.

  This macro defines a set of macros private to the current module to
  manipulate the record exclusively at compilation time.

  Please see the `Record` module's documentation for an introduction
  to records in Elixir.

  ## Examples

      defmodule User do
        defrecordp :user, [name: "José", age: "25"]
      end

  In the example above, a set of macros named `user` but with different
  arities will be defined to manipulate the underlying record:

      # To create records
      user()        #=> { :user, "José", 25 }
      user(age: 26) #=> { :user, "José", 26 }

      # To get a field from the record use pattern matching
      user(name: name) = user
      name #=> "José"

      # To update the record
      user(record, age: 26) #=> { :user, "José", 26 }

  By default, Elixir uses the record name as the first element of the tuple.
  In some cases though, this might be undesirable and one can explicitly
  define what the first element of the record should be:

      defmodule MyServer do
        defrecordp :state, MyServer, data: nil
      end

  This way, the record created will have `MyServer` as the first element,
  not `:state`:

      state() #=> { MyServer, nil }

  ## Types

  `defrecordp` allows a developer to generate a type
  automatically by simply providing a type to its fields.
  The following definition:

      defrecordp :user,
        name: "José" :: binary,
        age: 25 :: integer

  Will generate the following type:

      @typep user_t :: { :user, binary, integer }

  """
  defmacro defrecordp(name, tag // nil, fields) do
    Record.defrecordp(name, Macro.expand(tag, __CALLER__), fields)
  end

  @doc %S"""
  Defines an exception.

  Exceptions are simply records with three differences:

  1. Exceptions are required to define a function `exception/1`
     that receives keyword arguments and returns the exception.
     This function is a callback usually invoked by `raise/2`;

  2. Exceptions are required to provide a `message` field.
     This field must return a String with a formatted error message;

  3. Unlike records, exceptions are documented by default.

  Since exceptions are records, `defexception/3` has exactly
  the same API as `defrecord/3`.

  ## Raising exceptions

  The most common way to raise an exception is via the `raise/2`
  function:

      defexception MyException, [:message]
      raise MyException,
        message: "did not get what was expected, got: #{inspect value}"

  In many cases it is more convenient to pass the
  expected value to `raise` and generate the message in the `exception/1`
  callback:

      defexception MyException, [:message] do
        def exception(opts) do
          msg = "did not get what was expected, got: #{inspect opts[:actual]}"
          MyException[message: msg]
        end
      end

      raise MyException, actual: value

  The example above is the preferred mechanism for customizing
  exception messages.
  """
  defmacro defexception(name, fields, do_block // []) do
    { fields, do_block } =
      case is_list(fields) and Keyword.get(fields, :do, false) do
        false -> { fields, do_block }
        other -> { Keyword.delete(fields, :do), [do: other] }
      end

    do_block = Keyword.put(do_block, :do, quote do
      @moduledoc nil
      record_type message: String.t

      @doc false
      def exception(args), do: new(args)

      @doc false
      def exception(args, self), do: update(args, self)

      defoverridable exception: 1, exception: 2
      unquote(Keyword.get do_block, :do)
    end)

    fields = quote do: [__exception__: :__exception__] ++ unquote(fields)
    record = Record.defrecord(name, fields, do_block)

    quote do
      { :module, name, _, _ } = unquote(record)

      unless :erlang.function_exported(name, :message, 1) do
        Kernel.raise "expected exception #{inspect name} to implement message/1"
      end
    end
  end

  @doc """
  Defines a module as a protocol and specifies the API that
  should be defined by its implementations.

  ## Examples

  In Elixir, only `false` and `nil` are considered falsy values.
  Everything else evaluates to true in `if` clauses. Depending
  on the application, it may be important to specify a `blank?`
  protocol that returns a boolean for other data types that should
  be considered `blank?`. For instance, an empty list or an empty
  binary could be considered blanks.

  We could implement this protocol as follow:

      defprotocol Blank do
        @doc "Returns true if data is considered blank/empty"
        def blank?(data)
      end

  Now that the protocol is defined, we can implement it. We need
  to implement the protocol for each Elixir type. For example:

      # Integers are never blank
      defimpl Blank, for: Integer do
        def blank?(number), do: false
      end

      # Just empty list is blank
      defimpl Blank, for: List do
        def blank?([]), do: true
        def blank?(_),  do: false
      end

      # Just the atoms false and nil are blank
      defimpl Blank, for: Atom do
        def blank?(false), do: true
        def blank?(nil),   do: true
        def blank?(_),     do: false
      end

  And we would have to define the implementation for all types.
  The supported types available are:

  * Record (see below)
  * Tuple
  * Atom
  * List
  * BitString
  * Integer
  * Float
  * Function
  * PID
  * Port
  * Reference
  * Any (see below)

  ## Protocols + Records

  The real benefit of protocols comes when mixed with records.
  For instance, Elixir ships with many data types implemented as
  records, like `HashDict` and `HashSet`. We can implement the
  `Blank` protocol for those types as well:

      defimpl Blank, for: HashDict do
        def blank?(dict), do: Dict.empty?(dict)
      end

  Since records are tuples, if a protocol is not found a given
  type, it will fallback to `Tuple`.

  ## Fallback to any

  In some cases, it may be convenient to provide a default
  implementation for all types. This can be achieved by
  setting `@fallback_to_any` to `true` in the protocol
  definition:

      defprotocol Blank do
        @fallback_to_any true
        def blank?(data)
      end

  Which can now be implemented as:

      defimpl Blank, for: Any do
        def blank?(_), do: true
      end

  One may wonder why such fallback is not true by default.

  It is two-fold: first, the majority of protocols cannot
  implement an action in a generic way for all types. In fact,
  providing a default implementation may be harmful, because users
  may rely on the default implementation instead of providing a
  specialized one.

  Second, falling back to `Any` adds an extra lookup to all types,
  which is unnecessary overhead unless an implementation for Any is
  required.

  ## Types

  As in records, defining a protocol automatically defines a type
  named `t`, which can be used as:

      @spec present?(Blank.t) :: boolean
      def present?(blank) do
        not Blank.blank?(blank)
      end

  The `@spec` above expresses that all types allowed to implement the
  given protocol are valid argument types for the given function.

  ## Reflection

  Any protocol module contains three extra functions:


  * `__protocol__/1` - returns the protocol name when :name is given,
                       and a keyword list with the protocol functions
                       when :functions is given;

  * `impl_for/1` - receives a structure and returns the module that implements
                   the protocol for the structure, nil otherwise;

  * `impl_for!/1` - same as above but raises an error if an implementation is not found

  ## Consolidation

  In order to cope with code loading in development, protocols in
  Elixir provide a slow implementation of protocol dispatching in
  development.

  In order to speed up dispatching in production environments, where
  all implementations are now up-front, Elixir provides a feature
  called protocol consolidation. For this reason, all protocols are
  compiled with `debug_info` set to true, regardless of the option
  set by `elixirc` compiler.

  For more information on how to apply protocol consolidation to
  a given project, please check the `mix compile.protocols` task.
  """
  defmacro defprotocol(name, do: block) do
    Protocol.defprotocol(name, do: block)
  end

  @doc """
  Defines an implementation for the given protocol. See
  `defprotocol/2` for examples.

  Inside an implementation, the name of the protocol can be accessed
  via `@protocol` and the current target as `@for`.
  """
  defmacro defimpl(name, opts, do_block // []) do
    merged = Keyword.merge(opts, do_block)
    merged = Keyword.put_new(merged, :for, __CALLER__.module)
    Protocol.defimpl(name, merged)
  end

  @doc """
  Makes the given functions in the current module overridable. An overridable
  function is lazily defined, allowing a developer to customize it.

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

  As seen as in the example `super` can be used to call the default
  implementation.
  """
  defmacro defoverridable(tuples) do
    quote do
      Module.make_overridable(__MODULE__, unquote(tuples))
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

        def test_always_pass do
          true = true
        end
      end

  By calling `use`, a hook called `__using__` will be invoked in
  `ExUnit.Case` which will then do the proper setup.

  Simply put, `use` is simply a translation to:

      defmodule AssertionTest do
        require ExUnit.Case
        ExUnit.Case.__using__([async: true])

        def test_always_pass do
          true = true
        end
      end

  """
  defmacro use(module, args // []) do
    expanded = Macro.expand(module, __CALLER__)

    case is_atom(expanded) do
      false ->
        raise ArgumentError, message: "invalid arguments for use, expected an atom or alias as argument"
      true ->
        quote do
          require unquote(expanded)
          unquote(expanded).__using__(unquote(args))
        end
    end
  end

  @doc """
  Defines the given functions in the current module that will
  delegate to the given `target`. Functions defined with
  `defdelegate` are public and are allowed to be invoked
  from external. If you find yourself wishing to define a
  delegation as private, you should likely use import
  instead.

  Delegation only works with functions, delegating to macros
  is not supported.

  ## Options

  * `:to` - The expression to delegate to. Any expression
    is allowed and its results will be calculated on runtime;

  * `:as` - The function to call on the target given in `:to`.
    This parameter is optional and defaults to the name being
    delegated.

  * `:append_first` - If true, when delegated, first argument
    passed to the delegate will be relocated to the end of the
    arguments when dispatched to the target. The motivation behind
    this is because Elixir normalizes the "handle" as a first
    argument and some Erlang modules expect it as last argument.

  ## Examples

      defmodule MyList do
        defdelegate reverse(list), to: :lists
        defdelegate [reverse(list), map(callback, list)], to: :lists
        defdelegate other_reverse(list), to: :lists, as: :reverse
      end

      MyList.reverse([1, 2, 3])
      #=> [3,2,1]

      MyList.other_reverse([1, 2, 3])
      #=> [3,2,1]

  """
  defmacro defdelegate(funs, opts) do
    funs = Macro.escape(funs, unquote: true)
    quote bind_quoted: [funs: funs, opts: opts] do
      target = Keyword.get(opts, :to) ||
        raise ArgumentError, message: "Expected to: to be given as argument"

      append_first = Keyword.get(opts, :append_first, false)

      lc fun inlist List.wrap(funs) do
        { name, args } =
          case Macro.decompose_call(fun) do
            { _, _ } = pair -> pair
            _ -> raise ArgumentError, message: "invalid syntax in defdelegate #{Macro.to_string(fun)}"
          end

        actual_args =
          case append_first and args != [] do
            true  -> tl(args) ++ [hd(args)]
            false -> args
          end

        fun = Keyword.get(opts, :as, name)

        def unquote(name)(unquote_splicing(args)) do
          unquote(target).unquote(fun)(unquote_splicing(actual_args))
        end
      end
    end
  end

  ## Sigils

  @doc """
  Handles the sigil %S. It simply returns a string
  without escaping characters and without interpolations.

  ## Examples

      iex> %S(foo)
      "foo"
      iex> %S(f\#{o}o)
      "f\\\#{o}o"

  """
  defmacro sigil_S(string, []) do
    string
  end

  @doc """
  Handles the sigil %s. It returns a string as if it was double quoted
  string, unescaping characters and replacing interpolations.

  ## Examples

      iex> %s(foo)
      "foo"
      iex> %s(f\#{:o}o)
      "foo"

  """
  defmacro sigil_s({ :<<>>, line, pieces }, []) do
    { :<<>>, line, Macro.unescape_tokens(pieces) }
  end

  @doc """
  Handles the sigil %C. It simply returns a char list
  without escaping characters and without interpolations.

  ## Examples

      iex> %C(foo)
      'foo'
      iex> %C(f\#{o}o)
      'f\\\#{o}o'

  """
  defmacro sigil_C({ :<<>>, _line, [string] }, []) when is_binary(string) do
    String.to_char_list!(string)
  end

  @doc """
  Handles the sigil %c. It returns a char list as if it were a single
  quoted string, unescaping characters and replacing interpolations.

  ## Examples

      iex> %c(foo)
      'foo'
      iex> %c(f\#{:o}o)
      'foo'

  """

  # We can skip the runtime conversion if we are
  # creating a binary made solely of series of chars.
  defmacro sigil_c({ :<<>>, _line, [string] }, []) when is_binary(string) do
    String.to_char_list!(Macro.unescape_string(string))
  end

  defmacro sigil_c({ :<<>>, line, pieces }, []) do
    binary = { :<<>>, line, Macro.unescape_tokens(pieces) }
    quote do: String.to_char_list!(unquote(binary))
  end

  @doc """
  Handles the sigil %r. It returns a Regex pattern.

  ## Examples

      iex> Regex.match?(%r(foo), "foo")
      true

  """
  defmacro sigil_r({ :<<>>, _line, [string] }, options) when is_binary(string) do
    binary = Macro.unescape_string(string, fn(x) -> Regex.unescape_map(x) end)
    regex  = Regex.compile!(binary, :binary.list_to_bin(options))
    Macro.escape(regex)
  end

  defmacro sigil_r({ :<<>>, line, pieces }, options) do
    binary = { :<<>>, line, Macro.unescape_tokens(pieces, fn(x) -> Regex.unescape_map(x) end) }
    quote do: Regex.compile!(unquote(binary), unquote(:binary.list_to_bin(options)))
  end

  @doc """
  Handles the sigil %R. It returns a Regex pattern without escaping
  nor interpreting interpolations.

  ## Examples

      iex> Regex.match?(%R(f\#{1,3}o), "f\#o")
      true

  """
  defmacro sigil_R({ :<<>>, _line, [string] }, options) when is_binary(string) do
    regex = Regex.compile!(string, :binary.list_to_bin(options))
    Macro.escape(regex)
  end

  @doc """
  Handles the sigil %w. It returns a list of "words" split by whitespace.

  ## Modifiers

  * `s`: strings (default)
  * `a`: atoms
  * `c`: char lists

  ## Examples

      iex> %w(foo \#{:bar} baz)
      ["foo", "bar", "baz"]
      iex> %w(--source test/enum_test.exs)
      ["--source", "test/enum_test.exs"]
      iex> %w(foo bar baz)a
      [:foo, :bar, :baz]

  """

  defmacro sigil_w({ :<<>>, _line, [string] }, modifiers) when is_binary(string) do
    split_words(Macro.unescape_string(string), modifiers)
  end

  defmacro sigil_w({ :<<>>, line, pieces }, modifiers) do
    binary = { :<<>>, line, Macro.unescape_tokens(pieces) }
    split_words(binary, modifiers)
  end

  @doc """
  Handles the sigil %W. It returns a list of "words" split by whitespace
  without escaping nor interpreting interpolations.

  ## Modifiers

  * `s`: strings (default)
  * `a`: atoms
  * `c`: char lists

  ## Examples

      iex> %W(foo \#{bar} baz)
      ["foo", "\\\#{bar}", "baz"]

  """
  defmacro sigil_W({ :<<>>, _line, [string] }, modifiers) when is_binary(string) do
    split_words(string, modifiers)
  end

  defp split_words("", _modifiers), do: []

  defp split_words(string, modifiers) do
    mod =
      case modifiers do
        [] -> ?s
        [mod] when mod == ?s or mod == ?a or mod == ?c -> mod
        _else -> raise ArgumentError, message: "modifier must be one of: s, a, c"
      end

    case is_binary(string) do
      true ->
        case mod do
          ?s -> String.split(string)
          ?a -> lc p inlist String.split(string), do: binary_to_atom(p)
          ?c -> lc p inlist String.split(string), do: String.to_char_list!(p)
        end
      false ->
        case mod do
          ?s -> quote do: String.split(unquote(string))
          ?a -> quote do: lc(p inlist String.split(unquote(string)), do: binary_to_atom(p))
          ?c -> quote do: lc(p inlist String.split(unquote(string)), do: String.to_char_list!(p))
        end
    end
  end

  ## Shared functions

  # We need this check only for bootstrap purposes.
  # Once Kernel is loaded and we recompile, it is a no-op.
  case :code.ensure_loaded(Kernel) do
    { :module, _ } ->
      defp bootstraped?(_), do: true
      defp internal?, do: false
    { :error, _ } ->
      defp bootstraped?(module), do: :code.ensure_loaded(module) == { :module, module }
      defp internal?, do: :elixir_compiler.get_opt(:internal)
  end

  defp assert_module_scope(env, fun, arity) do
    case env_module(env) do
      nil -> raise ArgumentError, message: "cannot invoke #{fun}/#{arity} outside module"
      _   -> :ok
    end
  end

  defp assert_no_function_scope(env, fun, arity) do
    case env_function(env) do
      nil -> :ok
      _   -> raise ArgumentError, message: "cannot invoke #{fun}/#{arity} inside function/macro"
    end
  end

  defp env_module(env),   do: :erlang.element(2, env)
  defp env_line(env),     do: :erlang.element(4, env)
  defp env_function(env), do: :erlang.element(5, env)
  defp env_context(env),  do: :erlang.element(6, env)
  defp env_vars(env),     do: :erlang.element(13, env)

  defp expand_compact([{ :compact, false }|t]), do: expand_compact(t)
  defp expand_compact([{ :compact, true }|t]),  do: [:compact|expand_compact(t)]
  defp expand_compact([h|t]),                   do: [h|expand_compact(t)]
  defp expand_compact([]),                      do: []
end
