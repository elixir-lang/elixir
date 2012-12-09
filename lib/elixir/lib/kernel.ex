import Kernel, except: [raise: 1, raise: 2]

defmodule Kernel do
  @moduledoc """
  `Kernel` provides the default macros and functions
  Elixir imports into your environment. Those macros and functions
  can be skipped or cherry-picked via the `import` macro. For
  instance, if you want to tell Elixir to not import the `case`
  macro, you can do:

      import Kernel, except: [case: 2]

  Elixir also has special forms that are always imported and
  cannot be skipped. These are described in `Kernel.SpecialForms`.

  Some of the functions described in this module are simply
  a proxy to its Erlang counterpart. Although they are documented
  here for convenience, you can access their original documentation
  at http://www.erlang.org/doc/man/erlang.html.
  """

  @doc """
  Arithmetic plus. Allowed in guard clauses.

  ## Examples

      1 + 2 #=> 3

  """
  defmacro left + right

  @doc """
  Arithmetic minus. Allowed in guard clauses.

  ## Examples

      1 - 2 #=> -1

  """
  defmacro left - right

  @doc """
  Arithmetic multiplication. Allowed in guard clauses.

  ## Examples

      1 * 2 #=> 2

  """
  defmacro left * right

  @doc """
  Arithmetic division. Differently from other languages,
  the result is always a float. Use div and rem if you want
  a natural division or the remainder. Allowed in guard clauses.

  ## Examples

      1 / 2 #=> 0.5
      2 / 1 #=> 2.0

  """
  defmacro left / right

  @doc """
  Sends a message to the process identified on the left.
  A process can be identified bu its PID or, if it is registered,
  by an atom.

  ## Examples

      process = Process.self
      process <- { :ok, "Sending myself a message" }

  """
  defmacro pid <- msg

  @doc """
  Concatenates two lists. Allowed in guard clauses.

  ## Examples

      [1] ++ [2,3]
      #=> [1,2,3]

      'foo' ++ 'bar'
      #=> 'foobar'

  """
  defmacro left ++ right

  @doc """
  Removes the first occorrence of an item on the left
  for each item on the right. Allowed in guard clauses.

  ## Examples

      [1,2,3] -- [1,2]
      #=> [3]

      [1,2,3,2,1] -- [1,2,2]
      #=> [3,1]

  """
  defmacro left -- right

  @doc """
  Boolean or. Arguments needs to necessarily be booleans.
  Allowed in guard clauses.

  ## Examples

      true or false
      #=> true

  """
  defmacro left or right

  @doc """
  Boolean and. Arguments needs to necessarily be booleans.
  Allowed in guard clauses.

  ## Examples

      true and false
      #=> false

  """
  defmacro left and right

  @doc """
  Boolean xor. Arguments needs to necessarily be booleans.
  Allowed in guard clauses.

  ## Examples

      true xor false
      #=> true

  """
  defmacro left xor right

  @doc """
  Boolean not. Argument needs to necessarily be a boolean.
  Allowed in guard clauses.

  ## Examples

      not false
      #=> true

  """
  defmacro not arg

  @doc """
  It receives any argument and returns true if it is false
  or nil. Returns false otherwise. Not allowed in guard
  clauses.

  ## Examples

      !1        #=> false
      ![1,2,3]  #=> false
      !false    #=> true
      !nil      #=> true

  """
  defmacro !arg

  @doc """
  Return true if left is less than right.
  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 < 2
      #=> true

  """
  defmacro left < right

  @doc """
  Return true if left is more than right.
  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 > 2
      #=> false

  """
  defmacro left > right

  @doc """
  Return true if left is less than or equal to right.
  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 <= 2
      #=> true

  """
  defmacro left <= right

  @doc """
  Return true if left is more than or equal to right.
  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 >= 2
      #=> false

  """
  defmacro left >= right

  @doc """
  Returns true if the two items are equal.

  This operator considers 1 and 1.0 to be equal. For strict
  comparison, use `===` instead.

  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 == 2
      #=> false

      1 == 1.0
      #=> true

  """
  defmacro left == right

  @doc """
  Returns true if the two items are not equal.

  This operator considers 1 and 1.0 to be equal. For strict
  comparison, use `!==` instead.

  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 != 2
      #=> true
      1 != 1.0
      #=> false

  """
  defmacro left != right

  @doc """
  Returns true if the two items are strictly equal.
  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 === 2
      #=> false

      1 === 1.0
      #=> false

  """
  defmacro left === right

  @doc """
  Returns true if the two items are strictly not equal.
  As Erlang, Elixir can compare any term. Allowed in guard clauses.

  ## Examples

      1 !== 2
      #=> true

      1 !== 1.0
      #=> true

  """
  defmacro left !== right

  @doc """
  When used inside quoting, marks that the variable should not
  be hygienezed. Check `Kernel.SpecialForms.quote/1` for more
  information.
  """
  defmacro var!(var)

  @doc """
  Returns an integer or float which is the arithmetical absolute value of `number`.

  Allowed in guard tests.

  ## Examples

      abs(-3.33) #=> 3.33
      abs(-3)    #=> 3
  """
  @spec abs(number) :: number
  def abs(number) do
    :erlang.abs(number)
  end

  @doc """
  Invokes the given `fun` with the array of arguments `args`.

  ## Examples

      apply fn x -> x * 2 end, [2]
      #=> 4

  """
  @spec apply((... -> any), list) :: term
  def apply(fun, args) do
    :erlang.apply(fun, args)
  end

  @doc """
  Invokes the given `fun` from `module` with the array of arguments `args`.

  ## Examples

      apply List, reverse, [[1,2,3]]
      #=> [3,2,1]

  """
  @spec apply(atom, atom, list) :: term
  def apply(module, fun, args) do
    :erlang.apply(module, fun, args)
  end

  @doc """
  Returns a binary which corresponds to the text representation of `atom`.
  If `encoding` is latin1, there will be one byte for each character in the text
  representation. If `encoding` is utf8 or unicode, the characters will be encoded
  using UTF-8 (meaning that characters from 16#80 up to 0xFF will be encoded in
  two bytes).

  ## Examples

      atom_to_binary(:elixir, :utf8) #=> "elixir"

  """
  @spec atom_to_binary(atom, :utf8 | :unicode | :latin1) :: binary
  def atom_to_binary(atom, encoding) do
    :erlang.atom_to_binary(atom, encoding)
  end

  @doc """
  Returns a string which corresponds to the text representation of `atom`.

  ## Examples

      atom_to_list(:elixir) #=> 'elixir'

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

      binary_part "foo", 1, 2 #=> "oo"

  A negative length can be used to extract bytes at the end of a binary:

      binary_part "foo", 3, -1 #=> 1

  """
  @spec binary_part(binary, pos_integer, integer) :: binary
  def binary_part(binary, start, length) do
    :erlang.binary_part(binary, start, length)
  end

  @doc """
  Returns the atom whose text representation is `binary`. If `encoding` is latin1,
  no translation of bytes in the binary is done. If `encoding` is utf8 or unicode,
  the binary must contain valid UTF-8 sequences; furthermore, only Unicode
  characters up to 0xFF are allowed.

  ## Examples

      binary_to_atom("elixir", :utf8) #=> :elixir

  """
  @spec binary_to_atom(binary, :utf8 | :unicode | :latin1) :: atom
  def binary_to_atom(binary, encoding) do
    :erlang.binary_to_atom(binary, encoding)
  end

  @doc """
  Works like `binary_to_atom/2`, but the atom must already exist.
  """
  @spec binary_to_existing_atom(binary, :utf8 | :unicode | :latin1) :: atom
  def binary_to_existing_atom(binary, encoding) do
    :erlang.binary_to_existing_atom(binary, encoding)
  end

  @doc """
  Returns a list of integers which correspond to the bytes of `binary`.
  """
  @spec binary_to_list(binary) :: list
  def binary_to_list(binary) do
    :erlang.binary_to_list(binary)
  end

  @doc """
  As binary_to_list/1, but returns a list of integers corresponding to the bytes
  from position `start` to position `stop` in `binary`. Positions in the binary
  are numbered starting from 1.
  """
  @spec binary_to_list(binary, pos_integer, pos_integer) :: list
  def binary_to_list(binary, start, stop) do
    :erlang.binary_to_list(binary, start, stop)
  end

  @doc """
  Returns an Erlang term which is the result of decoding the binary
  object `binary`, which must be encoded according to the Erlang external
  term format.

  ## Examples

      binary_to_term(term_to_binary("foo")) #=> "foo"

  """
  @spec binary_to_term(binary) :: term
  def binary_to_term(binary) do
    :erlang.binary_to_term(binary)
  end

  @doc """
  As `binary_to_term/1`, but accepts a safe option useful when receiving
  binaries from an untrusted source.

  When enabled, it prevents decoding data that may be used to attack the
  Erlang system. In the event of receiving unsafe data, decoding fails
  with a badarg error.

  Currently, this prevents creation of new atoms directly, creation of
  new atoms indirectly (as they are embedded in certain structures like pids,
  refs, funs, etc), and creation of new external function references. None
  of those resources are currently garbage collected, so unchecked creation
  of them can exhaust available memory.

  ## Examples

      binary_to_term(term_to_binary("foo"), [:safe])

  """
  @spec binary_to_term(binary, [] | [:safe]) :: term
  def binary_to_term(binary, options) do
    :erlang.binary_to_term(binary, options)
  end

  @doc """
  Returns an integer which is the size in bits of `bitstring`.

  Allowed in guard tests.

  ## Examples

      bit_size(<<433|16,3|3>>) #=> 19
      bit_size(<<1,2,3>>) #=> 24

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
  (That is, if the number of bits in Bitstring is not divisible by 8, the resulting
  number of bytes will be rounded up.)

  Allowed in guard tests.

  ## Examples

      byte_size(<<433|16,3|3>>) #=> 3
      byte_size(<<1,2,3>>) #=> 3

  """
  @spec byte_size(bitstring) :: non_neg_integer
  def byte_size(bitstring) do
    :erlang.byte_size(bitstring)
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
  Converts the given number to a float. Allowed in guard clauses.
  """
  @spec float(number) :: float
  def float(number) do
    :erlang.float(number)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the given float.

  ## Examples

      float_to_list(7.0)
      #=> '7.00000000000000000000e+00'

  """
  @spec float_to_list(number) :: char_list
  def float_to_list(number) do
    :erlang.float_to_list(number)
  end

  @doc """
  Returns the head of a list, raises badarg if the list is empty.
  """
  @spec hd(list) :: term
  def hd(list) do
    :erlang.hd(list)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the given integer.

  ## Examples

      integer_to_list(7)
      #=> '7'

  """
  @spec integer_to_list(integer) :: char_list
  def integer_to_list(number) do
    :erlang.integer_to_list(number)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the
  given integer in the given case.

  ## Examples

      integer_to_list(1023, 16).
      #=> "3FF"

  """
  @spec integer_to_list(integer, pos_integer) :: char_list
  def integer_to_list(number, base) do
    :erlang.integer_to_list(number, base)
  end

  @doc """
  Returns the size of an iolist.

  ## Examples

      iolist_size([1,2|<<3,4>>])
      #=> 4

  """
  @spec iolist_size(iolist) :: non_neg_integer
  def iolist_size(item) do
    :erlang.iolist_size(item)
  end

  @doc """
  Returns a binary which is made from the integers and binaries in iolist.

  ## Examples

      bin1 = <<1,2,3>>
      bin2 = <<4,5>>
      bin3 = <<6>>

      iolist_to_binary([bin1,1,[2,3,bin2],4|bin3])
      #=> <<1,2,3,1,2,3,4,5,4,6>>

  """
  @spec iolist_to_binary(iolist) :: binary
  def iolist_to_binary(item) do
    :erlang.iolist_to_binary(item)
  end

  @doc """
  Returns true if the local node is alive; that is,
  if the node can be part of a distributed system.
  """
  @spec is_alive :: boolean
  def is_alive do
    :erlang.is_alive
  end

  @doc """
  Returns true if `term` is an atom; otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_atom(term) :: boolean
  def is_atom(term) do
    :erlang.is_atom(term)
  end

  @doc """
  Returns true if `term` is a binary; otherwise returns false.

  A binary always contains a complete number of bytes.

  Allowed in guard tests.
  """
  @spec is_binary(term) :: boolean
  def is_binary(term) do
    :erlang.is_binary(term)
  end

  @doc """
  Returns true if `term` is a bitstring (including a binary); otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_bitstring(term) :: boolean
  def is_bitstring(term) do
    :erlang.is_bitstring(term)
  end

  @doc """
  Returns true if `term` is either the atom `true` or the atom `false` (i.e. a boolean);
  otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_boolean(term) :: boolean
  def is_boolean(term) do
    :erlang.is_boolean(term)
  end

  @doc """
  Returns true if `term` is a floating point number; otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_float(term) :: boolean
  def is_float(term) do
    :erlang.is_float(term)
  end

  @doc """
  Returns true if `term` is a function; otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_function(term) :: boolean
  def is_function(term) do
    :erlang.is_function(term)
  end

  @doc """
  Returns true if `term` is a function that can be applied with `arity` number of arguments;
  otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_function(term, non_neg_integer) :: boolean
  def is_function(term, arity) do
    :erlang.is_function(term, arity)
  end

  @doc """
  Returns true if `term` is an integer; otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_integer(term) :: boolean
  def is_integer(term) do
    :erlang.is_integer(term)
  end

  @doc """
  Returns true if `term` is a list with zero or more elements; otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_list(term) :: boolean
  def is_list(term) do
    :erlang.is_list(term)
  end

  @doc """
  Returns true if `term` is either an integer or a floating point number;
  otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_number(term) :: boolean
  def is_number(term) do
    :erlang.is_number(term)
  end

  @doc """
  Returns true if `term` is a pid (process identifier); otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_pid(term) :: boolean
  def is_pid(term) do
    :erlang.is_pid(term)
  end

  @doc """
  Returns true if `term` is a port identifier; otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_port(term) :: boolean
  def is_port(term) do
    :erlang.is_port(term)
  end

  @doc """
  Returns true if `term` is a reference; otherwise returns false.

  Allowed in guard tests.
  """
  @spec is_reference(term) :: boolean
  def is_reference(term) do
    :erlang.is_reference(term)
  end

  @doc """
  Returns true if `term` is a tuple; otherwise returns false.

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

      length([1,2,3,4,5,6,7,8,9]) #=> 9
  """
  @spec length(list) :: non_neg_integer
  def length(list) do
    :erlang.length(list)
  end

  @doc """
  Returns the atom whose text representation is `char_list`.

  ## Examples

      list_to_atom('elixir') #=> :elixir
  """
  @spec list_to_atom(char_list) :: atom
  def list_to_atom(char_list) do
    :erlang.list_to_atom(char_list)
  end

  @doc """
  Returns a binary which is made from the content of `char_list`.

  ## Examples

      list_to_binary('Elixir') #=> "Elixir"
  """
  @spec list_to_binary(iolist) :: binary
  def list_to_binary(char_list) do
    :erlang.list_to_binary(char_list)
  end

  @doc """
  Returns a bitstring which is made from the integers and bitstrings in `bitstring_list`.
  (the last tail in `bitstring_list` is allowed to be a bitstring.)

  ## Examples

      bin1 = <<1,2,3>>
      bin2 = <<4,5>>
      bin3 = <<6,7|4>>

      list_to_bitstring([bin1,1,[2,3,bin2],4|bin3])
      #=> <<1,2,3,1,2,3,4,5,4,6,7|4>>

  """
  @spec list_to_bitstring(maybe_improper_list(char | binary | iolist | bitstring, binary | bitstring | [])) :: bitstring
  def list_to_bitstring(bitstring_list) do
    :erlang.list_to_bitstring(bitstring_list)
  end

  @doc """
  Returns the atom whose text representation is `char_list`, but only if there already
  exists such atom.
  """
  @spec list_to_existing_atom(char_list) :: atom
  def list_to_existing_atom(char_list) do
    :erlang.list_to_existing_atom(char_list)
  end

  @doc """
  Returns the float whose text representation is `char_list`.

  ## Examples

      list_to_float('2.2017764e+0') #=> 2.2017764
  """
  @spec list_to_float(char_list) :: float
  def list_to_float(char_list) do
    :erlang.list_to_float(char_list)
  end

  @doc """
  Returns an integer whose text representation is `char_list`.

  ## Examples

      list_to_integer('123') #=> 123
  """
  @spec list_to_integer(char_list) :: integer
  def list_to_integer(char_list) do
    :erlang.list_to_integer(char_list)
  end

  @doc """
  Returns an integer whose text representation in base `base` is `char_list`.

  ## Examples

      list_to_integer('3FF', 16) #=> 1023
  """
  @spec list_to_integer(char_list, non_neg_integer) :: integer
  def list_to_integer(char_list, base) do
    :erlang.list_to_integer(char_list, base)
  end

  @doc """
  Returns a pid whose text representation is `char_list`.

  ## Warning:

  This function is intended for debugging and for use in the Erlang
  operating system.

  It should not be used in application programs.

  ## Examples
      list_to_pid('<0.41>') #=> <0.4.1>
  """
  @spec list_to_pid(char_list) :: pid
  def list_to_pid(char_list) do
    :erlang.list_to_pid(char_list)
  end

  @doc """
  Returns a tuple which corresponds to `list`. `list` can contain any Erlang terms.

  ## Examples

      list_to_tuple([:share, [:elixir, 163]]). #=> {:share, [:elixir, 163]}
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

      make_ref()
      #=> #Ref<0.0.0.135>

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

      max(1, 2) #=> 2

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

      min(1, 2) #=> 1

  """
  @spec min(term, term) :: term
  def min(first, second) do
    :erlang.min(first, second)
  end

  @doc """
  Returns an atom representing the name of the local node.
  If the node is not alive, nonode@nohost is returned instead.

  Allowed in guard tests.
  """
  @spec node() :: node
  def node do
    :erlang.node
  end

  @doc """
  Returns the node where the given argmuent is located.
  The argument can be a pid, a reference, or a port.
  If the local node is not alive, nonode@nohost is returned.

  Allowed in guard tests.
  """
  @spec node(pid|reference|port) :: node
  def node(arg) do
    :erlang.node(arg)
  end

  @doc """
  Returns a char list which corresponds to the text representation of pid.
  This function is intended for debugging and for use in the Erlang operating
  system. It should not be used in application programs.

  ## Warning:

  This function is intended for debugging and for use in the Erlang
  operating system.

  It should not be used in application programs.
  """
  @spec pid_to_list(pid) :: list
  def pid_to_list(pid) do
    :erlang.pid_to_list(pid)
  end

  @doc """
  Returns an integer by rounding the given number.
  Allowed in guard tests.

  ## Examples

      round(5.5) #=> 6

  """
  @spec round(number) :: integer
  def round(number) do
    :erlang.round(number)
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
  or a binary. If possible, please use tuple_size or binary_size.
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

      current = Process.self
      child   = spawn(fn -> current <- { Process.self, 1 + 2 } end)

      receive
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

      spawn(SomeModule, :function, [1,2,3])

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

      current = Process.self
      child   = spawn_link(fn -> current <- { Process.self, 1 + 2 } end)

      receive
        { ^child, 3 } ->
          IO.puts "Received 3 back"
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

      spawn_link(SomeModule, :function, [1,2,3])

  """
  @spec spawn_link(module, atom, list) :: pid
  def spawn_link(module, fun, args) do
    :erlang.spawn_link(module, fun, args)
  end

  @doc """
  Returns a binary data which is the result of encoding the given term
  according to the Erlang external term format.

  This can be used for a variety of purposes, for example, writing a term
  to a file in an efficient way, or sending an Erlang term to some type
  of communications channel not supported by distributed :
  """
  @spec term_to_binary(term) :: binary
  def term_to_binary(term) do
    :erlang.term_to_binary(term)
  end

  @doc """
  The same as `term_to_binary/1` but also supports two options:

  * compressed: the level of compression to be used from 0 to 9;
  * minor_version: used to control the details of encoding. Can be 0 or 1,
    please read http://www.erlang.org/doc/man/erlang.html#term_to_binary-2
    for more details

  """
  @spec term_to_binary(term, list({:compressed, 0..9}|{:minor_version, 0}|{:minor_version, 1})) :: binary
  def term_to_binary(term, opts) do
    :erlang.term_to_binary(term, opts)
  end

  @doc """
  A non-local return from a function. Check try/2 for more information.
  """
  @spec throw(term) :: no_return
  def throw(term) do
    :erlang.throw(term)
  end

  @doc """
  Returns the tail of a list. Raises ArgumentError if the list is empty.
  """
  @spec tl(maybe_improper_list) :: maybe_improper_list
  def tl(list) do
    :erlang.tl(list)
  end

  @doc """
  Returns an integer by the truncating the given number.
  Allowed in guard clauses.

  ## Examples

      trunc(5.5) #=> 5

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
  Defines a module given by name with the given contents.

  It returns the module name, the module binary and the
  block contents result.

  ## Examples

      defmodule Foo do
        def bar, do: :baz
      end

      Foo.bar #=> :baz

  ## Nesting

  Nesting a module inside the other affects its name:

      defmodule Foo do
        defmodule Bar do
        end
      end

  In the example above, two modules `Foo` and `Foo.Bar`. The
  second can be accessed as `Bar` inside `Foo` in the same
  lexical scope. If the module Bar is moved away to another
  file, it needs to be referenced via the full name or an
  alias need to be set with the help of `Kernel.SpecialForms.alias/2`.

  ## Dynamic names

  Elixir module names can be dynamically generated. This is very
  useful for macros. For instance, one could write:

      defmodule binary_to_atom("Foo\#{1}", :utf8) do
        # contents ...
      end

  Elixir will accept any module name as long as the expression
  returns an atom.
  """
  defmacro defmodule(name, do: contents)

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
  two arguments and sum them.

  ## Dynamic generation with atoms

  Elixir follows the same rule as Erlang when it comes to
  function invocations. Calling a function is the same thing
  as "invoking at atom". That said, we could invoke a function
  named sum in these two equivalent ways:

      sum(1, 2)
      :sum.(1, 2)

  We can also use the atom format to define functions:

      defmodule Foo do
        def :sum.(a, b) do
          a + b
        end
      end

  In general, a developer never needs to use the format above
  except when he wants to dynamically define functions with macros.
  In such scenarios, the name needs to be given dynamically via
  the unquoting mechanism.

  Imagine a macro that receives keywords and defines a function
  for each entry in the keyword, using the key as function name
  and the value as the value returned by the function:

      defmacro defkv(keywords) do
        Enum.map keywords, fn {k,v} ->
          quote do
            def unquote(k).() do
              unquote(v)
            end
          end
        end
      end

  This macro could be invoked as:

      defkv one: 1, two: 2

  Notice in the example above, we define the function as `def unquote(k).()`
  because each entry `k` is a an atom and invoking `def unquote(k)()`
  would be invalid Elixir syntax.
  """
  defmacro def(name, do: contents)

  @doc """
  This macro allows a function to be defined more explicitly
  by accepting the name, args and guards as different entries.

  Differently from `def/2`, the macro arguments are evaluated
  and therefore requires quoting.

  The `name` must be an atom, the `arguments` a list where each
  element represents another argument and `guards` a list of
  clauses, where each clause is disjunct.

  ## Examples

  The most common mistake when using this macro is to pass the
  arguments without quoting:

      def :some_function, [first_arg, second_arg], is_list(first_arg) do
        # ...
      end

  However, the example above will fail because it will attempt to
  evaluate `[first_arg, second_arg]` and fail because the variable
  `first_arg` is not defined. Therefore, we need to use quote:

      name   = :some_function
      args   = quote(do: [first_arg, second_arg])
      guards = quote(do: is_list(first_arg))
      exprs  = quote(do: ...)

      def name, args, guards, do: exprs

  """
  defmacro def(name, args, guards, do: contents)

  @doc """
  Defines a function that is private. Private functions
  can only be accessible from the same module it is defined.

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
  defmacro defp(name, do: contents)

  @doc """
  The same as `def/4` but generates a private function.
  """
  defmacro defp(name, args, guards, do: contents)

  @doc %B"""
  Defines a record.

  A record is a tagged tuple which contains one or more elements
  and the first element is a module. This macro defines a module
  that generates accessors to manipulate the record at both
  compilation and runtime.

  ## Examples

      defrecord FileInfo, atime: nil, accesses: 0

  The line above will define a module named `FileInfo` which
  contains a function named `new` that returns a new record
  and other functions to read and set the values in the
  record:

      file_info = FileInfo.new(atime: now())
      file_info.atime         #=> Returns the value of atime
      file_info.atime(now())  #=> Updates the value of atime

  A record is simply a tuple where the first element is the record
  module name. We can get the record raw representation as follow:

      inspect FileInfo.new, raw: true
      #=> { FileInfo, nil, nil }

  Besides defining readers and writers for each attribute, Elixir also
  defines an `update_#{attribute}` function to update the value. Such
  functions expect a function as argument that receives the current
  value and must return the new one. For example, every time the file
  is accessed, the accesses counter can be incremented with:

      file_info.update_accesses(fn(old) -> old + 1 end)

  Which can be also written as:

      file_info.update_accesses(&1 + 1)

  ## Access syntax

  Records in Elixir can be expanded at compilation time to provide
  pattern matching and faster operations. For example, the clause
  below will only match if a `FileInfo` is given and the number of
  accesses is zero:

      def enforce_no_access(FileInfo[accesses: 0]), do: :ok

  The clause above will expand to:

      def enforce_no_access({ FileInfo, _, 0 }), do: :ok

  The downside of using such syntax is that, every time the record
  changes, your code now needs to be recompiled (which is usually
  not a concern since Elixir build tools by default recompiles the
  whole project whenever there is a change).

  Finally, keep in mind that Elixir triggers some optimizations whenever
  the access syntax is used. For example:

      def no_access?(FileInfo[] = file_info) do
        file_info.accesses == 0
      end

  Is translated to:

      def no_access?({ FileInfo, _, _ } = file_info) do
        elem(file_info, 1) == 0
      end

  Which provides faster get and set times for record operations.

  ## Documentation

  By default records are not documented and have `@moduledoc` set to false.

  ## Types

  Every record defines a type named `t` that can be accessed in typespecs.
  For example, assuming the `Config` record defined above, it could be used
  in typespecs as follow:

      @spec handle_config(Config.t) :: boolean()

  Inside the record definition, a developer can define his own types too:

      defrecord Config, counter: 0, failures: [] do
        @type kind :: term
        record_type counter: integer, failures: [kind]
      end

  When defining a type, all the fields not mentioned in the type are
  assumed to have type `term`.
  """
  defmacro defrecord(name, fields, opts // [], do_block // []) do
    Record.defrecord(name, fields, Keyword.merge(opts, do_block))
  end

  @doc """
  Defines a record with a set of private macros to manipulate it.

  A record is a tagged tuple which contains one or more elements
  and the first element is a module. This macro defines a set of
  macros private to the current module to manipulate the record
  exclusively at compilation time.

  `defrecordp` must be used instead of `defrecord` when there is
  no interest in exposing the record as a whole. In many ways,
  it is similar to Erlang records, since it is only available at
  compilation time.

  ## Examples

      defmodule User do
        defrecordp :user, [name: "José", age: "25"]
      end

  In the example above, a set of macros named `user` but with different
  arities will be defined to manipulate the underlying record:

      # To create records
      user()        #=> { User, "José", 25 }
      user(age: 26) #=> { User, "José", 26 }

      # To get a field from the record
      user(record, :name) #=> "José"

      # To get many fields from the record
      user(record, [:name, :age]) #=> ["José", 25]

      # To update the record
      user(record, age: 26) #=> { User, "José", 26 }

      # To convert the record to keywords
      user(record) #=> [name: "José", age: 25]

      # To match against the record
      user(name: name) = record
      name #=> "José"

  """
  defmacro defrecordp(name, fields) when is_atom(name) do
    Record.defrecordp(name, fields)
  end

  @doc """
  Defines an exception.

  Exceptions are simply records and therefore `defexception/4` has
  the same API and similar behavior to `defrecord/4` with two notable
  differences:

  1) Differently from records, exceptions are documented by default;

  2) Exceptions **must** implement `message/1` as API that return a
     binary as result;

  """
  defmacro defexception(name, fields, opts // [], do_block // []) do
    opts = Keyword.merge(opts, do_block)
    opts = Keyword.put(opts, :do, quote do
      @moduledoc nil
      record_type message: binary
      def exception(args), do: new(args)
      def exception(args, self), do: update(args, self)
      unquote(Keyword.get opts, :do)
    end)

    fields = [{ :__exception__, :__exception__ }|fields]
    record = Record.defrecord(name, fields, opts)

    check  = quote do
      Exception.check! Module.concat(__MODULE__, unquote(name))
    end

    [record, check]
  end

  @doc """
  Check if the given structure is an exception.

  ## Examples

      is_exception(Error.new) #=> true
      is_exception(1)         #=> false

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
  Check if the given structure is a record. It is basically
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
          is_tuple(unquote(thing)) and :erlang.element(1, unquote(thing)) == unquote(kind)
        end
      false ->
        quote do
          result = unquote(thing)
          is_tuple(result) and :erlang.element(1, result) == unquote(kind)
        end
    end
  end

  @doc """
  Check if the given argument is a regex.
  """
  defmacro is_regex(thing) do
    quote do
      is_record(unquote(thing), Regex)
    end
  end

  @doc """
  Check if the given argument is a range.
  """
  defmacro is_range(thing) do
    quote do
      is_record(unquote(thing), Range)
    end
  end

  @doc """
  Defines the current module as a protocol and specifies the API
  that should be implemented.

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

      # Numbers are never blank
      defimpl Blank, for: Number do
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
  The types available are:

  * Record
  * Tuple
  * Atom
  * List
  * BitString
  * Number
  * Function
  * PID
  * Port
  * Reference
  * Any

  ## Selecting implementations

  Implementing the protocol for all default types can be cumbersome.
  Even more, if you consider that Number, Function, PID, Port and
  Reference are never going to be blank, it would be easier if we
  could simply provide a default implementation.

  This can be achieved in Elixir as follows:

      defprotocol Blank do
        @only [Atom, Tuple, List, BitString, Any]
        def blank?(data)
      end

  If the protocol is invoked with a data type that is not an Atom,
  nor Tuple, nor List, nor BitString, Elixir will now dispatch to
  Any. That said, the default behavior could be implemented as:

      defimpl Blank, for: Any do
        def blank?(_), do: false
      end

  Now, all data types that we have not specified will be
  automatically considered non blank.

  ## Protocols + Records

  The real benefit of protocols comes when mixed with records. For instance,
  imagine we have a module called `RedBlack` that provides an API to create
  and manipulate Red-Black trees. This module represents such trees via a
  record named `RedBlack.Tree` and we want this tree to be considered blank
  in case it has no items. To achieve this, the developer just needs to
  implement the protocol for `RedBlack.Tree`:

      defimpl Blank, for: RedBlack.Tree do
        def blank?(tree), do: RedBlack.empty?(tree)
      end

  In the example above, we have implemented `blank?` for `RedBlack.Tree`
  that simply delegates to `RedBlack.empty?` passing the tree as argument.
  This implementation doesn't need to be defined inside the `RedBlack`
  tree or inside the record, but anywhere in the code.

  Finally, since records are simply tuples, one can add a default protocol
  implementation to any record by defining a default implementation for tuples.

  ## Types

  As in records, defining a protocol automatically defines a type named `t`,
  which can be used as:

      @spec present?(Blank.t) :: boolean
      def present?(blank) do
        not Blank.blank?(blank)
      end

  The `@spec` above expresses that all types allowed to implement the
  given protocol are valid argument types for the given function.
  """
  defmacro defprotocol(name, [do: block]) do
    Protocol.defprotocol(name, [do: block])
  end

  @doc """
  Defines an implementation for the given protocol. See
  `defprotocol/2` for examples.
  """
  defmacro defimpl(name, opts, do_block // []) do
    Protocol.defimpl(name, Keyword.merge(opts, do_block))
  end

  @doc """
  Makes the given functions in the current module overridable.
  An overridable function is lazily defined, allowing a
  developer to customize it.
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

  For example, in other to write tests using the ExUnit framework,
  a developers should use the `ExUnit.Case` module:

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
        ExUnit.Case.__using__([sync: true])

        def test_always_pass do
          true = true
        end
      end

  """
  defmacro use(module, args // []) do
    expanded = Macro.expand(module, __CALLER__)

    case is_atom(expanded) do
      false ->
        raise ArgumentError,
          message: "invalid arguments for use, expected an atom or alias as argument"
      true ->
        quote do
          require unquote(expanded)
          unquote(expanded).__using__(unquote(args))
        end
    end
  end

  @doc """
  Inspect the given arguments according to the Binary.Inspect protocol.

  ## Options

  The following options are supported:

  * :raw - tuples are not formatted as the inspect protocol, they are
    always shown as tuples, defaults to false;

  * :limit - the limit of items that are shown in tuples, bitstrings and
    lists. Do not apply to strings;

  ## Examples

      inspect(:foo)
      #=> ":foo"

  """
  defmacro inspect(arg, opts // []) do
    quote do: Binary.Inspect.inspect(unquote(arg), unquote(opts))
  end

  @doc """
  Convert the argument to a string according to the Binary.Chars protocol.
  This is the function invoked when there is string interpolation.

  ## Examples

      to_binary(:foo)
      #=> "foo"

  """
  defmacro to_binary(arg) do
    quote do: Binary.Chars.to_binary(unquote(arg))
  end

  @doc """
  Convert the argument to a list according to the List.Chars protocol.

  ## Examples

      to_char_list(:foo)
      #=> 'foo'

  """
  defmacro to_char_list(arg) do
    quote do: List.Chars.to_char_list(unquote(arg))
  end

  @doc """
  Define elem to get Tuple element according to Elixir conventions
  (i.e. it expects the tuple as first argument, zero-index based).

  It is implemented as a macro so it can be used in guards.

  ## Example

      tuple = { :foo, :bar, 3 }
      elem(tuple, 1) #=> :bar

  """
  defmacro elem(tuple, index) when is_integer(index) do
    quote do: :erlang.element(unquote(index + 1), unquote(tuple))
  end

  defmacro elem(tuple, index) do
    quote do: :erlang.element(unquote(index) + 1, unquote(tuple))
  end

  @doc """
  Define setelem to set Tuple element according to Elixir conventions
  (i.e. it expects the tuple as first argument, zero-index based).

  It is implemented as a macro so it can be used in guards.

  ## Example

      tuple = { :foo, :bar, 3 }
      setelem(tuple, 0, :baz) #=> { :baz, :bar, 3 }

  """
  defmacro setelem(tuple, index, value) when is_integer(index) do
    quote do: :erlang.setelement(unquote(index + 1), unquote(tuple), unquote(value))
  end

  defmacro setelem(tuple, index, value) do
    quote do: :erlang.setelement(unquote(index) + 1, unquote(tuple), unquote(value))
  end

  @doc """
  Provides an integer division macro according to Erlang semantics.
  Raises an error if one of the arguments is not an integer.
  Can be used in guard tests.

  ## Examples

      div 5, 2 #=> 2

  """
  defmacro div(left, right) do
    quote do: __op__ :div, unquote(left), unquote(right)
  end

  @doc """
  Provides an integer remainder macro according to Erlang semantics.
  Raises an error if one of the arguments is not an integer.
  Can be used in guard tests.

  ## Examples

      rem 5, 2 #=> 1

  """
  defmacro rem(left, right) do
    quote do: __op__ :rem, unquote(left), unquote(right)
  end

  @doc """
  Checks if the given argument is nil or not.
  Allowed in guard clauses.

  ## Examples

      nil? 1    #=> false
      nil? nil  #=> true

  """
  defmacro nil?(x) do
    quote do: unquote(x) == nil
  end

  @doc """
  A convenient macro that checks if the right side matches
  the left side. The left side is allowed to be a match pattern.

  ## Examples

      match?(1, 1) #=> true
      match?(1, 2) #=> false
      match?({1,_}, {1,2}) #=> true

  Match can also be used to filter or find a value in an enumerable:

      list = [{:a,1},{:b,2},{:a,3}]
      Enum.filter list, match?({:a, _}, _)

  Guard clauses can also be given to the match:

      list = [{:a,1},{:b,2},{:a,3}]
      Enum.filter list, match?({:a, x } when x < 2, &1)

  """
  defmacro match?({ :_, _, atom }, _right) when is_atom(atom) do
    # Special case underscore since it always matches.
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
  Returns an anonymous function based on the given arguments.

  ## Examples

      sum = function do
        (x, y) -> x + y
      end

      sum.(1, 2) #=> 3

  Notice that a function needs to be invoked using the dot between
  the function and the arguments.

  Multiple clauses can be specified as in `case`, `receive` and
  similar macros:

      sum = function do
        x, y when y > 0 -> x + y
        x, y -> x - y
      end

      sum.(1, 2) #=> 3

  ## Shortcut syntax

  In order to reduce verbosity, functions in Elixir can be written
  using a shortcut syntax via `fn`:

      Enum.map [1,2,3], fn x ->
        x * 2
      end

  Not only the example is shorter, it solves ambiguity issues. Since
  `do/end` always matches the furthest call, if we used the `function`
  macro as below:

      Enum.map [1,2,3], function do
        x -> x * 2
      end

  It would be parsed as:

      Enum.map([1,2,3], function) do
        x -> x * 2
      end

  The stab shortcut syntax has the proper precedence:

      Enum.map [1,2,3], fn
        x -> x * 2
      end

  Which is handled as:

      Enum.map([1,2,3], fn
        x -> x * 2
      end)

  ## Function retrieval

  The `function` macro can also be used to retrieve local or remote
  functions:

      f = function(:is_atom, 2)
      f.(:foo) #=> true

      f = function(List, :flatten, 1)
      f.([1,[2],3]) #=> [1,2,3]

  """
  defmacro function(args)

  @doc """
  Matches the given condition against the match clauses.

  ## Examples

      case thing do
        { :selector, i, value } when is_integer(i) ->
          value
        value -> value
      end

  In the example above, we compare `thing` with each given
  match clause and execute the first one that matches. If no
  clause matches, an error is raised.

  Since Elixir variables can be assigned more than once, variables
  in a match clause will always be assigned instead of matching with
  its previous values. For example:

      i = 1
      case 10 do
        i -> i * 2
      end

  The example above will return 20, because `i` is assigned to 10
  and then multiplied by 2. If you desire to match the value of `i`
  against the given condition, you need to use the `^` operator:

      i = 1
      case 10 do
        ^i -> i * 2
      end

  The example above will actually fail because 10 does not match 1.

  Finally, `case` accepts an `else:` branch as a fallback if none
  of the clauses match:

      case thing do
        { :selector, i, value } when is_integer(i) ->
          value
        _ ->
          thing
      end

  """
  defmacro case(condition, blocks)

  @doc """
  Execute the given expressions and catch any error, exit
  or throw that may have happened.

  ## Examples

      try do
        do_something_that_may_fail(some_arg)
      rescue
        ArgumentError ->
          IO.puts "Invalid argument given"
      catch
        value ->
          IO.puts "caught \#{value}"
      after
        IO.puts "This is printed regardless if it failed or succeed"
      end

  The rescue clause is used to handle exceptions, while the catch
  clause can be used to catch thrown values. Both catch and rescue
  clauses work based on pattern matching.

  Note that calls inside `try` are not tail recursive since the VM
  needs to keep the stacktrace in case an exception happens.

  ## Rescue clauses

  Besides relying on pattern matching, rescue clauses provides some
  conveniences around exceptions that allows one to rescue an
  exception by its name. All the following formats are valid rescue
  expressions:

      try do
        UndefinedModule.undefined_function
      rescue
        UndefinedFunctionError -> nil
      end

      try do
        UndefinedModule.undefined_function
      rescue
        [UndefinedFunctionError] -> nil
      end

      # rescue and assign to x
      try do
        UndefinedModule.undefined_function
      rescue
        x in [UndefinedFunctionError] -> nil
      end

      # rescue all and assign to x
      try do
        UndefinedModule.undefined_function
      rescue
        x -> nil
      end

  ## Catching exits and Erlang errors

  The catch clause works exactly the same as in erlang. Therefore,
  one can also handle exits/errors coming from Erlang as below:

      try do
        exit(1)
      catch
        :exit, 1 -> IO.puts "Exited with 1"
      end

      try do
        error(:sample)
      catch
        :error, :sample ->
          IO.puts "sample error"
      end

  Although the second form should be avoided in favor of raise/rescue
  control mechanisms.

  ## Variable visibility

  Since an expression inside `try` may not have been evaluted
  due to an exception, any variable created inside `try` cannot
  be accessed externaly. For instance:

      try do
        x = 1
        do_something_that_may_fail(same_arg)
        :ok
      catch
        _, _ -> :failed
      end

      x #=> Cannot access `x`

  In the example above, `x` cannot be accessed since it was defined
  inside the `try` clause. A common practice to address this issue
  is to return the variables defined inside `try`:

      x =
        try do
          x = 1
          do_something_that_may_fail(same_arg)
          x
        catch
          _, _ -> :failed
        end

  """
  defmacro try(args)

  @doc """
  The current process will hang until it receives a message
  from other processes that matches the given clauses.

  ## Examples

      receive do
        { :selector, i, value } when is_integer(i) ->
          value
        value when is_atom(value) ->
          value
        _ ->
          IO.puts :stderr, "Unexpected message received"
      end

  The match clauses above follows the same rules as `case/2`.

  An optional after clause can be given in case the message was not
  received after the specified period of time:

      receive do
        { :selector, i, value } when is_integer(i) ->
          value
        value when is_atom(value) ->
          value
        _ ->
          IO.puts :stderr, "Unexpected message received"
      after
        5000 ->
          IO.puts :stderr, "No message in 5 seconds"
      end

  The `after` clause can be specified even if there are no match clauses.
  There are two special cases for the timout value given to after

  * `:infinity` - The process should wait indefinitely for a matching
  message, this is the same as not using a timeout.

  * 0 - if there is no matching message in the mailbox, the timeout
  will occur immediately.
  """
  defmacro receive(args)

  @doc """
  This macro is a shortcut to read and add attributes to the module
  being compiled. Elixir module attributes are similar to Erlang's with
  some differences. The canonical example for attributes is annotating
  that a module implements the OTP behavior called `gen_server`:

      defmodule MyServer do
        @behavior :gen_server
        # ... callbacks ...
      end

  Elixir supports by default all Erlang module attributes but any developer
  can also add custom attributes:

      defmodule MyServer do
        @my_data 13
        IO.inspect @my_data #=> 13
      end

  Differently from Erlang, such attributes are not stored in the module by
  default since it is common in Elixir to use such attributes to store
  temporary data. A developer can configure an attribute to behave closer
  to Erlang by calling `Module.register_attribute/2`.

  Finally notice that attributes can also be read inside functions:

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

  @doc """
  Provides an `if` macro. This macro expects the first argument to
  be a condition and the rest are keyword arguments.

  ## One-liner examples

      if(foo, do: bar)

  In the example above, bar will be returned if foo evalutes to
  true (i.e. it is not false nor nil). Otherwise, nil will be returned.

  An else option can be given to specify the opposite:

      if(foo, do: bar, else: bar)

  ## Blocks examples

  Elixir also allows you to pass a block to the if macro. The first
  example above would be translated to:

      if foo do
        bar
      end

  Notice that do/end becomes delimiters. The second example would
  then translate do:

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
        _ in [false, nil] -> unquote(else_clause)
        _                 -> unquote(do_clause)
      end
    end
  end

  @doc """
  Execute the first clause where the condition returns true,
  raises an error otherwise.

  ## Examples

      cond do
        1 + 1 == 2 ->
          "This will never match"
        2 * 2 != 4 ->
          "Nor this"
        true ->
          "This will"
      end

  """
  defmacro cond([do: { :->, _, pairs }]) do
    [{ [condition], clause }|t] = :lists.reverse pairs

    new_acc =
      case condition do
        { :_, _, atom } when is_atom(atom) ->
          raise ArgumentError, message: <<"unbound variable _ inside cond. ",
            "If you want the last clause to match, you probably meant to use true ->">>
        x when is_atom(x) and not x in [false, nil] ->
          clause
        _ ->
          quote do
            case !unquote(condition) do
              false -> unquote(clause)
            end
          end
      end

    build_cond_clauses(t, new_acc)
  end

  @doc """
  Provides a unless macro that executes the expression
  unless a value evalutes to true. Check `if` for examples
  and documentation.
  """
  defmacro unless(clause, options) do
    do_clause   = Keyword.get(options, :do, nil)
    else_clause = Keyword.get(options, :else, nil)
    quote do
      if(unquote(clause), do: unquote(else_clause), else: unquote(do_clause))
    end
  end

  @doc """
  Allows you to destructure two lists, assigning each
  term in the right to the left. Differently from pattern
  matching via `=`, if the sizes of the left and right
  lists don't match,, structuring simply stops instead
  of raising an error.

  ## Examples

      destructure [x,y,z], [1,2,3,4,5]
      x #=> 1
      y #=> 2
      z #=> 3

  Notice in the example above, even though the right
  size has more entries than the left, structuring works
  fine. If the right size is smaller, the remaining items
  are simply assigned to nil:

      destructure [x,y,z], [1]
      x #=> 1
      y #=> nil
      z #=> nil

  The left side supports any expression you would use
  on the left side of a match:

      x = 1
      destructure [^x,y,z], [1,2,3]

  The example above will only work if x matches
  the first value from the right side. Otherwise,
  it will raise a CaseClauseError.
  """
  defmacro destructure(left, right) when is_list(left) do
    List.foldl left, right, fn item, acc ->
      quote do
        case unquote(acc) do
          [unquote(item)|t] ->
            t
          other when other == [] or other == nil ->
            unquote(item) = nil
        end
      end
    end
  end

  @doc """
  Returns a integer whose text representation is `some_binary`.

  ## Examples

      binary_to_integer "123" #=> 123

  """
  defmacro binary_to_integer(some_binary) do
    quote do
      list_to_integer(binary_to_list(unquote(some_binary)))
    end
  end

  @doc """
  Returns an integer whose text representation in base `base`
  is `some_binary`.

  ## Examples

      binary_to_integer("3FF", 16) #=> 1023

  """
  defmacro binary_to_integer(some_binary, base) do
    quote do
      list_to_integer(binary_to_list(unquote(some_binary)), unquote(base))
    end
  end

  @doc """
  Returns a float whose text representation is `some_binary`.

  ## Examples

      binary_to_float "2.2017764e+0" #=> 2.2017764

  """
  defmacro binary_to_float(some_binary) do
    quote do
      list_to_float(binary_to_list(unquote(some_binary)))
    end
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_integer`.

  ## Examples

      integer_to_binary 123 #=> "123"

  """
  defmacro integer_to_binary(some_integer) do
    quote do
      list_to_binary(integer_to_list(unquote(some_integer)))
    end
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_integer` in base `base`.

  ## Examples

      integer_to_binary 77 #=> "77"

  """
  defmacro integer_to_binary(some_integer, base) do
    quote do
      list_to_binary(integer_to_list(unquote(some_integer), unquote(base)))
    end
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_float`.

  ## Examples

      float_to_binary 7.0 #=> "7.00000000000000000000e+00"

  """
  defmacro float_to_binary(some_float) do
    quote do
      list_to_binary(float_to_list(unquote(some_float)))
    end
  end

  @doc """
  Returns the atom whose text representation is
  `some_binary` in UTF8 encoding.

  ## Examples

      binary_to_atom "my_atom" #=> :my_atom

  """
  defmacro binary_to_atom(some_binary) do
    quote do
      binary_to_atom(unquote(some_binary), :utf8)
    end
  end

  @doc """
  Works like `binary_to_atom` but the atom must exist.

  ## Examples

      :my_atom                          #=> :my_atom
      binary_to_existing_atom "my_atom" #=> :my_atom

  """
  defmacro binary_to_existing_atom(some_binary) do
    quote do
      binary_to_existing_atom(unquote(some_binary), :utf8)
    end
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_atom` in UTF8 encoding.

  ## Examples

      atom_to_binary :my_atom #=> "my_atom"

  """
  defmacro atom_to_binary(some_atom) do
    quote do
      atom_to_binary(unquote(some_atom), :utf8)
    end
  end

  @doc """
  Concatenates two binaries.

  ## Examples

      "foo" <> "bar" #=> "foobar"

  The `<>` operator can also be used in guard clauses as
  long as the first part is a literal binary:

      "foo" <> x = "foobar"
      x #=> "bar"

  """
  defmacro left <> right do
    concats = extract_concatenations({ :<>, 0, [left, right] })
    quote do: << unquote_splicing(concats) >>
  end

  @doc """
  Returns a range with the specified start and end.
  Includes both ends.

  ## Examples

      0 in 1..3 #=> false
      1 in 1..3 #=> true
      2 in 1..3 #=> true
      3 in 1..3 #=> true

  """
  defmacro first .. last do
    { :{}, 0, [Elixir.Range, first, last] }
  end

  @doc """
  Provides a short-circuit operator that executes the second
  expression only if the first one evalutes to true (i.e. it is
  not nil nor false). Returns the first expression otherwise.

  ## Examples

      true && true         #=> true
      nil && true          #=> nil
      true && 1            #=> 1
      false && error(:bad) #=> false

  Notice that, differently from Erlang `and` operator,
  this operator accepts any expression as arguments,
  not only booleans, however it is not allowed in guards.
  """
  defmacro left && right do
    quote do
      case unquote(left) do
        andand in [false, nil] ->
          andand
        _ ->
          unquote(right)
      end
    end
  end

  @doc """
  Provides a short-circuit operator that executes the second
  expression only if the first one does not evalute to true (i.e. it
  is not nil nor false). Returns the first expression otherwise.

  ## Examples

      false || false       #=> false
      nil || true          #=> true
      false || 1           #=> 1
      true || error(:bad)  #=> true

  Notice that, differently from Erlang `or` operator,
  this operator accepts any expression as arguments,
  not only booleans, however it is not allowed in guards.
  """
  defmacro left || right do
    quote do
      case unquote(left) do
        oror in [false, nil] ->
          unquote(right)
        oror ->
          oror
      end
    end
  end

  @doc """
  Returns true if the element on the left is equal (==) to
  any of the items in the right. For now, it only accepts
  a list as the right argument.

  ## Examples

      x = 1
      x in [1,2,3] #=> true

  This macro simply translates the expression above to:

      x == 1 or x == 2 or x == 3

  ## Clauses

  Whenever used inside a function or a case clause, you can
  optionally omit the variable declaration, for example:

      case 3 do
        x when x in [1,2] -> x * 2
        _ -> 0
      end

  Could be rewritten as:

      case 3 do
        x in [1,2] -> x * 2
        _ -> 0
      end

  In this case, Elixir will automatically expand it and define
  the variable for us.
  """
  defmacro left in right

  @doc """
  Matches the term on the left against the regular expression
  on the right. It returns nil if not match happened or the
  first match otherwise.

  ## Examples

      "abcd" =~ %r/c(d)/  #=> 2
      "abcd" =~ %r/e/     #=> nil

  """
  defmacro left =~ right do
    quote do
      Regex.index(unquote(right), unquote(left))
    end
  end

  @doc """
  `/>` is called the pipeline operator as it is useful
  to write pipeline style expressions. This operator
  tntroduces the expression on the left as the first
  argument to the expression on the right.

  ## Examples

      [1,[2],3] /> List.flatten /> Enum.map(&1 * 2)
      #=> [2,4,6]

  The expression above is simply translated to:

      Enum.map(List.flatten([1,[2],3]), &1 * 2)

  Please be aware of operator precendence, when using
  this operator. For example, the following expression:

      String.graphemes "Hello" /> Enum.reverse

  Is translated to:

      String.graphemes("Hello" /> Enum.reverse)

  Which will result in an error as Enum.Iterator protocol
  is not defined for binaries. Adding explicit parenthesis
  is recommended:

      String.graphemes("Hello") /> Enum.reverse

  """
  defmacro left /> right do
    pipeline_op(left, right)
  end

  defp pipeline_op(left, { :/>, _, [middle, right] }) do
    pipeline_op(pipeline_op(left, middle), right)
  end

  defp pipeline_op(left, { call, line, atom }) when is_atom(atom) do
    { call, line, [left] }
  end

  defp pipeline_op(left, { call, line, args }) when is_list(args) do
    { call, line, [left|args] }
  end

  defp pipeline_op(left, atom) when is_atom(atom) do
    { { :., 0, [left, atom] }, 0, [] }
  end

  defp pipeline_op(_, other) do
    raise ArgumentError, message: "Unsupported expression in pipeline (:/>) operator: #{inspect other}"
  end

  @doc """
  Raises an error.

  If the argument is a binary, it raises RuntimeError with the message.
  If anything else, becomes a call to raise(argument, []).

  ## Examples

      raise "Given values do not match"

      try do
        1 + :foo
      rescue
        x in [BadargError] ->
          IO.puts "that was expected"
          raise x
      end

  """
  @spec raise(binary | atom | tuple) :: no_return
  def raise(msg) when is_binary(msg) do
    :erlang.error RuntimeError[message: msg]
  end

  def raise(exception) do
    raise(exception, [])
  end

  @doc """
  Raises an error.

  It calls `.exception` on the given argument passing
  the args in order to retrieve the appropriate exception
  structure.

  Any module defined via `defexception` automatically
  defines `exception(args)` that returns a new instance
  of the record and a `exception(args, current)` that
  updates the current exception.

  Re-raising an exception will retrieve the previous
  stacktrace so it keps the properties of the original
  exception.

  ## Examples

      raise ArgumentError, message: "Sample"

  """
  @spec raise(tuple | atom, list) :: no_return
  def raise(exception, args) when is_tuple(exception) and tuple_size(exception) > 1 and
      :erlang.element(2, exception) == :__exception__ do
    :erlang.raise(:error, exception.exception(args), :erlang.get_stacktrace)
  end

  def raise(exception, args) do
    :erlang.error exception.exception(args)
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
        :erlang.function_exported(:erlang.element(1, module), function, arity + 1)
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

  ## Atoms

  Whenever invoked on an atom, the access protocol is expanded
  at compilation time rather than on runtime. This feature is used
  by records to allow a developer to match against an specific part
  of a record:

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

  The example above is slightly faster than `State.new(counter: :counter)`
  because the record is expanded at compilation time and not at runtime.
  If a field is not specified on creation, it will have its default value.

  Finally, as in Erlang, Elixir also allows the following syntax:

      new_uri = State[_: 1]

  In this case **all** fields will be set to `1`. Notice that,
  as in Erlang, in case an expression is given, it will be
  evaluated multiple times:

      new_uri = State[_: IO.puts "Hello"]

  In this case, `"Hello"` will be printed twice (one per each field).

  ## Examples

      a = { :a, :b, :c }
      a[1] #=> :a
      access a, 1 #=> :a

  """
  defmacro access(element, args) do
    caller = __CALLER__
    atom   = Macro.expand(element, caller)

    case is_atom(atom) and atom != nil do
      true ->
        fields =
          try do
            module = caller.module

            # We are using the access protocol in the same
            # module that defines it. It works, but we need
            # to read the field values from @__record__.
            case atom do
              ^module -> Module.get_attribute(module, :__record__)
              _ -> atom.__record__(:fields)
            end
          rescue
            UndefinedFunctionError ->
              # We first try to call __access__ and just then check if
              # it is loaded so we allow the ParallelCompiler to solve
              # conflicts.
              case :code.ensure_loaded(atom) do
                { :error, _ } ->
                  raise "expected module #{inspect atom} to be loaded and defined"
                _ ->
                  raise "cannot use module #{inspect atom} in access protocol because it does not export __record__/1"
              end
          end

        Record.access(atom, fields, args, caller)
      false ->
        case caller.in_match? do
          true  -> raise "invalid usage of access protocol in signature"
          false -> quote do: Access.access(unquote(element), unquote(args))
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
    this is a disparity between conventions used in Elixir and :
    Elixir's convention is to pass the "handle" as a first argument,
    while in Erlang the convention is to pass it as the last argument

  ## Examples

      defmodule MyList do
        defdelegate reverse(list), to: :lists
        defdelegate [reverse(list), map(callback, list)], to: :lists
        defdelegate other_reverse(list), to: :lists, as: :reverse
      end

      MyList.reverse([1,2,3])
      #=> [3,2,1]

      MyList.other_reverse([1,2,3])
      #=> [3,2,1]

  """
  defmacro defdelegate(funs, opts) when is_list(funs) do
    do_delegate(funs, opts)
  end

  defmacro defdelegate(other, opts) do
    do_delegate([other], opts)
  end

  defp do_delegate(funs, opts) do
    target = Keyword.get(opts, :to) ||
      raise(ArgumentError, message: "Expected to: to be given as argument")

    append_first = Keyword.get(opts, :append_first, false)

    lc fun inlist funs do
      case Macro.extract_args(fun) do
        { name, args } -> :ok
        :error -> raise ArgumentError, message: "invalid syntax in defdelegate #{Macro.to_binary(fun)}"
      end

      actual_args =
        case append_first and args != [] do
          true  -> tl(args) ++ [hd(args)]
          false -> args
        end

      fun = Keyword.get(opts, :as, name)

      quote do
        def unquote(name).(unquote_splicing(args)) do
          apply unquote(target), unquote(fun), [unquote_splicing(actual_args)]
        end
      end
    end
  end

  @doc """
  Handles the sigil %B. It simples returns a binary
  without escaping characters and without interpolations.

  ## Examples

      %B(foo)      #=> "foo"
      %B(f\#{o}o)  #=> "f\\\#{o}o"

  """
  defmacro __B__(string, []) do
    string
  end

  @doc """
  Handles the sigil %b. It returns a binary as if it was double quoted
  string, unescaping characters and replacing interpolations.

  ## Examples

      %b(foo)       #=> "foo"
      %b(f\#{:o}o)  #=> "foo"

  """
  defmacro __b__({ :<<>>, line, pieces }, []) do
    { :<<>>, line, Macro.unescape_tokens(pieces) }
  end

  @doc """
  Handles the sigil %C. It simply returns a char list
  without escaping characters and without interpolations.

  ## Examples

      %C(foo)      #=> 'foo'
      %C(f\#{o}o)  #=> 'f\\\#{o}o'

  """
  defmacro __C__({ :<<>>, _line, [string] }, []) when is_binary(string) do
    binary_to_list(string)
  end

  @doc """
  Handles the sigil %c. It returns a char list as if it was a single
  quoted string, unescaping characters and replacing interpolations.

  ## Examples

      %c(foo)       #=> 'foo'
      %c(f\#{:o}o)  #=> 'foo'

  """

  # We can skip the runtime conversion if we are
  # creating a binary made solely of series of chars.
  defmacro __c__({ :<<>>, _line, [string] }, []) when is_binary(string) do
    binary_to_list(Macro.unescape_binary(string))
  end

  defmacro __c__({ :<<>>, line, pieces }, []) do
    binary = { :<<>>, line, Macro.unescape_tokens(pieces) }
    quote do: binary_to_list(unquote(binary))
  end

  @doc """
  Handles the sigil %r. It returns a Regex pattern.

  ## Examples

      Regex.match? %r(foo), "foo"  #=> true

  """
  defmacro __r__({ :<<>>, _line, [string] }, options) when is_binary(string) do
    binary = Macro.unescape_binary(string, Regex.unescape_map(&1))
    regex  = Regex.compile!(binary, options)
    Macro.escape(regex)
  end

  defmacro __r__({ :<<>>, line, pieces }, options) do
    binary = { :<<>>, line, Macro.unescape_tokens(pieces, Regex.unescape_map(&1)) }
    quote do: Regex.compile!(unquote(binary), unquote(options))
  end

  @doc """
  Handles the sigil %R. It returns a Regex pattern without escaping
  nor interpreting interpolations.

  ## Examples

      Regex.match? %R(f\#{1,3}o), "f\#o"  #=> true

  """
  defmacro __R__({ :<<>>, _line, [string] }, options) when is_binary(string) do
    regex = Regex.compile!(string, options)
    Macro.escape(regex)
  end

  ## Private functions

  # Extracts concatenations in order to optimize many
  # concatenations into one single clause.
  defp extract_concatenations({ :<>, _, [left, right] }) do
    [wrap_concatenation(left) | extract_concatenations(right)]
  end

  defp extract_concatenations(other) do
    [wrap_concatenation(other)]
  end

  # If it is a binary, we don't need to add the binary
  # tag. This allows us to use <> function signatures.
  defp wrap_concatenation(binary) when is_binary(binary) do
    binary
  end

  defp wrap_concatenation(other) do
    { :::, 0, [other, { :binary, 0, nil }] }
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
  defp build_cond_clauses([{ [condition], clause }|t], acc) do
    new_acc = quote do
      case unquote(condition) do
        _ in [false, nil] -> unquote(acc)
        _                 -> unquote(clause)
      end
    end

    build_cond_clauses(t, new_acc)
  end

  defp build_cond_clauses([], acc), do: acc
end
