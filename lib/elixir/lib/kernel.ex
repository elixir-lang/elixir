# Use elixir_bootstrap module to be able to bootstrap Kernel.
# The bootstrap module provides simpler implementations of the
# functions removed, simple enough to bootstrap.
import Kernel,
  except: [@: 1, defmodule: 2, def: 1, def: 2, defp: 2, defmacro: 1, defmacro: 2, defmacrop: 2]

import :elixir_bootstrap

defmodule Kernel do
  @moduledoc """
  `Kernel` is Elixir's default environment.

  It mainly consists of:

    * basic language primitives, such as arithmetic operators, spawning of processes,
      data type handling, and others
    * macros for control-flow and defining new functionality (modules, functions, and the like)
    * guard checks for augmenting pattern matching

  You can invoke `Kernel` functions and macros anywhere in Elixir code
  without the use of the `Kernel.` prefix since they have all been
  automatically imported. For example, in IEx, you can call:

      iex> is_number(13)
      true

  If you don't want to import a function or macro from `Kernel`, use the `:except`
  option and then list the function/macro by arity:

      import Kernel, except: [if: 2, unless: 2]

  See `import/2` for more information on importing.

  Elixir also has special forms that are always imported and
  cannot be skipped. These are described in `Kernel.SpecialForms`.

  ## The standard library

  `Kernel` provides the basic capabilities the Elixir standard library
  is built on top of. It is recommended to explore the standard library
  for advanced functionality. Here are the main groups of modules in the
  standard library (this list is not a complete reference, see the
  documentation sidebar for all entries).

  ### Built-in types

  The following modules handle Elixir built-in data types:

    * `Atom` - literal constants with a name (`true`, `false`, and `nil` are atoms)
    * `Float` - numbers with floating point precision
    * `Function` - a reference to code chunk, created with the `fn/1` special form
    * `Integer` - whole numbers (not fractions)
    * `List` - collections of a variable number of elements (linked lists)
    * `Map` - collections of key-value pairs
    * `Process` - light-weight threads of execution
    * `Port` - mechanisms to interact with the external world
    * `Tuple` - collections of a fixed number of elements

  There are two data types without an accompanying module:

    * Bitstring - a sequence of bits, created with `<<>>/1`.
      When the number of bits is divisible by 8, they are called binaries and can
      be manipulated with Erlang's `:binary` module
    * Reference - a unique value in the runtime system, created with `make_ref/0`

  ### Data types

  Elixir also provides other data types that are built on top of the types
  listed above. Some of them are:

    * `Date` - `year-month-day` structs in a given calendar
    * `DateTime` - date and time with time zone in a given calendar
    * `Exception` - data raised from errors and unexpected scenarios
    * `MapSet` - unordered collections of unique elements
    * `NaiveDateTime` - date and time without time zone in a given calendar
    * `Keyword` - lists of two-element tuples, often representing optional values
    * `Range` - inclusive ranges between two integers
    * `Regex` - regular expressions
    * `String` - UTF-8 encoded binaries representing characters
    * `Time` - `hour:minute:second` structs in a given calendar
    * `URI` - representation of URIs that identify resources
    * `Version` - representation of versions and requirements

  ### System modules

  Modules that interface with the underlying system, such as:

    * `IO` - handles input and output
    * `File` - interacts with the underlying file system
    * `Path` - manipulates file system paths
    * `System` - reads and writes system information

  ### Protocols

  Protocols add polymorphic dispatch to Elixir. They are contracts
  implementable by data types. See `Protocol` for more information on
  protocols. Elixir provides the following protocols in the standard library:

    * `Collectable` - collects data into a data type
    * `Enumerable` - handles collections in Elixir. The `Enum` module
      provides eager functions for working with collections, the `Stream`
      module provides lazy functions
    * `Inspect` - converts data types into their programming language
      representation
    * `List.Chars` - converts data types to their outside world
      representation as charlists (non-programming based)
    * `String.Chars` - converts data types to their outside world
      representation as strings (non-programming based)

  ### Process-based and application-centric functionality

  The following modules build on top of processes to provide concurrency,
  fault-tolerance, and more.

    * `Agent` - a process that encapsulates mutable state
    * `Application` - functions for starting, stopping and configuring
      applications
    * `GenServer` - a generic client-server API
    * `Registry` - a key-value process-based storage
    * `Supervisor` - a process that is responsible for starting,
      supervising and shutting down other processes
    * `Task` - a process that performs computations
    * `Task.Supervisor` - a supervisor for managing tasks exclusively

  ### Supporting documents

  Under the "Pages" section in sidebar you will find tutorials, guides,
  and reference documents that outline Elixir semantics and behaviors
  in more detail. Those are:

    * [Compatibility and deprecations](compatibility-and-deprecations.md) - lists
      compatibility between every Elixir version and Erlang/OTP, release schema;
      lists all deprecated functions, when they were deprecated and alternatives
    * [Library guidelines](library-guidelines.md) - general guidelines, anti-patterns,
      and rules for those writing libraries
    * [Naming conventions](naming-conventions.md) - naming conventions for Elixir code
    * [Operators reference](operators.md) - lists all Elixir operators and their precedences
    * [Patterns and guards](patterns-and-guards.md) - an introduction to patterns,
      guards, and extensions
    * [Syntax reference](syntax-reference.md) - the language syntax reference
    * [Typespecs reference](typespecs.md)- types and function specifications, including list of types
    * [Unicode syntax](unicode-syntax.md) - outlines Elixir support for Unicode

  ## Guards

  This module includes the built-in guards used by Elixir developers.
  They are a predefined set of functions and macros that augment pattern
  matching, typically invoked after the `when` operator. For example:

      def drive(%User{age: age}) when age >= 16 do
        ...
      end

  The clause above will only be invoked if the user's age is more than
  or equal to 16. Guards also support joining multiple conditions with
  `and` and `or`. The whole guard is true if all guard expressions will
  evaluate to `true`. A more complete introduction to guards is available
  in the [Patterns and guards](patterns-and-guards.md) page.

  ## Truthy and falsy values

  Besides the booleans `true` and `false`, Elixir has the
  concept of a "truthy" or "falsy" value.

    *  a value is truthy when it is neither `false` nor `nil`
    *  a value is falsy when it is either `false` or `nil`

  Elixir has functions, like `and/2`, that *only* work with
  booleans, but also functions that work with these
  truthy/falsy values, like `&&/2` and `!/1`.

  ## Structural comparison

  The functions in this module perform structural comparison. This allows
  different data types to be compared using comparison operators:

      1 < :an_atom

  This is possible so Elixir developers can create collections, such as
  dictionaries and ordered sets, that store a mixture of data types in them.
  To understand why this matters, let's discuss the two types of comparisons
  we find in software: _structural_ and _semantic_.

  Structural means we are comparing the underlying data structures and we often
  want those operations to be as fast as possible, because it is used to power
  several algorithms and data structures in the language. A semantic comparison
  worries about what each data type represents. For example, semantically
  speaking, it doesn't make sense to compare `Time` with `Date`.

  One example that shows the differences between structural and semantic
  comparisons are strings: "alien" sorts less than "office" (`"alien" < "office"`)
  but "álien" is greater than "office". This happens because `<` compares the
  underlying bytes that form the string. If you were doing alphabetical listing,
  you may want "álien" to also appear before "office".

  This means **comparisons in Elixir are structural**, as it has the goal
  of comparing data types as efficiently as possible to create flexible
  and performant data structures. This distinction is specially important
  for functions that provide ordering, such as `>/2`, `</2`, `>=/2`,
  `<=/2`, `min/2`, and `max/2`. For example:

      ~D[2017-03-31] > ~D[2017-04-01]

  will return `true` because structural comparison compares the `:day`
  field before `:month` or `:year`. Luckily, the Elixir compiler will
  detect whenever comparing structs or whenever comparing code that is
  either always true or false, and emit a warning accordingly.

  In order to perform semantic comparisons, the relevant data-types
  provide a `compare/2` function, such as `Date.compare/2`:

      iex> Date.compare(~D[2017-03-31], ~D[2017-04-01])
      :lt

  Alternatively, you can use the functions in the `Enum` module to
  sort or compute a maximum/minimum:

      iex> Enum.sort([~D[2017-03-31], ~D[2017-04-01]], Date)
      [~D[2017-03-31], ~D[2017-04-01]]
      iex> Enum.max([~D[2017-03-31], ~D[2017-04-01]], Date)
      ~D[2017-04-01]

  The second argument is precisely the module to be used for semantic
  comparison. Keeping this distinction is important, because if semantic
  comparison was used by default for implementing data structures and
  algorithms, they could become orders of magnitude slower!

  Finally, note there is an overall structural sorting order, called
  "Term Ordering", defined below. This order is provided for reference
  purposes, it is not required by Elixir developers to know it by heart.

  ### Term ordering

  ```
  number < atom < reference < function < port < pid < tuple < map < list < bitstring
  ```

  When comparing two numbers of different types (a number being either
  an integer or a float), a conversion to the type with greater precision
  will always occur, unless the comparison operator used is either `===/2`
  or `!==`. A float will be considered more precise than an integer, unless
  the float is greater/less than +/-9007199254740992.0 respectively,
  at which point all the significant figures of the float are to the left
  of the decimal point. This behavior exists so that the comparison of large
  numbers remains transitive.

  The collection types are compared using the following rules:

  * Tuples are compared by size, then element by element.
  * Maps are compared by size, then by keys in ascending term order,
    then by values in key order. In the specific case of maps' key
    ordering, integers are always considered to be less than floats.
  * Lists are compared element by element.
  * Bitstrings are compared byte by byte, incomplete bytes are compared bit by bit.
  * Atoms are compared using their string value, codepoint by codepoint.

  ### Examples

  We can check the truthiness of a value by using the `!/1`
  function twice.

  Truthy values:

      iex> !!true
      true
      iex> !!5
      true
      iex> !![1,2]
      true
      iex> !!"foo"
      true

  Falsy values (of which there are exactly two):

      iex> !!false
      false
      iex> !!nil
      false

  ## Inlining

  Some of the functions described in this module are inlined by
  the Elixir compiler into their Erlang counterparts in the
  [`:erlang`](`:erlang`) module.
  Those functions are called BIFs (built-in internal functions)
  in Erlang-land and they exhibit interesting properties, as some
  of them are allowed in guards and others are used for compiler
  optimizations.

  Most of the inlined functions can be seen in effect when
  capturing the function:

      iex> &Kernel.is_atom/1
      &:erlang.is_atom/1

  Those functions will be explicitly marked in their docs as
  "inlined by the compiler".
  """

  # We need this check only for bootstrap purposes.
  # Once Kernel is loaded and we recompile, it is a no-op.
  @compile {:inline, bootstrapped?: 1}
  case :code.ensure_loaded(Kernel) do
    {:module, _} ->
      defp bootstrapped?(_), do: true

    {:error, _} ->
      defp bootstrapped?(module), do: :code.ensure_loaded(module) == {:module, module}
  end

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
  @doc guard: true
  @spec abs(number) :: number
  def abs(number) do
    :erlang.abs(number)
  end

  @doc """
  Invokes the given anonymous function `fun` with the list of
  arguments `args`.

  If the number of arguments is known at compile time, prefer
  `fun.(arg_1, arg_2, ..., arg_n)` as it is clearer than
  `apply(fun, [arg_1, arg_2, ..., arg_n])`.

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
  Invokes the given function from `module` with the list of
  arguments `args`.

  `apply/3` is used to invoke functions where the module, function
  name or arguments are defined dynamically at runtime. For this
  reason, you can't invoke macros using `apply/3`, only functions.

  If the number of arguments and the function name are known at compile time,
  prefer `module.function(arg_1, arg_2, ..., arg_n)` as it is clearer than
  `apply(module, :function, [arg_1, arg_2, ..., arg_n])`.

  `apply/3` cannot be used to call private functions.

  Inlined by the compiler.

  ## Examples

      iex> apply(Enum, :reverse, [[1, 2, 3]])
      [3, 2, 1]

  """
  @spec apply(module, function_name :: atom, [any]) :: any
  def apply(module, function_name, args) do
    :erlang.apply(module, function_name, args)
  end

  @doc """
  Extracts the part of the binary at `start` with `size`.

  If `start` or `size` reference in any way outside the binary,
  an `ArgumentError` exception is raised.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> binary_part("foo", 1, 2)
      "oo"

  A negative `size` can be used to extract bytes that come *before* the byte
  at `start`:

      iex> binary_part("Hello", 5, -3)
      "llo"

  An `ArgumentError` is raised when the `size` is outside of the binary:

      binary_part("Hello", 0, 10)
      ** (ArgumentError) argument error

  """
  @doc guard: true
  @spec binary_part(binary, non_neg_integer, integer) :: binary
  def binary_part(binary, start, size) do
    :erlang.binary_part(binary, start, size)
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
  @doc guard: true
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
  @doc guard: true
  @spec byte_size(bitstring) :: non_neg_integer
  def byte_size(bitstring) do
    :erlang.byte_size(bitstring)
  end

  @doc """
  Returns the smallest integer greater than or equal to `number`.

  If you want to perform ceil operation on other decimal places,
  use `Float.ceil/2` instead.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> ceil(10)
      10

      iex> ceil(10.1)
      11

      iex> ceil(-10.1)
      -10

  """
  @doc since: "1.8.0", guard: true
  @spec ceil(number) :: integer
  def ceil(number) do
    :erlang.ceil(number)
  end

  @doc """
  Performs an integer division.

  Raises an `ArithmeticError` exception if one of the arguments is not an
  integer, or when the `divisor` is `0`.

  `div/2` performs *truncated* integer division. This means that
  the result is always rounded towards zero.

  If you want to perform floored integer division (rounding towards negative infinity),
  use `Integer.floor_div/2` instead.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      div(5, 2)
      #=> 2

      div(6, -4)
      #=> -1

      div(-99, 2)
      #=> -49

      div(100, 0)
      ** (ArithmeticError) bad argument in arithmetic expression

  """
  @doc guard: true
  @spec div(integer, neg_integer | pos_integer) :: integer
  def div(dividend, divisor) do
    :erlang.div(dividend, divisor)
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
  as a crash. This means the default supervisor behavior kicks in,
  error reports are emitted, and so forth.

  This behavior is relied on in many different places. For example,
  `ExUnit` uses `exit(:shutdown)` when exiting the test process to
  signal linked processes, supervision trees and so on to politely
  shut down too.

  ## CLI exits

  Building on top of the exit signals mentioned above, if the
  process started by the command line exits with any of the three
  reasons above, its exit is considered normal and the Operating
  System process will exit with status 0.

  It is, however, possible to customize the operating system exit
  signal by invoking:

      exit({:shutdown, integer})

  This will cause the operating system process to exit with the status given by
  `integer` while signaling all linked Erlang processes to politely
  shut down.

  Any other exit reason will cause the operating system process to exit with
  status `1` and linked Erlang processes to crash.
  """
  @spec exit(term) :: no_return
  def exit(reason) do
    :erlang.exit(reason)
  end

  @doc """
  Returns the largest integer smaller than or equal to `number`.

  If you want to perform floor operation on other decimal places,
  use `Float.floor/2` instead.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> floor(10)
      10

      iex> floor(9.7)
      9

      iex> floor(-9.7)
      -10

  """
  @doc since: "1.8.0", guard: true
  @spec floor(number) :: integer
  def floor(number) do
    :erlang.floor(number)
  end

  @doc """
  Returns the head of a list. Raises `ArgumentError` if the list is empty.

  The head of a list is its first element.

  It works with improper lists.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      hd([1, 2, 3, 4])
      #=> 1

      hd([1 | 2])
      #=> 1

  Giving it an empty list raises:

      hd([])
      ** (ArgumentError) argument error

  """
  @doc guard: true
  @spec hd(nonempty_maybe_improper_list(elem, term)) :: elem when elem: term
  def hd(list) do
    :erlang.hd(list)
  end

  @doc """
  Returns `true` if `term` is an atom, otherwise returns `false`.

  Note `true`, `false`, and `nil` are atoms in Elixir, as well as
  module names. Therefore this function will return `true` to all
  of those values.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_atom(:name)
      true

      iex> is_atom(false)
      true

      iex> is_atom(AnAtom)
      true

      iex> is_atom("string")
      false

  """
  @doc guard: true
  @spec is_atom(term) :: boolean
  def is_atom(term) do
    :erlang.is_atom(term)
  end

  @doc """
  Returns `true` if `term` is a binary, otherwise returns `false`.

  A binary always contains a complete number of bytes.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_binary("foo")
      true
      iex> is_binary(<<1::3>>)
      false

  """
  @doc guard: true
  @spec is_binary(term) :: boolean
  def is_binary(term) do
    :erlang.is_binary(term)
  end

  @doc """
  Returns `true` if `term` is a bitstring (including a binary), otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_bitstring("foo")
      true
      iex> is_bitstring(<<1::3>>)
      true

  """
  @doc guard: true
  @spec is_bitstring(term) :: boolean
  def is_bitstring(term) do
    :erlang.is_bitstring(term)
  end

  @doc """
  Returns `true` if `term` is either the atom `true` or the atom `false` (i.e.,
  a boolean), otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_boolean(false)
      true

      iex> is_boolean(true)
      true

      iex> is_boolean(:test)
      false

  """
  @doc guard: true
  @spec is_boolean(term) :: boolean
  def is_boolean(term) do
    :erlang.is_boolean(term)
  end

  @doc """
  Returns `true` if `term` is a floating-point number, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_float(term) :: boolean
  def is_float(term) do
    :erlang.is_float(term)
  end

  @doc """
  Returns `true` if `term` is a function, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_function(fn x -> x + x end)
      true

      iex> is_function("not a function")
      false

  """
  @doc guard: true
  @spec is_function(term) :: boolean
  def is_function(term) do
    :erlang.is_function(term)
  end

  @doc """
  Returns `true` if `term` is a function that can be applied with `arity` number of arguments;
  otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_function(fn x -> x * 2 end, 1)
      true
      iex> is_function(fn x -> x * 2 end, 2)
      false

  """
  @doc guard: true
  @spec is_function(term, non_neg_integer) :: boolean
  def is_function(term, arity) do
    :erlang.is_function(term, arity)
  end

  @doc """
  Returns `true` if `term` is an integer, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_integer(term) :: boolean
  def is_integer(term) do
    :erlang.is_integer(term)
  end

  @doc """
  Returns `true` if `term` is a list with zero or more elements, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_list(term) :: boolean
  def is_list(term) do
    :erlang.is_list(term)
  end

  @doc """
  Returns `true` if `term` is either an integer or a floating-point number;
  otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_number(term) :: boolean
  def is_number(term) do
    :erlang.is_number(term)
  end

  @doc """
  Returns `true` if `term` is a PID (process identifier), otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_pid(term) :: boolean
  def is_pid(term) do
    :erlang.is_pid(term)
  end

  @doc """
  Returns `true` if `term` is a port identifier, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_port(term) :: boolean
  def is_port(term) do
    :erlang.is_port(term)
  end

  @doc """
  Returns `true` if `term` is a reference, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_reference(term) :: boolean
  def is_reference(term) do
    :erlang.is_reference(term)
  end

  @doc """
  Returns `true` if `term` is a tuple, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec is_tuple(term) :: boolean
  def is_tuple(term) do
    :erlang.is_tuple(term)
  end

  @doc """
  Returns `true` if `term` is a map, otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  > #### Structs are maps {: .info}
  >
  > Structs are also maps, and many of Elixir data structures are implemented
  > using structs: `Range`s, `Regex`es, `Date`s...
  >
  >     iex> is_map(1..10)
  >     true
  >     iex> is_map(~D[2024-04-18])
  >     true
  >
  > If you mean to specifically check for non-struct maps, use
  > `is_non_struct_map/1` instead.
  >
  >     iex> is_non_struct_map(1..10)
  >     false
  """
  @doc guard: true
  @spec is_map(term) :: boolean
  def is_map(term) do
    :erlang.is_map(term)
  end

  @doc """
  Returns `true` if `key` is a key in `map`, otherwise returns `false`.

  It raises `BadMapError` if the first element is not a map.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_map_key(%{a: "foo", b: "bar"}, :a)
      true

      iex> is_map_key(%{a: "foo", b: "bar"}, :c)
      false
  """
  @doc guard: true, since: "1.10.0"
  @spec is_map_key(map, term) :: boolean
  def is_map_key(map, key) do
    :erlang.is_map_key(key, map)
  end

  @doc """
  Returns the length of `list`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> length([1, 2, 3, 4, 5, 6, 7, 8, 9])
      9

  """
  @doc guard: true
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

      make_ref()
      #=> #Reference<0.0.0.135>

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
  @doc guard: true
  @spec map_size(map) :: non_neg_integer
  def map_size(map) do
    :erlang.map_size(map)
  end

  @doc """
  Returns the biggest of the two given terms according to
  their structural comparison.

  If the terms compare equal, the first one is returned.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Inlined by the compiler.

  ## Examples

      iex> max(1, 2)
      2
      iex> max("a", "b")
      "b"

  """
  @spec max(first, second) :: first | second when first: term, second: term
  def max(first, second) do
    :erlang.max(first, second)
  end

  @doc """
  Returns the smallest of the two given terms according to
  their structural comparison.

  If the terms compare equal, the first one is returned.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Inlined by the compiler.

  ## Examples

      iex> min(1, 2)
      1
      iex> min("foo", "bar")
      "bar"

  """
  @spec min(first, second) :: first | second when first: term, second: term
  def min(first, second) do
    :erlang.min(first, second)
  end

  @doc """
  Returns an atom representing the name of the local node.
  If the node is not alive, `:nonode@nohost` is returned instead.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec node() :: node
  def node do
    :erlang.node()
  end

  @doc """
  Returns the node where the given argument is located.
  The argument can be a PID, a reference, or a port.
  If the local node is not alive, `:nonode@nohost` is returned.

  Allowed in guard tests. Inlined by the compiler.
  """
  @doc guard: true
  @spec node(pid | reference | port) :: node
  def node(arg) do
    :erlang.node(arg)
  end

  @doc """
  Computes the remainder of an integer division.

  `rem/2` uses truncated division, which means that
  the result will always have the sign of the `dividend`.

  Raises an `ArithmeticError` exception if one of the arguments is not an
  integer, or when the `divisor` is `0`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> rem(5, 2)
      1
      iex> rem(6, -4)
      2

  """
  @doc guard: true
  @spec rem(integer, neg_integer | pos_integer) :: integer
  def rem(dividend, divisor) do
    :erlang.rem(dividend, divisor)
  end

  @doc """
  Rounds a number to the nearest integer.

  If the number is equidistant to the two nearest integers, rounds away from zero.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> round(5.6)
      6

      iex> round(5.2)
      5

      iex> round(-9.9)
      -10

      iex> round(-9)
      -9

      iex> round(2.5)
      3

      iex> round(-2.5)
      -3

  """
  @doc guard: true
  @spec round(number) :: integer
  def round(number) do
    :erlang.round(number)
  end

  @doc """
  Sends a message to the given `dest` and returns the message.

  `dest` may be a remote or local PID, a local port, a locally
  registered name, or a tuple in the form of `{registered_name, node}` for a
  registered name at another node.

  For additional documentation, see the [`!` operator Erlang
  documentation](https://www.erlang.org/doc/reference_manual/expressions#send).

  Inlined by the compiler.

  ## Examples

      iex> send(self(), :hello)
      :hello

  """
  @spec send(dest :: Process.dest(), message) :: message when message: any
  def send(dest, message) do
    :erlang.send(dest, message)
  end

  @doc """
  Returns the PID (process identifier) of the calling process.

  Allowed in guard clauses. Inlined by the compiler.
  """
  @doc guard: true
  @spec self() :: pid
  def self() do
    :erlang.self()
  end

  @doc """
  Spawns the given function and returns its PID.

  Typically developers do not use the `spawn` functions, instead they use
  abstractions such as `Task`, `GenServer` and `Agent`, built on top of
  `spawn`, that spawns processes with more conveniences in terms of
  introspection and debugging.

  Check the `Process` module for more process-related functions.

  The anonymous function receives 0 arguments, and may return any value.

  Inlined by the compiler.

  ## Examples

      current = self()
      child = spawn(fn -> send(current, {self(), 1 + 2}) end)

      receive do
        {^child, 3} -> IO.puts("Received 3 back")
      end

  """
  @spec spawn((-> any)) :: pid
  def spawn(fun) do
    :erlang.spawn(fun)
  end

  @doc """
  Spawns the given function `fun` from the given `module` passing it the given
  `args` and returns its PID.

  Typically developers do not use the `spawn` functions, instead they use
  abstractions such as `Task`, `GenServer` and `Agent`, built on top of
  `spawn`, that spawns processes with more conveniences in terms of
  introspection and debugging.

  Check the `Process` module for more process-related functions.

  Inlined by the compiler.

  ## Examples

      spawn(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn(module, atom, list) :: pid
  def spawn(module, fun, args) do
    :erlang.spawn(module, fun, args)
  end

  @doc """
  Spawns the given function, links it to the current process, and returns its PID.

  Typically developers do not use the `spawn` functions, instead they use
  abstractions such as `Task`, `GenServer` and `Agent`, built on top of
  `spawn`, that spawns processes with more conveniences in terms of
  introspection and debugging.

  Check the `Process` module for more process-related functions. For more
  information on linking, check `Process.link/1`.

  The anonymous function receives 0 arguments, and may return any value.

  Inlined by the compiler.

  ## Examples

      current = self()
      child = spawn_link(fn -> send(current, {self(), 1 + 2}) end)

      receive do
        {^child, 3} -> IO.puts("Received 3 back")
      end

  """
  @spec spawn_link((-> any)) :: pid
  def spawn_link(fun) do
    :erlang.spawn_link(fun)
  end

  @doc """
  Spawns the given function `fun` from the given `module` passing it the given
  `args`, links it to the current process, and returns its PID.

  Typically developers do not use the `spawn` functions, instead they use
  abstractions such as `Task`, `GenServer` and `Agent`, built on top of
  `spawn`, that spawns processes with more conveniences in terms of
  introspection and debugging.

  Check the `Process` module for more process-related functions. For more
  information on linking, check `Process.link/1`.

  Inlined by the compiler.

  ## Examples

      spawn_link(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn_link(module, atom, list) :: pid
  def spawn_link(module, fun, args) do
    :erlang.spawn_link(module, fun, args)
  end

  @doc """
  Spawns the given function, monitors it and returns its PID
  and monitoring reference.

  Typically developers do not use the `spawn` functions, instead they use
  abstractions such as `Task`, `GenServer` and `Agent`, built on top of
  `spawn`, that spawns processes with more conveniences in terms of
  introspection and debugging.

  Check the `Process` module for more process-related functions.

  The anonymous function receives 0 arguments, and may return any value.

  Inlined by the compiler.

  ## Examples

      current = self()
      spawn_monitor(fn -> send(current, {self(), 1 + 2}) end)

  """
  @spec spawn_monitor((-> any)) :: {pid, reference}
  def spawn_monitor(fun) do
    :erlang.spawn_monitor(fun)
  end

  @doc """
  Spawns the given module and function passing the given args,
  monitors it and returns its PID and monitoring reference.

  Typically developers do not use the `spawn` functions, instead they use
  abstractions such as `Task`, `GenServer` and `Agent`, built on top of
  `spawn`, that spawns processes with more conveniences in terms of
  introspection and debugging.

  Check the `Process` module for more process-related functions.

  Inlined by the compiler.

  ## Examples

      spawn_monitor(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn_monitor(module, atom, list) :: {pid, reference}
  def spawn_monitor(module, fun, args) do
    :erlang.spawn_monitor(module, fun, args)
  end

  @doc """
  Pipes the first argument, `value`, into the second argument, a function `fun`,
  and returns `value` itself.

  Useful for running synchronous side effects in a pipeline, using the `|>/2` operator.

  ## Examples

      iex> tap(1, fn x -> x + 1 end)
      1

  Most commonly, this is used in pipelines, using the `|>/2` operator.
  For example, let's suppose you want to inspect part of a data structure.
  You could write:

      %{a: 1}
      |> Map.update!(:a, & &1 + 2)
      |> tap(&IO.inspect(&1.a))
      |> Map.update!(:a, & &1 * 2)

  """
  @doc since: "1.12.0"
  defmacro tap(value, fun) do
    quote bind_quoted: [fun: fun, value: value] do
      _ = fun.(value)
      value
    end
  end

  @doc """
  A non-local return from a function.

  Using `throw/1` is generally discouraged, as it allows a function
  to escape from its regular execution flow, which can make the code
  harder to read. Furthermore, all thrown values must be caught by
  `try/catch`. See `try/1` for more information.

  Inlined by the compiler.
  """
  @spec throw(term) :: no_return
  def throw(term) do
    :erlang.throw(term)
  end

  @doc """
  Returns the tail of a list. Raises `ArgumentError` if the list is empty.

  The tail of a list is the list without its first element.

  It works with improper lists.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      tl([1, 2, 3, :go])
      #=> [2, 3, :go]

      tl([:one])
      #=> []

      tl([:a, :b | :improper_end])
      #=> [:b | :improper_end]

      tl([:a | %{b: 1}])
      #=> %{b: 1}

  Giving it an empty list raises:

      tl([])
      ** (ArgumentError) argument error

  """
  @doc guard: true
  @spec tl(nonempty_maybe_improper_list(elem, last)) :: maybe_improper_list(elem, last) | last
        when elem: term, last: term
  def tl(list) do
    :erlang.tl(list)
  end

  @doc """
  Returns the integer part of `number`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> trunc(5.4)
      5

      iex> trunc(-5.99)
      -5

      iex> trunc(-5)
      -5

  """
  @doc guard: true
  @spec trunc(number) :: integer
  def trunc(number) do
    :erlang.trunc(number)
  end

  @doc """
  Returns the size of a tuple.

  This operation happens in constant time.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> tuple_size({:a, :b, :c})
      3

  """
  @doc guard: true
  @spec tuple_size(tuple) :: non_neg_integer
  def tuple_size(tuple) do
    :erlang.tuple_size(tuple)
  end

  @doc """
  Arithmetic addition operator.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 + 2
      3

  """
  @doc guard: true
  @spec integer + integer :: integer
  @spec float + float :: float
  @spec integer + float :: float
  @spec float + integer :: float
  def left + right do
    :erlang.+(left, right)
  end

  @doc """
  Arithmetic subtraction operator.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 - 2
      -1

  """
  @doc guard: true
  @spec integer - integer :: integer
  @spec float - float :: float
  @spec integer - float :: float
  @spec float - integer :: float
  def left - right do
    :erlang.-(left, right)
  end

  @doc """
  Arithmetic positive unary operator.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> +1
      1

  """
  @doc guard: true
  @spec +integer :: integer
  @spec +float :: float
  def +value do
    :erlang.+(value)
  end

  @doc """
  Arithmetic negative unary operator.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> -2
      -2

  """
  @doc guard: true
  @spec -0 :: 0
  @spec -pos_integer :: neg_integer
  @spec -neg_integer :: pos_integer
  @spec -float :: float
  def -value do
    :erlang.-(value)
  end

  @doc """
  Arithmetic multiplication operator.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 * 2
      2

  """
  @doc guard: true
  @spec integer * integer :: integer
  @spec float * float :: float
  @spec integer * float :: float
  @spec float * integer :: float
  def left * right do
    :erlang.*(left, right)
  end

  @doc """
  Arithmetic division operator.

  The result is always a float. Use `div/2` and `rem/2` if you want
  an integer division or the remainder.

  Raises `ArithmeticError` if `right` is 0 or 0.0.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      1 / 2
      #=> 0.5

      -3.0 / 2.0
      #=> -1.5

      5 / 1
      #=> 5.0

      7 / 0
      ** (ArithmeticError) bad argument in arithmetic expression

  """
  @doc guard: true
  @spec number / number :: float
  def left / right do
    :erlang./(left, right)
  end

  @doc """
  List concatenation operator. Concatenates a proper list and a term, returning a list.

  The complexity of `a ++ b` is proportional to `length(a)`, so avoid repeatedly
  appending to lists of arbitrary length, for example, `list ++ [element]`.
  Instead, consider prepending via `[element | rest]` and then reversing.

  If the `right` operand is not a proper list, it returns an improper list.
  If the `left` operand is not a proper list, it raises `ArgumentError`.
  If the `left` operand is an empty list, it returns the `right` operand.

  Inlined by the compiler.

  ## Examples

      iex> [1] ++ [2, 3]
      [1, 2, 3]

      iex> ~c"foo" ++ ~c"bar"
      ~c"foobar"

      # a non-list on the right will return an improper list
      # with said element at the end
      iex> [1, 2] ++ 3
      [1, 2 | 3]
      iex> [1, 2] ++ {3, 4}
      [1, 2 | {3, 4}]

      # improper list on the right will return an improper list
      iex> [1] ++ [2 | 3]
      [1, 2 | 3]

      # empty list on the left will return the right operand
      iex> [] ++ 1
      1

  The `++/2` operator is right associative, meaning:

      iex> [1, 2, 3] -- [1] ++ [2]
      [3]

  As it is equivalent to:

      iex> [1, 2, 3] -- ([1] ++ [2])
      [3]

  """
  @spec [] ++ a :: a when a: term()
  @spec nonempty_list() ++ term() :: maybe_improper_list()
  def left ++ right do
    :erlang.++(left, right)
  end

  @doc """
  List subtraction operator. Removes the first occurrence of an element
  on the left list for each element on the right.

  This function is optimized so the complexity of `a -- b` is proportional
  to `length(a) * log(length(b))`. See also the [Erlang efficiency
  guide](https://www.erlang.org/doc/system/efficiency_guide.html).

  Inlined by the compiler.

  ## Examples

      iex> [1, 2, 3] -- [1, 2]
      [3]

      iex> [1, 2, 3, 2, 1] -- [1, 2, 2]
      [3, 1]

  The `--/2` operator is right associative, meaning:

      iex> [1, 2, 3] -- [2] -- [3]
      [1, 3]

  As it is equivalent to:

      iex> [1, 2, 3] -- ([2] -- [3])
      [1, 3]

  """
  @spec list -- list :: list
  def left -- right do
    :erlang.--(left, right)
  end

  @doc """
  Strictly boolean "not" operator.

  `value` must be a boolean; if it's not, an `ArgumentError` exception is raised.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> not false
      true

  """
  @doc guard: true
  @spec not true :: false
  @spec not false :: true
  def not value do
    :erlang.not(value)
  end

  @doc """
  Less-than operator.

  Returns `true` if `left` is less than `right`.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 < 2
      true

  """
  @doc guard: true
  @spec term < term :: boolean
  def left < right do
    :erlang.<(left, right)
  end

  @doc """
  Greater-than operator.

  Returns `true` if `left` is more than `right`.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 > 2
      false

  """
  @doc guard: true
  @spec term > term :: boolean
  def left > right do
    :erlang.>(left, right)
  end

  @doc """
  Less-than or equal to operator.

  Returns `true` if `left` is less than or equal to `right`.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 <= 2
      true

  """
  @doc guard: true
  @spec term <= term :: boolean
  def left <= right do
    :erlang."=<"(left, right)
  end

  @doc """
  Greater-than or equal to operator.

  Returns `true` if `left` is more than or equal to `right`.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 >= 2
      false

  """
  @doc guard: true
  @spec term >= term :: boolean
  def left >= right do
    :erlang.>=(left, right)
  end

  @doc """
  Equal to operator. Returns `true` if the two terms are equal.

  This operator considers 1 and 1.0 to be equal. For stricter
  semantics, use `===/2` instead.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 == 2
      false

      iex> 1 == 1.0
      true

  """
  @doc guard: true
  @spec term == term :: boolean
  def left == right do
    :erlang.==(left, right)
  end

  @doc """
  Not equal to operator.

  Returns `true` if the two terms are not equal.

  This operator considers 1 and 1.0 to be equal. For match
  comparison, use `!==/2` instead.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 != 2
      true

      iex> 1 != 1.0
      false

  """
  @doc guard: true
  @spec term != term :: boolean
  def left != right do
    :erlang."/="(left, right)
  end

  @doc """
  Strictly equal to operator.

  Returns `true` if the two terms are exactly equal.

  The terms are only considered to be exactly equal if they
  have the same value and are of the same type. For example,
  `1 == 1.0` returns `true`, but since they are of different
  types, `1 === 1.0` returns `false`.

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 === 2
      false

      iex> 1 === 1.0
      false

  """
  @doc guard: true
  @spec term === term :: boolean
  def left === right do
    :erlang."=:="(left, right)
  end

  @doc """
  Strictly not equal to operator.

  Returns `true` if the two terms are not exactly equal.
  See `===/2` for a definition of what is considered "exactly equal".

  This performs a structural comparison where all Elixir
  terms can be compared with each other. See the ["Structural
  comparison"](#module-structural-comparison) section
  for more information.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 !== 2
      true

      iex> 1 !== 1.0
      true

  """
  @doc guard: true
  @spec term !== term :: boolean
  def left !== right do
    :erlang."=/="(left, right)
  end

  @doc """
  Gets the element at the zero-based `index` in `tuple`.

  It raises `ArgumentError` when index is negative or it is out of range of the tuple elements.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      tuple = {:foo, :bar, 3}
      elem(tuple, 1)
      #=> :bar

      elem({}, 0)
      ** (ArgumentError) argument error

      elem({:foo, :bar}, 2)
      ** (ArgumentError) argument error

  """
  @doc guard: true
  @spec elem(tuple, non_neg_integer) :: term
  def elem(tuple, index) do
    :erlang.element(index + 1, tuple)
  end

  @doc """
  Puts `value` at the given zero-based `index` in `tuple`.

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

  defp optimize_boolean({:case, meta, args}) do
    {:case, [{:optimize_boolean, true} | meta], args}
  end

  @doc """
  Strictly boolean "or" operator.

  If `left` is `true`, returns `true`, otherwise returns `right`.

  Requires only the `left` operand to be a boolean since it short-circuits.
  If the `left` operand is not a boolean, a `BadBooleanError` exception is
  raised.

  Allowed in guard tests.

  ## Examples

      iex> true or false
      true

      iex> false or 42
      42

      iex> 42 or false
      ** (BadBooleanError) expected a boolean on left-side of "or", got: 42

  """
  @doc guard: true
  defmacro left or right do
    case __CALLER__.context do
      nil -> build_boolean_check(:or, left, true, right)
      :match -> invalid_match!(:or)
      :guard -> quote(do: :erlang.orelse(unquote(left), unquote(right)))
    end
  end

  @doc """
  Strictly boolean "and" operator.

  If `left` is `false`, returns `false`, otherwise returns `right`.

  Requires only the `left` operand to be a boolean since it short-circuits. If
  the `left` operand is not a boolean, a `BadBooleanError` exception is raised.

  Allowed in guard tests.

  ## Examples

      iex> true and false
      false

      iex> true and "yay!"
      "yay!"

      iex> "yay!" and true
      ** (BadBooleanError) expected a boolean on left-side of "and", got: "yay!"

  """
  @doc guard: true
  defmacro left and right do
    case __CALLER__.context do
      nil -> build_boolean_check(:and, left, right, false)
      :match -> invalid_match!(:and)
      :guard -> quote(do: :erlang.andalso(unquote(left), unquote(right)))
    end
  end

  defp build_boolean_check(operator, check, true_clause, false_clause) do
    optimize_boolean(
      quote do
        case unquote(check) do
          false -> unquote(false_clause)
          true -> unquote(true_clause)
          other -> :erlang.error({:badbool, unquote(operator), other})
        end
      end
    )
  end

  @doc """
  Boolean "not" operator.

  Receives any value (not just booleans) and returns `true` if `value`
  is `false` or `nil`; returns `false` otherwise.

  Not allowed in guard clauses.

  ## Examples

      iex> !Enum.empty?([])
      false

      iex> !List.first([])
      true

  """
  defmacro !value

  defmacro !{:!, _, [value]} do
    assert_no_match_or_guard_scope(__CALLER__.context, "!")

    optimize_boolean(
      quote do
        case unquote(value) do
          x when :"Elixir.Kernel".in(x, [false, nil]) -> false
          _ -> true
        end
      end
    )
  end

  defmacro !value do
    assert_no_match_or_guard_scope(__CALLER__.context, "!")

    optimize_boolean(
      quote do
        case unquote(value) do
          x when :"Elixir.Kernel".in(x, [false, nil]) -> true
          _ -> false
        end
      end
    )
  end

  @doc """
  Binary concatenation operator. Concatenates two binaries.

  Raises an `ArgumentError` if one of the sides aren't binaries.

  ## Examples

      iex> "foo" <> "bar"
      "foobar"

  The `<>/2` operator can also be used in pattern matching (and guard clauses) as
  long as the left argument is a literal binary:

      iex> "foo" <> x = "foobar"
      iex> x
      "bar"

  `x <> "bar" = "foobar"` would result in an `ArgumentError` exception.

  """
  defmacro left <> right do
    concats = extract_concatenations({:<>, [], [left, right]}, __CALLER__)
    quote(do: <<unquote_splicing(concats)>>)
  end

  # Extracts concatenations in order to optimize many
  # concatenations into one single clause.
  defp extract_concatenations({:<>, _, [left, right]}, caller) do
    [wrap_concatenation(left, :left, caller) | extract_concatenations(right, caller)]
  end

  defp extract_concatenations(other, caller) do
    [wrap_concatenation(other, :right, caller)]
  end

  defp wrap_concatenation(binary, _side, _caller) when is_binary(binary) do
    binary
  end

  defp wrap_concatenation(literal, _side, _caller)
       when is_list(literal) or is_atom(literal) or is_integer(literal) or is_float(literal) do
    :erlang.error(
      ArgumentError.exception(
        "expected binary argument in <> operator but got: #{Macro.to_string(literal)}"
      )
    )
  end

  defp wrap_concatenation(other, side, caller) do
    expanded = expand_concat_argument(other, side, caller)
    {:"::", [], [expanded, {:binary, [], nil}]}
  end

  defp expand_concat_argument(arg, :left, %{context: :match} = caller) do
    expanded_arg =
      case bootstrapped?(Macro) do
        true -> Macro.expand(arg, caller)
        false -> arg
      end

    case expanded_arg do
      {var, _, nil} when is_atom(var) ->
        invalid_concat_left_argument_error(Atom.to_string(var))

      _ ->
        expanded_arg
    end
  end

  defp expand_concat_argument(arg, _, _) do
    arg
  end

  defp invalid_concat_left_argument_error(arg) do
    :erlang.error(
      ArgumentError.exception(
        "cannot perform prefix match because the left operand of <> has unknown size. " <>
          "The left operand of <> inside a match should either be a literal binary or " <>
          "an existing variable with the pin operator (such as ^some_var). Got: #{arg}"
      )
    )
  end

  @doc """
  Raises an exception.

  If `message` is a string, it raises a `RuntimeError` exception with it.

  If `message` is an atom, it just calls `raise/2` with the atom as the first
  argument and `[]` as the second one.

  If `message` is an exception struct, it is raised as is.

  If `message` is anything else, `raise` will fail with an `ArgumentError`
  exception.

  ## Examples

      iex> raise "oops"
      ** (RuntimeError) oops

      try do
        1 + :foo
      rescue
        x in [ArithmeticError] ->
          IO.puts("that was expected")
          raise x
      end

  """
  defmacro raise(message) do
    # Try to figure out the type at compilation time
    # to avoid dead code and make Dialyzer happy.
    message =
      case not is_binary(message) and bootstrapped?(Macro) do
        true -> Macro.expand(message, __CALLER__)
        false -> message
      end

    erlang_error =
      fn x ->
        quote do
          :erlang.error(unquote(x), :none, error_info: %{module: Exception})
        end
      end

    case message do
      message when is_binary(message) ->
        erlang_error.(quote do: RuntimeError.exception(unquote(message)))

      {:<<>>, _, _} = message ->
        erlang_error.(quote do: RuntimeError.exception(unquote(message)))

      alias when is_atom(alias) ->
        erlang_error.(quote do: unquote(alias).exception([]))

      _ ->
        erlang_error.(quote do: Kernel.Utils.raise(unquote(message)))
    end
  end

  @doc """
  Raises an exception.

  Calls the `exception/1` function on the given argument (which has to be a
  module name like `ArgumentError` or `RuntimeError`) passing `attributes`
  in order to retrieve the exception struct.

  Any module that contains a call to the `defexception/1` macro automatically
  implements the `c:Exception.exception/1` callback expected by `raise/2`.
  For more information, see `defexception/1`.

  ## Examples

      iex> raise(ArgumentError, "Sample")
      ** (ArgumentError) Sample

  """
  defmacro raise(exception, attributes) do
    quote do
      :erlang.error(unquote(exception).exception(unquote(attributes)))
    end
  end

  @doc """
  Raises an exception preserving a previous stacktrace.

  Works like `raise/1` but does not generate a new stacktrace.

  Note that `__STACKTRACE__` can be used inside catch/rescue
  to retrieve the current stacktrace.

  ## Examples

      try do
        raise "oops"
      rescue
        exception ->
          reraise exception, __STACKTRACE__
      end

  """
  defmacro reraise(message, stacktrace) do
    # Try to figure out the type at compilation time
    # to avoid dead code and make Dialyzer happy.
    case Macro.expand(message, __CALLER__) do
      message when is_binary(message) ->
        quote do
          :erlang.error(
            :erlang.raise(:error, RuntimeError.exception(unquote(message)), unquote(stacktrace))
          )
        end

      {:<<>>, _, _} = message ->
        quote do
          :erlang.error(
            :erlang.raise(:error, RuntimeError.exception(unquote(message)), unquote(stacktrace))
          )
        end

      alias when is_atom(alias) ->
        quote do
          :erlang.error(:erlang.raise(:error, unquote(alias).exception([]), unquote(stacktrace)))
        end

      message ->
        quote do
          :erlang.error(
            :erlang.raise(:error, Kernel.Utils.raise(unquote(message)), unquote(stacktrace))
          )
        end
    end
  end

  @doc """
  Raises an exception preserving a previous stacktrace.

  `reraise/3` works like `reraise/2`, except it passes arguments to the
  `exception/1` function as explained in `raise/2`.

  ## Examples

      try do
        raise "oops"
      rescue
        exception ->
          reraise WrapperError, [exception: exception], __STACKTRACE__
      end

  """
  defmacro reraise(exception, attributes, stacktrace) do
    quote do
      :erlang.raise(
        :error,
        unquote(exception).exception(unquote(attributes)),
        unquote(stacktrace)
      )
    end
  end

  @doc """
  Text-based match operator. Matches the term on the `left`
  against the regular expression or string on the `right`.

  If `right` is a regular expression, returns `true` if `left` matches right.

  If `right` is a string, returns `true` if `left` contains `right`.

  ## Examples

      iex> "abcd" =~ ~r/c(d)/
      true

      iex> "abcd" =~ ~r/e/
      false

      iex> "abcd" =~ ~r//
      true

      iex> "abcd" =~ "bc"
      true

      iex> "abcd" =~ "ad"
      false

      iex> "abcd" =~ "abcd"
      true

      iex> "abcd" =~ ""
      true

  For more information about regular expressions, please check the `Regex` module.
  """
  @spec String.t() =~ (String.t() | Regex.t()) :: boolean
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

      iex> inspect([1, 2, 3, 4, 5], limit: 3)
      "[1, 2, 3, ...]"

      iex> inspect([1, 2, 3], pretty: true, width: 0)
      "[1,\n 2,\n 3]"

      iex> inspect("olá" <> <<0>>)
      "<<111, 108, 195, 161, 0>>"

      iex> inspect("olá" <> <<0>>, binaries: :as_strings)
      "\"olá\\0\""

      iex> inspect("olá", binaries: :as_binaries)
      "<<111, 108, 195, 161>>"

      iex> inspect(~c"bar")
      "~c\"bar\""

      iex> inspect([0 | ~c"bar"])
      "[0, 98, 97, 114]"

      iex> inspect(100, base: :octal)
      "0o144"

      iex> inspect(100, base: :hex)
      "0x64"

  Note that the `Inspect` protocol does not necessarily return a valid
  representation of an Elixir term. In such cases, the inspected result
  must start with `#`. For example, inspecting a function will return:

      inspect(fn a, b -> a + b end)
      #=> #Function<...>

  The `Inspect` protocol can be derived to hide certain fields
  from structs, so they don't show up in logs, inspects and similar.
  See the "Deriving" section of the documentation of the `Inspect`
  protocol for more information.
  """
  @spec inspect(Inspect.t(), keyword) :: String.t()
  def inspect(term, opts \\ []) when is_list(opts) do
    opts = Inspect.Opts.new(opts)

    limit =
      case opts.pretty do
        true -> opts.width
        false -> :infinity
      end

    doc = Inspect.Algebra.group(Inspect.Algebra.to_doc(term, opts))
    IO.iodata_to_binary(Inspect.Algebra.format(doc, limit))
  end

  @doc """
  Creates and updates a struct.

  The `struct` argument may be an atom (which defines `defstruct`)
  or a `struct` itself. The second argument is any `Enumerable` that
  emits two-element tuples (key-value pairs) during enumeration.

  Keys in the `Enumerable` that don't exist in the struct are automatically
  discarded. Note that keys must be atoms, as only atoms are allowed when
  defining a struct. If there are duplicate keys in the `Enumerable`, the last
  entry will be taken (same behavior as `Map.new/1`).

  This function is useful for dynamically creating and updating structs, as
  well as for converting maps to structs; in the latter case, just inserting
  the appropriate `:__struct__` field into the map may not be enough and
  `struct/2` should be used instead.

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

      # String keys are ignored
      struct(User, %{"name" => "meg"})
      #=> %User{name: "john"}

  """
  @spec struct(module | struct, Enumerable.t()) :: struct
  def struct(struct, fields \\ []) do
    struct(struct, fields, fn
      {:__struct__, _val}, acc ->
        acc

      {key, val}, acc ->
        case acc do
          %{^key => _} -> %{acc | key => val}
          _ -> acc
        end
    end)
  end

  @doc """
  Similar to `struct/2` but checks for key validity.

  The function `struct!/2` emulates the compile time behavior
  of structs. This means that:

    * when building a struct, as in `struct!(SomeStruct, key: :value)`,
      it is equivalent to `%SomeStruct{key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      If the struct is enforcing any key via `@enforce_keys`, those will
      be enforced as well;

    * when updating a struct, as in `struct!(%SomeStruct{}, key: :value)`,
      it is equivalent to `%SomeStruct{struct | key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      However, updating structs does not enforce keys, as keys are enforced
      only when building;

  """
  @spec struct!(module | struct, Enumerable.t()) :: struct
  def struct!(struct, fields \\ [])

  def struct!(struct, fields) when is_atom(struct) do
    validate_struct!(struct.__struct__(fields), struct, 1)
  end

  def struct!(struct, fields) when is_map(struct) do
    struct(struct, fields, fn
      {:__struct__, _}, acc ->
        acc

      {key, val}, acc ->
        Map.replace!(acc, key, val)
    end)
  end

  defp struct(struct, [], _fun) when is_atom(struct) do
    validate_struct!(struct.__struct__(), struct, 0)
  end

  defp struct(struct, fields, fun) when is_atom(struct) do
    struct(validate_struct!(struct.__struct__(), struct, 0), fields, fun)
  end

  defp struct(%_{} = struct, [], _fun) do
    struct
  end

  defp struct(%_{} = struct, fields, fun) do
    Enum.reduce(fields, struct, fun)
  end

  defp validate_struct!(%{__struct__: module} = struct, module, _arity) do
    struct
  end

  defp validate_struct!(%{__struct__: struct_name}, module, arity) when is_atom(struct_name) do
    error_message =
      "expected struct name returned by #{inspect(module)}.__struct__/#{arity} to be " <>
        "#{inspect(module)}, got: #{inspect(struct_name)}"

    :erlang.error(ArgumentError.exception(error_message))
  end

  defp validate_struct!(expr, module, arity) do
    error_message =
      "expected #{inspect(module)}.__struct__/#{arity} to return a map with a :__struct__ " <>
        "key that holds the name of the struct (atom), got: #{inspect(expr)}"

    :erlang.error(ArgumentError.exception(error_message))
  end

  @doc """
  Returns `true` if `term` is a struct, otherwise returns `false`.

  Allowed in guard tests.

  ## Examples

      iex> is_struct(URI.parse("/"))
      true

      iex> is_struct(%{})
      false

  """
  @doc since: "1.10.0", guard: true
  defmacro is_struct(term) do
    case __CALLER__.context do
      nil ->
        quote do
          case unquote(term) do
            %_{} -> true
            _ -> false
          end
        end

      :match ->
        invalid_match!(:is_struct)

      :guard ->
        quote do
          is_map(unquote(term)) and :erlang.is_map_key(:__struct__, unquote(term)) and
            is_atom(:erlang.map_get(:__struct__, unquote(term)))
        end
    end
  end

  @doc """
  Returns `true` if `term` is a struct of `name`, otherwise returns `false`.

  `is_struct/2` does not check that `name` exists and is a valid struct.
  If you want such validations, you must pattern match on the struct
  instead, such as `match?(%URI{}, arg)`.

  Allowed in guard tests.

  ## Examples

      iex> is_struct(URI.parse("/"), URI)
      true

      iex> is_struct(URI.parse("/"), Macro.Env)
      false

  """
  @doc since: "1.11.0", guard: true
  defmacro is_struct(term, name) do
    case __CALLER__.context do
      nil ->
        quote generated: true do
          case unquote(name) do
            name when is_atom(name) ->
              case unquote(term) do
                %{__struct__: ^name} -> true
                _ -> false
              end

            _ ->
              raise ArgumentError
          end
        end

      :match ->
        invalid_match!(:is_struct)

      :guard ->
        quote do
          is_map(unquote(term)) and
            (is_atom(unquote(name)) or :fail) and
            :erlang.is_map_key(:__struct__, unquote(term)) and
            :erlang.map_get(:__struct__, unquote(term)) == unquote(name)
        end
    end
  end

  @doc """
  Returns `true` if `term` is a map that is not a struct, otherwise
  returns `false`.

  Allowed in guard tests.

  ## Examples

      iex> is_non_struct_map(%{})
      true

      iex> is_non_struct_map(URI.parse("/"))
      false

      iex> is_non_struct_map(nil)
      false

  """
  @doc since: "1.17.0", guard: true
  defmacro is_non_struct_map(term) do
    case __CALLER__.context do
      nil ->
        quote do
          case unquote(term) do
            %_{} -> false
            %{} -> true
            _ -> false
          end
        end

      :match ->
        invalid_match!(:is_non_struct_map)

      :guard ->
        quote do
          is_map(unquote(term)) and
            not (:erlang.is_map_key(:__struct__, unquote(term)) and
                   is_atom(:erlang.map_get(:__struct__, unquote(term))))
        end
    end
  end

  @doc """
  Returns `true` if `term` is an exception, otherwise returns `false`.

  Allowed in guard tests.

  ## Examples

      iex> is_exception(%RuntimeError{})
      true

      iex> is_exception(%{})
      false

  """
  @doc since: "1.11.0", guard: true
  defmacro is_exception(term) do
    case __CALLER__.context do
      nil ->
        quote do
          case unquote(term) do
            %_{__exception__: true} -> true
            _ -> false
          end
        end

      :match ->
        invalid_match!(:is_exception)

      :guard ->
        quote do
          is_map(unquote(term)) and :erlang.is_map_key(:__struct__, unquote(term)) and
            is_atom(:erlang.map_get(:__struct__, unquote(term))) and
            :erlang.is_map_key(:__exception__, unquote(term)) and
            :erlang.map_get(:__exception__, unquote(term)) == true
        end
    end
  end

  @doc """
  Returns `true` if `term` is an exception of `name`, otherwise returns `false`.

  Allowed in guard tests.

  ## Examples

      iex> is_exception(%RuntimeError{}, RuntimeError)
      true

      iex> is_exception(%RuntimeError{}, Macro.Env)
      false

  """
  @doc since: "1.11.0", guard: true
  defmacro is_exception(term, name) do
    case __CALLER__.context do
      nil ->
        quote do
          case unquote(name) do
            name when is_atom(name) ->
              case unquote(term) do
                %{__struct__: ^name, __exception__: true} -> true
                _ -> false
              end

            _ ->
              raise ArgumentError
          end
        end

      :match ->
        invalid_match!(:is_exception)

      :guard ->
        quote do
          is_map(unquote(term)) and
            (is_atom(unquote(name)) or :fail) and
            :erlang.is_map_key(:__struct__, unquote(term)) and
            :erlang.map_get(:__struct__, unquote(term)) == unquote(name) and
            :erlang.is_map_key(:__exception__, unquote(term)) and
            :erlang.map_get(:__exception__, unquote(term)) == true
        end
    end
  end

  @doc """
  Pipes the first argument, `value`, into the second argument, a function `fun`,
  and returns the result of calling `fun`.

  In other words, it invokes the function `fun` with `value` as argument,
  and returns its result.

  This is most commonly used in pipelines, using the `|>/2` operator, allowing you
  to pipe a value to a function outside of its first argument.

  ### Examples

      iex> 1 |> then(fn x -> x * 2 end)
      2

      iex> 1 |> then(fn x -> Enum.drop(["a", "b", "c"], x) end)
      ["b", "c"]
  """
  @doc since: "1.12.0"
  defmacro then(value, fun) do
    quote do
      unquote(fun).(unquote(value))
    end
  end

  @doc """
  Gets a value from a nested structure with nil-safe handling.

  Uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a
  function, which is detailed in a later section.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_in(users, ["john", :age])
      27
      iex> # Equivalent to:
      iex> users["john"][:age]
      27

  `get_in/2` can also use the accessors in the `Access` module
  to traverse more complex data structures. For example, here we
  use `Access.all/0` to traverse a list:

      iex> users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
      iex> get_in(users, [Access.all(), :age])
      [27, 23]

  In case any of the components returns `nil`, `nil` will be returned
  and `get_in/2` won't traverse any further:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_in(users, ["unknown", :age])
      nil
      iex> # Equivalent to:
      iex> users["unknown"][:age]
      nil

  ## Functions as keys

  If a key given to `get_in/2` is a function, the function will be invoked
  passing three arguments:

    * the operation (`:get`)
    * the data to be accessed
    * a function to be invoked next

  This means `get_in/2` can be extended to provide custom lookups.
  That's precisely how the `Access.all/0` key in the previous section
  behaves. For example, we can manually implement such traversal as
  follows:

      iex> users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
      iex> all = fn :get, data, next -> Enum.map(data, next) end
      iex> get_in(users, [all, :age])
      [27, 23]

  The `Access` module ships with many convenience accessor functions.
  See `Access.all/0`, `Access.key/2`, and others as examples.

  ## Working with structs

  By default, structs do not implement the `Access` behaviour required
  by this function. Therefore, you can't do this:

      get_in(some_struct, [:some_key, :nested_key])

  There are two alternatives. Given structs have predefined keys,
  we can use the `struct.field` notation:

      some_struct.some_key.nested_key

  However, the code above will fail if any of the values return `nil`.
  If you also want to handle nil values, you can use `get_in/1`:

      get_in(some_struct.some_key.nested_key)

  Pattern-matching is another option for handling such cases,
  which can be especially useful if you want to match on several
  fields at once or provide custom return values:

      case some_struct do
        %{some_key: %{nested_key: value}} -> value
        %{} -> nil
      end

  """
  @spec get_in(Access.t(), nonempty_list(term)) :: term
  def get_in(data, keys)

  def get_in(nil, [_ | _]), do: nil

  def get_in(data, [h]) when is_function(h), do: h.(:get, data, & &1)
  def get_in(data, [h | t]) when is_function(h), do: h.(:get, data, &get_in(&1, t))

  def get_in(data, [h]), do: Access.get(data, h)
  def get_in(data, [h | t]), do: get_in(Access.get(data, h), t)

  @doc """
  Puts a value in a nested structure.

  Uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users, ["john", :age], 28)
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  If any of the intermediate values are nil, it will raise:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users, ["jane", :age], "oops")
      ** (ArgumentError) could not put/update key :age on a nil value

  """
  @spec put_in(Access.t(), nonempty_list(term), term) :: Access.t()
  def put_in(data, [_ | _] = keys, value) do
    elem(get_and_update_in(data, keys, fn _ -> {nil, value} end), 1)
  end

  @doc """
  Updates a key in a nested structure.

  Uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  `data` is a nested structure (that is, a map, keyword
  list, or struct that implements the `Access` behaviour).
  The `fun` argument receives the value of `key` (or `nil`
  if `key` is not present) and the result replaces the value
  in the structure.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users, ["john", :age], &(&1 + 1))
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  Note the current value given to the anonymous function may be `nil`.
  If any of the intermediate values are nil, it will raise:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users, ["jane", :age], & &1 + 1)
      ** (ArgumentError) could not put/update key :age on a nil value

  """
  @spec update_in(Access.t(), nonempty_list(term), (term -> term)) :: Access.t()
  def update_in(data, [_ | _] = keys, fun) when is_function(fun) do
    elem(get_and_update_in(data, keys, fn x -> {nil, fun.(x)} end), 1)
  end

  @doc """
  Gets a value and updates a nested structure.

  `data` is a nested structure (that is, a map, keyword
  list, or struct that implements the `Access` behaviour).

  The `fun` argument receives the value of `key` (or `nil` if `key`
  is not present) and must return one of the following values:

    * a two-element tuple `{current_value, new_value}`. In this case,
      `current_value` is the retrieved value which can possibly be operated on before
      being returned. `new_value` is the new value to be stored under `key`.

    * `:pop`, which implies that the current value under `key`
      should be removed from the structure and returned.

  This function uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a function,
  which is detailed in a later section.

  ## Examples

  This function is useful when there is a need to retrieve the current
  value (or something calculated in function of the current value) and
  update it at the same time. For example, it could be used to read the
  current age of a user while increasing it by one in one pass:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_and_update_in(users, ["john", :age], &{&1, &1 + 1})
      {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

  Note the current value given to the anonymous function may be `nil`.
  If any of the intermediate values are nil, it will raise:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_and_update_in(users, ["jane", :age], &{&1, &1 + 1})
      ** (ArgumentError) could not put/update key :age on a nil value

  ## Functions as keys

  If a key is a function, the function will be invoked passing three
  arguments:

    * the operation (`:get_and_update`)
    * the data to be accessed
    * a function to be invoked next

  This means `get_and_update_in/3` can be extended to provide custom
  lookups. The downside is that functions cannot be stored as keys
  in the accessed data structures.

  When one of the keys is a function, the function is invoked.
  In the example below, we use a function to get and increment all
  ages inside a list:

      iex> users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
      iex> all = fn :get_and_update, data, next ->
      ...>   data |> Enum.map(next) |> Enum.unzip()
      ...> end
      iex> get_and_update_in(users, [all, :age], &{&1, &1 + 1})
      {[27, 23], [%{name: "john", age: 28}, %{name: "meg", age: 24}]}

  If the previous value before invoking the function is `nil`,
  the function *will* receive `nil` as a value and must handle it
  accordingly (be it by failing or providing a sane default).

  The `Access` module ships with many convenience accessor functions,
  like the `all` anonymous function defined above. See `Access.all/0`,
  `Access.key/2`, and others as examples.
  """
  @spec get_and_update_in(
          structure,
          keys,
          (term | nil -> {current_value, new_value} | :pop)
        ) :: {current_value, new_structure :: structure}
        when structure: Access.t(),
             keys: nonempty_list(term),
             current_value: Access.value(),
             new_value: Access.value()
  def get_and_update_in(data, keys, fun)

  def get_and_update_in(data, [head], fun) when is_function(head, 3),
    do: head.(:get_and_update, data, fun)

  def get_and_update_in(data, [head | tail], fun) when is_function(head, 3),
    do: head.(:get_and_update, data, &get_and_update_in(&1, tail, fun))

  def get_and_update_in(data, [head], fun) when is_function(fun, 1),
    do: Access.get_and_update(data, head, fun)

  def get_and_update_in(data, [head | tail], fun) when is_function(fun, 1),
    do: Access.get_and_update(data, head, &get_and_update_in(&1, tail, fun))

  @doc """
  Pops a key from the given nested structure.

  Uses the `Access` protocol to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> pop_in(users, ["john", :age])
      {27, %{"john" => %{}, "meg" => %{age: 23}}}

  In case any entry returns `nil`, its key will be removed
  and the deletion will be considered a success.

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> pop_in(users, ["jane", :age])
      {nil, %{"john" => %{age: 27}, "meg" => %{age: 23}}}

  """
  @spec pop_in(data, nonempty_list(Access.get_and_update_fun(term, data) | term)) :: {term, data}
        when data: Access.container()
  def pop_in(data, keys)

  def pop_in(nil, [key | _]) do
    raise ArgumentError, "could not pop key #{inspect(key)} on a nil value"
  end

  def pop_in(data, [_ | _] = keys) do
    pop_in_data(data, keys)
  end

  defp pop_in_data(nil, [_ | _]), do: :pop

  defp pop_in_data(data, [fun]) when is_function(fun),
    do: fun.(:get_and_update, data, fn _ -> :pop end)

  defp pop_in_data(data, [fun | tail]) when is_function(fun),
    do: fun.(:get_and_update, data, &pop_in_data(&1, tail))

  defp pop_in_data(data, [key]), do: Access.pop(data, key)

  defp pop_in_data(data, [key | tail]),
    do: Access.get_and_update(data, key, &pop_in_data(&1, tail))

  @doc """
  Gets a key from the nested structure via the given `path`, with
  nil-safe handling.

  This is similar to `get_in/2`, except the path is extracted via
  a macro rather than passing a list. For example:

      get_in(opts[:foo][:bar])

  Is equivalent to:

      get_in(opts, [:foo, :bar])

  Additionally, this macro can traverse structs:

      get_in(struct.foo.bar)

  In case any of the keys returns `nil`, then `nil` will be returned
  and `get_in/1` won't traverse any further.

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. For more information about the supported path
  expressions, please check `get_and_update_in/2` docs.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_in(users["john"].age)
      27
      iex> get_in(users["unknown"].age)
      nil

  """
  defmacro get_in(path) do
    {[h | t], _} = unnest(path, [], true, "get_in/1")
    nest_get_in(h, quote(do: x), t)
  end

  defp nest_get_in(h, _var, []) do
    h
  end

  defp nest_get_in(h, var, [{:map, key} | tail]) do
    quote generated: true do
      case unquote(h) do
        %{unquote(key) => unquote(var)} -> unquote(nest_get_in(var, var, tail))
        nil -> nil
        unquote(var) -> :erlang.error({:badkey, unquote(key), unquote(var)})
      end
    end
  end

  defp nest_get_in(h, var, [{:access, key} | tail]) do
    h = quote do: Access.get(unquote(h), unquote(key))
    nest_get_in(h, var, tail)
  end

  @doc """
  Puts a value in a nested structure via the given `path`.

  This is similar to `put_in/3`, except the path is extracted via
  a macro rather than passing a list. For example:

      put_in(opts[:foo][:bar], :baz)

  Is equivalent to:

      put_in(opts, [:foo, :bar], :baz)

  This also works with nested structs and the `struct.path.to.value` way to specify
  paths:

      put_in(struct.foo.bar, :baz)

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
    case unnest(path, [], true, "put_in/2") do
      {[h | t], true} ->
        nest_map_update_in(h, t, quote(do: fn _ -> unquote(value) end))

      {[h | t], false} ->
        expr = nest_get_and_update_in(h, t, quote(do: fn _ -> {nil, unquote(value)} end))
        quote(do: :erlang.element(2, unquote(expr)))
    end
  end

  @doc """
  Pops a key from the nested structure via the given `path`.

  This is similar to `pop_in/2`, except the path is extracted via
  a macro rather than passing a list. For example:

      pop_in(opts[:foo][:bar])

  Is equivalent to:

      pop_in(opts, [:foo, :bar])

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. For more information about the supported path
  expressions, please check `get_and_update_in/2` docs.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> pop_in(users["john"][:age])
      {27, %{"john" => %{}, "meg" => %{age: 23}}}

      iex> users = %{john: %{age: 27}, meg: %{age: 23}}
      iex> pop_in(users.john[:age])
      {27, %{john: %{}, meg: %{age: 23}}}

  In case any entry returns `nil`, its key will be removed
  and the deletion will be considered a success.
  """
  defmacro pop_in(path) do
    {[h | t], _} = unnest(path, [], true, "pop_in/1")
    nest_pop_in(:map, h, t)
  end

  @doc """
  Updates a nested structure via the given `path`.

  This is similar to `update_in/3`, except the path is extracted via
  a macro rather than passing a list. For example:

      update_in(opts[:foo][:bar], &(&1 + 1))

  Is equivalent to:

      update_in(opts, [:foo, :bar], &(&1 + 1))

  This also works with nested structs and the `struct.path.to.value` way to specify
  paths:

      update_in(struct.foo.bar, &(&1 + 1))

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
    case unnest(path, [], true, "update_in/2") do
      {[h | t], true} ->
        nest_map_update_in(h, t, fun)

      {[h | t], false} ->
        expr = nest_get_and_update_in(h, t, quote(do: fn x -> {nil, unquote(fun).(x)} end))
        quote(do: :erlang.element(2, unquote(expr)))
    end
  end

  @doc """
  Gets a value and updates a nested data structure via the given `path`.

  This is similar to `get_and_update_in/3`, except the path is extracted
  via a macro rather than passing a list. For example:

      get_and_update_in(opts[:foo][:bar], &{&1, &1 + 1})

  Is equivalent to:

      get_and_update_in(opts, [:foo, :bar], &{&1, &1 + 1})

  This also works with nested structs and the `struct.path.to.value` way to specify
  paths:

      get_and_update_in(struct.foo.bar, &{&1, &1 + 1})

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. See the "Paths" section below.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_and_update_in(users["john"].age, &{&1, &1 + 1})
      {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

  ## Paths

  A path may start with a variable, local or remote call, and must be
  followed by one or more:

    * `foo[bar]` - accesses the key `bar` in `foo`; in case `foo` is nil,
      `nil` is returned

    * `foo.bar` - accesses a map/struct field; in case the field is not
      present, an error is raised

  Here are some valid paths:

      users["john"][:age]
      users["john"].age
      User.all()["john"].age
      all_users()["john"].age

  Here are some invalid ones:

      # Does a remote call after the initial value
      users["john"].do_something(arg1, arg2)

      # Does not access any key or field
      users

  """
  defmacro get_and_update_in(path, fun) do
    {[h | t], _} = unnest(path, [], true, "get_and_update_in/2")
    nest_get_and_update_in(h, t, fun)
  end

  defp nest_map_update_in([], fun), do: fun

  defp nest_map_update_in(list, fun) do
    quote do
      fn x -> unquote(nest_map_update_in(quote(do: x), list, fun)) end
    end
  end

  defp nest_map_update_in(h, [{:map, key} | t], fun) do
    quote do
      Map.update!(unquote(h), unquote(key), unquote(nest_map_update_in(t, fun)))
    end
  end

  defp nest_get_and_update_in([], fun), do: fun

  defp nest_get_and_update_in(list, fun) do
    quote do
      fn x -> unquote(nest_get_and_update_in(quote(do: x), list, fun)) end
    end
  end

  defp nest_get_and_update_in(h, [{:access, key} | t], fun) do
    quote do
      Access.get_and_update(unquote(h), unquote(key), unquote(nest_get_and_update_in(t, fun)))
    end
  end

  defp nest_get_and_update_in(h, [{:map, key} | t], fun) do
    quote do
      Map.get_and_update!(unquote(h), unquote(key), unquote(nest_get_and_update_in(t, fun)))
    end
  end

  defp nest_pop_in(kind, list) do
    quote do
      fn x -> unquote(nest_pop_in(kind, quote(do: x), list)) end
    end
  end

  defp nest_pop_in(:map, h, [{:access, key}]) do
    quote do
      case unquote(h) do
        nil -> {nil, nil}
        h -> Access.pop(h, unquote(key))
      end
    end
  end

  defp nest_pop_in(_, _, [{:map, key}]) do
    raise ArgumentError,
          "cannot use pop_in when the last segment is a map/struct field. " <>
            "This would effectively remove the field #{inspect(key)} from the map/struct"
  end

  defp nest_pop_in(_, h, [{:map, key} | t]) do
    quote do
      Map.get_and_update!(unquote(h), unquote(key), unquote(nest_pop_in(:map, t)))
    end
  end

  defp nest_pop_in(_, h, [{:access, key}]) do
    quote do
      case unquote(h) do
        nil -> :pop
        h -> Access.pop(h, unquote(key))
      end
    end
  end

  defp nest_pop_in(_, h, [{:access, key} | t]) do
    quote do
      Access.get_and_update(unquote(h), unquote(key), unquote(nest_pop_in(:access, t)))
    end
  end

  defp unnest({{:., _, [Access, :get]}, _, [expr, key]}, acc, _all_map?, kind) do
    unnest(expr, [{:access, key} | acc], false, kind)
  end

  defp unnest({{:., _, [expr, key]}, _, []}, acc, all_map?, kind)
       when is_tuple(expr) and :erlang.element(1, expr) != :__aliases__ and
              :erlang.element(1, expr) != :__MODULE__ do
    unnest(expr, [{:map, key} | acc], all_map?, kind)
  end

  defp unnest(other, [], _all_map?, kind) do
    raise ArgumentError,
          "expected expression given to #{kind} to access at least one element, " <>
            "got: #{Macro.to_string(other)}"
  end

  defp unnest(other, acc, all_map?, kind) do
    case proper_start?(other) do
      true ->
        {[other | acc], all_map?}

      false ->
        raise ArgumentError,
              "expression given to #{kind} must start with a variable, local or remote call " <>
                "and be followed by an element access, got: #{Macro.to_string(other)}"
    end
  end

  defp proper_start?({{:., _, [expr, _]}, _, _args})
       when is_atom(expr)
       when :erlang.element(1, expr) == :__aliases__
       when :erlang.element(1, expr) == :__MODULE__,
       do: true

  defp proper_start?({atom, _, _args})
       when is_atom(atom),
       do: true

  defp proper_start?(other), do: not is_tuple(other)

  @doc """
  Converts the argument to a string according to the
  `String.Chars` protocol.

  This is the function invoked when there is string interpolation.

  ## Examples

      iex> to_string(:foo)
      "foo"

  """
  defmacro to_string(term) do
    quote(do: :"Elixir.String.Chars".to_string(unquote(term)))
  end

  @doc """
  Converts the given term to a charlist according to the `List.Chars` protocol.

  ## Examples

      iex> to_charlist(:foo)
      ~c"foo"

  """
  defmacro to_charlist(term) do
    quote(do: List.Chars.to_charlist(unquote(term)))
  end

  @doc """
  Returns `true` if `term` is `nil`, `false` otherwise.

  Allowed in guard clauses.

  ## Examples

      iex> is_nil(1 + 2)
      false

      iex> is_nil(nil)
      true

  """
  @doc guard: true
  defmacro is_nil(term) do
    quote(do: unquote(term) == nil)
  end

  @doc """
  A convenience macro that checks if the right side (an expression) matches the
  left side (a pattern).

  ## Examples

      iex> match?(1, 1)
      true

      iex> match?({1, _}, {1, 2})
      true

      iex> map = %{a: 1, b: 2}
      iex> match?(%{a: _}, map)
      true

      iex> a = 1
      iex> match?(^a, 1)
      true

  `match?/2` is very useful when filtering or finding a value in an enumerable:

      iex> list = [a: 1, b: 2, a: 3]
      iex> Enum.filter(list, &match?({:a, _}, &1))
      [a: 1, a: 3]

  Guard clauses can also be given to the match:

      iex> list = [a: 1, b: 2, a: 3]
      iex> Enum.filter(list, &match?({:a, x} when x < 2, &1))
      [a: 1]

  Variables assigned in the match will not be available outside of the
  function call (unlike regular pattern matching with the `=` operator):

      iex> match?(_x, 1)
      true
      iex> binding()
      []

  ## Values vs patterns

  Remember the pin operator matches _values_, not _patterns_.
  Passing a variable as the pattern will always return `true` and will
  result in a warning that the variable is unused:

      # don't do this
      pattern = %{a: :a}
      match?(pattern, %{b: :b})

  Similarly, moving an expression out the pattern may no longer preserve
  its semantics. For example:

      match?([_ | _], [1, 2, 3])
      #=> true

      pattern = [_ | _]
      match?(pattern, [1, 2, 3])
      ** (CompileError) invalid use of _. _ can only be used inside patterns to ignore values and cannot be used in expressions. Make sure you are inside a pattern or change it accordingly

  Another example is that a map as a pattern performs a subset match, but not
  once assigned to a variable:

      match?(%{x: 1}, %{x: 1, y: 2})
      #=> true

      attrs = %{x: 1}
      match?(^attrs, %{x: 1, y: 2})
      #=> false

  The pin operator will check if the values are equal, using `===/2`, while
  patterns have their own rules when matching maps, lists, and so forth.
  Such behavior is not specific to `match?/2`. The following code also
  throws an exception:

      attrs = %{x: 1}
      ^attrs = %{x: 1, y: 2}
      #=> (MatchError) no match of right hand side value: %{x: 1, y: 2}

  """
  defmacro match?(pattern, expr) do
    success =
      quote do
        unquote(pattern) -> true
      end

    failure =
      quote generated: true do
        _ -> false
      end

    {:case, [], [expr, [do: success ++ failure]]}
  end

  @doc """
  Module attribute unary operator.

  Reads and writes attributes in the current module.

  The canonical example for attributes is annotating that a module
  implements an OTP behaviour, such as `GenServer`:

      defmodule MyServer do
        @behaviour GenServer
        # ... callbacks ...
      end

  By default Elixir supports all the module attributes supported by Erlang, but
  custom attributes can be used as well:

      defmodule MyServer do
        @my_data 13
        IO.inspect(@my_data)
        #=> 13
      end

  Unlike Erlang, such attributes are not stored in the module by default since
  it is common in Elixir to use custom attributes to store temporary data that
  will be available at compile-time. Custom attributes may be configured to
  behave closer to Erlang by using `Module.register_attribute/3`.

  > #### Prefixing module attributes {: .tip}
  >
  > Libraries and frameworks should consider prefixing any
  > module attributes that are private by underscore, such as `@_my_data`,
  > so code completion tools do not show them on suggestions and prompts.

  Finally, note that attributes can also be read inside functions:

      defmodule MyServer do
        @my_data 11
        def first_data, do: @my_data
        @my_data 13
        def second_data, do: @my_data
      end

      MyServer.first_data()
      #=> 11

      MyServer.second_data()
      #=> 13

  It is important to note that reading an attribute takes a snapshot of
  its current value. In other words, the value is read at compilation
  time and not at runtime. Check the `Module` module for other functions
  to manipulate module attributes.

  ## Attention! Multiple references of the same attribute

  As mentioned above, every time you read a module attribute, a snapshot
  of its current value is taken. Therefore, if you are storing large
  values inside module attributes (for example, embedding external files
  in module attributes), you should avoid referencing the same attribute
  multiple times. For example, don't do this:

      @files %{
        example1: File.read!("lib/example1.data"),
        example2: File.read!("lib/example2.data")
      }

      def example1, do: @files[:example1]
      def example2, do: @files[:example2]

  In the above, each reference to `@files` may end-up with a complete
  and individual copy of the whole `@files` module attribute. Instead,
  reference the module attribute once in a private function:

      @files %{
        example1: File.read!("lib/example1.data"),
        example2: File.read!("lib/example2.data")
      }

      defp files(), do: @files
      def example1, do: files()[:example1]
      def example2, do: files()[:example2]

  """
  defmacro @expr

  defmacro @{:__aliases__, _meta, _args} do
    raise ArgumentError, "module attributes set via @ cannot start with an uppercase letter"
  end

  defmacro @{name, meta, args} do
    assert_module_scope(__CALLER__, :@, 1)
    function? = __CALLER__.function != nil

    cond do
      # Check for Macro as it is compiled later than Kernel
      not bootstrapped?(Macro) ->
        nil

      not function? and (__CALLER__.context == :match or __CALLER__.context == :guard) ->
        raise ArgumentError,
              """
              invalid usage of module attributes. Module attributes cannot be used inside \
              pattern matching (and guards) outside of a function. If you are trying to \
              define an attribute, do not do this:

                  @foo = :value

              Instead, do this:

                  @foo :value
              """

      # Typespecs attributes are currently special cased by the compiler
      is_list(args) and typespec?(name) ->
        case bootstrapped?(Kernel.Typespec) do
          false ->
            :ok

          true ->
            pos = :elixir_locals.cache_env(__CALLER__)
            %{line: line, file: file, module: module} = __CALLER__

            quote do
              Kernel.Typespec.deftypespec(
                unquote(name),
                unquote(Macro.escape(hd(args), unquote: true)),
                unquote(line),
                unquote(file),
                unquote(module),
                unquote(pos)
              )
            end
        end

      true ->
        do_at(args, meta, name, function?, __CALLER__)
    end
  end

  # @attribute(value)
  defp do_at([arg], meta, name, function?, env) do
    line =
      case :lists.keymember(:context, 1, meta) do
        true -> nil
        false -> env.line
      end

    cond do
      function? ->
        raise ArgumentError, "cannot set attribute @#{name} inside function/macro"

      name == :behavior ->
        raise ArgumentError, "@behavior attribute is not supported, please use @behaviour instead"

      :lists.member(name, [:moduledoc, :typedoc, :doc]) ->
        arg = {env.line, arg}

        quote do
          Module.__put_attribute__(__MODULE__, unquote(name), unquote(arg), unquote(line), [])
        end

      true ->
        {arg, traces} = collect_traces(name, arg, env)

        quote do
          Module.__put_attribute__(
            __MODULE__,
            unquote(name),
            unquote(arg),
            unquote(line),
            unquote(Macro.escape(traces))
          )
        end
    end
  end

  # @attribute()
  defp do_at([], meta, name, function?, env) do
    IO.warn(
      "the @#{name}() notation (with parentheses) is deprecated, please use @#{name} (without parentheses) instead",
      env
    )

    do_at(nil, meta, name, function?, env)
  end

  # @attribute
  defp do_at(args, _meta, name, function?, env) when is_atom(args) do
    line = env.line
    doc_attr? = :lists.member(name, [:moduledoc, :typedoc, :doc])

    case function? do
      true ->
        value =
          case Module.__get_attribute__(env.module, name, line, false) do
            {_, doc} when doc_attr? -> doc
            other -> other
          end

        try do
          :elixir_quote.escape(value, :none, false)
        rescue
          ex in [ArgumentError] ->
            raise ArgumentError,
                  "cannot inject attribute @#{name} into function/macro because " <>
                    Exception.message(ex)
        end

      false when doc_attr? ->
        quote do
          case Module.__get_attribute__(__MODULE__, unquote(name), unquote(line), false) do
            {_, doc} -> doc
            other -> other
          end
        end

      false ->
        quote do
          Module.__get_attribute__(__MODULE__, unquote(name), unquote(line), true)
        end
    end
  end

  # Error cases
  defp do_at([{call, meta, ctx_or_args}, [{:do, _} | _] = kw], _meta, name, _function?, _env) do
    args =
      case is_atom(ctx_or_args) do
        true -> []
        false -> ctx_or_args
      end

    code = "\n@#{name} (#{Macro.to_string({call, meta, args ++ [kw]})})"

    raise ArgumentError, """
    expected 0 or 1 argument for @#{name}, got 2.

    It seems you are trying to use the do-syntax with @module attributes \
    but the do-block binds to the attribute name. You probably want \
    to wrap the argument value in parentheses, like this:
    #{String.replace(code, "\n", "\n    ")}
    """
  end

  defp do_at(args, _meta, name, _function?, _env) do
    raise ArgumentError, "expected 0 or 1 argument for @#{name}, got: #{length(args)}"
  end

  # Those are always compile-time dependencies, so we can skip the trace.
  defp collect_traces(:before_compile, arg, _env), do: {arg, []}
  defp collect_traces(:after_compile, arg, _env), do: {arg, []}
  defp collect_traces(:after_verify, arg, _env), do: {arg, []}
  defp collect_traces(:on_definition, arg, _env), do: {arg, []}

  defp collect_traces(_name, arg, %{lexical_tracker: pid} = env) when is_pid(pid) do
    env = %{env | function: {:__info__, 1}}

    {arg, aliases} =
      Macro.expand_literals(arg, %{}, fn
        {:__aliases__, _, _} = alias, acc ->
          case Macro.expand(alias, env) do
            atom when is_atom(atom) -> {atom, Map.put(acc, atom, [])}
            _ -> {alias, acc}
          end

        node, acc ->
          {Macro.expand(node, env), acc}
      end)

    case map_size(aliases) do
      0 -> {arg, []}
      _ -> {arg, [{env.line, env.lexical_tracker, env.tracers, :maps.keys(aliases)}]}
    end
  end

  defp collect_traces(_name, arg, _env) do
    {arg, []}
  end

  defp typespec?(:type), do: true
  defp typespec?(:typep), do: true
  defp typespec?(:opaque), do: true
  defp typespec?(:spec), do: true
  defp typespec?(:callback), do: true
  defp typespec?(:macrocallback), do: true
  defp typespec?(_), do: false

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

    bindings =
      for {v, c} <- Macro.Env.vars(__CALLER__), c == context do
        {v, wrap_binding(in_match?, {v, [generated: true], c})}
      end

    :lists.usort(bindings)
  end

  defp wrap_binding(true, var) do
    quote(do: ^unquote(var))
  end

  defp wrap_binding(_, var) do
    var
  end

  @doc """
  Provides an `if/2` macro.

  This macro expects the first argument to be a condition and the second
  argument to be a keyword list. Generally speaking, Elixir developers
  prefer to use pattern matching and guards in function definitions and
  `case/2`, as they are succinct and precise. However, not all conditions
  can be expressed through patterns and guards, which makes `if/2` a viable
  alternative.

  Similar to `case/2`, any assignment in the condition will be available
  on both clauses, as well as after the `if` expression.

  ## One-liner examples

      if(foo, do: bar)

  In the example above, `bar` will be returned if `foo` evaluates to
  a truthy value (neither `false` nor `nil`). Otherwise, `nil` will be
  returned.

  An `else` option can be given to specify the opposite:

      if(foo, do: bar, else: baz)

  ## Blocks examples

  It's also possible to pass a block to the `if/2` macro. The first
  example above would be translated to:

      if foo do
        bar
      end

  Note that `do`-`end` become delimiters. The second example would
  translate to:

      if foo do
        bar
      else
        baz
      end

  If you find yourself nesting conditionals inside conditionals,
  consider using `cond/1`.
  """
  defmacro if(condition, clauses) do
    build_if(condition, clauses)
  end

  defp build_if(condition, do: do_clause) do
    build_if(condition, do: do_clause, else: nil)
  end

  defp build_if(condition, do: do_clause, else: else_clause) do
    optimize_boolean(
      quote do
        case unquote(condition) do
          x when :"Elixir.Kernel".in(x, [false, nil]) -> unquote(else_clause)
          _ -> unquote(do_clause)
        end
      end
    )
  end

  defp build_if(_condition, _arguments) do
    raise ArgumentError,
          "invalid or duplicate keys for if, only \"do\" and an optional \"else\" are permitted"
  end

  @doc """
  Provides an `unless` macro.

  This macro evaluates and returns the `do` block passed in as the second
  argument if `condition` evaluates to a falsy value (`false` or `nil`).
  Otherwise, it returns the value of the `else` block if present or `nil` if not.

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
  # TODO: Deprecate this on Elixir v1.22.
  @doc deprecated: "Use if/2 instead"
  defmacro unless(condition, clauses) do
    build_unless(condition, clauses)
  end

  defp build_unless(condition, do: do_clause) do
    build_unless(condition, do: do_clause, else: nil)
  end

  defp build_unless(condition, do: do_clause, else: else_clause) do
    quote do
      if(unquote(condition), do: unquote(else_clause), else: unquote(do_clause))
    end
  end

  defp build_unless(_condition, _arguments) do
    raise ArgumentError,
          "invalid or duplicate keys for unless, " <>
            "only \"do\" and an optional \"else\" are permitted"
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
  remaining elements are simply set to `nil`:

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
    quote do
      unquote(left) = Kernel.Utils.destructure(unquote(right), unquote(length(left)))
    end
  end

  @doc """
  Creates a range from `first` to `last`.

  If first is less than last, the range will be increasing from
  first to last. If first is equal to last, the range will contain
  one element, which is the number itself.

  If first is more than last, the range will be decreasing from first
  to last, albeit this behavior is deprecated. Instead prefer to
  explicitly list the step with `first..last//-1`.

  See the `Range` module for more information.

  ## Examples

      iex> 0 in 1..3
      false
      iex> 2 in 1..3
      true

      iex> Enum.to_list(1..3)
      [1, 2, 3]

  """
  defmacro first..last do
    case bootstrapped?(Macro) do
      true ->
        first = Macro.expand(first, __CALLER__)
        last = Macro.expand(last, __CALLER__)
        validate_range!(first, last)
        stepless_range(__CALLER__.context, first, last, __CALLER__)

      false ->
        stepless_range(__CALLER__.context, first, last, __CALLER__)
    end
  end

  defp stepless_range(_context, first, last, caller)
       when is_integer(first) and is_integer(last) do
    step =
      if first <= last do
        1
      else
        # TODO: Always use 1 as an step in Elixir v2.0
        IO.warn(
          "#{first}..#{last} has a default step of -1, please write #{first}..#{last}//-1 instead",
          Macro.Env.stacktrace(caller)
        )

        -1
      end

    {:%{}, [], [__struct__: Elixir.Range, first: first, last: last, step: step]}
  end

  defp stepless_range(nil, first, last, _caller) do
    quote(do: Elixir.Range.new(unquote(first), unquote(last)))
  end

  defp stepless_range(:guard, first, last, caller) do
    # We need to compute the step using guards. We don't have conditionals,
    # but we can emulate them using a map access.
    step =
      quote do
        :erlang.map_get(
          :erlang.>(unquote(first), unquote(last)),
          %{false: unquote(1), true: unquote(-1)}
        )
      end

    # TODO: Always assume step 1 in Elixir v2.0
    range = "#{Macro.to_string(first)}..#{Macro.to_string(last)}"

    IO.warn(
      "#{range} inside guards requires an explicit step, please write #{range}//1 or #{range}//-1 instead",
      Macro.Env.stacktrace(caller)
    )

    {:%{}, [], [__struct__: Elixir.Range, first: first, last: last, step: step]}
  end

  defp stepless_range(:match, first, last, caller) do
    # TODO: Make me an error in Elixir v2.0
    range = "#{Macro.to_string(first)}..#{Macro.to_string(last)}"

    IO.warn(
      "#{range} inside match is deprecated, " <>
        "you must always match on the step: #{range}//var or #{range}//_ if you want to ignore it",
      Macro.Env.stacktrace(caller)
    )

    {:%{}, [], [__struct__: Elixir.Range, first: first, last: last]}
  end

  @doc """
  Creates a range from `first` to `last` with `step`.

  See the `Range` module for more information.

  ## Examples

      iex> 0 in 1..3//1
      false
      iex> 2 in 1..3//1
      true
      iex> 2 in 1..3//2
      false

      iex> Enum.to_list(1..3//1)
      [1, 2, 3]
      iex> Enum.to_list(1..3//2)
      [1, 3]
      iex> Enum.to_list(3..1//-1)
      [3, 2, 1]
      iex> Enum.to_list(1..0//1)
      []

  """
  @doc since: "1.12.0"
  defmacro first..last//step do
    case bootstrapped?(Macro) do
      true ->
        first = Macro.expand(first, __CALLER__)
        last = Macro.expand(last, __CALLER__)
        step = Macro.expand(step, __CALLER__)
        validate_range!(first, last)
        validate_step!(step)
        range(__CALLER__.context, first, last, step)

      false ->
        range(__CALLER__.context, first, last, step)
    end
  end

  defp range(context, first, last, step)
       when is_integer(first) and is_integer(last) and is_integer(step)
       when context != nil do
    {:%{}, [], [__struct__: Elixir.Range, first: first, last: last, step: step]}
  end

  defp range(nil, first, last, step) do
    quote(do: Elixir.Range.new(unquote(first), unquote(last), unquote(step)))
  end

  defp validate_range!(first, last)
       when is_float(first) or is_float(last) or is_atom(first) or is_atom(last) or
              is_binary(first) or is_binary(last) or is_list(first) or is_list(last) do
    raise ArgumentError,
          "ranges (first..last//step) expect both sides to be integers, " <>
            "got: #{Macro.to_string({:.., [], [first, last]})}"
  end

  defp validate_range!(_, _), do: :ok

  defp validate_step!(step)
       when is_float(step) or is_atom(step) or is_binary(step) or is_list(step) or step == 0 do
    raise ArgumentError,
          "ranges (first..last//step) expect the step to be a non-zero integer, " <>
            "got: #{Macro.to_string(step)}"
  end

  defp validate_step!(_), do: :ok

  @doc """
  Creates the full-slice range `0..-1//1`.

  This macro returns a range with the following properties:

    * When enumerated, it is empty

    * When used as a `slice`, it returns the sliced element as is

  See `..///3` and the `Range` module for more information.

  ## Examples

      iex> Enum.to_list(..)
      []

      iex> String.slice("Hello world!", ..)
      "Hello world!"

  """
  @doc since: "1.14.0"
  defmacro (..) do
    range(__CALLER__.context, 0, -1, 1)
  end

  @doc """
  Boolean "and" operator.

  Provides a short-circuit operator that evaluates and returns
  the second expression only if the first one evaluates to a truthy value
  (neither `false` nor `nil`). Returns the first expression
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

  Note that, unlike `and/2`, this operator accepts any expression
  as the first argument, not only booleans.
  """
  defmacro left && right do
    assert_no_match_or_guard_scope(__CALLER__.context, "&&")

    quote do
      case unquote(left) do
        x when :"Elixir.Kernel".in(x, [false, nil]) ->
          x

        _ ->
          unquote(right)
      end
    end
  end

  @doc """
  Boolean "or" operator.

  Provides a short-circuit operator that evaluates and returns the second
  expression only if the first one does not evaluate to a truthy value (that is,
  it is either `nil` or `false`). Returns the first expression otherwise.

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

  Note that, unlike `or/2`, this operator accepts any expression
  as the first argument, not only booleans.
  """
  defmacro left || right do
    assert_no_match_or_guard_scope(__CALLER__.context, "||")

    quote do
      case unquote(left) do
        x when :"Elixir.Kernel".in(x, [false, nil]) ->
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

  The `|>/2` operator is mostly useful when there is a desire to execute a series
  of operations resembling a pipeline:

      iex> [1, [2], 3] |> List.flatten() |> Enum.map(fn x -> x * 2 end)
      [2, 4, 6]

  In the example above, the list `[1, [2], 3]` is passed as the first argument
  to the `List.flatten/1` function, then the flattened list is passed as the
  first argument to the `Enum.map/2` function which doubles each element of the
  list.

  In other words, the expression above simply translates to:

      Enum.map(List.flatten([1, [2], 3]), fn x -> x * 2 end)

  ## Pitfalls

  There are two common pitfalls when using the pipe operator.

  The first one is related to operator precedence. For example,
  the following expression:

      String.graphemes "Hello" |> Enum.reverse

  Translates to:

      String.graphemes("Hello" |> Enum.reverse())

  which results in an error as the `Enumerable` protocol is not defined
  for binaries. Adding explicit parentheses resolves the ambiguity:

      String.graphemes("Hello") |> Enum.reverse()

  Or, even better:

      "Hello" |> String.graphemes() |> Enum.reverse()

  The second limitation is that Elixir always pipes to a function
  call. Therefore, to pipe into an anonymous function, you need to
  invoke it:

      some_fun = &Regex.replace(~r/l/, &1, "L")
      "Hello" |> some_fun.()

  Alternatively, you can use `then/2` for the same effect:

      some_fun = &Regex.replace(~r/l/, &1, "L")
      "Hello" |> then(some_fun)

  `then/2` is most commonly used when you want to pipe to a function
  but the value is expected outside of the first argument, such as
  above. By replacing `some_fun` by its value, we get:

      "Hello" |> then(&Regex.replace(~r/l/, &1, "L"))

  """
  defmacro left |> right do
    fun = fn {x, pos}, acc ->
      Macro.pipe(acc, x, pos)
    end

    :lists.foldl(fun, left, Macro.unpipe(right))
  end

  @doc """
  Returns `true` if `module` is loaded and contains a
  public `function` with the given `arity`, otherwise `false`.

  Note that this function does not load the module in case
  it is not loaded. Check `Code.ensure_loaded/1` for more
  information.

  Inlined by the compiler.

  ## Examples

      iex> function_exported?(Enum, :map, 2)
      true

      iex> function_exported?(Enum, :map, 10)
      false

      iex> function_exported?(List, :to_string, 1)
      true
  """
  @spec function_exported?(module, atom, arity) :: boolean
  def function_exported?(module, function, arity) do
    :erlang.function_exported(module, function, arity)
  end

  @doc """
  Returns `true` if `module` is loaded and contains a
  public `macro` with the given `arity`, otherwise `false`.

  Note that this function does not load the module in case
  it is not loaded. Check `Code.ensure_loaded/1` for more
  information.

  If `module` is an Erlang module (as opposed to an Elixir module), this
  function always returns `false`.

  ## Examples

      iex> macro_exported?(Kernel, :use, 2)
      true

      iex> macro_exported?(:erlang, :abs, 1)
      false

  """
  @spec macro_exported?(module, atom, arity) :: boolean
  def macro_exported?(module, macro, arity)
      when is_atom(module) and is_atom(macro) and is_integer(arity) and
             (arity >= 0 and arity <= 255) do
    function_exported?(module, :__info__, 1) and
      :lists.member({macro, arity}, module.__info__(:macros))
  end

  @doc """
  Power operator.

  It takes two numbers for input. If both are integers and the right-hand
  side (the `exponent`) is also greater than or equal to 0, then the result
  will also be an integer. Otherwise it returns a float.

  ## Examples

      iex> 2 ** 2
      4
      iex> 2 ** -4
      0.0625

      iex> 2.0 ** 2
      4.0
      iex> 2 ** 2.0
      4.0

  """
  @doc since: "1.13.0"
  @spec integer ** non_neg_integer :: integer
  @spec integer ** neg_integer :: float
  @spec float ** float :: float
  @spec integer ** float :: float
  @spec float ** integer :: float
  def base ** exponent when is_integer(base) and is_integer(exponent) and exponent >= 0 do
    integer_pow(base, 1, exponent)
  end

  def base ** exponent when is_number(base) and is_number(exponent) do
    :math.pow(base, exponent)
  end

  # https://en.wikipedia.org/wiki/Exponentiation_by_squaring
  defp integer_pow(_, _, 0),
    do: 1

  defp integer_pow(b, a, 1),
    do: b * a

  defp integer_pow(b, a, e) when :erlang.band(e, 1) == 0,
    do: integer_pow(b * b, a, :erlang.bsr(e, 1))

  defp integer_pow(b, a, e),
    do: integer_pow(b * b, a * b, :erlang.bsr(e, 1))

  @doc """
  Membership operator.

  Checks if the element on the left-hand side is a member of the
  collection on the right-hand side.

  ## Examples

      iex> x = 1
      iex> x in [1, 2, 3]
      true

  This operator (which is a macro) simply translates to a call to
  `Enum.member?/2`. The example above would translate to:

      Enum.member?([1, 2, 3], x)

  Elixir also supports `left not in right`, which evaluates to
  `not(left in right)`:

      iex> x = 1
      iex> x not in [1, 2, 3]
      false

  ## Guards

  The `in/2` operator (as well as `not in`) can be used in guard clauses as
  long as the right-hand side is a range or a list.

  If the right-hand side is a list, Elixir will expand the operator to a valid
  guard expression which needs to check each value. For example:

      when x in [1, 2, 3]

  translates to:

      when x === 1 or x === 2 or x === 3

  However, this construct will be inefficient for large lists. In such cases, it
  is best to stop using guards and use a more appropriate data structure, such
  as `MapSet`.

  If the right-hand side is a range, a more efficient comparison check will be
  done. For example:

      when x in 1..1000

  translates roughly to:

      when x >= 1 and x <= 1000

  ### AST considerations

  `left not in right` is parsed by the compiler into the AST:

      {:not, _, [{:in, _, [left, right]}]}

  This is the same AST as `not(left in right)`.

  Additionally, `Macro.to_string/2` and `Code.format_string!/2`
  will translate all occurrences of this AST to `left not in right`.
  """
  @doc guard: true
  defmacro left in right do
    in_body? = __CALLER__.context == nil

    expand =
      case bootstrapped?(Macro) do
        true -> &Macro.expand(&1, __CALLER__)
        false -> & &1
      end

    case expand.(right) do
      [] when not in_body? ->
        false

      [] ->
        quote do
          _ = unquote(left)
          false
        end

      [head | tail] = list ->
        # We only expand lists in the body if they are relatively
        # short and it is made only of literal expressions.
        case not in_body? or small_literal_list?(right) do
          true -> in_var(in_body?, left, &in_list(&1, head, tail, expand, list, in_body?))
          false -> quote(do: :lists.member(unquote(left), unquote(right)))
        end

      %{} = right ->
        raise ArgumentError, "found unescaped value on the right side of in/2: #{inspect(right)}"

      right ->
        with {:%{}, _meta, fields} <- right,
             [__struct__: Elixir.Range, first: first, last: last, step: step] <-
               :lists.usort(fields) do
          in_var(in_body?, left, &in_range(&1, expand.(first), expand.(last), expand.(step)))
        else
          _ when in_body? ->
            quote(do: Elixir.Enum.member?(unquote(right), unquote(left)))

          _ ->
            raise_on_invalid_args_in_2(right)
        end
    end
  end

  defp raise_on_invalid_args_in_2(right) do
    raise ArgumentError, <<
      "invalid right argument for operator \"in\", it expects a compile-time proper list ",
      "or compile-time range on the right side when used in guard expressions, got: ",
      Macro.to_string(right)::binary
    >>
  end

  defp in_var(false, ast, fun), do: fun.(ast)

  defp in_var(true, {atom, _, context} = var, fun) when is_atom(atom) and is_atom(context),
    do: fun.(var)

  defp in_var(true, ast, fun) do
    quote do
      var = unquote(ast)
      unquote(fun.(quote(do: var)))
    end
  end

  defp small_literal_list?(list) when is_list(list) and length(list) <= 32 do
    :lists.all(fn x -> is_binary(x) or is_atom(x) or is_number(x) end, list)
  end

  defp small_literal_list?(_list), do: false

  defp in_range(left, first, last, step) when is_integer(step) do
    in_range_literal(left, first, last, step)
  end

  defp in_range(left, first, last, step) do
    quoted =
      quote do
        :erlang.is_integer(unquote(left)) and :erlang.is_integer(unquote(first)) and
          :erlang.is_integer(unquote(last)) and
          ((:erlang.>(unquote(step), 0) and
              unquote(increasing_compare(left, first, last))) or
             (:erlang.<(unquote(step), 0) and
                unquote(decreasing_compare(left, first, last))))
      end

    in_range_step(quoted, left, first, step)
  end

  defp in_range_literal(left, first, first, _step) when is_integer(first) do
    quote do: :erlang."=:="(unquote(left), unquote(first))
  end

  defp in_range_literal(left, first, last, step) when step > 0 do
    quoted =
      quote do
        :erlang.andalso(
          :erlang.is_integer(unquote(left)),
          unquote(increasing_compare(left, first, last))
        )
      end

    in_range_step(quoted, left, first, step)
  end

  defp in_range_literal(left, first, last, step) when step < 0 do
    quoted =
      quote do
        :erlang.andalso(
          :erlang.is_integer(unquote(left)),
          unquote(decreasing_compare(left, first, last))
        )
      end

    in_range_step(quoted, left, first, step)
  end

  defp in_range_step(quoted, _left, _first, step) when step == 1 or step == -1 do
    quoted
  end

  defp in_range_step(quoted, left, first, step) do
    quote do
      :erlang.andalso(
        unquote(quoted),
        :erlang."=:="(:erlang.rem(unquote(left) - unquote(first), unquote(step)), 0)
      )
    end
  end

  defp in_list(left, head, tail, expand, right, in_body?) do
    [head | tail] = :lists.map(&comp(left, &1, expand, right, in_body?), [head | tail])
    :lists.foldl(&quote(do: :erlang.orelse(unquote(&2), unquote(&1))), head, tail)
  end

  defp comp(left, {:|, _, [head, tail]}, expand, right, in_body?) do
    case expand.(tail) do
      [] ->
        quote(do: :erlang."=:="(unquote(left), unquote(head)))

      [tail_head | tail] ->
        quote do
          :erlang.orelse(
            :erlang."=:="(unquote(left), unquote(head)),
            unquote(in_list(left, tail_head, tail, expand, right, in_body?))
          )
        end

      tail when in_body? ->
        quote do
          :erlang.orelse(
            :erlang."=:="(unquote(left), unquote(head)),
            :lists.member(unquote(left), unquote(tail))
          )
        end

      _ ->
        raise_on_invalid_args_in_2(right)
    end
  end

  defp comp(left, right, _expand, _right, _in_body?) do
    quote(do: :erlang."=:="(unquote(left), unquote(right)))
  end

  defp increasing_compare(var, first, last) do
    quote do
      :erlang.andalso(
        :erlang.>=(unquote(var), unquote(first)),
        :erlang."=<"(unquote(var), unquote(last))
      )
    end
  end

  defp decreasing_compare(var, first, last) do
    quote do
      :erlang.andalso(
        :erlang."=<"(unquote(var), unquote(first)),
        :erlang.>=(unquote(var), unquote(last))
      )
    end
  end

  @doc """
  Marks that the given variable should not be hygienized.

  This macro expects a variable and it is typically invoked
  inside `quote/2` to mark that a variable
  should not be hygienized. See `quote/2` for more information.

  ## Examples

      iex> Kernel.var!(example) = 1
      1
      iex> Kernel.var!(example)
      1

  """
  defmacro var!(var, context \\ nil)

  defmacro var!({name, meta, atom}, context) when is_atom(name) and is_atom(atom) do
    # Remove counter and force them to be vars
    meta = :lists.keydelete(:counter, 1, meta)
    meta = :lists.keystore(:if_undefined, 1, meta, {:if_undefined, :raise})

    case Macro.expand(context, __CALLER__) do
      context when is_atom(context) ->
        {name, meta, context}

      other ->
        raise ArgumentError,
              "expected var! context to expand to an atom, got: #{Macro.to_string(other)}"
    end
  end

  defmacro var!(other, _context) do
    raise ArgumentError, "expected a variable to be given to var!, got: #{Macro.to_string(other)}"
  end

  @doc """
  When used inside quoting, marks that the given alias should not
  be hygienized. This means the alias will be expanded when
  the macro is expanded.

  Check `quote/2` for more information.
  """
  defmacro alias!(alias) when is_atom(alias) do
    alias
  end

  defmacro alias!({:__aliases__, meta, args}) do
    # Simply remove the alias metadata from the node
    # so it does not affect expansion.
    {:__aliases__, :lists.keydelete(:alias, 1, meta), args}
  end

  @doc """
  Returns a binary starting at the offset `start` and of the given `size`.

  This is similar to `binary_part/3` except that if `start + size`
  is greater than the binary size, it automatically clips it to
  the binary size instead of raising. Opposite to `binary_part/3`,
  this function is not allowed in guards.

  This function works with bytes. For a slicing operation that
  considers characters, see `String.slice/3`.

  ## Examples

      iex> binary_slice("elixir", 0, 6)
      "elixir"
      iex> binary_slice("elixir", 0, 5)
      "elixi"
      iex> binary_slice("elixir", 1, 4)
      "lixi"
      iex> binary_slice("elixir", 0, 10)
      "elixir"

  If `start` is negative, it is normalized against the binary
  size and clamped to 0:

      iex> binary_slice("elixir", -3, 10)
      "xir"
      iex> binary_slice("elixir", -10, 10)
      "elixir"

  If the `size` is zero, an empty binary is returned:

      iex> binary_slice("elixir", 1, 0)
      ""

  If `start` is greater than or equal to the binary size,
  an empty binary is returned:

      iex> binary_slice("elixir", 10, 10)
      ""

  """
  @doc since: "1.14.0"
  def binary_slice(binary, start, size)
      when is_binary(binary) and is_integer(start) and is_integer(size) and size >= 0 do
    total = byte_size(binary)
    start = if start < 0, do: max(total + start, 0), else: start

    case start < total do
      true -> :erlang.binary_part(binary, start, min(size, total - start))
      false -> ""
    end
  end

  @doc """
  Returns a binary from the offset given by the start of the
  range to the offset given by the end of the range.

  If the start or end of the range are negative, they are converted
  into positive indices based on the binary size. For example,
  `-1` means the last byte of the binary.

  This is similar to `binary_part/3` except that it works with ranges
  and it is not allowed in guards.

  This function works with bytes. For a slicing operation that
  considers characters, see `String.slice/2`.

  ## Examples

      iex> binary_slice("elixir", 0..5)
      "elixir"
      iex> binary_slice("elixir", 1..3)
      "lix"
      iex> binary_slice("elixir", 1..10)
      "lixir"

      iex> binary_slice("elixir", -4..-1)
      "ixir"
      iex> binary_slice("elixir", -4..6)
      "ixir"
      iex> binary_slice("elixir", -10..10)
      "elixir"

  For ranges where `start > stop`, you need to explicitly
  mark them as increasing:

      iex> binary_slice("elixir", 2..-1//1)
      "ixir"
      iex> binary_slice("elixir", 1..-2//1)
      "lixi"

  You can use `../0` as a shortcut for `0..-1//1`, which returns
  the whole binary as is:

      iex> binary_slice("elixir", ..)
      "elixir"

  The step can be any positive number. For example, to
  get every 2 characters of the binary:

      iex> binary_slice("elixir", 0..-1//2)
      "eii"

  If the first position is after the string ends or after
  the last position of the range, it returns an empty string:

      iex> binary_slice("elixir", 10..3//1)
      ""
      iex> binary_slice("elixir", -10..-7)
      ""
      iex> binary_slice("a", 1..1500)
      ""

  """
  @doc since: "1.14.0"
  def binary_slice(binary, first..last//step)
      when is_binary(binary) and step > 0 do
    total = byte_size(binary)

    first = if first < 0, do: max(first + total, 0), else: first
    last = if last < 0, do: last + total, else: last

    amount = last - first + 1

    if first < total and amount > 0 do
      part = binary_part(binary, first, min(amount, total - first))

      if step == 1 do
        part
      else
        <<first_byte, rest::binary>> = part
        for <<_::size(^step - 1)-bytes, byte <- rest>>, into: <<first_byte>>, do: <<byte>>
      end
    else
      ""
    end
  end

  def binary_slice(binary, _.._//_ = range) when is_binary(binary) do
    raise ArgumentError,
          "binary_slice/2 does not accept ranges with negative steps, got: #{inspect(range)}"
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

      defmodule Number do
        def one, do: 1
        def two, do: 2
      end
      #=> {:module, Number, <<70, 79, 82, ...>>, {:two, 0}}

      Number.one()
      #=> 1

      Number.two()
      #=> 2

  ## Module names and aliases

  Module names (and aliases) must start with an ASCII uppercase character which
  may be followed by any ASCII letter, number, or underscore. Elixir's
  [Naming Conventions](naming-conventions.md) suggest for module names and aliases
  to be written in the `CamelCase` format.

  You can also use atoms as the module name, although they must only contain ASCII
  characters.

  ## Nesting

  Nesting a module inside another module affects the name of the nested module:

      defmodule Foo do
        defmodule Bar do
        end
      end

  In the example above, two modules - `Foo` and `Foo.Bar` - are created.
  When nesting, Elixir automatically creates an alias to the inner module,
  allowing the second module `Foo.Bar` to be accessed as `Bar` in the same
  lexical scope where it's defined (the `Foo` module). This only happens
  if the nested module is defined via an alias.

  If the `Foo.Bar` module is moved somewhere else, the references to `Bar` in
  the `Foo` module need to be updated to the fully-qualified name (`Foo.Bar`) or
  an alias has to be explicitly set in the `Foo` module with the help of
  `alias/2`.

      defmodule Foo.Bar do
        # code
      end

      defmodule Foo do
        alias Foo.Bar
        # code here can refer to "Foo.Bar" as just "Bar"
      end

  ## Dynamic names

  Elixir module names can be dynamically generated. This is very
  useful when working with macros. For instance, one could write:

      defmodule Module.concat(["Foo", "Bar"]) do
        # contents ...
      end

  Elixir will accept any module name as long as the expression passed as the
  first argument to `defmodule/2` evaluates to an atom.
  Note that, when a dynamic name is used, Elixir won't nest the name under
  the current module nor automatically set up an alias.

  ## Reserved module names

  If you attempt to define a module that already exists, you will get a
  warning saying that a module has been redefined.

  There are some modules that Elixir does not currently implement but it
  may be implement in the future. Those modules are reserved and defining
  them will result in a compilation error:

      defmodule Any do
        # code
      end
      ** (CompileError) iex:1: module Any is reserved and cannot be defined

  Elixir reserves the following module names: `Elixir`, `Any`, `BitString`,
  `PID`, and `Reference`.
  """
  defmacro defmodule(alias, do_block)

  defmacro defmodule(alias, do: block) do
    env = __CALLER__
    assert_no_match_or_guard_scope(env.context, "defmodule/2")
    expanded = expand_module_alias(alias, env)

    {expanded, with_alias} =
      case is_atom(expanded) do
        true ->
          {full, old, opts} = alias_defmodule(alias, expanded, env)
          # Expand the module considering the current environment/nesting
          meta = [defined: full] ++ alias_meta(alias)
          {full, {:require, meta, [old, opts]}}

        false ->
          {expanded, nil}
      end

    escaped =
      case env do
        %{function: nil, lexical_tracker: pid} when is_pid(pid) ->
          integer = Kernel.LexicalTracker.write_cache(pid, block)
          quote(do: Kernel.LexicalTracker.read_cache(unquote(pid), unquote(integer)))

        %{} ->
          :elixir_quote.escape(block, :none, false)
      end

    versioned_vars = env.versioned_vars
    prune = :erlang.is_map_key({:elixir, :prune_binding}, versioned_vars)

    var_meta =
      case prune do
        true -> [generated: true, keep_unused: true]
        false -> [generated: true]
      end

    module_vars = :lists.map(&module_var(&1, var_meta), :maps.keys(versioned_vars))

    quote do
      unquote(with_alias)

      :elixir_module.compile(
        unquote(expanded),
        unquote(escaped),
        unquote(module_vars),
        unquote(prune),
        __ENV__
      )
    end
  end

  defp alias_meta({:__aliases__, meta, _}), do: meta
  defp alias_meta(_), do: []

  # We don't want to trace :alias_reference since we are defining the alias
  defp expand_module_alias({:__aliases__, meta, list} = alias, env) do
    case :elixir_aliases.expand_or_concat(meta, list, env, true) do
      receiver when is_atom(receiver) ->
        receiver

      [head | tail] ->
        case Macro.expand(head, env) do
          head when is_atom(head) -> :elixir_aliases.concat([head | tail])
          _ -> alias
        end
    end
  end

  defp expand_module_alias(other, env), do: Macro.expand(other, env)

  # defmodule Elixir.Alias
  defp alias_defmodule({:__aliases__, _, [:"Elixir", _ | _]}, module, _env),
    do: {module, module, []}

  # defmodule Alias in root
  defp alias_defmodule({:__aliases__, _, _}, module, %{module: nil}), do: {module, module, []}

  # defmodule Alias nested
  defp alias_defmodule({:__aliases__, _, [h | t]}, _module, env) when is_atom(h) do
    module = :elixir_aliases.concat([env.module, h])
    alias = String.to_atom("Elixir." <> Atom.to_string(h))
    opts = [as: alias, warn: false]

    case t do
      [] -> {module, module, opts}
      _ -> {String.to_atom(Enum.join([module | t], ".")), module, opts}
    end
  end

  # defmodule _
  defp alias_defmodule(_raw, module, _env) do
    {module, module, []}
  end

  defp module_var({name, kind}, meta) when is_atom(kind), do: {name, meta, kind}
  defp module_var({name, kind}, meta), do: {name, [counter: kind] ++ meta, nil}

  @doc ~S"""
  Defines a public function with the given name and body.

  ## Examples

      defmodule Foo do
        def bar, do: :baz
      end

      Foo.bar()
      #=> :baz

  A function that expects arguments can be defined as follows:

      defmodule Foo do
        def sum(a, b) do
          a + b
        end
      end

  In the example above, a `sum/2` function is defined; this function receives
  two arguments and returns their sum.

  ## Default arguments

  `\\` is used to specify a default value for a parameter of a function. For
  example:

      defmodule MyMath do
        def multiply_by(number, factor \\ 2) do
          number * factor
        end
      end

      MyMath.multiply_by(4, 3)
      #=> 12

      MyMath.multiply_by(4)
      #=> 8

  The compiler translates this into multiple functions with different arities,
  here `MyMath.multiply_by/1` and `MyMath.multiply_by/2`, that represent cases when
  arguments for parameters with default values are passed or not passed.

  When defining a function with default arguments as well as multiple
  explicitly declared clauses, you must write a function head that declares the
  defaults. For example:

      defmodule MyString do
        def join(string1, string2 \\ nil, separator \\ " ")

        def join(string1, nil, _separator) do
          string1
        end

        def join(string1, string2, separator) do
          string1 <> separator <> string2
        end
      end

  Note that `\\` can't be used with anonymous functions because they
  can only have a sole arity.

  ### Keyword lists with default arguments

  Functions containing many arguments can benefit from using `Keyword`
  lists to group and pass attributes as a single value.

      defmodule MyConfiguration do
        @default_opts [storage: "local"]

        def configure(resource, opts \\ []) do
          opts = Keyword.merge(@default_opts, opts)
          storage = opts[:storage]
          # ...
        end
      end

  The difference between using `Map` and `Keyword` to store many
  arguments is `Keyword`'s keys:

    * must be atoms
    * can be given more than once
    * ordered, as specified by the developer

  ## Function names

  Function and variable names in Elixir must start with an underscore or a
  Unicode letter that is not in uppercase or titlecase. They may continue
  using a sequence of Unicode letters, numbers, and underscores. They may
  end in `?` or `!`. Elixir's [Naming Conventions](naming-conventions.md)
  suggest for function and variable names to be written in the `snake_case`
  format.

  ## `rescue`/`catch`/`after`/`else`

  Function bodies support `rescue`, `catch`, `after`, and `else` as `try/1`
  does (known as "implicit try"). For example, the following two functions are equivalent:

      def convert(number) do
        try do
          String.to_integer(number)
        rescue
          e in ArgumentError -> {:error, e.message}
        end
      end

      def convert(number) do
        String.to_integer(number)
      rescue
        e in ArgumentError -> {:error, e.message}
      end

  """
  defmacro def(call, expr \\ nil) do
    assert_no_match_or_guard_scope(__CALLER__.context, "def/2")
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

      Foo.bar()
      #=> 3

      Foo.sum(1, 2)
      ** (UndefinedFunctionError) undefined function Foo.sum/2

  """
  defmacro defp(call, expr \\ nil) do
    assert_no_match_or_guard_scope(__CALLER__.context, "defp/2")
    define(:defp, call, expr, __CALLER__)
  end

  @doc """
  Defines a public macro with the given name and body.

  Macros must be defined before its usage.

  Check `def/2` for rules on naming and default arguments.

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
        IO.puts("It works")
      end

  """
  defmacro defmacro(call, expr \\ nil) do
    assert_no_match_or_guard_scope(__CALLER__.context, "defmacro/2")
    define(:defmacro, call, expr, __CALLER__)
  end

  @doc """
  Defines a private macro with the given name and body.

  Private macros are only accessible from the same module in which they are
  defined.

  Private macros must be defined before its usage.

  Check `defmacro/2` for more information, and check `def/2` for rules on
  naming and default arguments.

  """
  defmacro defmacrop(call, expr \\ nil) do
    assert_no_match_or_guard_scope(__CALLER__.context, "defmacrop/2")
    define(:defmacrop, call, expr, __CALLER__)
  end

  defp define(kind, call, expr, env) do
    module = assert_module_scope(env, kind, 2)
    assert_no_function_scope(env, kind, 2)

    unquoted_call = :elixir_quote.has_unquotes(call)
    unquoted_expr = :elixir_quote.has_unquotes(expr)

    store =
      case unquoted_expr or unquoted_call do
        true ->
          :elixir_quote.escape({call, expr}, :none, true)

        false ->
          key = :erlang.unique_integer()
          :elixir_module.write_cache(module, key, {call, expr})
          key
      end

    pos = :elixir_locals.cache_env(env)

    quote do
      :elixir_def.store_definition(unquote(kind), unquote(store), unquote(pos))
    end
  end

  @doc """
  Defines a struct.

  A struct is a tagged map that allows developers to provide
  default values for keys, tags to be used in polymorphic
  dispatches and compile time assertions.

  It is only possible to define a struct per module, as the
  struct is tied to the module itself.

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

  The `fields` argument is usually a keyword list with field names
  as atom keys and default values as corresponding values. `defstruct/1`
  also supports a list of atoms as its argument: in that case, the atoms
  in the list will be used as the struct's field names and they will all
  default to `nil`.

      defmodule Post do
        defstruct [:title, :content, :author]
      end

  Add documentation to a struct with the `@doc` attribute, like a function.

      defmodule Post do
        @doc "A post. The content should be valid Markdown."
        defstruct [:title, :content, :author]
      end

  Once a struct is defined, it is possible to create them as follows:

      %Post{title: "Hello world!"}

  For more information on creating, updating, and pattern matching on
  structs, please check `%/2`.

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
        @derive MyProtocol
        defstruct name: nil, age: nil
      end

      MyProtocol.call(john) # it works!

  A common example is to `@derive` the `Inspect` protocol to hide certain fields
  when the struct is printed:

      defmodule User do
        @derive {Inspect, only: :name}
        defstruct name: nil, age: nil
      end

  For each protocol in `@derive`, Elixir will assert the protocol has
  been implemented for `Any`. If the `Any` implementation defines a
  `__deriving__/3` callback, the callback will be invoked and it should define
  the implementation module. Otherwise an implementation that simply points to
  the `Any` implementation is automatically derived. For more information on
  the `__deriving__/3` callback, see `Protocol.derive/3`.

  ## Enforcing keys

  When building a struct, Elixir will automatically guarantee all keys
  belong to the struct:

      %User{name: "john", unknown: :key}
      ** (KeyError) key :unknown not found in: %User{age: 21, name: nil}

  Elixir also allows developers to enforce that certain keys must always be
  given when building the struct:

      defmodule User do
        @enforce_keys [:name]
        defstruct name: nil, age: 10 + 11
      end

  Now trying to build a struct without the name key will fail:

      %User{age: 21}
      ** (ArgumentError) the following keys must also be given when building struct User: [:name]

  Keep in mind `@enforce_keys` is a simple compile-time guarantee
  to aid developers when building structs. It is not enforced on
  updates and it does not provide any sort of value-validation.

  ## Types

  It is recommended to define types for structs. By convention, such a type
  is called `t`. To define a struct inside a type, the struct literal syntax
  is used:

      defmodule User do
        defstruct name: "John", age: 25
        @type t :: %__MODULE__{name: String.t(), age: non_neg_integer}
      end

  It is recommended to only use the struct syntax when defining the struct's
  type. When referring to another struct, it's better to use `User.t()` instead of
  `%User{}`.

  The types of the struct fields that are not included in `%User{}` default to
  `term()` (see `t:term/0`).

  Structs whose internal structure is private to the local module (pattern
  matching them or directly accessing their fields should not be allowed) should
  use the `@opaque` attribute. Structs whose internal structure is public should
  use `@type`.
  """
  defmacro defstruct(fields) do
    header =
      quote bind_quoted: [fields: fields, bootstrapped?: bootstrapped?(Enum)] do
        {struct, derive, escaped_struct, kv, body} =
          Kernel.Utils.defstruct(__MODULE__, fields, bootstrapped?, __ENV__)

        case derive do
          [] -> :ok
          _ -> Protocol.__derive__(derive, __MODULE__, __ENV__)
        end
      end

    # We attach the line: 0 to struct functions because we don't want
    # the generated callbacks to count towards code coverage and metrics,
    # especially since they are often expanded at compile-time.
    functions =
      quote line: 0, unquote: false do
        def __struct__(), do: unquote(escaped_struct)
        def __struct__(unquote(kv)), do: unquote(body)
      end

    footer =
      quote do
        Kernel.Utils.announce_struct(__MODULE__)
        struct
      end

    {:__block__, [], [header, functions, footer]}
  end

  @doc ~S"""
  Defines an exception.

  Exceptions are structs backed by a module that implements
  the `Exception` behaviour. The `Exception` behaviour requires
  two functions to be implemented:

    * [`exception/1`](`c:Exception.exception/1`) - receives the arguments given to `raise/2`
      and returns the exception struct. The default implementation
      accepts either a set of keyword arguments that is merged into
      the struct or a string to be used as the exception's message.

    * [`message/1`](`c:Exception.message/1`) - receives the exception struct and must return its
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
        message: "did not get what was expected, got: #{inspect(value)}"

  In many cases it is more convenient to pass the expected value to
  `raise/2` and generate the message in the `c:Exception.exception/1` callback:

      defmodule MyAppError do
        defexception [:message]

        @impl true
        def exception(value) do
          msg = "did not get what was expected, got: #{inspect(value)}"
          %MyAppError{message: msg}
        end
      end

      raise MyAppError, value

  The example above shows the preferred strategy for customizing
  exception messages.
  """
  defmacro defexception(fields) do
    quote bind_quoted: [fields: fields] do
      Elixir.Kernel.@(behaviour(Exception))
      struct = defstruct([__exception__: true] ++ fields)

      if Map.has_key?(struct, :message) do
        Elixir.Kernel.@(impl(true))

        def message(exception) do
          exception.message
        end

        defoverridable message: 1

        Elixir.Kernel.@(impl(true))

        def exception(msg) when Kernel.is_binary(msg) do
          exception(message: msg)
        end
      end

      # Calls to Kernel functions must be fully-qualified to ensure
      # reproducible builds, otherwise, this macro will generate ASTs
      # with different metadata (:imports, :context) depending on if
      # it is the bootstrapped version or not.
      Elixir.Kernel.@(impl(true))

      def exception(args) when Kernel.is_list(args) do
        struct!(__MODULE__, args)
      end

      defoverridable exception: 1
    end
  end

  @doc """
  Defines a protocol.

  See the `Protocol` module for more information.
  """
  defmacro defprotocol(name, do_block)

  defmacro defprotocol(name, do: block) do
    Protocol.__protocol__(name, do: block)
  end

  @doc """
  Defines an implementation for the given protocol.

  See the `Protocol` module for more information.
  """
  defmacro defimpl(name, opts, do_block \\ []) do
    Protocol.__impl__(name, opts, do_block, __CALLER__)
  end

  @doc """
  Makes the given definitions in the current module overridable.

  If the user defines a new function or macro with the same name
  and arity, then the overridable ones are discarded. Otherwise, the
  original definitions are used.

  It is possible for the overridden definition to have a different visibility
  than the original: a public function can be overridden by a private
  function and vice-versa.

  Macros cannot be overridden as functions and vice-versa.

  ## Example

      defmodule DefaultMod do
        defmacro __using__(_opts) do
          quote do
            def test(x, y) do
              x + y
            end

            defoverridable test: 2
          end
        end
      end

      defmodule ChildMod do
        use DefaultMod

        def test(x, y) do
          x * y + super(x, y)
        end
      end

  As seen as in the example above, `super` can be used to call the default
  implementation.

  > #### Disclaimer {: .tip}
  >
  > Use `defoverridable` with care. If you need to define multiple modules
  > with the same behaviour, it may be best to move the default implementation
  > to the caller, and check if a callback exists via `Code.ensure_loaded?/1` and
  > `function_exported?/3`.
  >
  > For example, in the example above, imagine there is a module that calls the
  > `test/2` function. This module could be defined as such:
  >
  >     defmodule CallsTest do
  >       def receives_module_and_calls_test(module, x, y) do
  >         if Code.ensure_loaded?(module) and function_exported?(module, :test, 2) do
  >           module.test(x, y)
  >         else
  >           x + y
  >         end
  >       end
  >     end

  ## Example with behaviour

  You can also pass a behaviour to `defoverridable` and it will mark all of the
  callbacks in the behaviour as overridable:


      defmodule Behaviour do
        @callback test(number(), number()) :: number()
      end

      defmodule DefaultMod do
        defmacro __using__(_opts) do
          quote do
            @behaviour Behaviour

            def test(x, y) do
              x + y
            end

            defoverridable Behaviour
          end
        end
      end

      defmodule ChildMod do
        use DefaultMod

        def test(x, y) do
          x * y + super(x, y)
        end
      end

  """
  defmacro defoverridable(keywords_or_behaviour) do
    quote do
      Module.make_overridable(__MODULE__, unquote(keywords_or_behaviour))
    end
  end

  @doc """
  Defines a macro suitable for use in guard expressions.

  It raises at compile time if the `guard` uses expressions that aren't
  allowed in [guard clauses](patterns-and-guards.html#guards),
  and otherwise creates a macro that can be used both inside or outside guards.

  When defining your own guards, consider the
  [naming conventions](naming-conventions.html#is_-prefix-is_foo)
  around boolean-returning guards.

  ## Example

      defmodule Integer.Guards do
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
      end

      defmodule Collatz do
        @moduledoc "Tools for working with the Collatz sequence."
        import Integer.Guards

        @doc "Determines the number of steps `n` takes to reach `1`."
        # If this function never converges, please let me know what `n` you used.
        def converge(n) when n > 0, do: step(n, 0)

        defp step(1, step_count) do
          step_count
        end

        defp step(n, step_count) when is_even(n) do
          step(div(n, 2), step_count + 1)
        end

        defp step(n, step_count) do
          step(3 * n + 1, step_count + 1)
        end
      end

  """
  @doc since: "1.6.0"
  @spec defguard(Macro.t()) :: Macro.t()
  defmacro defguard(guard) do
    define_guard(:defmacro, guard, __CALLER__)
  end

  @doc """
  Defines a private macro suitable for use in guard expressions.

  It raises at compile time if the `guard` uses expressions that aren't
  allowed in [guard clauses](patterns-and-guards.html#guards),
  and otherwise creates a private macro that can be used
  both inside or outside guards in the current module.

  When defining your own guards, consider the
  [naming conventions](naming-conventions.html#is_-prefix-is_foo)
  around boolean-returning guards.

  Similar to `defmacrop/2`, `defguardp/1` must be defined before its use
  in the current module.
  """
  @doc since: "1.6.0"
  @spec defguardp(Macro.t()) :: Macro.t()
  defmacro defguardp(guard) do
    define_guard(:defmacrop, guard, __CALLER__)
  end

  defp define_guard(kind, guard, env) do
    case :elixir_utils.extract_guards(guard) do
      {call, [_, _ | _]} ->
        raise ArgumentError,
              "invalid syntax in defguard #{Macro.to_string(call)}, " <>
                "only a single when clause is allowed"

      {call, impls} ->
        case decompose_args(call) do
          :error ->
            raise ArgumentError, "invalid syntax in defguard #{Macro.to_string(call)}"

          args ->
            validate_variable_only_args!(call, args)

            macro_definition =
              case impls do
                [] ->
                  define(kind, call, nil, env)

                [guard] ->
                  quoted =
                    quote do
                      require Kernel.Utils
                      Kernel.Utils.defguard(unquote(args), unquote(guard))
                    end

                  define(kind, call, [do: quoted], env)
              end

            quote do
              Elixir.Kernel.@(doc(guard: true))
              unquote(macro_definition)
            end
        end
    end
  end

  defp decompose_args({name, _, args}) when is_atom(name) and is_atom(args), do: []

  defp decompose_args({name, _, args}) when is_atom(name) and is_list(args), do: args

  defp decompose_args({{:unquote, _, _}, _, args}) when is_atom(args), do: []

  defp decompose_args({{:unquote, _, _}, _, args}) when is_list(args), do: args

  defp decompose_args(_), do: :error

  defp validate_variable_only_args!(call, args) do
    Enum.each(args, fn
      {ref, _meta, context} when is_atom(ref) and is_atom(context) ->
        :ok

      {:\\, _m1, [{ref, _m2, context}, _default]} when is_atom(ref) and is_atom(context) ->
        :ok

      _match ->
        raise ArgumentError, "invalid syntax in defguard #{Macro.to_string(call)}"
    end)
  end

  @doc """
  Uses the given module in the current context.

  When calling:

      use MyModule, some: :options

  Elixir will invoke `MyModule.__using__/1` passing the second argument of
  `use` as its argument. Since `__using__/1` is typically a macro, all
  the usual macro rules apply, and its return value should be quoted code
  that is then inserted where `use/2` is called.

  > #### Code injection {: .warning}
  >
  > `use MyModule` works as a **code-injection point** in the caller.
  > Given the caller of `use MyModule` has little control over how the
  > code is injected, `use/2` should be used with care. If you can,
  > avoid use in favor of `import/2` or `alias/2` whenever possible.

  ## Examples

  For example, to write test cases using the `ExUnit` framework provided
  with Elixir, a developer should `use` the `ExUnit.Case` module:

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        test "always pass" do
          assert true
        end
      end

  In this example, Elixir will call the `__using__/1` macro in the
  `ExUnit.Case` module with the keyword list `[async: true]` as its
  argument.

  In other words, `use/2` translates to:

      defmodule AssertionTest do
        require ExUnit.Case
        ExUnit.Case.__using__(async: true)

        test "always pass" do
          assert true
        end
      end

  where `ExUnit.Case` defines the `__using__/1` macro:

      defmodule ExUnit.Case do
        defmacro __using__(opts) do
          # do something with opts
          quote do
            # return some code to inject in the caller
          end
        end
      end

  ## Best practices

  `__using__/1` is typically used when there is a need to set some state
  (via module attributes) or callbacks (like `@before_compile`, see the
  documentation for `Module` for more information) into the caller.

  `__using__/1` may also be used to alias, require, or import functionality
  from different modules:

      defmodule MyModule do
        defmacro __using__(_opts) do
          quote do
            import MyModule.Foo
            import MyModule.Bar
            import MyModule.Baz

            alias MyModule.Repo
          end
        end
      end

  However, do not provide `__using__/1` if all it does is to import,
  alias or require the module itself. For example, avoid this:

      defmodule MyModule do
        defmacro __using__(_opts) do
          quote do
            import MyModule
          end
        end
      end

  In such cases, developers should instead import or alias the module
  directly, so that they can customize those as they wish,
  without the indirection behind `use/2`. Developers must also avoid
  defining functions inside `__using__/1`.

  Given `use MyModule` can generate any code, it may not be easy for
  developers to understand the impact of `use MyModule`.

  For this reason, to provide guidance and clarity, we recommend developers
  to include an admonition block in their `@moduledoc` that explains how
  `use MyModule` impacts their code. As an example, the `GenServer` documentation
  outlines:

  > #### `use GenServer` {: .info}
  >
  > When you `use GenServer`, the `GenServer` module will
  > set `@behaviour GenServer` and define a `child_spec/1`
  > function, so your module can be used as a child
  > in a supervision tree.

  This provides a quick summary of how using a module impacts the user code.
  Keep in mind to only list changes made to the public API of the module.
  For example, if `use MyModule` sets an internal attribute called
  `@_my_module_info` and this attribute is never meant to be public,
  it must not be listed.

  For convenience, the markup notation to generate the admonition block
  above is:

  ```
  > #### `use GenServer` {: .info}
  >
  > When you `use GenServer`, the GenServer module will
  > set `@behaviour GenServer` and define a `child_spec/1`
  > function, so your module can be used as a child
  > in a supervision tree.
  ```
  """
  defmacro use(module, opts \\ []) do
    calls =
      Enum.map(expand_aliases(module, __CALLER__), fn
        expanded when is_atom(expanded) ->
          quote do
            require unquote(expanded)
            unquote(expanded).__using__(unquote(opts))
          end

        _otherwise ->
          raise ArgumentError,
                "invalid arguments for use, " <>
                  "expected a compile time atom or alias, got: #{Macro.to_string(module)}"
      end)

    quote(do: (unquote_splicing(calls)))
  end

  defp expand_aliases({{:., _, [base, :{}]}, _, refs}, env) do
    base = Macro.expand(base, env)

    Enum.map(refs, fn
      {:__aliases__, _, ref} ->
        Module.concat([base | ref])

      ref when is_atom(ref) ->
        Module.concat(base, ref)

      other ->
        other
    end)
  end

  defp expand_aliases(module, env) do
    [Macro.expand(module, env)]
  end

  @doc """
  Defines a function that delegates to another module.

  Functions defined with `defdelegate/2` are public and can be invoked from
  outside the module they're defined in, as if they were defined using `def/2`.
  Therefore, `defdelegate/2` is about extending the current module's public API.
  If what you want is to invoke a function defined in another module without
  using its full module name, then use `alias/2` to shorten the module name or use
  `import/2` to be able to invoke the function without the module name altogether.

  Delegation only works with functions; delegating macros is not supported.

  Check `def/2` for rules on naming and default arguments.

  ## Options

    * `:to` - the module to dispatch to.

    * `:as` - the function to call on the target given in `:to`.
      This parameter is optional and defaults to the name being
      delegated (`funs`).

  ## Examples

      defmodule MyList do
        defdelegate reverse(list), to: Enum
        defdelegate other_reverse(list), to: Enum, as: :reverse
      end

      MyList.reverse([1, 2, 3])
      #=> [3, 2, 1]

      MyList.other_reverse([1, 2, 3])
      #=> [3, 2, 1]

  """
  defmacro defdelegate(funs, opts) do
    funs = Macro.escape(funs, unquote: true)
    opts = Macro.expand_literals(opts, %{__CALLER__ | function: {:__info__, 1}})

    quote bind_quoted: [funs: funs, opts: opts] do
      target = Kernel.Utils.defdelegate_all(funs, opts, __ENV__)

      # TODO: Remove List.wrap when multiple funs are no longer supported
      for fun <- List.wrap(funs) do
        {name, args, as, as_args} = Kernel.Utils.defdelegate_each(fun, opts)
        Elixir.Kernel.@(doc(delegate_to: {target, as, :erlang.length(as_args)}))

        # Build the call AST by hand so it doesn't get a
        # context and it warns on things like missing @impl
        def unquote({name, [line: __ENV__.line], args}) do
          unquote(target).unquote(as)(unquote_splicing(as_args))
        end
      end
    end
  end

  @doc """
  Debugs the given `code`.

  `dbg/2` can be used to debug the given `code` through a configurable debug function.
  It returns the result of the given code.

  ## Examples

  Let's take this call to `dbg/2`:

      dbg(Atom.to_string(:debugging))
      #=> "debugging"

  It returns the string `"debugging"`, which is the result of the `Atom.to_string/1` call.
  Additionally, the call above prints:

      [my_file.ex:10: MyMod.my_fun/0]
      Atom.to_string(:debugging) #=> "debugging"

  The default debugging function prints additional debugging info when dealing with
  pipelines. It prints the values at every "step" of the pipeline.

      "Elixir is cool!"
      |> String.trim_trailing("!")
      |> String.split()
      |> List.first()
      |> dbg()
      #=> "Elixir"

  The code above prints:

      [my_file.ex:10: MyMod.my_fun/0]
      "Elixir is cool!" #=> "Elixir is cool!"
      |> String.trim_trailing("!") #=> "Elixir is cool"
      |> String.split() #=> ["Elixir", "is", "cool"]
      |> List.first() #=> "Elixir"

  With no arguments, `dbg()` debugs information about the current binding. See `binding/1`.

  ## `dbg` inside IEx

  You can enable IEx to replace `dbg` with its `IEx.pry/0` backend by calling:

      $ iex --dbg pry

  In such cases, `dbg` will start a `pry` session where you can interact with
  the imports, aliases, and variables of the current environment at the location
  of the `dbg` call.

  If you call `dbg` at the end of a pipeline (using `|>`) within IEx, you are able
  to go through each step of the pipeline one by one by entering "next" (or "n").

  Note `dbg` only supports stepping for pipelines (in other words, it can only
  step through the code it sees). For general stepping, you can set breakpoints
  using `IEx.break!/4`.

  For more information, [see IEx documentation](https://hexdocs.pm/iex/IEx.html#module-dbg-and-breakpoints).

  ## Configuring the debug function

  One of the benefits of `dbg/2` is that its debugging logic is configurable,
  allowing tools to extend `dbg` with enhanced behaviour. This is done, for
  example, by `IEx` which extends `dbg` with an interactive shell where you
  can directly inspect and access values.

  The debug function can be configured at compile time through the `:dbg_callback`
  key of the `:elixir` application. The debug function must be a
  `{module, function, args}` tuple. The `function` function in `module` will be
  invoked with three arguments *prepended* to `args`:

    1. The AST of `code`
    2. The AST of `options`
    3. The `Macro.Env` environment of where `dbg/2` is invoked

  The debug function is invoked at compile time and it must also return an AST.
  The AST is expected to ultimately return the result of evaluating the debugged
  expression.

  Here's a simple example:

      defmodule MyMod do
        def debug_fun(code, options, caller, device) do
          quote do
            result = unquote(code)
            IO.inspect(unquote(device), result, label: unquote(Macro.to_string(code)))
          end
        end
      end

  To configure the debug function:

      # In config/config.exs
      config :elixir, :dbg_callback, {MyMod, :debug_fun, [:stdio]}

  ### Default debug function

  By default, the debug function we use is `Macro.dbg/3`. It just prints
  information about the code to standard output and returns the value
  returned by evaluating `code`. `options` are used to control how terms
  are inspected. They are the same options accepted by `inspect/2`.
  """
  @doc since: "1.14.0"
  defmacro dbg(code \\ quote(do: binding()), options \\ []) do
    {mod, fun, args} = Application.compile_env!(__CALLER__, :elixir, :dbg_callback)
    Macro.compile_apply(mod, fun, [code, options, __CALLER__ | args], __CALLER__)
  end

  hour_in_ms = 1000 * 60 * 60
  day_in_ms = 24 * hour_in_ms
  week_in_ms = 7 * day_in_ms

  @doc """
  Constructs a millisecond timeout from the given components, duration, or timeout.

  This function is useful for constructing timeouts to use in functions that
  expect `t:timeout/0` values (such as `Process.send_after/4` and many others).

  ## Argument

  The `duration` argument can be one of a `Duration`, a `t:timeout/0`, or a list
  of components. Each of these is described below.

  ### Passing `Duration`s

  `t:Duration.t/0` structs can be converted to timeouts. The given duration must have
  `year` and `month` fields set to `0`, since those cannot be reliably converted to
  milliseconds (due to the varying number of days in a month and year).

  Microseconds in durations are converted to milliseconds (through `System.convert_time_unit/3`).

  ### Passing components

  The `duration` argument can also be keyword list which can contain the following
  keys, each appearing at most once with a non-negative integer value:

    * `:week` - the number of weeks (a week is always 7 days)
    * `:day` - the number of days (a day is always 24 hours)
    * `:hour` - the number of hours
    * `:minute` - the number of minutes
    * `:second` - the number of seconds
    * `:millisecond` - the number of milliseconds

  The timeout is calculated as the sum of the components, each multiplied by
  the corresponding factor.

  ### Passing timeouts

  You can also pass timeouts directly to this functions, that is, milliseconds or
  the atom `:infinity`. In this case, this function just returns the given argument.

  ## Examples

  With a keyword list:

      iex> to_timeout(hour: 1, minute: 30)
      5400000

  With a duration:

      iex> to_timeout(%Duration{hour: 1, minute: 30})
      5400000

  With a timeout:

      iex> to_timeout(5400000)
      5400000
      iex> to_timeout(:infinity)
      :infinity

  """
  @doc since: "1.17.0"
  @spec to_timeout([{unit, non_neg_integer()}] | timeout() | Duration.t()) :: timeout()
        when unit: :week | :day | :hour | :minute | :second | :millisecond
  def to_timeout(duration)

  def to_timeout(:infinity), do: :infinity
  def to_timeout(timeout) when is_integer(timeout) and timeout >= 0, do: timeout

  def to_timeout(%{__struct__: Duration} = duration) do
    case duration do
      %{year: year} when year != 0 ->
        raise ArgumentError,
              "duration with a non-zero year cannot be reliably converted to timeouts"

      %{month: month} when month != 0 ->
        raise ArgumentError,
              "duration with a non-zero month cannot be reliably converted to timeouts"

      _other ->
        {microsecond, _precision} = duration.microsecond
        millisecond = :erlang.convert_time_unit(microsecond, :microsecond, :millisecond)

        duration.week * unquote(week_in_ms) +
          duration.day * unquote(day_in_ms) +
          duration.hour * unquote(hour_in_ms) +
          duration.minute * 60_000 +
          duration.second * 1000 +
          millisecond
    end
  end

  def to_timeout(components) when is_list(components) do
    reducer = fn
      {key, value}, {acc, seen_keys} when is_integer(value) and value >= 0 ->
        case :lists.member(key, seen_keys) do
          true ->
            raise ArgumentError, "timeout component #{inspect(key)} is duplicated"

          false ->
            :ok
        end

        factor =
          case key do
            :week ->
              unquote(week_in_ms)

            :day ->
              unquote(day_in_ms)

            :hour ->
              unquote(hour_in_ms)

            :minute ->
              60_000

            :second ->
              1000

            :millisecond ->
              1

            other ->
              raise ArgumentError, """
              timeout component #{inspect(other)} is not a valid timeout component, valid \
              values are: :week, :day, :hour, :minute, :second, :millisecond\
              """
          end

        {acc + value * factor, [key | seen_keys]}

      {key, value}, {_acc, _seen_keys} ->
        raise ArgumentError,
              "timeout component #{inspect(key)} must be a non-negative " <>
                "integer, got: #{inspect(value)}"
    end

    elem(:lists.foldl(reducer, {0, _seen_keys = []}, components), 0)
  end

  ## Sigils

  @doc ~S"""
  Handles the sigil `~S` for strings.

  It returns a string without interpolations and without escape
  characters.

  ## Examples

      iex> ~S(foo)
      "foo"
      iex> ~S(f#{o}o)
      "f\#{o}o"
      iex> ~S(\o/)
      "\\o/"

  """
  defmacro sigil_S(term, modifiers)
  defmacro sigil_S({:<<>>, _, [binary]}, []) when is_binary(binary), do: binary

  @doc ~S"""
  Handles the sigil `~s` for strings.

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

  defmacro sigil_s({:<<>>, _, [piece]}, []) when is_binary(piece) do
    :elixir_interpolation.unescape_string(piece)
  end

  defmacro sigil_s({:<<>>, line, pieces}, []) do
    {:<<>>, line, unescape_tokens(pieces)}
  end

  @doc ~S"""
  Handles the sigil `~C` for charlists.

  It returns a charlist without interpolations and without escape
  characters.

  A charlist is a list of integers where all the integers are valid code points.
  The three expressions below are equivalent:

      ~C"foo\n"
      [?f, ?o, ?o, ?\\, ?n]
      [102, 111, 111, 92, 110]

  In practice, charlists are mostly used in specific scenarios such as
  interfacing with older Erlang libraries that do not accept binaries as arguments.

  ## Examples

      iex> ~C(foo)
      ~c"foo"

      iex> ~C(f#{o}o)
      ~c"f\#{o}o"

      iex> ~C(foo\n)
      ~c"foo\\n"

  """
  defmacro sigil_C(term, modifiers)

  defmacro sigil_C({:<<>>, _meta, [string]}, []) when is_binary(string) do
    String.to_charlist(string)
  end

  @doc ~S"""
  Handles the sigil `~c` for charlists.

  It returns a charlist, unescaping characters and replacing interpolations.

  A charlist is a list of integers where all the integers are valid code points.
  The three expressions below are equivalent:

      ~c"foo"
      [?f, ?o, ?o]
      [102, 111, 111]

  In practice, charlists are mostly used in specific scenarios such as
  interfacing with older Erlang libraries that do not accept binaries as arguments.

  ## Examples

      iex> ~c(foo)
      ~c"foo"

      iex> ~c(f#{:o}o)
      ~c"foo"

      iex> ~c(f\#{:o}o)
      ~c"f\#{:o}o"

  The list is only printed as a `~c` sigil if all code points are within the
  ASCII range:

      iex> ~c"hełło"
      [104, 101, 322, 322, 111]

      iex> [104, 101, 108, 108, 111]
      ~c"hello"

  See `Inspect.Opts` for more information.
  """
  defmacro sigil_c(term, modifiers)

  # We can skip the runtime conversion if we are
  # creating a binary made solely of series of chars.
  defmacro sigil_c({:<<>>, _meta, [string]}, []) when is_binary(string) do
    String.to_charlist(:elixir_interpolation.unescape_string(string))
  end

  defmacro sigil_c({:<<>>, _meta, pieces}, []) do
    quote(do: List.to_charlist(unquote(unescape_list_tokens(pieces))))
  end

  @doc ~S"""
  Handles the sigil `~r` for regular expressions.

  It returns a regular expression pattern, unescaping characters and replacing
  interpolations.

  More information on regular expressions can be found in the `Regex` module.

  ## Examples

      iex> Regex.match?(~r/foo/, "foo")
      true

      iex> Regex.match?(~r/a#{:b}c/, "abc")
      true

  While the `~r` sigil allows parens and brackets to be used as delimiters,
  it is preferred to use `"` or `/` to avoid escaping conflicts with reserved
  regex characters.
  """
  defmacro sigil_r(term, modifiers)

  defmacro sigil_r({:<<>>, _meta, [string]}, options) when is_binary(string) do
    binary = :elixir_interpolation.unescape_string(string, &regex_unescape_map/1)
    regex = Regex.compile!(binary, :binary.list_to_bin(options))
    Macro.escape(regex)
  end

  defmacro sigil_r({:<<>>, meta, pieces}, options) do
    binary = {:<<>>, meta, unescape_tokens(pieces, &regex_unescape_map/1)}
    quote(do: Regex.compile!(unquote(binary), unquote(:binary.list_to_bin(options))))
  end

  defp regex_unescape_map(:newline), do: true
  defp regex_unescape_map(_), do: false

  @doc false
  defmacro sigil_R({:<<>>, _meta, [string]}, options) when is_binary(string) do
    IO.warn(
      "~R/.../ is deprecated, use ~r/.../ instead",
      Macro.Env.stacktrace(__CALLER__)
    )

    regex = Regex.compile!(string, :binary.list_to_bin(options))
    Macro.escape(regex)
  end

  @doc ~S"""
  Handles the sigil `~D` for dates.

  By default, this sigil uses the built-in `Calendar.ISO`, which
  requires dates to be written in the ISO8601 format:

      ~D[yyyy-mm-dd]

  such as:

      ~D[2015-01-13]

  If you are using alternative calendars, any representation can
  be used as long as you follow the representation by a single space
  and the calendar name:

      ~D[SOME-REPRESENTATION My.Alternative.Calendar]

  The lower case `~d` variant does not exist as interpolation
  and escape characters are not useful for date sigils.

  More information on dates can be found in the `Date` module.

  ## Examples

      iex> ~D[2015-01-13]
      ~D[2015-01-13]

  """
  defmacro sigil_D(date_string, modifiers)

  defmacro sigil_D({:<<>>, _, [string]}, []) do
    {{:ok, {year, month, day}}, calendar} = parse_with_calendar!(string, :parse_date, "Date")
    to_calendar_struct(Date, calendar: calendar, year: year, month: month, day: day)
  end

  @doc ~S"""
  Handles the sigil `~T` for times.

  By default, this sigil uses the built-in `Calendar.ISO`, which
  requires times to be written in the ISO8601 format:

      ~T[hh:mm:ss]
      ~T[hh:mm:ss.ssssss]

  such as:

      ~T[13:00:07]
      ~T[13:00:07.123]

  If you are using alternative calendars, any representation can
  be used as long as you follow the representation by a single space
  and the calendar name:

      ~T[SOME-REPRESENTATION My.Alternative.Calendar]

  The lower case `~t` variant does not exist as interpolation
  and escape characters are not useful for time sigils.

  More information on times can be found in the `Time` module.

  ## Examples

      iex> ~T[13:00:07]
      ~T[13:00:07]
      iex> ~T[13:00:07.001]
      ~T[13:00:07.001]

  """
  defmacro sigil_T(time_string, modifiers)

  defmacro sigil_T({:<<>>, _, [string]}, []) do
    {{:ok, {hour, minute, second, microsecond}}, calendar} =
      parse_with_calendar!(string, :parse_time, "Time")

    to_calendar_struct(Time,
      calendar: calendar,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    )
  end

  @doc ~S"""
  Handles the sigil `~N` for naive date times.

  By default, this sigil uses the built-in `Calendar.ISO`, which
  requires naive date times to be written in the ISO8601 format:

      ~N[yyyy-mm-dd hh:mm:ss]
      ~N[yyyy-mm-dd hh:mm:ss.ssssss]
      ~N[yyyy-mm-ddThh:mm:ss.ssssss]

  such as:

      ~N[2015-01-13 13:00:07]
      ~N[2015-01-13T13:00:07.123]

  If you are using alternative calendars, any representation can
  be used as long as you follow the representation by a single space
  and the calendar name:

      ~N[SOME-REPRESENTATION My.Alternative.Calendar]

  The lower case `~n` variant does not exist as interpolation
  and escape characters are not useful for date time sigils.

  More information on naive date times can be found in the
  `NaiveDateTime` module.

  ## Examples

      iex> ~N[2015-01-13 13:00:07]
      ~N[2015-01-13 13:00:07]
      iex> ~N[2015-01-13T13:00:07.001]
      ~N[2015-01-13 13:00:07.001]

  """
  defmacro sigil_N(naive_datetime_string, modifiers)

  defmacro sigil_N({:<<>>, _, [string]}, []) do
    {{:ok, {year, month, day, hour, minute, second, microsecond}}, calendar} =
      parse_with_calendar!(string, :parse_naive_datetime, "NaiveDateTime")

    to_calendar_struct(NaiveDateTime,
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    )
  end

  @doc ~S"""
  Handles the sigil `~U` to create a UTC `DateTime`.

  By default, this sigil uses the built-in `Calendar.ISO`, which
  requires UTC date times to be written in the ISO8601 format:

      ~U[yyyy-mm-dd hh:mm:ssZ]
      ~U[yyyy-mm-dd hh:mm:ss.ssssssZ]
      ~U[yyyy-mm-ddThh:mm:ss.ssssss+00:00]

  such as:

      ~U[2015-01-13 13:00:07Z]
      ~U[2015-01-13T13:00:07.123+00:00]

  If you are using alternative calendars, any representation can
  be used as long as you follow the representation by a single space
  and the calendar name:

      ~U[SOME-REPRESENTATION My.Alternative.Calendar]

  The given `datetime_string` must include "Z" or "00:00" offset
  which marks it as UTC, otherwise an error is raised.

  The lower case `~u` variant does not exist as interpolation
  and escape characters are not useful for date time sigils.

  More information on date times can be found in the `DateTime` module.

  ## Examples

      iex> ~U[2015-01-13 13:00:07Z]
      ~U[2015-01-13 13:00:07Z]
      iex> ~U[2015-01-13T13:00:07.001+00:00]
      ~U[2015-01-13 13:00:07.001Z]

  """
  @doc since: "1.9.0"
  defmacro sigil_U(datetime_string, modifiers)

  defmacro sigil_U({:<<>>, _, [string]}, []) do
    {{:ok, {year, month, day, hour, minute, second, microsecond}, offset}, calendar} =
      parse_with_calendar!(string, :parse_utc_datetime, "UTC DateTime")

    if offset != 0 do
      raise ArgumentError,
            "cannot parse #{inspect(string)} as UTC DateTime for #{inspect(calendar)}, reason: :non_utc_offset"
    end

    to_calendar_struct(DateTime,
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      time_zone: "Etc/UTC",
      zone_abbr: "UTC",
      utc_offset: 0,
      std_offset: 0
    )
  end

  defp parse_with_calendar!(string, fun, context) do
    {calendar, string} = extract_calendar(string)
    result = apply(calendar, fun, [string])
    {maybe_raise!(result, calendar, context, string), calendar}
  end

  defp extract_calendar(string) do
    case :binary.split(string, " ", [:global]) do
      [_] -> {Calendar.ISO, string}
      parts -> maybe_atomize_calendar(List.last(parts), string)
    end
  end

  defp maybe_atomize_calendar(<<alias, _::binary>> = last_part, string)
       when alias >= ?A and alias <= ?Z do
    string = binary_part(string, 0, byte_size(string) - byte_size(last_part) - 1)
    {String.to_atom("Elixir." <> last_part), string}
  end

  defp maybe_atomize_calendar(_last_part, string) do
    {Calendar.ISO, string}
  end

  defp maybe_raise!({:error, reason}, calendar, type, string) do
    raise ArgumentError,
          "cannot parse #{inspect(string)} as #{type} for #{inspect(calendar)}, " <>
            "reason: #{inspect(reason)}"
  end

  defp maybe_raise!(other, _calendar, _type, _string), do: other

  defp to_calendar_struct(type, fields) do
    quote do
      %{unquote_splicing([__struct__: type] ++ fields)}
    end
  end

  @doc ~S"""
  Handles the sigil `~w` for list of words.

  It returns a list of "words" split by whitespace. Character unescaping and
  interpolation happens for each word.

  ## Modifiers

    * `s`: words in the list are strings (default)
    * `a`: words in the list are atoms
    * `c`: words in the list are charlists

  ## Examples

      iex> ~w(foo #{:bar} baz)
      ["foo", "bar", "baz"]

      iex> ~w(foo #{" bar baz "})
      ["foo", "bar", "baz"]

      iex> ~w(--source test/enum_test.exs)
      ["--source", "test/enum_test.exs"]

      iex> ~w(foo bar baz)a
      [:foo, :bar, :baz]

      iex> ~w(foo bar baz)c
      [~c"foo", ~c"bar", ~c"baz"]

  """
  defmacro sigil_w(term, modifiers)

  defmacro sigil_w({:<<>>, _meta, [string]}, modifiers) when is_binary(string) do
    split_words(:elixir_interpolation.unescape_string(string), modifiers, __CALLER__)
  end

  defmacro sigil_w({:<<>>, meta, pieces}, modifiers) do
    binary = {:<<>>, meta, unescape_tokens(pieces)}
    split_words(binary, modifiers, __CALLER__)
  end

  @doc ~S"""
  Handles the sigil `~W` for list of words.

  It returns a list of "words" split by whitespace without interpolations
  and without escape characters.

  ## Modifiers

    * `s`: words in the list are strings (default)
    * `a`: words in the list are atoms
    * `c`: words in the list are charlists

  ## Examples

      iex> ~W(foo #{bar} baz)
      ["foo", "\#{bar}", "baz"]

  """
  defmacro sigil_W(term, modifiers)

  defmacro sigil_W({:<<>>, _meta, [string]}, modifiers) when is_binary(string) do
    split_words(string, modifiers, __CALLER__)
  end

  defp split_words(string, [], caller) do
    split_words(string, [?s], caller)
  end

  defp split_words(string, [mod], caller)
       when mod == ?s or mod == ?a or mod == ?c do
    case is_binary(string) do
      true ->
        parts = String.split(string)

        parts_with_trailing_comma =
          :lists.filter(&(byte_size(&1) > 1 and :binary.last(&1) == ?,), parts)

        if parts_with_trailing_comma != [] do
          stacktrace = Macro.Env.stacktrace(caller)

          IO.warn(
            "the sigils ~w/~W do not allow trailing commas at the end of each word. " <>
              "If the comma is necessary, define a regular list with [...], otherwise remove the comma.",
            stacktrace
          )
        end

        case mod do
          ?s -> parts
          ?a -> :lists.map(&String.to_atom/1, parts)
          ?c -> :lists.map(&String.to_charlist/1, parts)
        end

      false ->
        parts = quote(do: String.split(unquote(string)))

        case mod do
          ?s -> parts
          ?a -> quote(do: :lists.map(&String.to_atom/1, unquote(parts)))
          ?c -> quote(do: :lists.map(&String.to_charlist/1, unquote(parts)))
        end
    end
  end

  defp split_words(_string, _mods, _caller) do
    raise ArgumentError, "modifier must be one of: s, a, c"
  end

  ## Shared functions

  defp assert_module_scope(env, fun, arity) do
    case env.module do
      nil -> raise ArgumentError, "cannot invoke #{fun}/#{arity} outside module"
      mod -> mod
    end
  end

  defp assert_no_function_scope(env, fun, arity) do
    case env.function do
      nil -> :ok
      _ -> raise ArgumentError, "cannot invoke #{fun}/#{arity} inside function/macro"
    end
  end

  defp assert_no_match_or_guard_scope(context, exp) do
    case context do
      :match ->
        invalid_match!(exp)

      :guard ->
        raise ArgumentError,
              "invalid expression in guard, #{exp} is not allowed in guards. " <>
                "To learn more about guards, visit: https://hexdocs.pm/elixir/patterns-and-guards.html"

      _ ->
        :ok
    end
  end

  defp invalid_match!(exp) do
    raise ArgumentError,
          "invalid expression in match, #{exp} is not allowed in patterns " <>
            "such as function clauses, case clauses or on the left side of the = operator"
  end

  # Helper to handle the :ok | :error tuple returned from :elixir_interpolation.unescape_tokens
  # We need to do this for bootstrapping purposes, actual code can use Macro.unescape_string.
  defp unescape_tokens(tokens) do
    :lists.map(
      fn token ->
        case is_binary(token) do
          true -> :elixir_interpolation.unescape_string(token)
          false -> token
        end
      end,
      tokens
    )
  end

  defp unescape_tokens(tokens, unescape_map) do
    :lists.map(
      fn token ->
        case is_binary(token) do
          true -> :elixir_interpolation.unescape_string(token, unescape_map)
          false -> token
        end
      end,
      tokens
    )
  end

  defp unescape_list_tokens(tokens) do
    escape = fn
      {:"::", _, [expr, _]} -> expr
      binary when is_binary(binary) -> :elixir_interpolation.unescape_string(binary)
    end

    :lists.map(escape, tokens)
  end

  @doc false
  defmacro to_char_list(arg) do
    IO.warn(
      "Kernel.to_char_list/1 is deprecated, use Kernel.to_charlist/1 instead",
      Macro.Env.stacktrace(__CALLER__)
    )

    quote(do: Kernel.to_charlist(unquote(arg)))
  end
end
