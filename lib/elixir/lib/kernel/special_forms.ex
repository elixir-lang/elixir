defmodule Kernel.SpecialForms do
  @moduledoc """
  Special forms are the basic building blocks of Elixir, and therefore
  cannot be overridden by the developer.

  We define them in this module. Some of these forms are lexical (like
  `alias/2`, `case/2`, etc.). The macros `{}/1` and `<<>>/1` are also special
  forms used to define tuple and binary data structures respectively.

  This module also documents macros that return information about Elixir's
  compilation environment, such as (`__ENV__/0`, `__MODULE__/0`, `__DIR__/0` and `__CALLER__/0`).

  Finally, it also documents two special forms, `__block__/1` and
  `__aliases__/1`, which are not intended to be called directly by the
  developer but they appear in quoted contents since they are essential
  in Elixir's constructs.
  """

  defmacrop error!(args) do
    quote do
      _ = unquote(args)

      message =
        "Elixir's special forms are expanded by the compiler and must not be invoked directly"

      :erlang.error(RuntimeError.exception(message))
    end
  end

  @doc """
  Creates a tuple.

  More information about the tuple data type and about functions to manipulate
  tuples can be found in the `Tuple` module; some functions for working with
  tuples are also available in `Kernel` (such as `Kernel.elem/2` or
  `Kernel.tuple_size/1`).

  ## AST representation

  Only two-element tuples are considered literals in Elixir and return themselves
  when quoted. Therefore, all other tuples are represented in the AST as calls to
  the `:{}` special form.

      iex> quote do
      ...>   {1, 2}
      ...> end
      {1, 2}

      iex> quote do
      ...>   {1, 2, 3}
      ...> end
      {:{}, [], [1, 2, 3]}

  """
  defmacro unquote(:{})(args), do: error!([args])

  @doc """
  Creates a map.

  See the `Map` module for more information about maps, their syntax, and ways to
  access and manipulate them.

  ## AST representation

  Regardless of whether `=>` or the keyword syntax is used, key-value pairs in
  maps are always represented internally as a list of two-element tuples for
  simplicity:

      iex> quote do
      ...>   %{"a" => :b, c: :d}
      ...> end
      {:%{}, [], [{"a", :b}, {:c, :d}]}

  """
  defmacro unquote(:%{})(args), do: error!([args])

  @doc """
  Matches on or builds a struct.

  A struct is a tagged map that allows developers to provide
  default values for keys, tags to be used in polymorphic
  dispatches and compile time assertions.

  Structs are usually defined with the `Kernel.defstruct/1` macro:

      defmodule User do
        defstruct name: "john", age: 27
      end

  Now a struct can be created as follows:

      %User{}

  Underneath a struct is just a map with a `:__struct__` key
  pointing to the `User` module:

      %User{} == %{__struct__: User, name: "john", age: 27}

  The struct fields can be given when building the struct:

      %User{age: 31}
      #=> %{__struct__: User, name: "john", age: 31}

  Or also on pattern matching to extract values out:

      %User{age: age} = user

  An update operation specific for structs is also available:

      %User{user | age: 28}

  The advantage of structs is that they validate that the given
  keys are part of the defined struct. The example below will fail
  because there is no key `:full_name` in the `User` struct:

      %User{full_name: "john doe"}

  The syntax above will guarantee the given keys are valid at
  compilation time and it will guarantee at runtime the given
  argument is a struct, failing with `BadStructError` otherwise.

  Although structs are maps, by default structs do not implement
  any of the protocols implemented for maps. Check
  `Kernel.defprotocol/2` for more information on how structs
  can be used with protocols for polymorphic dispatch. Also
  see `Kernel.struct/2` and `Kernel.struct!/2` for examples on
  how to create and update structs dynamically.

  ## Pattern matching on struct names

  Besides allowing pattern matching on struct fields, such as:

      %User{age: age} = user

  Structs also allow pattern matching on the struct name:

      %struct_name{} = user
      struct_name #=> User

  You can also assign the struct name to `_` when you want to
  check if something is a struct but you are not interested in
  its name:

      %_{} = user

  """
  defmacro unquote(:%)(struct, map), do: error!([struct, map])

  @doc """
  Defines a new bitstring.

  ## Examples

      iex> <<1, 2, 3>>
      <<1, 2, 3>>

  ## Types

  A bitstring is made of many segments and each segment has a
  type. There are 9 types used in bitstrings:

  - `integer`
  - `float`
  - `bits` (alias for `bitstring`)
  - `bitstring`
  - `binary`
  - `bytes` (alias for `binary`)
  - `utf8`
  - `utf16`
  - `utf32`

  When no type is specified, the default is `integer`:

      iex> <<1, 2, 3>>
      <<1, 2, 3>>

  Elixir also accepts by default the segment to be a literal
  string or a literal charlist, which are by default expanded to integers:

      iex> <<0, "foo">>
      <<0, 102, 111, 111>>

  Variables or any other type need to be explicitly tagged:

      iex> rest = "oo"
      iex> <<102, rest>>
      ** (ArgumentError) argument error

  We can solve this by explicitly tagging it as `binary`:

      iex> rest = "oo"
      iex> <<102, rest::binary>>
      "foo"

  The `utf8`, `utf16`, and `utf32` types are for Unicode code points. They
  can also be applied to literal strings and charlists:

      iex> <<"foo"::utf16>>
      <<0, 102, 0, 111, 0, 111>>
      iex> <<"foo"::utf32>>
      <<0, 0, 0, 102, 0, 0, 0, 111, 0, 0, 0, 111>>

  ## Options

  Many options can be given by using `-` as separator. Order is
  arbitrary, so the following are all equivalent:

      <<102::integer-native, rest::binary>>
      <<102::native-integer, rest::binary>>
      <<102::unsigned-big-integer, rest::binary>>
      <<102::unsigned-big-integer-size(8), rest::binary>>
      <<102::unsigned-big-integer-8, rest::binary>>
      <<102::8-integer-big-unsigned, rest::binary>>
      <<102, rest::binary>>

  ### Unit and Size

  The length of the match is equal to the `unit` (a number of bits) times the
  `size` (the number of repeated segments of length `unit`).

  Type      | Default Unit
  --------- | ------------
  `integer` | 1 bit
  `float`   | 1 bit
  `binary`  | 8 bits

  Sizes for types are a bit more nuanced. The default size for integers is 8.

  For floats, it is 64. For floats, `size * unit` must result in 32 or 64,
  corresponding to [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point)
  binary32 and binary64, respectively.

  For binaries, the default is the size of the binary. Only the last binary in a
  match can use the default size. All others must have their size specified
  explicitly, even if the match is unambiguous. For example:

      iex> <<name::binary-size(5), " the ", species::binary>> = <<"Frank the Walrus">>
      "Frank the Walrus"
      iex> {name, species}
      {"Frank", "Walrus"}

  The size can be a variable:

      iex> name_size = 5
      iex> <<name::binary-size(name_size), " the ", species::binary>> = <<"Frank the Walrus">>
      iex> {name, species}
      {"Frank", "Walrus"}

  And the variable can be defined in the match itself (prior to its use):

      iex> <<name_size::size(8), name::binary-size(name_size), " the ", species::binary>> = <<5, "Frank the Walrus">>
      iex> {name, species}
      {"Frank", "Walrus"}

  However, the size cannot be defined in the match outside the binary/bitstring match:

      {name_size, <<name::binary-size(name_size), _rest::binary>>} = {5, <<"Frank the Walrus">>}
      ** (CompileError): undefined variable "name_size" in bitstring segment

  Failing to specify the size for the non-last causes compilation to fail:

      <<name::binary, " the ", species::binary>> = <<"Frank the Walrus">>
      ** (CompileError): a binary field without size is only allowed at the end of a binary pattern

  #### Shortcut Syntax

  Size and unit can also be specified using a syntax shortcut
  when passing integer values:

      iex> x = 1
      iex> <<x::8>> == <<x::size(8)>>
      true
      iex> <<x::8*4>> == <<x::size(8)-unit(4)>>
      true

  This syntax reflects the fact the effective size is given by
  multiplying the size by the unit.

  ### Modifiers

  Some types have associated modifiers to clear up ambiguity in byte
  representation.

  Modifier             | Relevant Type(s)
  -------------------- | ----------------
  `signed`             | `integer`
  `unsigned` (default) | `integer`
  `little`             | `integer`, `float`, `utf16`, `utf32`
  `big` (default)      | `integer`, `float`, `utf16`, `utf32`
  `native`             | `integer`, `utf16`, `utf32`

  ### Sign

  Integers can be `signed` or `unsigned`, defaulting to `unsigned`.

      iex> <<int::integer>> = <<-100>>
      <<156>>
      iex> int
      156
      iex> <<int::integer-signed>> = <<-100>>
      <<156>>
      iex> int
      -100

  `signed` and `unsigned` are only used for matching binaries (see below) and
  are only used for integers.

      iex> <<-100::signed, _rest::binary>> = <<-100, "foo">>
      <<156, 102, 111, 111>>

  ### Endianness

  Elixir has three options for endianness: `big`, `little`, and `native`.
  The default is `big`:

      iex> <<number::little-integer-size(16)>> = <<0, 1>>
      <<0, 1>>
      iex> number
      256
      iex> <<number::big-integer-size(16)>> = <<0, 1>>
      <<0, 1>>
      iex> number
      1

  `native` is determined by the VM at startup and will depend on the
  host operating system.

  ## Binary/Bitstring Matching

  Binary matching is a powerful feature in Elixir that is useful for extracting
  information from binaries as well as pattern matching.

  Binary matching can be used by itself to extract information from binaries:

      iex> <<"Hello, ", place::binary>> = "Hello, World"
      "Hello, World"
      iex> place
      "World"

  Or as a part of function definitions to pattern match:

      defmodule ImageTyper do
        @png_signature <<137::size(8), 80::size(8), 78::size(8), 71::size(8),
                         13::size(8), 10::size(8), 26::size(8), 10::size(8)>>
        @jpg_signature <<255::size(8), 216::size(8)>>

        def type(<<@png_signature, rest::binary>>), do: :png
        def type(<<@jpg_signature, rest::binary>>), do: :jpg
        def type(_), do: :unknown
      end

  ### Performance & Optimizations

  The Erlang compiler can provide a number of optimizations on binary creation
  and matching. To see optimization output, set the `bin_opt_info` compiler
  option:

      ERL_COMPILER_OPTIONS=bin_opt_info mix compile

  To learn more about specific optimizations and performance considerations,
  check out
  [Erlang's Efficiency Guide on handling binaries](http://www.erlang.org/doc/efficiency_guide/binaryhandling.html).
  """
  defmacro unquote(:<<>>)(args), do: error!([args])

  @doc """
  Defines a remote call, a call to an anonymous function, or an alias.

  The dot (`.`) in Elixir can be used for remote calls:

      iex> String.downcase("FOO")
      "foo"

  In this example above, we have used `.` to invoke `downcase` in the
  `String` module, passing `"FOO"` as argument.

  The dot may be used to invoke anonymous functions too:

      iex> (fn n -> n end).(7)
      7

  in which case there is a function on the left hand side.

  We can also use the dot for creating aliases:

      iex> Hello.World
      Hello.World

  This time, we have joined two aliases, defining the final alias
  `Hello.World`.

  ## Syntax

  The right side of `.` may be a word starting with an uppercase letter, which represents
  an alias, a word starting with lowercase or underscore, any valid language
  operator or any name wrapped in single- or double-quotes. Those are all valid
  examples:

      iex> Kernel.Sample
      Kernel.Sample

      iex> Kernel.length([1, 2, 3])
      3

      iex> Kernel.+(1, 2)
      3

      iex> Kernel."+"(1, 2)
      3

  Wrapping the function name in single- or double-quotes is always a
  remote call. Therefore `Kernel."Foo"` will attempt to call the function "Foo"
  and not return the alias `Kernel.Foo`. This is done by design as module names
  are more strict than function names.

  When the dot is used to invoke an anonymous function there is only one
  operand, but it is still written using a postfix notation:

      iex> negate = fn n -> -n end
      iex> negate.(7)
      -7

  ## Quoted expression

  When `.` is used, the quoted expression may take two distinct
  forms. When the right side starts with a lowercase letter (or
  underscore):

      iex> quote do
      ...>   String.downcase("FOO")
      ...> end
      {{:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}, [], ["FOO"]}

  Notice we have an inner tuple, containing the atom `:.` representing
  the dot as first element:

      {:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}

  This tuple follows the general quoted expression structure in Elixir,
  with the name as first argument, some keyword list as metadata as second,
  and the list of arguments as third. In this case, the arguments are the
  alias `String` and the atom `:downcase`. The second argument in a remote call
  is **always** an atom.

  In the case of calls to anonymous functions, the inner tuple with the dot
  special form has only one argument, reflecting the fact that the operator is
  unary:

      iex> quote do
      ...>   negate.(0)
      ...> end
      {{:., [], [{:negate, [], __MODULE__}]}, [], [0]}

  When the right side is an alias (i.e. starts with uppercase), we get instead:

      iex> quote do
      ...>   Hello.World
      ...> end
      {:__aliases__, [alias: false], [:Hello, :World]}

  We go into more details about aliases in the `__aliases__/1` special form
  documentation.

  ## Unquoting

  We can also use unquote to generate a remote call in a quoted expression:

      iex> x = :downcase
      iex> quote do
      ...>   String.unquote(x)("FOO")
      ...> end
      {{:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}, [], ["FOO"]}

  Similar to `Kernel."FUNCTION_NAME"`, `unquote(x)` will always generate a remote call,
  independent of the value of `x`. To generate an alias via the quoted expression,
  one needs to rely on `Module.concat/2`:

      iex> x = Sample
      iex> quote do
      ...>   Module.concat(String, unquote(x))
      ...> end
      {{:., [], [{:__aliases__, [alias: false], [:Module]}, :concat]}, [],
       [{:__aliases__, [alias: false], [:String]}, Sample]}

  """
  defmacro unquote(:.)(left, right), do: error!([left, right])

  @doc """
  `alias/2` is used to set up aliases, often useful with modules' names.

  ## Examples

  `alias/2` can be used to set up an alias for any module:

      defmodule Math do
        alias MyKeyword, as: Keyword
      end

  In the example above, we have set up `MyKeyword` to be aliased
  as `Keyword`. So now, any reference to `Keyword` will be
  automatically replaced by `MyKeyword`.

  In case one wants to access the original `Keyword`, it can be done
  by accessing `Elixir`:

      Keyword.values #=> uses MyKeyword.values
      Elixir.Keyword.values #=> uses Keyword.values

  Notice that calling `alias` without the `:as` option automatically
  sets an alias based on the last part of the module. For example:

      alias Foo.Bar.Baz

  Is the same as:

      alias Foo.Bar.Baz, as: Baz

  We can also alias multiple modules in one line:

      alias Foo.{Bar, Baz, Biz}

  Is the same as:

      alias Foo.Bar
      alias Foo.Baz
      alias Foo.Biz

  ## Lexical scope

  `import/2`, `require/2` and `alias/2` are called directives and all
  have lexical scope. This means you can set up aliases inside
  specific functions and it won't affect the overall scope.

  ## Warnings

  If you alias a module and you don't use the alias, Elixir is
  going to issue a warning implying the alias is not being used.

  In case the alias is generated automatically by a macro,
  Elixir won't emit any warnings though, since the alias
  was not explicitly defined.

  Both warning behaviours could be changed by explicitly
  setting the `:warn` option to `true` or `false`.

  """
  defmacro alias(module, opts), do: error!([module, opts])

  @doc """
  Requires a module in order to use its macros.

  ## Examples

  Public functions in modules are globally available, but in order to use
  macros, you need to opt-in by requiring the module they are defined in.

  Let's suppose you created your own `if/2` implementation in the module
  `MyMacros`. If you want to invoke it, you need to first explicitly
  require the `MyMacros`:

      defmodule Math do
        require MyMacros
        MyMacros.if do_something, it_works
      end

  An attempt to call a macro that was not loaded will raise an error.

  ## Alias shortcut

  `require/2` also accepts `:as` as an option so it automatically sets
  up an alias. Please check `alias/2` for more information.

  """
  defmacro require(module, opts), do: error!([module, opts])

  @doc """
  Imports functions and macros from other modules.

  `import/2` allows one to easily access functions or macros from
  other modules without using the qualified name.

  ## Examples

  If you are using several functions from a given module, you can
  import those functions and reference them as local functions,
  for example:

      iex> import List
      iex> flatten([1, [2], 3])
      [1, 2, 3]

  ## Selector

  By default, Elixir imports functions and macros from the given
  module, except the ones starting with underscore (which are
  usually callbacks):

      import List

  A developer can filter to import only macros or functions via
  the only option:

      import List, only: :functions
      import List, only: :macros

  Alternatively, Elixir allows a developer to pass pairs of
  name/arities to `:only` or `:except` as a fine grained control
  on what to import (or not):

      import List, only: [flatten: 1]
      import String, except: [split: 2]

  Notice that calling `except` is always exclusive on a previously
  declared `import/2`. If there is no previous import, then it applies
  to all functions and macros in the module. For example:

      import List, only: [flatten: 1, keyfind: 4]
      import List, except: [flatten: 1]

  After the two import calls above, only `List.keyfind/4` will be
  imported.

  ## Underscore functions

  By default functions starting with `_` are not imported. If you really want
  to import a function starting with `_` you must explicitly include it in the
  `:only` selector.

      import File.Stream, only: [__build__: 3]

  ## Lexical scope

  It is important to notice that `import/2` is lexical. This means you
  can import specific macros inside specific functions:

      defmodule Math do
        def some_function do
          # 1) Disable "if/2" from Kernel
          import Kernel, except: [if: 2]

          # 2) Require the new "if/2" macro from MyMacros
          import MyMacros

          # 3) Use the new macro
          if do_something, it_works
        end
      end

  In the example above, we imported macros from `MyMacros`,
  replacing the original `if/2` implementation by our own
  within that specific function. All other functions in that
  module will still be able to use the original one.

  ## Warnings

  If you import a module and you don't use any of the imported
  functions or macros from this module, Elixir is going to issue
  a warning implying the import is not being used.

  In case the import is generated automatically by a macro,
  Elixir won't emit any warnings though, since the import
  was not explicitly defined.

  Both warning behaviours could be changed by explicitly
  setting the `:warn` option to `true` or `false`.

  ## Ambiguous function/macro names

  If two modules `A` and `B` are imported and they both contain
  a `foo` function with an arity of `1`, an error is only emitted
  if an ambiguous call to `foo/1` is actually made; that is, the
  errors are emitted lazily, not eagerly.
  """
  defmacro import(module, opts), do: error!([module, opts])

  @doc """
  Returns the current environment information as a `Macro.Env` struct.

  In the environment you can access the current filename,
  line numbers, set up aliases, the current function and others.
  """
  defmacro __ENV__, do: error!([])

  @doc """
  Returns the current module name as an atom or `nil` otherwise.

  Although the module can be accessed in the `__ENV__/0`, this macro
  is a convenient shortcut.
  """
  defmacro __MODULE__, do: error!([])

  @doc """
  Returns the absolute path of the directory of the current file as a binary.

  Although the directory can be accessed as `Path.dirname(__ENV__.file)`,
  this macro is a convenient shortcut.
  """
  defmacro __DIR__, do: error!([])

  @doc """
  Returns the current calling environment as a `Macro.Env` struct.

  In the environment you can access the filename, line numbers,
  set up aliases, the function and others.
  """
  defmacro __CALLER__, do: error!([])

  @doc """
  Returns the stacktrace for the currently handled exception.

  It is available only in the `catch` and `rescue` clauses of `try/1`
  expressions.

  To retrieve the stacktrace of the current process, use
  `Process.info(self(), :current_stacktrace)` instead.
  """
  defmacro __STACKTRACE__, do: error!([])

  @doc """
  Accesses an already bound variable in match clauses. Also known as the pin operator.

  ## Examples

  Elixir allows variables to be rebound via static single assignment:

      iex> x = 1
      iex> x = x + 1
      iex> x
      2

  However, in some situations, it is useful to match against an existing
  value, instead of rebinding. This can be done with the `^` special form,
  colloquially known as the pin operator:

      iex> x = 1
      iex> ^x = List.first([1])
      iex> ^x = List.first([2])
      ** (MatchError) no match of right hand side value: 2

  Note that `^x` always refers to the value of `x` prior to the match. The
  following example will match:

      iex> x = 0
      iex> {x, ^x} = {1, 0}
      iex> x
      1

  """
  defmacro ^var, do: error!([var])

  @doc """
  Matches the value on the right against the pattern on the left.
  """
  defmacro left = right, do: error!([left, right])

  @doc """
  Used by types and bitstrings to specify types.

  This operator is used in two distinct occasions in Elixir.
  It is used in typespecs to specify the type of a variable,
  function or of a type itself:

      @type number :: integer | float
      @spec add(number, number) :: number

  It may also be used in bit strings to specify the type
  of a given bit segment:

      <<int::integer-little, rest::bits>> = bits

  Read the documentation on the `Typespec` page and
  `<<>>/1` for more information on typespecs and
  bitstrings respectively.
  """
  defmacro left :: right, do: error!([left, right])

  @doc ~S"""
  Gets the representation of any expression.

  ## Examples

      iex> quote do
      ...>   sum(1, 2, 3)
      ...> end
      {:sum, [], [1, 2, 3]}

  ## Elixir's AST (Abstract Syntax Tree)

  Any Elixir code can be represented using Elixir data structures.
  The building block of Elixir macros is a tuple with three elements,
  for example:

      {:sum, [], [1, 2, 3]}

  The tuple above represents a function call to `sum` passing 1, 2 and
  3 as arguments. The tuple elements are:

    * The first element of the tuple is always an atom or
      another tuple in the same representation.

    * The second element of the tuple represents metadata.

    * The third element of the tuple are the arguments for the
      function call. The third argument may be an atom, which is
      usually a variable (or a local call).

  Besides the tuple described above, Elixir has a few literals that
  are also part of its AST. Those literals return themselves when
  quoted. They are:

      :sum         #=> Atoms
      1            #=> Integers
      2.0          #=> Floats
      [1, 2]       #=> Lists
      "strings"    #=> Strings
      {key, value} #=> Tuples with two elements

  Any other value, such as a map or a four-element tuple, must be escaped
  (`Macro.escape/1`) before being introduced into an AST.

  ## Options

    * `:unquote` - when `false`, disables unquoting. This means any `unquote`
      call will be kept as is in the AST, instead of replaced by the `unquote`
      arguments. For example:

          iex> quote do
          ...>   unquote("hello")
          ...> end
          "hello"

          iex> quote unquote: false do
          ...>   unquote("hello")
          ...> end
          {:unquote, [], ["hello"]}

    * `:location` - when set to `:keep`, keeps the current line and file from
      quote. Read the Stacktrace information section below for more
      information.

    * `:line` - sets the quoted expressions to have the given line.

    * `:generated` - marks the given chunk as generated so it does not emit warnings.
      Currently it only works on special forms (for example, you can annotate a `case`
      but not an `if`).

    * `:context` - sets the resolution context.

    * `:bind_quoted` - passes a binding to the macro. Whenever a binding is
      given, `unquote/1` is automatically disabled.

  ## Quote and macros

  `quote/2` is commonly used with macros for code generation. As an exercise,
  let's define a macro that multiplies a number by itself (squared). In practice,
  there is no reason to define such a macro (and it would actually be
  seen as a bad practice), but it is simple enough that it allows us to focus
  on the important aspects of quotes and macros:

      defmodule Math do
        defmacro squared(x) do
          quote do
            unquote(x) * unquote(x)
          end
        end
      end

  We can invoke it as:

      import Math
      IO.puts("Got #{squared(5)}")

  At first, there is nothing in this example that actually reveals it is a
  macro. But what is happening is that, at compilation time, `squared(5)`
  becomes `5 * 5`. The argument `5` is duplicated in the produced code, we
  can see this behaviour in practice though because our macro actually has
  a bug:

      import Math
      my_number = fn ->
        IO.puts("Returning 5")
        5
      end
      IO.puts("Got #{squared(my_number.())}")

  The example above will print:

      Returning 5
      Returning 5
      Got 25

  Notice how "Returning 5" was printed twice, instead of just once. This is
  because a macro receives an expression and not a value (which is what we
  would expect in a regular function). This means that:

      squared(my_number.())

  Actually expands to:

      my_number.() * my_number.()

  Which invokes the function twice, explaining why we get the printed value
  twice! In the majority of the cases, this is actually unexpected behaviour,
  and that's why one of the first things you need to keep in mind when it
  comes to macros is to **not unquote the same value more than once**.

  Let's fix our macro:

      defmodule Math do
        defmacro squared(x) do
          quote do
            x = unquote(x)
            x * x
          end
        end
      end

  Now invoking `squared(my_number.())` as before will print the value just
  once.

  In fact, this pattern is so common that most of the times you will want
  to use the `bind_quoted` option with `quote/2`:

      defmodule Math do
        defmacro squared(x) do
          quote bind_quoted: [x: x] do
            x * x
          end
        end
      end

  `:bind_quoted` will translate to the same code as the example above.
  `:bind_quoted` can be used in many cases and is seen as good practice,
  not only because it helps prevent us from running into common mistakes, but also
  because it allows us to leverage other tools exposed by macros, such as
  unquote fragments discussed in some sections below.

  Before we finish this brief introduction, you will notice that, even though
  we defined a variable `x` inside our quote:

      quote do
        x = unquote(x)
        x * x
      end

  When we call:

      import Math
      squared(5)
      x
      #=> ** (CompileError) undefined variable x or undefined function x/0

  We can see that `x` did not leak to the user context. This happens
  because Elixir macros are hygienic, a topic we will discuss at length
  in the next sections as well.

  ## Hygiene in variables

  Consider the following example:

      defmodule Hygiene do
        defmacro no_interference do
          quote do
            a = 1
          end
        end
      end

      require Hygiene

      a = 10
      Hygiene.no_interference()
      a
      #=> 10

  In the example above, `a` returns 10 even if the macro
  is apparently setting it to 1 because variables defined
  in the macro do not affect the context the macro is executed in.
  If you want to set or get a variable in the caller's context, you
  can do it with the help of the `var!` macro:

      defmodule NoHygiene do
        defmacro interference do
          quote do
            var!(a) = 1
          end
        end
      end

      require NoHygiene

      a = 10
      NoHygiene.interference()
      a
      #=> 1

  You cannot even access variables defined in the same module unless
  you explicitly give it a context:

      defmodule Hygiene do
        defmacro write do
          quote do
            a = 1
          end
        end

        defmacro read do
          quote do
            a
          end
        end
      end

      Hygiene.write()
      Hygiene.read()
      #=> ** (RuntimeError) undefined variable a or undefined function a/0

  For such, you can explicitly pass the current module scope as
  argument:

      defmodule ContextHygiene do
        defmacro write do
          quote do
            var!(a, ContextHygiene) = 1
          end
        end

        defmacro read do
          quote do
            var!(a, ContextHygiene)
          end
        end
      end

      ContextHygiene.write()
      ContextHygiene.read()
      #=> 1

  ## Hygiene in aliases

  Aliases inside quote are hygienic by default.
  Consider the following example:

      defmodule Hygiene do
        alias Map, as: M

        defmacro no_interference do
          quote do
            M.new()
          end
        end
      end

      require Hygiene
      Hygiene.no_interference()
      #=> %{}

  Notice that, even though the alias `M` is not available
  in the context the macro is expanded, the code above works
  because `M` still expands to `Map`.

  Similarly, even if we defined an alias with the same name
  before invoking a macro, it won't affect the macro's result:

      defmodule Hygiene do
        alias Map, as: M

        defmacro no_interference do
          quote do
            M.new()
          end
        end
      end

      require Hygiene
      alias SomethingElse, as: M
      Hygiene.no_interference()
      #=> %{}

  In some cases, you want to access an alias or a module defined
  in the caller. For such, you can use the `alias!` macro:

      defmodule Hygiene do
        # This will expand to Elixir.Nested.hello()
        defmacro no_interference do
          quote do
            Nested.hello()
          end
        end

        # This will expand to Nested.hello() for
        # whatever is Nested in the caller
        defmacro interference do
          quote do
            alias!(Nested).hello()
          end
        end
      end

      defmodule Parent do
        defmodule Nested do
          def hello, do: "world"
        end

        require Hygiene
        Hygiene.no_interference()
        #=> ** (UndefinedFunctionError) ...

        Hygiene.interference()
        #=> "world"
      end

  ## Hygiene in imports

  Similar to aliases, imports in Elixir are hygienic. Consider the
  following code:

      defmodule Hygiene do
        defmacrop get_length do
          quote do
            length([1, 2, 3])
          end
        end

        def return_length do
          import Kernel, except: [length: 1]
          get_length
        end
      end

      Hygiene.return_length()
      #=> 3

  Notice how `Hygiene.return_length/0` returns `3` even though the `Kernel.length/1`
  function is not imported. In fact, even if `return_length/0`
  imported a function with the same name and arity from another
  module, it wouldn't affect the function result:

      def return_length do
        import String, only: [length: 1]
        get_length
      end

  Calling this new `return_length/0` will still return `3` as result.

  Elixir is smart enough to delay the resolution to the latest
  possible moment. So, if you call `length([1, 2, 3])` inside quote,
  but no `length/1` function is available, it is then expanded in
  the caller:

      defmodule Lazy do
        defmacrop get_length do
          import Kernel, except: [length: 1]

          quote do
            length("hello")
          end
        end

        def return_length do
          import Kernel, except: [length: 1]
          import String, only: [length: 1]
          get_length
        end
      end

      Lazy.return_length()
      #=> 5

  ## Stacktrace information

  When defining functions via macros, developers have the option of
  choosing if runtime errors will be reported from the caller or from
  inside the quote. Let's see an example:

      # adder.ex
      defmodule Adder do
        @doc "Defines a function that adds two numbers"
        defmacro defadd do
          quote location: :keep do
            def add(a, b), do: a + b
          end
        end
      end

      # sample.ex
      defmodule Sample do
        import Adder
        defadd
      end

      require Sample
      Sample.add(:one, :two)
      #=> ** (ArithmeticError) bad argument in arithmetic expression
      #=>     adder.ex:5: Sample.add/2

  When using `location: :keep` and invalid arguments are given to
  `Sample.add/2`, the stacktrace information will point to the file
  and line inside the quote. Without `location: :keep`, the error is
  reported to where `defadd` was invoked. `location: :keep` affects
  only definitions inside the quote.

  ## Binding and unquote fragments

  Elixir quote/unquote mechanisms provide a functionality called
  unquote fragments. Unquote fragments provide an easy way to generate
  functions on the fly. Consider this example:

      kv = [foo: 1, bar: 2]
      Enum.each(kv, fn {k, v} ->
        def unquote(k)(), do: unquote(v)
      end)

  In the example above, we have generated the functions `foo/0` and
  `bar/0` dynamically. Now, imagine that we want to convert this
  functionality into a macro:

      defmacro defkv(kv) do
        Enum.map(kv, fn {k, v} ->
          quote do
            def unquote(k)(), do: unquote(v)
          end
        end)
      end

  We can invoke this macro as:

      defkv [foo: 1, bar: 2]

  However, we can't invoke it as follows:

      kv = [foo: 1, bar: 2]
      defkv kv

  This is because the macro is expecting its arguments to be a
  keyword list at **compilation** time. Since in the example above
  we are passing the representation of the variable `kv`, our
  code fails.

  This is actually a common pitfall when developing macros. We are
  assuming a particular shape in the macro. We can work around it
  by unquoting the variable inside the quoted expression:

      defmacro defkv(kv) do
        quote do
          Enum.each(unquote(kv), fn {k, v} ->
            def unquote(k)(), do: unquote(v)
          end)
        end
      end

  If you try to run our new macro, you will notice it won't
  even compile, complaining that the variables `k` and `v`
  do not exist. This is because of the ambiguity: `unquote(k)`
  can either be an unquote fragment, as previously, or a regular
  unquote as in `unquote(kv)`.

  One solution to this problem is to disable unquoting in the
  macro, however, doing that would make it impossible to inject the
  `kv` representation into the tree. That's when the `:bind_quoted`
  option comes to the rescue (again!). By using `:bind_quoted`, we
  can automatically disable unquoting while still injecting the
  desired variables into the tree:

      defmacro defkv(kv) do
        quote bind_quoted: [kv: kv] do
          Enum.each(kv, fn {k, v} ->
            def unquote(k)(), do: unquote(v)
          end)
        end
      end

  In fact, the `:bind_quoted` option is recommended every time
  one desires to inject a value into the quote.
  """
  defmacro quote(opts, block), do: error!([opts, block])

  @doc """
  Unquotes the given expression inside a quoted expression.

  This function expects a valid Elixir AST, also known as
  quoted expression, as argument. If you would like to `unquote`
  any value, such as a map or a four-element tuple, you should
  call `Macro.escape/1` before unquoting.

  ## Examples

  Imagine the situation you have a quoted expression and
  you want to inject it inside some quote. The first attempt
  would be:

      value =
        quote do
          13
        end

      quote do
        sum(1, value, 3)
      end

  Which would then return:

      {:sum, [], [1, {:value, [], Elixir}, 3]}

  Which is not the expected result. For this, we use `unquote`:

      iex> value =
      ...>   quote do
      ...>     13
      ...>   end
      iex> quote do
      ...>   sum(1, unquote(value), 3)
      ...> end
      {:sum, [], [1, 13, 3]}

  If you want to unquote a value that is not a quoted expression,
  such as a map, you need to call `Macro.escape/1` before:

      iex> value = %{foo: :bar}
      iex> quote do
      ...>   process_map(unquote(Macro.escape(value)))
      ...> end
      {:process_map, [], [{:%{}, [], [foo: :bar]}]}

  If you forget to escape it, Elixir will raise an error
  when compiling the code.
  """
  defmacro unquote(:unquote)(expr), do: error!([expr])

  @doc """
  Unquotes the given list expanding its arguments.

  Similar to `unquote/1`.

  ## Examples

      iex> values = [2, 3, 4]
      iex> quote do
      ...>   sum(1, unquote_splicing(values), 5)
      ...> end
      {:sum, [], [1, 2, 3, 4, 5]}

  """
  defmacro unquote(:unquote_splicing)(expr), do: error!([expr])

  @doc ~S"""
  Comprehensions allow you to quickly build a data structure from
  an enumerable or a bitstring.

  Let's start with an example:

      iex> for n <- [1, 2, 3, 4], do: n * 2
      [2, 4, 6, 8]

  A comprehension accepts many generators and filters. Enumerable
  generators are defined using `<-`:

      # A list generator:
      iex> for n <- [1, 2, 3, 4], do: n * 2
      [2, 4, 6, 8]

      # A comprehension with two generators
      iex> for x <- [1, 2], y <- [2, 3], do: x * y
      [2, 3, 4, 6]

  Filters can also be given:

      # A comprehension with a generator and a filter
      iex> for n <- [1, 2, 3, 4, 5, 6], rem(n, 2) == 0, do: n
      [2, 4, 6]

  Generators can also be used to filter as it removes any value
  that doesn't match the pattern on the left side of `<-`:

      iex> users = [user: "john", admin: "meg", guest: "barbara"]
      iex> for {type, name} when type != :guest <- users do
      ...>   String.upcase(name)
      ...> end
      ["JOHN", "MEG"]

  Bitstring generators are also supported and are very useful when you
  need to organize bitstring streams:

      iex> pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>
      iex> for <<r::8, g::8, b::8 <- pixels>>, do: {r, g, b}
      [{213, 45, 132}, {64, 76, 32}, {76, 0, 0}, {234, 32, 15}]

  Variable assignments inside the comprehension, be it in generators,
  filters or inside the block, are not reflected outside of the
  comprehension.

  ## The `:into` and `:uniq` options

  In the examples above, the result returned by the comprehension was
  always a list. The returned result can be configured by passing an
  `:into` option, that accepts any structure as long as it implements
  the `Collectable` protocol.

  For example, we can use bitstring generators with the `:into` option
  to easily remove all spaces in a string:

      iex> for <<c <- " hello world ">>, c != ?\s, into: "", do: <<c>>
      "helloworld"

  The `IO` module provides streams, that are both `Enumerable` and
  `Collectable`, here is an upcase echo server using comprehensions:

      for line <- IO.stream(:stdio, :line), into: IO.stream(:stdio, :line) do
        String.upcase(line)
      end

  Similarly, `uniq: true` can also be given to comprehensions to guarantee
  the results are only added to the collection if they were not returned
  before. For example:

      iex> for x <- [1, 1, 2, 3], uniq: true, do: x * 2
      [2, 4, 6]

      iex> for <<x <- "abcabc">>, uniq: true, into: "", do: <<x - 32>>
      "ABC"

  ## The `:reduce` option

  While the `:into` option allows us to customize the comprehension behaviour
  to a given data type, such as putting all of the values inside a map or inside
  a binary, it is not always enough.

  For example, imagine that you have a binary with letters where you want to
  count how many times each lowercase letter happens, ignoring all uppercase
  ones. For instance, for the string `"AbCabCABc"`, we want to return the map
  `%{"a" => 1, "b" => 2, "c" => 1}`.

  If we were to use `:into`, we would need a data type that computes the
  frequency of each element it holds. While there is no such data type in
  Elixir, you could implement one yourself.

  A simpler option would be to use comprehensions for the mapping and
  filtering of letters, and then we invoke `Enum.reduce/3` to build a map,
  for example:

      iex> letters = for <<x <- "AbCabCABc">>, x in ?a..?z, do: <<x>>
      iex> Enum.reduce(letters, %{}, fn x, acc -> Map.update(acc, x, 1, & &1 + 1) end)
      %{"a" => 1, "b" => 2, "c" => 1}

  While the above is straight-forward, it has the downside of traversing the
  data at least twice. If you are expecting long strings as inputs, this can
  be quite expensive.

  Luckily, comprehensions also support the `:reduce` option, which would allow
  us to fuse both steps above into a single step:

      iex> for <<x <- "AbCabCABc">>, x in ?a..?z, reduce: %{} do
      ...>   acc -> Map.update(acc, <<x>>, 1, & &1 + 1)
      ...> end
      %{"a" => 1, "b" => 2, "c" => 1}

  When the `:reduce` key is given, its value is used as the initial accumulator
  and the `do` block must be changed to use `->` clauses, where the left side
  of `->` receives the accumulated value of the previous iteration and the
  expression on the right side must return the new accumulator value. Once there are no more
  elements, the final accumulated value is returned. If there are no elements
  at all, then the initial accumulator value is returned.
  """
  defmacro for(args), do: error!([args])

  @doc """
  Used to combine matching clauses.

  Let's start with an example:

      iex> opts = %{width: 10, height: 15}
      iex> with {:ok, width} <- Map.fetch(opts, :width),
      ...>      {:ok, height} <- Map.fetch(opts, :height) do
      ...>   {:ok, width * height}
      ...> end
      {:ok, 150}

  If all clauses match, the `do` block is executed, returning its result.
  Otherwise the chain is aborted and the non-matched value is returned:

      iex> opts = %{width: 10}
      iex> with {:ok, width} <- Map.fetch(opts, :width),
      ...>      {:ok, height} <- Map.fetch(opts, :height) do
      ...>   {:ok, width * height}
      ...> end
      :error

  Guards can be used in patterns as well:

      iex> users = %{"melany" => "guest", "bob" => :admin}
      iex> with {:ok, role} when not is_binary(role) <- Map.fetch(users, "bob") do
      ...>   {:ok, to_string(role)}
      ...> end
      {:ok, "admin"}

  As in `for/1`, variables bound inside `with/1` won't leak.
  Expressions without `<-` may also be used in clauses. For instance,
  you can perform regular matches with the `=` operator:

      iex> width = nil
      iex> opts = %{width: 10, height: 15}
      iex> with {:ok, width} <- Map.fetch(opts, :width),
      ...>      double_width = width * 2,
      ...>      {:ok, height} <- Map.fetch(opts, :height) do
      ...>   {:ok, double_width * height}
      ...> end
      {:ok, 300}
      iex> width
      nil

  The behaviour of any expression in a clause is the same as outside.
  For example, `=` will raise a `MatchError` instead of returning the
  non-matched value:

      with :foo = :bar, do: :ok
      #=> ** (MatchError) no match of right hand side value: :bar

  As with any other function or macro call in Elixir, explicit parens can
  also be used around the arguments before the `do`/`end` block:

      iex> opts = %{width: 10, height: 15}
      iex> with(
      ...>   {:ok, width} <- Map.fetch(opts, :width),
      ...>   {:ok, height} <- Map.fetch(opts, :height)
      ...> ) do
      ...>   {:ok, width * height}
      ...> end
      {:ok, 150}

  The choice between parens and no parens is a matter of preference.

  An `else` option can be given to modify what is being returned from
  `with` in the case of a failed match:

      iex> opts = %{width: 10}
      iex> with {:ok, width} <- Map.fetch(opts, :width),
      ...>      {:ok, height} <- Map.fetch(opts, :height) do
      ...>   {:ok, width * height}
      ...> else
      ...>   :error ->
      ...>     {:error, :wrong_data}
      ...> end
      {:error, :wrong_data}

  If an `else` block is used and there are no matching clauses, a `WithClauseError`
  exception is raised.
  """
  defmacro with(args), do: error!([args])

  @doc """
  Defines an anonymous function.

  ## Examples

      iex> add = fn a, b -> a + b end
      iex> add.(1, 2)
      3

  Anonymous functions can also have multiple clauses. All clauses
  should expect the same number of arguments:

      iex> negate = fn
      ...>   true -> false
      ...>   false -> true
      ...> end
      iex> negate.(false)
      true

  """
  defmacro unquote(:fn)(clauses), do: error!([clauses])

  @doc """
  Internal special form for block expressions.

  This is the special form used whenever we have a block
  of expressions in Elixir. This special form is private
  and should not be invoked directly:

      iex> quote do
      ...>   1
      ...>   2
      ...>   3
      ...> end
      {:__block__, [], [1, 2, 3]}

  """
  defmacro unquote(:__block__)(args), do: error!([args])

  @doc """
  Captures or creates an anonymous function.

  ## Capture

  The capture operator is most commonly used to capture a
  function with given name and arity from a module:

      iex> fun = &Kernel.is_atom/1
      iex> fun.(:atom)
      true
      iex> fun.("string")
      false

  In the example above, we captured `Kernel.is_atom/1` as an
  anonymous function and then invoked it.

  The capture operator can also be used to capture local functions,
  including private ones, and imported functions by omitting the
  module name:

      &local_function/1

  ## Anonymous functions

  The capture operator can also be used to partially apply
  functions, where `&1`, `&2` and so on can be used as value
  placeholders. For example:

      iex> double = &(&1 * 2)
      iex> double.(2)
      4

  In other words, `&(&1 * 2)` is equivalent to `fn x -> x * 2 end`.

  We can partially apply a remote function with placeholder:

      iex> take_five = &Enum.take(&1, 5)
      iex> take_five.(1..10)
      [1, 2, 3, 4, 5]

  Another example while using an imported or local function:

      iex> first_elem = &elem(&1, 0)
      iex> first_elem.({0, 1})
      0

  The `&` operator can be used with more complex expressions:

      iex> fun = &(&1 + &2 + &3)
      iex> fun.(1, 2, 3)
      6

  As well as with lists and tuples:

      iex> fun = &{&1, &2}
      iex> fun.(1, 2)
      {1, 2}

      iex> fun = &[&1 | &2]
      iex> fun.(1, [2, 3])
      [1, 2, 3]

  The only restrictions when creating anonymous functions is that at
  least one placeholder must be present, i.e. it must contain at least
  `&1`, and that block expressions are not supported:

      # No placeholder, fails to compile.
      &(:foo)

      # Block expression, fails to compile.
      &(&1; &2)

  """
  defmacro unquote(:&)(expr), do: error!([expr])

  @doc """
  Internal special form to hold aliases information.

  It is usually compiled to an atom:

      iex> quote do
      ...>   Foo.Bar
      ...> end
      {:__aliases__, [alias: false], [:Foo, :Bar]}

  Elixir represents `Foo.Bar` as `__aliases__` so calls can be
  unambiguously identified by the operator `:.`. For example:

      iex> quote do
      ...>   Foo.bar
      ...> end
      {{:., [], [{:__aliases__, [alias: false], [:Foo]}, :bar]}, [], []}

  Whenever an expression iterator sees a `:.` as the tuple key,
  it can be sure that it represents a call and the second argument
  in the list is an atom.

  On the other hand, aliases hold some properties:

    1. The head element of aliases can be any term that must expand to
       an atom at compilation time.

    2. The tail elements of aliases are guaranteed to always be atoms.

    3. When the head element of aliases is the atom `:Elixir`, no expansion happens.

  """
  defmacro unquote(:__aliases__)(args), do: error!([args])

  @doc """
  Calls the overridden function when overriding it with `Kernel.defoverridable/1`.

  See `Kernel.defoverridable/1` for more information and documentation.
  """
  defmacro super(args), do: error!([args])

  @doc ~S"""
  Matches the given expression against the given clauses.

  ## Examples

      case thing do
        {:selector, i, value} when is_integer(i) ->
          value
        value ->
          value
      end

  In the example above, we match `thing` against each clause "head"
  and execute the clause "body" corresponding to the first clause
  that matches.

  If no clause matches, an error is raised.
  For this reason, it may be necessary to add a final catch-all clause (like `_`)
  which will always match.

      x = 10

      case x do
        0 ->
          "This clause won't match"
        _ ->
          "This clause would match any value (x = #{x})"
      end
      #=> "This clause would match any value (x = 10)"

  ## Variable handling

  Notice that variables bound in a clause "head" do not leak to the
  outer context:

      case data do
        {:ok, value} -> value
        :error -> nil
      end

      value
      #=> unbound variable value

  However, variables explicitly bound in the clause "body" are
  accessible from the outer context:

      value = 7

      case lucky? do
        false -> value = 13
        true -> true
      end

      value
      #=> 7 or 13

  In the example above, `value` is going to be `7` or `13` depending on
  the value of `lucky?`. In case `value` has no previous value before
  case, clauses that do not explicitly bind a value have the variable
  bound to `nil`.

  If you want to pattern match against an existing variable,
  you need to use the `^/1` operator:

      x = 1

      case 10 do
        ^x -> "Won't match"
        _ -> "Will match"
      end
      #=> "Will match"

  """
  defmacro case(condition, clauses), do: error!([condition, clauses])

  @doc """
  Evaluates the expression corresponding to the first clause that
  evaluates to a truthy value.

      cond do
        hd([1, 2, 3]) ->
          "1 is considered as true"
      end
      #=> "1 is considered as true"

  Raises an error if all conditions evaluate to `nil` or `false`.
  For this reason, it may be necessary to add a final always-truthy condition
  (anything non-`false` and non-`nil`), which will always match.

  ## Examples

      cond do
        1 + 1 == 1 ->
          "This will never match"
        2 * 2 != 4 ->
          "Nor this"
        true ->
          "This will"
      end
      #=> "This will"

  """
  defmacro cond(clauses), do: error!([clauses])

  @doc ~S"""
  Evaluates the given expressions and handles any error, exit,
  or throw that may have happened.

  ## Examples

      try do
        do_something_that_may_fail(some_arg)
      rescue
        ArgumentError ->
          IO.puts "Invalid argument given"
      catch
        value ->
          IO.puts "Caught #{inspect(value)}"
      else
        value ->
          IO.puts "Success! The result was #{inspect(value)}"
      after
        IO.puts "This is printed regardless if it failed or succeeded"
      end

  The `rescue` clause is used to handle exceptions while the `catch`
  clause can be used to catch thrown values and exits.
  The `else` clause can be used to control flow based on the result of
  the expression. `catch`, `rescue`, and `else` clauses work based on
  pattern matching (similar to the `case` special form).

  Calls inside `try/1` are not tail recursive since the VM needs to keep
  the stacktrace in case an exception happens. To retrieve the stacktrace,
  access `__STACKTRACE__/0` inside the `rescue` or `catch` clause.

  ## `rescue` clauses

  Besides relying on pattern matching, `rescue` clauses provide some
  conveniences around exceptions that allow one to rescue an
  exception by its name. All the following formats are valid patterns
  in `rescue` clauses:

      # Rescue a single exception without binding the exception
      # to a variable
      try do
        UndefinedModule.undefined_function
      rescue
        UndefinedFunctionError -> nil
      end

      # Rescue any of the given exception without binding
      try do
        UndefinedModule.undefined_function
      rescue
        [UndefinedFunctionError, ArgumentError] -> nil
      end

      # Rescue and bind the exception to the variable "x"
      try do
        UndefinedModule.undefined_function
      rescue
        x in [UndefinedFunctionError] -> nil
      end

      # Rescue all kinds of exceptions and bind the rescued exception
      # to the variable "x"
      try do
        UndefinedModule.undefined_function
      rescue
        x -> nil
      end

  ### Erlang errors

  Erlang errors are transformed into Elixir ones when rescuing:

      try do
        :erlang.error(:badarg)
      rescue
        ArgumentError -> :ok
      end
      #=> :ok

  The most common Erlang errors will be transformed into their
  Elixir counterpart. Those which are not will be transformed
  into the more generic `ErlangError`:

      try do
        :erlang.error(:unknown)
      rescue
        ErlangError -> :ok
      end
      #=> :ok

  In fact, `ErlangError` can be used to rescue any error that is
  not a proper Elixir error. For example, it can be used to rescue
  the earlier `:badarg` error too, prior to transformation:

      try do
        :erlang.error(:badarg)
      rescue
        ErlangError -> :ok
      end
      #=> :ok

  ## `catch` clauses

  The `catch` clause can be used to catch thrown values, exits, and errors.

  ### Catching thrown values

  `catch` can be used to catch values thrown by `Kernel.throw/1`:

      try do
        throw(:some_value)
      catch
        thrown_value ->
          IO.puts "A value was thrown: #{inspect(thrown_value)}"
      end

  ### Catching values of any kind

  The `catch` clause also supports catching exits and errors. To do that, it
  allows matching on both the *kind* of the caught value as well as the value
  itself:

      try do
        exit(:shutdown)
      catch
        :exit, value
          IO.puts "Exited with value #{inspect(value)}"
      end

      try do
        exit(:shutdown)
      catch
        kind, value when kind in [:exit, :throw] ->
          IO.puts "Caught exit or throw with value #{inspect(value)}"
      end

  The `catch` clause also supports `:error` alongside `:exit` and `:throw` as
  in Erlang, although this is commonly avoided in favor of `raise`/`rescue` control
  mechanisms. One reason for this is that when catching `:error`, the error is
  not automatically transformed into an Elixir error:

      try do
        :erlang.error(:badarg)
      catch
        :error, :badarg -> :ok
      end
      #=> :ok

  ## `after` clauses

  An `after` clause allows you to define cleanup logic that will be invoked both
  when the block of code passed to `try/1` succeeds and also when an error is raised. Note
  that the process will exit as usual when receiving an exit signal that causes
  it to exit abruptly and so the `after` clause is not guaranteed to be executed.
  Luckily, most resources in Elixir (such as open files, ETS tables, ports, sockets,
  and so on) are linked to or monitor the owning process and will automatically clean
  themselves up if that process exits.

      File.write!("tmp/story.txt", "Hello, World")
      try do
        do_something_with("tmp/story.txt")
      after
        File.rm("tmp/story.txt")
      end

  ## `else` clauses

  `else` clauses allow the result of the body passed to `try/1` to be pattern
  matched on:

      x = 2
      try do
        1 / x
      rescue
        ArithmeticError ->
          :infinity
      else
        y when y < 1 and y > -1 ->
          :small
        _ ->
          :large
      end

  If an `else` clause is not present and no exceptions are raised,
  the result of the expression will be returned:

      x = 1
      ^x =
        try do
          1 / x
        rescue
          ArithmeticError ->
            :infinity
        end

  However, when an `else` clause is present but the result of the expression
  does not match any of the patterns then an exception will be raised. This
  exception will not be caught by a `catch` or `rescue` in the same `try`:

      x = 1
      try do
        try do
          1 / x
        rescue
          # The TryClauseError cannot be rescued here:
          TryClauseError ->
            :error_a
        else
          0 ->
            :small
        end
      rescue
        # The TryClauseError is rescued here:
        TryClauseError ->
          :error_b
      end

  Similarly, an exception inside an `else` clause is not caught or rescued
  inside the same `try`:

      try do
        try do
          nil
        catch
          # The exit(1) call below can not be caught here:
          :exit, _ ->
            :exit_a
        else
          _ ->
            exit(1)
        end
      catch
        # The exit is caught here:
        :exit, _ ->
          :exit_b
      end

  This means the VM no longer needs to keep the stacktrace once inside
  an `else` clause and so tail recursion is possible when using a `try`
  with a tail call as the final call inside an `else` clause. The same
  is true for `rescue` and `catch` clauses.

  Only the result of the tried expression falls down to the `else` clause.
  If the `try` ends up in the `rescue` or `catch` clauses, their result
  will not fall down to `else`:

      try do
        throw(:catch_this)
      catch
        :throw, :catch_this ->
          :it_was_caught
      else
        # :it_was_caught will not fall down to this "else" clause.
        other ->
          {:else, other}
      end

  ## Variable handling

  Since an expression inside `try` may not have been evaluated
  due to an exception, any variable created inside `try` cannot
  be accessed externally. For instance:

      try do
        x = 1
        do_something_that_may_fail(same_arg)
        :ok
      catch
        _, _ -> :failed
      end

      x
      #=> unbound variable "x"

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
  defmacro try(args), do: error!([args])

  @doc """
  Checks if there is a message matching the given clauses
  in the current process mailbox.

  In case there is no such message, the current process hangs
  until a message arrives or waits until a given timeout value.

  ## Examples

      receive do
        {:selector, number, name} when is_integer(number) ->
          name
        name when is_atom(name) ->
          name
        _ ->
          IO.puts :stderr, "Unexpected message received"
      end

  An optional `after` clause can be given in case the message was not
  received after the given timeout period, specified in milliseconds:

      receive do
        {:selector, number, name} when is_integer(number) ->
          name
        name when is_atom(name) ->
          name
        _ ->
          IO.puts :stderr, "Unexpected message received"
      after
        5000 ->
          IO.puts :stderr, "No message in 5 seconds"
      end

  The `after` clause can be specified even if there are no match clauses.
  The timeout value given to `after` can be any expression evaluating to
  one of the allowed values:

    * `:infinity` - the process should wait indefinitely for a matching
      message, this is the same as not using the after clause

    * `0` - if there is no matching message in the mailbox, the timeout
      will occur immediately

    * positive integer smaller than or equal to `4_294_967_295` (`0xFFFFFFFF`
      in hexadecimal notation) - it should be possible to represent the timeout
      value as an unsigned 32-bit integer.

  ## Variable handling

  The `receive/1` special form handles variables exactly as the `case/2`
  special macro. For more information, check the docs for `case/2`.
  """
  defmacro receive(args), do: error!([args])
end
