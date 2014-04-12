defmodule Kernel.SpecialForms do
  @moduledoc """
  In this module we define Elixir special forms. Special forms
  cannot be overridden by the developer and are the basic
  building blocks of Elixir code.

  Some of those forms are lexical (like `alias`, `case`, etc).
  The macros `{}` and `<<>>` are also special forms used to define
  tuple and binary data structures respectively.

  This module also documents Elixir's pseudo variables (`__ENV__`,
  `__MODULE__`, `__DIR__` and `__CALLER__`). Pseudo variables return
  information about Elixir's compilation environment and can only
  be read, never assigned to.

  Finally, it also documents 2 special forms, `__block__` and
  `__aliases__`, which are not intended to be called directly by the
  developer but they appear in quoted contents since they are essential
  in Elixir's constructs.
  """

  @doc """
  Creates a tuple.

  Only two item tuples are considered literals in Elixir.
  Therefore all other tuples are represented in the AST
  as a call to the special form `:{}`.

  Conveniences for manipulating tuples can be found in the
  `Tuple` module. Some functions for working with tuples are
  also available in `Kernel`, namely `Kernel.elem/2`,
  `Kernel.set_elem/3` and `Kernel.tuple_size/1`.

  ## Examples

      iex> { 1, 2, 3 }
      { 1, 2, 3 }

      iex> quote do: { 1, 2, 3 }
      { :{}, [], [1,2,3] }

  """
  defmacro unquote(:{})(args)

  @doc """
  Creates a map.

  Maps are key-value stores where keys are compared
  using the match operator (`===`). Maps can be created with
  the `%{}` special form where keys are associated via `=>`:

      %{ 1 => 2 }

  Maps also support the keyword notation, as other special forms,
  as long as they are at the end of the argument list:

      %{ hello: :world, with: :keywords }
      %{ :hello => :world, with: :keywords }

  If a map has duplicated keys, the last key will always have
  higher precedence:

      iex> %{ a: :b, a: :c }
      %{ a: :c }

  Conveniences for manipulating maps can be found in the
  `Map` module.

  ## Access syntax

  Besides the access functions available in the `Map` module,
  like `Map.get/3` and `Map.fetch/2`, a map can be accessed using the
  `.` operator:

      iex> map = %{ a: :b }
      iex> map.a
      :b

  Note that the `.` operator expects the field to exist in the map.
  If not, an `ArgumentError` is raised.

  ## Update syntax

  Maps also support an update syntax:

      iex> map = %{ :a => :b }
      iex> %{ map | :a => :c }
      %{ :a => :c }

  Notice the update syntax requires the given keys to exist.
  Trying to update a key that does not exist will raise an `ArgumentError`.

  ## AST representation

  Regardless if `=>` or the keywords syntax is used, Maps are
  always represented internally as a list of two-items tuples
  for simplicity:

      iex> quote do: %{ :a => :b, c: :d }
      { :%{}, [], [{:a, :b}, {:c, :d}] }

  """
  defmacro unquote(:%{})(args)

  @doc """
  Creates a struct.

  A struct is a tagged map that allows developers to provide
  default values for keys, tags to be used in polymorphic
  dispatches and compile time assertions.

  To define a struct, you just need to implement the `__struct__/0`
  function in a module:

      defmodule User do
        def __struct__ do
          %{ name: "josé", age: 27 }
        end
      end

  Now a struct can be created as follow:

      %User{}

  Underneath, a struct is just a map with a `__struct__` field
  pointing to the User module:

      %User{} == %{ __struct__: User, name: "josé", age: 27 }

  A struct also validates the given keys are part of the defined
  struct. The example below will fail because there is no key
  `:full_name` in the user struct:

      %User{ full_name: "José Valim" }

  Note that a struct specifies a minimum set of keys required
  for operations. Other keys can be added to structs via the
  regular map operations:

      user = %User{}
      %{ user | a_non_struct_key: :value }

  An update operation specific for structs is also available:

      %User{ user | age: 28 }

  The syntax above will guarantee the given keys are valid at
  compilation time and it will guarantee at runtime the given
  argument is a struct, failing with `BadStructError` otherwise.

  Check `Kernel.defprotocol/2` for more information on how structs
  can be used with protocols for polymorphic dispatch. Also,
  see `Kernel.struct/2` for examples on how to create and update
  structs dynamically.
  """
  defmacro unquote(:%)(struct, map)

  @doc """
  Defines a new bitstring.

  ## Examples

      iex> << 1, 2, 3 >>
      << 1, 2, 3 >>

  ## Bitstring types

  A bitstring is made of many segments. Each segment has a
  type, which defaults to integer:

      iex> <<1, 2, 3>>
      <<1, 2, 3>>

  Elixir also accepts by default the segment to be a literal
  string or a literal char list, which are by expanded to integers:

      iex> <<0, "foo">>
      <<0, 102, 111, 111>>

  Any other type needs to be explicitly tagged. For example,
  in order to store a float type in the binary, one has to do:

      iex> <<3.14 :: float>>
      <<64, 9, 30, 184, 81, 235, 133, 31>>

  This also means that variables need to be explicitly tagged,
  otherwise Elixir defaults to integer:

      iex> rest = "oo"
      iex> <<102, rest>>
      ** (ArgumentError) argument error

  We can solve this by explicitly tagging it as a binary:

      <<102, rest :: binary>>

  The type can be integer, float, bitstring/bits, binary/bytes,
  utf8, utf16 or utf32, e.g.:

      <<102 :: float, rest :: binary>>

  An integer can be any arbitrary precision integer. A float is an
  IEEE 754 binary32 or binary64 floating point number. A bitstring
  is an arbitrary series of bits. A binary is a special case of
  bitstring that has a total size divisible by 8.

  The utf8, utf16, and utf32 types are for UTF code points. They
  can also be applied to literal strings and char lists:

      iex> <<"foo" :: utf16>>
      <<0,102,0,111,0,111>>

  The bits type is an alias for bitstring. The bytes type is an
  alias for binary.

  The signedness can also be given as signed or unsigned. The
  signedness only matters for matching. If unspecified, it
  defaults to unsigned. Example:

      iex> <<-100 :: signed, _rest :: binary>> = <<-100, "foo">>
      <<156,102,111,111>>

  This match would have failed if we did not specify that the
  value -100 is signed. If we're matching into a variable instead
  of a value, the signedness won't be checked; rather, the number
  will simply be interpreted as having the given (or implied)
  signedness, e.g.:

      iex> <<val, _rest :: binary>> = <<-100, "foo">>
      iex> val
      156

  Here, `val` is interpreted as unsigned.

  Signedness is only relevant on integers.

  The endianness of a segment can be big, little or native (the
  latter meaning it will be resolved at VM load time). Passing
  many options can be done by giving a list:

      <<102 :: [integer, native], rest :: binary>>

  Or:

      <<102 :: [unsigned, big, integer], rest :: binary>>

  And so on.

  Endianness only makes sense for integers and some UTF code
  point types (utf16 and utf32).

  Finally, we can also specify size and unit for each segment. The
  unit is multiplied by the size to give the effective size of
  the segment:

      iex> <<102, _rest :: [size(2), unit(8)]>> = "foo"
      "foo"

      iex> <<102, _rest :: size(16)>> = "foo"
      "foo"

      iex> <<102, _rest :: size(32)>> = "foo"
      ** (MatchError) no match of right hand side value: "foo"

  In the example above, the first two expressions matches
  because the string "foo" takes 24 bits and we are matching
  against a segment of 24 bits as well, 8 of which are taken by
  the integer 102 and the remaining 16 bits are specified on
  the rest. On the last example, we expect a rest with size 32,
  which won't match.

  Size and unit are not applicable to utf8, utf16, and utf32.

  The default size for integers is 8. For floats, it is 64. For
  binaries, it is the size of the binary. Only the last binary
  in a binary match can use the default size (all others must
  have their size specified explicitly). Bitstrings do not have
  a default size.

  Size can also be specified using a syntax shortcut. Instead of
  writing `size(8)`, one can write just `8` and it will be interpreted
  as `size(8)`

      iex> << 1 :: 3 >> == << 1 :: size(3) >>
      true

  The default unit for integers, floats, and bitstrings is 1. For
  binaries, it is 8.

  For floats, unit * size must result in 32 or 64, corresponding
  to binary32 and binary64, respectively.
  """
  defmacro unquote(:<<>>)(args)

  @doc """
  Defines a remote call or an alias.

  The dot (`.`) in Elixir can be used for remote calls:

      iex> String.downcase("FOO")
      "foo"

  In this example above, we have used `.` to invoke `downcase` in the
  `String` alias, passing "FOO" as argument. We can also use the dot
  for creating aliases:

      iex> Hello.World
      Hello.World

  This time, we have joined two aliases, defining the final alias
  `Hello.World`.

  ## Syntax

  The right side of `.` may be a word starting in upcase, which represents
  an alias, a word starting with lowercase or underscore, any valid language
  operator or any name wrapped in single- or double-quotes. Those are all valid
  examples:

      iex> Kernel.Sample
      Kernel.Sample

      iex> Kernel.length([1,2,3])
      3

      iex> Kernel.+(1, 2)
      3

      iex> Kernel."length"([1,2,3])
      3

      iex> Kernel.'+'(1, 2)
      3

  Note that `Kernel."HELLO"` will be treated as a remote call and not an alias.
  This choice was done so every time single- or double-quotes are used, we have
  a remote call irregardless of the quote contents. This decision is also reflected
  in the quoted expressions discussed below.

  ## Runtime (dynamic) behaviour

  The result returned by `.` is always specified by the right-side:

      iex> x = String
      iex> x.downcase("FOO")
      "foo"
      iex> x.Sample
      String.Sample

  In case the right-side is also dynamic, `.`'s behaviour can be reproduced
  at runtime via `apply/3` and `Module.concat/2`:

      iex> apply(:erlang, :+, [1,2])
      3

      iex> Module.concat(Kernel, Sample)
      Kernel.Sample

  ## Quoted expression

  When `.` is used, the quoted expression may take two distinct
  forms. When the right side starts with a lowercase letter (or
  underscore):

      iex> quote do: String.downcase("FOO")
      {{:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}, [], ["FOO"]}

  Notice we have an inner tuple, containing the atom `:.` representing
  the dot as first element:

      {:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}

  This tuple follows the general quoted expression structure in Elixir,
  with the name as first argument, some keyword list as metadata as second,
  and the number of arguments as third. In this case, the arguments is the
  alias `String` and the atom `:downcase`. The second argument is **always**
  an atom:

      iex> quote do: String."downcase"("FOO")
      {{:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}, [], ["FOO"]}

  The tuple containing `:.` is wrapped in another tuple, which actually
  represents the function call, and has `"FOO"` as argument.

  When the right side is an alias (i.e. starts with uppercase), we get instead:

      iex> quote do: Hello.World
      {:__aliases__, [alias: false], [:Hello, :World]}

  We got into more details about aliases in the `__aliases__` special form
  documentation.

  ## Unquoting

  We can also use unquote to generate a remote call in a quoted expression:

      iex> x = :downcase
      iex> quote do: String.unquote(x)("FOO")
      {{:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}, [], ["FOO"]}

  Similar to `Kernel."HELLO"`, `unquote(x)` will always generate a remote call,
  independent of the value of `x`. To generate an alias via the quoted expression,
  one needs to rely on `Module.concat/2`:

      iex> x = Sample
      iex> quote do: Module.concat(String, unquote(x))
      {{:., [], [{:__aliases__, [alias: false], [:Module]}, :concat]}, [],
       [{:__aliases__, [alias: false], [:String]}, Sample]}

  """
  defmacro unquote(:.)(left, right)

  @doc """
  `alias` is used to setup aliases, often useful with modules names.

  ## Examples

  `alias` can be used to setup an alias for any module:

      defmodule Math do
        alias MyKeyword, as: Keyword
      end

  In the example above, we have set up `MyKeyword` to be aliased
  as `Keyword`. So now, any reference to `Keyword` will be
  automatically replaced by `MyKeyword`.

  In case one wants to access the original `Keyword`, it can be done
  by accessing `Elixir`:

      Keyword.values   #=> uses MyKeyword.values
      Elixir.Keyword.values #=> uses Keyword.values

  Notice that calling `alias` without the `as:` option automatically
  sets an alias based on the last part of the module. For example:

      alias Foo.Bar.Baz

  Is the same as:

      alias Foo.Bar.Baz, as: Baz

  ## Lexical scope

  `import`, `require` and `alias` are called directives and all
  have lexical scope. This means you can set up aliases inside
  specific functions and it won't affect the overall scope.

  ## Warnings

  If you alias a module and you don't use the alias, Elixir is
  going to issue a warning implying the alias is not being used.

  In case the alias is generated automatically by a macro,
  Elixir won't emit any warnings though, since the alias
  was not explicitly defined.

  Both warning behaviours could be changed by explicitly
  setting the `:warn` option to true or false.
  """
  defmacro alias(module, opts)

  @doc """
  Requires a given module to be compiled and loaded.

  ## Examples

  Notice that usually modules should not be required before usage,
  the only exception is if you want to use the macros from a module.
  In such cases, you need to explicitly require them.

  Let's suppose you created your own `if` implementation in the module
  `MyMacros`. If you want to invoke it, you need to first explicitly
  require the `MyMacros`:

      defmodule Math do
        require MyMacros
        MyMacros.if do_something, it_works
      end

  An attempt to call a macro that was not loaded will raise an error.

  ## Alias shortcut

  `require` also accepts `as:` as an option so it automatically sets
  up an alias. Please check `alias` for more information.

  """
  defmacro require(module, opts)

  @doc """
  Imports function and macros from other modules.

  `import` allows one to easily access functions or macros from
  others modules without using the qualified name.

  ## Examples

  If you are using several functions from a given module, you can
  import those functions and reference them as local functions,
  for example:

      iex> import List
      iex> flatten([1, [2], 3])
      [1,2,3]

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

  Notice that calling `except` for a previously declared `import`
  simply filters the previously imported elements. For example:

      import List, only: [flatten: 1, keyfind: 3]
      import List, except: [flatten: 1]

  After the two import calls above, only `List.keyfind/3` will be
  imported.

  ## Lexical scope

  It is important to notice that `import` is lexical. This means you
  can import specific macros inside specific functions:

      defmodule Math do
        def some_function do
          # 1) Disable `if/2` from Kernel
          import Kernel, except: [if: 2]

          # 2) Require the new `if` macro from MyMacros
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
  setting the `:warn` option to true or false.

  ## Ambiguous function/macro names

  If two modules `A` and `B` are imported and they both contain
  a `foo` function with an arity of `1`, an error is only emitted
  if an ambiguous call to `foo/1` is actually made; that is, the
  errors are emitted lazily, not eagerly.
  """
  defmacro import(module, opts)

  @doc """
  Returns the current environment information as a `Macro.Env[]` record.

  In the environment you can access the current filename,
  line numbers, set up aliases, the current function and others.
  """
  defmacro __ENV__

  @doc """
  Returns the current module name as an atom or `nil` otherwise.

  Although the module can be accessed in the `__ENV__`, this macro
  is a convenient shortcut.
  """
  defmacro __MODULE__

  @doc """
  Returns the current directory as a binary.

  Although the directory can be accessed as `Path.dirname(__ENV__.file)`,
  this macro is a convenient shortcut.
  """
  defmacro __DIR__

  @doc """
  Accesses an already bound variable in match clauses.

  ## Examples

  Elixir allows variables to be rebound via static single assignment:

      iex> x = 1
      iex> x = 2
      iex> x
      2

  However, in some situations, it is useful to match against an existing
  value, instead of rebinding. This can be done with the `^` special form:

      iex> x = 1
      iex> ^x = List.first([1])
      iex> ^x = List.first([2])
      ** (MatchError) no match of right hand side value: 2

  Note that `^` always refers to the value of x prior to the match. The
  following example will match:

      iex> x = 0
      iex> { x, ^x } = { 1, 0 }
      iex> x
      1

  """
  defmacro ^(var)

  @doc ~S"""
  Gets the representation of any expression.

  ## Examples

      quote do: sum(1, 2, 3)
      #=> { :sum, [], [1, 2, 3] }

  ## Explanation

  Any Elixir code can be represented using Elixir data structures.
  The building block of Elixir macros is a tuple with three elements,
  for example:

      { :sum, [], [1, 2, 3] }

  The tuple above represents a function call to `sum` passing 1, 2 and
  3 as arguments. The tuple elements are:

  * The first element of the tuple is always an atom or
    another tuple in the same representation;
  * The second element of the tuple represents metadata;
  * The third element of the tuple are the arguments for the
    function call. The third argument may be an atom, which is
    usually a variable (or a local call);

  ## Options

  * `:unquote` - When false, disables unquoting. Useful when you have a quote
                 inside another quote and want to control what quote is
                 able to unquote;
  * `:location` - When set to `:keep`, keeps the current line and file from quote.
                  Read the Stacktrace information section below for more information;
  * `:hygiene` - Allows a developer to disable hygiene selectively;
  * `:context` - Sets the resolution context;
  * `:bind_quoted` - Passes a binding to the macro. Whenever a binding is given,
                    `unquote` is automatically disabled;

  ## Quote literals

  Besides the tuple described above, Elixir has a few literals that
  when quoted return themselves. They are:

      :sum         #=> Atoms
      1            #=> Integers
      2.0          #=> Floats
      [1, 2]       #=> Lists
      "strings"    #=> Strings
      {key, value} #=> Tuples with two elements

  ## Quote and macros

  `quote` is commonly used with macros for code generation. As an exercise,
  let's define a macro that multiplies a number by itself (squared). Note
  there is no reason to define such as a macro (and it would actually be
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
      IO.puts "Got #{squared(5)}"

  At first, there is nothing in this example that actually reveals it is a
  macro. But what is happening is that, at compilation time, `squared(5)`
  becomes `5 * 5`. The argument `5` is duplicated in the produced code, we
  can see this behaviour in practice though because our macro actually has
  a bug:

      import Math
      my_number = fn ->
        IO.puts "Returning 5"
        5
      end
      IO.puts "Got #{squared(my_number.())}"

  The example above will print:

      Returning 5
      Returning 5
      25

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

  Now invoking `square(my_number.())` as before will print the value just
  once.

  In fact, this pattern is so common that most of the times you will want
  to use the `bind_quoted` option with `quote`:

      defmodule Math do
        defmacro squared(x) do
          quote bind_quoted: [x: x] do
            x * x
          end
        end
      end

  `:bind_quoted` will translate to the same code as the example above.
  `:bind_quoted` can be used in many cases and is seen as good practice,
  not only because it helps us from running into common mistakes but also
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
      x #=> ** (RuntimeError) undefined function or variable: x

  We can see that `x` did not leak to the user context. This happens
  because Elixir macros are hygienic, a topic we will discuss at length
  in the next sections as well.

  ## Hygiene in variables

  Consider the following example:

      defmodule Hygiene do
        defmacro no_interference do
          quote do: a = 1
        end
      end

      require Hygiene

      a = 10
      Hygiene.no_interference
      a #=> 10

  In the example above, `a` returns 10 even if the macro
  is apparently setting it to 1 because variables defined
  in the macro does not affect the context the macro is executed in.
  If you want to set or get a variable in the caller's context, you
  can do it with the help of the `var!` macro:

      defmodule NoHygiene do
        defmacro interference do
          quote do: var!(a) = 1
        end
      end

      require NoHygiene

      a = 10
      NoHygiene.interference
      a #=> 1

  Note that you cannot even access variables defined in the same
  module unless you explicitly give it a context:

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

      Hygiene.write
      Hygiene.read
      #=> ** (RuntimeError) undefined function or variable: a

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

      ContextHygiene.write
      ContextHygiene.read
      #=> 1

  Hygiene for variables can be disabled overall as:

      quote hygiene: [vars: false], do: x

  ## Hygiene in aliases

  Aliases inside quote are hygienic by default.
  Consider the following example:

      defmodule Hygiene do
        alias HashDict, as: D

        defmacro no_interference do
          quote do: D.new
        end
      end

      require Hygiene
      Hygiene.no_interference #=> #HashDict<[]>

  Notice that, even though the alias `D` is not available
  in the context the macro is expanded, the code above works
  because `D` still expands to `HashDict`.

  Similarly, even if we defined an alias with the same name
  before invoking a macro, it won't affect the macro's result:

      defmodule Hygiene do
        alias HashDict, as: D

        defmacro no_interference do
          quote do: D.new
        end
      end

      require Hygiene
      alias SomethingElse, as: D
      Hygiene.no_interference #=> #HashDict<[]>

  In some cases, you want to access an alias or a module defined
  in the caller. For such, you can use the `alias!` macro:

      defmodule Hygiene do
        # This will expand to Elixir.Nested.hello
        defmacro no_interference do
          quote do: Nested.hello
        end

        # This will expand to Nested.hello for
        # whatever is Nested in the caller
        defmacro interference do
          quote do: alias!(Nested).hello
        end
      end

      defmodule Parent do
        defmodule Nested do
          def hello, do: "world"
        end

        require Hygiene
        Hygiene.no_interference
        #=> ** (UndefinedFunctionError) ...

        Hygiene.interference
        #=> "world"
      end

  ## Hygiene in imports

  Similar to aliases, imports in Elixir are hygienic. Consider the
  following code:

      defmodule Hygiene do
        defmacrop get_size do
          quote do
            size("hello")
          end
        end

        def return_size do
          import Kernel, except: [size: 1]
          get_size
        end
      end

      Hygiene.return_size #=> 5

  Notice how `return_size` returns 5 even though the `size/1`
  function is not imported. In fact, even if `return_size` imported
  a function from another module, it wouldn't affect the function
  result:

      def return_size do
        import Dict, only: [size: 1]
        get_size
      end

  Calling this new `return_size` will still return 5 as result.

  Elixir is smart enough to delay the resolution to the latest
  moment possible. So, if you call `size("hello")` inside quote,
  but no `size/1` function is available, it is then expanded in
  the caller:

      defmodule Lazy do
        defmacrop get_size do
          import Kernel, except: [size: 1]

          quote do
            size([a: 1, b: 2])
          end
        end

        def return_size do
          import Kernel, except: [size: 1]
          import Dict, only: [size: 1]
          get_size
        end
      end

      Lazy.return_size #=> 2

  As in aliases, import expansion can be explicitly disabled
  via the `hygiene: [imports: false]` option.

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

  When using `location: :keep` and invalid arguments are given to
  `Sample.add/2`, the stacktrace information will point to the file
  and line inside the quote. Without `location: :keep`, the error is
  reported to where `defadd` was invoked. Note `location: :keep` affects
  only definitions inside the quote.

  ## Binding and unquote fragments

  Elixir quote/unquote mechanisms provides a functionality called
  unquote fragments. Unquote fragments provide an easy way to generate
  functions on the fly. Consider this example:

      kv = [foo: 1, bar: 2]
      Enum.each kv, fn { k, v } ->
        def unquote(k)(), do: unquote(v)
      end

  In the example above, we have generated the functions `foo/0` and
  `bar/0` dynamically. Now, imagine that, we want to convert this
  functionality into a macro:

      defmacro defkv(kv) do
        Enum.map kv, fn { k, v } ->
          quote do
            def unquote(k)(), do: unquote(v)
          end
        end
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

  This is actually a common pitfall when developing macros. In
  practice, we want to avoid doing work at compilation time as
  much as possible. That said, let's attempt to improve our macro:

      defmacro defkv(kv) do
        quote do
          Enum.each unquote(kv), fn { k, v } ->
            def unquote(k)(), do: unquote(v)
          end
        end
      end

  If you try to run our new macro, you will notice it won't
  even compile, complaining that the variables `k` and `v`
  does not exist. This is because of the ambiguity: `unquote(k)`
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
          Enum.each kv, fn { k, v } ->
            def unquote(k)(), do: unquote(v)
          end
        end
      end

  In fact, the `:bind_quoted` option is recommended every time
  one desires to inject a value into the quote.
  """
  defmacro quote(opts, block)

  @doc """
  Unquotes the given expression from inside a macro.

  ## Examples

  Imagine the situation you have a variable `name` and
  you want to inject it inside some quote. The first attempt
  would be:

      value = 13
      quote do: sum(1, value, 3)

  Which would then return:

      { :sum, [], [1, { :value, [], quoted }, 3] }

  Which is not the expected result. For this, we use unquote:

      value = 13
      quote do: sum(1, unquote(value), 3)
      #=> { :sum, [], [1, 13, 3] }

  """
  defmacro unquote(:unquote)(expr)

  @doc """
  Unquotes the given list expanding its arguments. Similar
  to unquote.

  ## Examples

      values = [2, 3, 4]
      quote do: sum(1, unquote_splicing(values), 5)
      #=> { :sum, [], [1, 2, 3, 4, 5] }

  """
  defmacro unquote(:unquote_splicing)(expr)

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
      iex> for x <- [1, 2], y <- [2, 3], do: x*y
      [2, 3, 4, 6]

  Filters can also be given:

      # A comprehension with a generator and a filter
      iex> for n <- [1, 2, 3, 4, 5, 6], rem(n, 2) == 0, do: n
      [2, 4, 6]

  Bitstring generators are also supported and are very useful when you
  need to organize bitstring streams:

      iex> pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>
      iex> for <<r::8, g::8, b::8 <- pixels >>, do: {r, g, b}
      [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

  Variable assignments inside the comprehension, be it in generators,
  filters or inside the block, are not reflected outside of the
  comprehension.

  ## Into

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

  """
  defmacro for(args)

  @doc """
  Defines an anonymous function.

  ## Examples

      iex> add = fn a, b -> a + b end
      iex> add.(1, 2)
      3

  """
  defmacro unquote(:fn)(clauses)

  @doc """
  Internal special form for block expressions.

  This is the special form used whenever we have a block
  of expressions in Elixir. This special form is private
  and should not be invoked directly:

      iex> quote do: (1; 2; 3)
      { :__block__, [], [1, 2, 3] }

  """
  defmacro __block__(args)

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

  The capture operator can be also be used to partially apply
  functions, where `&1`, `&2` and so on can be used as value
  placeholders. For example:

      iex> double = &(&1 * 2)
      iex> double.(2)
      4

  In other words, `&(&1 * 2)` is equivalent to `fn x -> x * 2 end`.
  Another example using a local function:

      iex> fun = &is_atom(&1)
      iex> fun.(:atom)
      true

  The `&` operator can be used with more complex expressions:

      iex> fun = &(&1 + &2 + &3)
      iex> fun.(1, 2, 3)
      6

  As well as with lists and tuples:

      iex> fun = &{&1, &2}
      iex> fun.(1, 2)
      { 1, 2 }

      iex> fun = &[&1|&2]
      iex> fun.(1, 2)
      [1|2]

  The only restrictions when creating anonymous functions is that at
  least one placeholder must be present, i.e. it must contain at least
  `&1`:

      # No placeholder fails to compile
      &var

      # Block expressions are also not supported
      &(foo(&1, &2); &3 + &4)

  """
  defmacro unquote(:&)(expr)

  @doc """
  Internal special form to hold aliases information.

  It is usually compiled to an atom:

      iex> quote do: Foo.Bar
      {:__aliases__, [alias: false], [:Foo, :Bar]}

  Elixir represents `Foo.Bar` as `__aliases__` so calls can be
  unambiguously identified by the operator `:.`. For example:

      iex> quote do: Foo.bar
      {{:., [], [{:__aliases__, [alias: false], [:Foo]}, :bar]}, [], []}

  Whenever an expression iterator sees a `:.` as the tuple key,
  it can be sure that it represents a call and the second argument
  in the list is an atom.

  On the other hand, aliases holds some properties:

  1) The head element of aliases can be any term;

  2) The tail elements of aliases are guaranteed to always be atoms;

  3) When the head element of aliases is the atom `:Elixir`, no expansion happen;

  4) When the head element of aliases is not an atom, it is expanded at runtime:

        quote do: some_var.Foo
        {:__aliases__, [], [{:some_var, [], Elixir}, :Foo]}

     Since `some_var` is not available at compilation time, the compiler
     expands such expression to:

        Module.concat [some_var, Foo]

  """
  defmacro __aliases__(args)

  @doc """
  Calls the overriden function when overriding it with `defoverridable`.
  See `Kernel.defoverridable` for more information and documentation.
  """
  defmacro super(args)

  @doc """
  Matches the given expression against the given clauses.

  ## Examples

      case thing do
        { :selector, i, value } when is_integer(i) ->
          value
        value ->
          value
      end

  In the example above, we match `thing` against each clause "head"
  and execute the clause "body" corresponding to the first clause
  that matches. If no clause matches, an error is raised.

  ## Variables handling

  Notice that variables bound in a clause "head" do not leak to the
  outer context:

      case data do
        { :ok, value } -> value
        :error -> nil
      end

      value #=> unbound variable value

  However, variables explicitly bound in the clause "body" are
  accessible from the outer context:

      value = 7

      case lucky? do
        false -> value = 13
        true  -> true
      end

      value #=> 7 or 13

  In the example above, value is going to be `7` or `13` depending on
  the value of `lucky?`. In case `value` has no previous value before
  case, clauses that do not explicitly bind a value have the variable
  bound to nil.
  """
  defmacro case(condition, blocks)

  @doc ~S"""
  Evaluate the given expressions and handle any error, exit
  or throw that may have happened.

  ## Examples

      try do
        do_something_that_may_fail(some_arg)
      rescue
        ArgumentError ->
          IO.puts "Invalid argument given"
      catch
        value ->
          IO.puts "caught #{value}"
      else
        value ->
          IO.puts "Success! The result was #{value}"
      after
        IO.puts "This is printed regardless if it failed or succeed"
      end

  The rescue clause is used to handle exceptions, while the catch
  clause can be used to catch thrown values. The else clause can
  be used to control flow based on the result of the expression.
  Catch, rescue and else clauses work based on pattern matching.

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

      # rescue and bind to x
      try do
        UndefinedModule.undefined_function
      rescue
        x in [UndefinedFunctionError] -> nil
      end

      # rescue all and bind to x
      try do
        UndefinedModule.undefined_function
      rescue
        x -> nil
      end

  ## Erlang errors

  Erlang errors are transformed into Elixir ones during rescue:

      try do
        :erlang.error(:badarg)
      rescue
        ArgumentError -> :ok
      end

  The most common Erlang errors will be transformed into their
  Elixir counter-part. Those which are not will be transformed
  into `ErlangError`:

      try do
        :erlang.error(:unknown)
      rescue
        ErlangError -> :ok
      end

  In fact, ErlangError can be used to rescue any error that is
  not an Elixir error proper. For example, it can be used to rescue
  the earlier `:badarg` error too, prior to transformation:

      try do
        :erlang.error(:badarg)
      rescue
        ErlangError -> :ok
      end

  ## Catching throws and exits

  The catch clause can be used to catch throws values and exits.

      try do
        exit(1)
      catch
        :exit, 1 -> IO.puts "Exited with 1"
      end

      try do
        throw(:sample)
      catch
        :throw, :sample ->
          IO.puts "sample thrown"
      end

  catch values also support `:error`, as in Erlang, although it is
  commonly avoided in favor of raise/rescue control mechanisms.

  ## Else clauses

  Else clauses allow the result of the expression to be pattern
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

  If an else clause is not present the result of the expression will
  be return, if no exceptions are raised:

      x = 1
      ^x =
        try do
          1 / x
        rescue
          ArithmeticError ->
            :infinity
        end

  However when an else clause is present but the result of the expression
  does not match any of the patterns an exception will be raised. This
  exception will not be caught by a catch or rescue in the same try:

      x = 1
      try do
        try do
          1 / x
        rescue
          # The TryClauseError can not be rescued here:
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

  Similarly an exception inside an else clause is not caught or rescued
  inside the same try:

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
  an else clause and so tail recursion is possible when using a `try`
  with a tail call as the final call inside an else clause. The same
  is true for rescue and catch clauses.

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

      x #=> unbound variable `x`

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
  Checks if there is a message matching the given clauses
  in the current process mailbox.

  In case there is no such message, the current process hangs
  until a message arrives or waits until a given timeout value.

  ## Examples

      receive do
        { :selector, i, value } when is_integer(i) ->
          value
        value when is_atom(value) ->
          value
        _ ->
          IO.puts :stderr, "Unexpected message received"
      end

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
  There are two special cases for the timeout value given to `after`

  * `:infinity` - The process should wait indefinitely for a matching
  message, this is the same as not using a timeout.

  * 0 - if there is no matching message in the mailbox, the timeout
  will occur immediately.

  ## Variables handling

  The `receive` special form handles variables exactly as the `case`
  special macro. For more information, check the docs for `case/2`.
  """
  defmacro receive(args)
end
