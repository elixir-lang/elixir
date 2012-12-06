defmodule Kernel.SpecialForms do
  @moduledoc """
  In this module we define Elixir special forms. Special forms
  cannot be overriden by the developer and are the basic
  building blocks of Elixir code.

  Some of those forms are lexical (like `alias`, `import`, etc).
  The macros `{}`, `[]` and `<<>>` are also special forms used
  to define data structures, respectively tuples, lists and binaries.

  This module also documents Elixir's pseudo variables (`__MODULE__`,
  `__FILE__`, `__ENV__` and `__CALLER__`). Pseudo variables return
  information about Elixir's compilation environment and can only
  be read, never assigned to.

  Finally, it also documents 3 special forms (`__block__`,
  `__scope__` and `__aliases__`), which are not intended to be
  called directly by the developer but they appear in quoted
  contents since they are essential in Elixir's constructions.
  """

  @doc """
  Defines a new tuple.

  ## Examples

      :{}.(1,2,3)
      { 1, 2, 3 }

  """
  defmacro :{}.(args)

  @doc """
  Defines a new list.

  ## Examples

      :[].(1,2,3)
      [ 1, 2, 3 ]

  """
  defmacro :[].(args)

  @doc """
  Defines a new bitstring.

  ## Examples

      :<<>>.(1,2,3)
      << 1, 2, 3 >>

  ## Bitstring types

  A bitstring may contain many parts and those may have
  specific types. Most of the time, Elixir will figure out
  the part's type and won't require any work from you:

      <<102, "oo">>
      #=> "foo"

  Above we have two parts: the first is an integer and the
  second is a binary. If we use any other Elixir expression,
  Elixir can no longer guess the type:

      rest = "oo"
      <<102, rest>>
      #=> ** (ArgumentError) argument error

  When a variable or expression is given as a binary part,
  Elixir defaults the type of that part to an unsigned
  little-endian integer. In the example above, since we haven't
  specified a type, Elixir expected an integer but we passed a
  binary, resulting in `ArgumentError`. We can solve this by
  explicitly tagging it as a binary:

      <<102, rest :: binary>>

  The type can be integer, float, binary, bytes, bitstring,
  bits, utf8, utf16 or utf32, e.g.:

      <<102 :: float, rest :: binary>>

  Integer can be any arbitrary precision integer. A float is an
  IEEE 754 binary32 or binary64 floating point number. A bitstring
  is an arbitrary series of bits. A binary is a special case of
  bitstring that has a total size divisible by 8.

  The utf8, utf16, and utf32 types are for UTF code points.

  The bits type is an alias for bitstring. The bytes type is an
  alias for binary.

  The signedness can also be given as signed or unsigned. The
  signedness only matters for matching. If unspecified, it
  defaults to unsigned. Example:

      <<-100 :: signed, rest :: binary>> = <<-100, "foo">>
      #=> <<156,102,111,111>>

  This match would have failed if we did not specify that the
  value -100 is signed. If we're matching into a variable instead
  of a value, the signedness won't be checked; rather, the number
  will simply be interpreted as having the given (or implied)
  signedness, e.g.:

      <<val, rest :: binary>> = <<-100, "foo">>
      val
      #=> 156

  Here, `val` is interpreted as unsigned.

  Signedness is only relevant on integers.

  The endianness of a part can be big, little or native (the
  latter meaning it will be resolved at VM load time). Passing
  many options can be done by giving a list:

      <<102 :: [integer, native], rest :: binary>>

  Or:

      <<102 :: [unsigned, big, integer], rest :: binary>>

  And so on.

  Endianness only makes sense for integers and some UTF code
  point types (utf16 and utf32).

  Finally, we can also specify size and unit for each part. The
  unit is multiplied by the size to give the effective size of
  the part:

      <<102, rest :: [size(2), unit(8)]>> = "foo"
      "foo"

      <<102, rest :: size(16)>> = "foo"
      "foo"

      <<102, rest :: size(32)>> = "foo"
      ** (MatchError) no match of right hand side value: "foo"

  In the example above, the first two expressions matches
  because the string "foo" takes 24 bits and we are matching
  against a part of 24 bits as well, 8 of which are taken by
  the integer 102 and the remaining 16 bits are specified on
  the rest. On the last example, we expect a rest with size 32,
  which won't match.

  Size and unit are not applicable to utf8, utf16, and utf32.

  The default size for integers is 8. For floats, it is 64. For
  binaries, it is the size of the binary. Only the last binary
  in a binary match can use the default size (all others must
  have their size specified explicitly). Bitstrings do not have
  a default size.

  The default unit for integers, floats, and bitstrings is 1. For
  binaries, it is 8.

  For floats, unit * size must result in 32 or 64, corresponding
  to binary32 and binary64, respectively.
  """
  defmacro :<<>>.(args)

  @doc """
  `alias` is used to setup atom aliases, often useful with modules names.

  ## Examples

  `alias` can be used to setup an alias for any module:

      defmodule Math do
        alias MyKeyword, as: Keyword
      end

  In the example above, we have set up `MyKeyword` to be alias
  as `Keyword`. So now, any reference to `Keyword` will be
  automatically replaced by `MyKeyword`.

  In case one wants to access the original `Keyword`, it can be done
  by accessing Elixir:

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
  """
  defmacro alias(module, opts)

  @doc """
  `require` is used to require the presence of external
  modules so macros can be invoked.

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
  `import` allows one to easily access functions or macros from
  others modules without using the qualified name.

  ## Examples

  If you are using several functions from a given module, you can
  import those functions and reference them as local functions,
  for example:

      import List
      flatten([1,[2],3]) #=> [1,2,3]

  ## Selector

  By default, Elixir imports functions and macros from the given
  module, except the ones starting with underscore (which are
  usually callbacks):

      import List

  A developer can change this behavior to include all macros and
  functions, regardless if it starts with underscore, by passing
  `:all` as first argument:

      import :all, List

  It can also be customized to import only all functions or
  all macros:

      import :functions, List
      import :macros, List

  Alternatively, Elixir allows a developer to specify `:only`
  or `:except` as a fine grained control on what to import (or
  not):

      import List, only: [flatten: 1]

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
  during that specific function. All other functions in that
  module will still be able to use the original one.

  ## Alias/Require shortcut

  All imported modules are also required by default. `import`
  also accepts `as:` as an option so it automatically sets up
  an alias. Please check `alias` for more information.
  """
  defmacro import(module, opts)

  @doc """
  Returns the current environment information as a `Macro.Env`
  record. In the environment you can access the current filename,
  line numbers, set up aliases, the current function and others.
  """
  defmacro __ENV__

  @doc """
  Returns the current module name as an atom or nil otherwise.
  Although the module can be accessed in the __ENV__, this macro
  is a convenient shortcut.
  """
  defmacro __MODULE__

  @doc """
  Returns the current file name as a binary.
  Although the file can be accessed in the __ENV__, this macro
  is a convenient shortcut.
  """
  defmacro __FILE__

  @doc """
  Allows you to get the representation of any expression.

  ## Examples

      quote do: sum(1, 2, 3)
      #=> { :sum, 0, [1, 2, 3] }

  ## Homoiconicity

  Elixir is an homoiconic language. Any Elixir program can be
  represented using its own data structures. The building block
  of Elixir homoiconicity is a tuple with three elements, for example:

      { :sum, 1, [1, 2, 3] }

  The tuple above represents a function call to sum passing 1, 2 and
  3 as arguments. The tuple elements are:

  * The first element of the tuple is always an atom or
    another tuple in the same representation;
  * The second element of the tuple is always an integer
    representing the line number;
  * The third element of the tuple are the arguments for the
    function call. The third argument may be an atom, meaning
    that it may be a variable.

  ## Macro literals

  Besides the tuple described above, Elixir has a few literals that
  when quoted return themselves. They are:

      :sum         #=> Atoms
      1            #=> Integers
      2.0          #=> Floats
      [1,2]        #=> Lists
      "binaries"   #=> Binaries
      {key, value} #=> Tuple with two elements

  ## Hygiene

  Elixir macros are hygienic regarding to variables. This means
  a variable defined in a macro cannot affect the scope where
  the macro is included. Consider the following example:

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
  is apparently setting it to 1 because the variables defined
  in the macro does not affect the context the macro is
  executed. If you want to set or get a variable, you can do
  it with the help of the `var!` macro:

      defmodule NoHygiene do
        defmacro interference do
          quote do: var!(a) = 1
        end
      end

      require NoHygiene

      a = 10
      NoHygiene.interference
      a #=> 1

  Notice that aliases are not hygienic in Elixir, ambiguity
  must be solved by prepending Elixir:

      quote do
        Elixir.Foo #=> Access the root Foo
        Foo        #=> Access the Foo alias in the current module
                       (if any is set), then fallback to Elixir.Foo
      end

  ## Options

  * `:hygiene` - When false, disables hygiene;
  * `:unquote` - When false, disables unquoting. Useful when you have a quote
    inside another quote and want to control which quote is able to unquote;
  * `:location` - When set to `:keep`, keeps the current line and file on quotes.
                  Read the Stacktrace information section below for more information;

  ## Stacktrace information

  One of Elixir goals is to provide proper stacktrace whenever there is an
  exception. In order to work properly with macros, the default behavior
  in quote is to set the line to 0. When a macro is invoked and the quoted
  expressions is expanded, 0 is replaced by the line of the call site.

  This is a good behavior for the majority of the cases, except if the macro
  is defining new functions. Consider this example:

      defmodule MyServer do
        use GenServer.Behaviour
      end

  `GenServer.Behaviour` defines new functions in our `MyServer` module.
  However, if there is an exception in any of these functions, we want
  the stacktrace to point to the `GenServer.Behaviour` and not the line
  that calls `use GenServer.Behaviour`. For this reason, there is an
  option called `:location` that when set to `:keep` keeps these proper
  semantics:

      quote location: :keep do
        def handle_call(request, _from, state) do
          { :reply, :undef, state }
        end
      end

  It is important to warn though that `location: :keep` evaluates the
  code as if it was defined inside `GenServer.Behaviour` file, in
  particular, the macro `__FILE__` will always point to
  `GenServer.Behaviour` file.
  """
  defmacro quote(opts, do: contents)

  @doc """
  Unquotes the given expression from inside a macro.

  ## Examples

  Imagine the situation you have a variable `name` and
  you want to inject it inside some quote. The first attempt
  would be:

      value = 13
      quote do: sum(1, value, 3)

  Which would then return:

      { :sum, 0, [1, { :value, 0, quoted }, 3] }

  Which is not the expected result. For this, we use unquote:

      value = 13
      quote do: sum(1, unquote(value), 3)
      #=> { :sum, 0, [1, 13, 3] }

  """
  defmacro unquote(expr)

  @doc """
  Unquotes the given list expanding its arguments. Similar
  to unquote.

  ## Examples

      values = [2,3,4]
      quote do: sum(1, unquote_splicing(values), 5)
      #=> { :sum, 0, [1, 2, 3, 4, 5] }

  """
  defmacro unquote_splicing(expr)

  @doc """
  List comprehensions allow you to quickly build a list from another list:

      lc n inlist [1,2,3,4], do: n * 2
      #=> [2,4,6,8]

  A comprehension accepts many generators and also filters. Generators
  are defined using both `inlist` and `inbits` operators, allowing you
  to loop lists and bitstrings:

      # A list generator:
      lc n inlist [1,2,3,4], do: n * 2
      #=> [2,4,6,8]

      # A bit string generator:
      lc <<n>> inbits <<1,2,3,4>>, do: n * 2
      #=> [2,4,6,8]

      # A generator from a variable:
      list = [1,2,3,4]
      lc n inlist list, do: n * 2
      #=> [2,4,6,8]

      # A comprehension with two generators
      lc x inlist [1,2], y inlist [2,3], do: x*y
      #=> [2,3,4,6]

  Filters can also be given:

      # A comprehension with a generator and a filter
      lc n inlist [1,2,3,4,5,6], rem(n, 2) == 0, do: n
      #=> [2,4,6]

  Bit string generators are quite useful when you need to
  organize bit string streams:

      iex> pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>
      iex> lc <<r:8,g:8,b:8>> inbits pixels, do: {r,g,b}
      [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

  """
  defmacro lc(args)

  @doc """
  Defines a bit comprehension. It follows the same syntax as
  a list comprehension but expects each element returned to
  be a bitstring. For example, here is how to remove all
  spaces from a string:

      bc <<c>> inbits " hello world ", c != ? , do: <<c>>
      "helloworld"

  """
  defmacro bc(args)

  @doc """
  This is the special form used whenever we have a block
  of expressions in Elixir. This special form is private
  and should not be invoked directly:

      quote do: (1; 2; 3)
      #=> { :__block__, 0, [1,2,3] }

  """
  defmacro __block__(args)

  @doc """
  This is the special form used whenever we have to temporarily
  change the scope information of a block. Used when `quote` is
  invoked with `location: :keep` to execute a given block as if
  it belonged to another file.

      quote location: :keep, do: 1
      #=> { :__scope__, 1,[[file: "iex"],[do: 1]] }

  Check `quote/1` for more information.
  """
  defmacro __scope__(opts, args)

  @doc """
  This is the special form used to hold aliases information.
  It is usually compiled to an atom:

      quote do: Foo.Bar
      { :__aliases__, 0, [:Foo,:Bar] }

  Elixir represents `Foo.Bar` as `__aliases__` so calls can be
  unambiguously identified by the operator `:.`. For example:

      quote do: Foo.bar
      {{:.,0,[{:__aliases__,0,[:Foo]},:bar]},0,[]}

  Whenever an expression iterator sees a `:.` as the tuple key,
  it can be sure that it represents a call and the second element
  of the arguments list is an atom.

  On the other hand, aliases holds some properties:

  1) The head element of aliases can be any term;

  2) The tail elements of aliases are guaranteed to always be atoms;

  3) When the head element of aliases is the atom :Elixir, no expansion happen;

  4) When the head element of aliases is not an atom, it is expanded at runtime:

        quote do: some_var.Foo
        {:__aliases__,0,[{:some_var,0,:quoted},:Bar]}

     Since `some_var` is not available at compilation time, the compiler
     expands such expression to:

        Module.concat [some_var, Foo]

  """
  defmacro __aliases__(args)
end
