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
  contents since they are essential in Elixir's constructs.
  """

  @doc """
  Defines a new tuple.

  ## Examples

      iex> { 1, 2, 3 }
      { 1, 2, 3 }

  """
  defmacro :{}.(args)

  @doc """
  Defines a new list.

  ## Examples

      iex> [ 1, 2, 3 ]
      [ 1, 2, 3 ]

  """
  defmacro :[].(args)

  @doc """
  Defines a new bitstring.

  ## Examples

      iex> << 1, 2, 3 >>
      << 1, 2, 3 >>

  ## Bitstring types

  A bitstring may contain many parts and those may have
  specific types. Most of the time, Elixir will figure out
  the part's type and won't require any work from you:

      iex> <<102, "oo">>
      "foo"

  Above we have two parts: the first is an integer and the
  second is a binary. If we use any other Elixir expression,
  Elixir can no longer guess the type:

      iex> rest = "oo"
      ...> <<102, rest>>
      ** (ArgumentError) argument error

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

      iex> <<-100 :: signed, _rest :: binary>> = <<-100, "foo">>
      <<156,102,111,111>>

  This match would have failed if we did not specify that the
  value -100 is signed. If we're matching into a variable instead
  of a value, the signedness won't be checked; rather, the number
  will simply be interpreted as having the given (or implied)
  signedness, e.g.:

      iex> <<val, _rest :: binary>> = <<-100, "foo">>
      ...> val
      156

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

      iex> <<102, _rest :: [size(2), unit(8)]>> = "foo"
      "foo"

      iex> <<102, _rest :: size(16)>> = "foo"
      "foo"

      iex> <<102, _rest :: size(32)>> = "foo"
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
  defmacro :<<>>.(args)

  @doc """
  `alias` is used to setup atom aliases, often useful with modules names.

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

      iex> import List
      ...> flatten([1, [2], 3])
      [1,2,3]

  ## Selector

  By default, Elixir imports functions and macros from the given
  module, except the ones starting with underscore (which are
  usually callbacks):

      import List

  A developer can change this behavior to include all macros and
  functions, regardless if it starts with an underscore, by passing
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
  within that specific function. All other functions in that
  module will still be able to use the original one.

  ## Alias/Require shortcut

  All imported modules are also required by default. `import`
  also accepts `as:` as an option so it automatically sets up
  an alias. Please check `alias` for more information.

  ## Warnings

  If you import a module and you don't use any of the imported
  functions or macros from this module, Elixir is going to issue
  a warning implying the import is not being used.

  In case the import is generated automatically by a macro,
  Elixir won't emit any warnings though, since the import
  was not explicitly defined.

  Both warning behaviors could be changed by explicitily
  setting the `:warn` option to true or false.

  ## Ambiguous function/macro names

  If two modules `A` and `B` are imported and they both contain
  a `foo` function with an arity of `1`, an error is only emitted
  if an ambiguous call to `foo/1` is actually made; that is, the
  errors are emitted lazily, not eagerly.
  """
  defmacro import(module, opts)

  @doc """
  Returns the current environment information as a `Macro.Env`
  record. In the environment you can access the current filename,
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
  Returns the current file name as a binary.
  Although the file can be accessed in the `__ENV__`, this macro
  is a convenient shortcut.
  """
  defmacro __FILE__

  @doc """
  Returns the current directory as a binary.
  """
  defmacro __DIR__

  @doc """
  Allows you to get the representation of any expression.

  ## Examples

      iex> quote do: sum(1, 2, 3)
      { :sum, [], [1, 2, 3] }

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
  * `:location` - When set to `:keep`, keeps the current line and file on quotes.
                  Read the Stacktrace information section below for more information;
  * `:hygiene` - Allows a developer to disable hygiene selectively;
  * `:context` - Sets the resolution context;
  * `:binding` - Passes a binding to the macro. Whenever a binding is given,
                 `unquote` is automatically disabled;

  ## Macro literals

  Besides the tuple described above, Elixir has a few literals that
  when quoted return themselves. They are:

      :sum         #=> Atoms
      1            #=> Integers
      2.0          #=> Floats
      [1, 2]       #=> Lists
      "binaries"   #=> Binaries
      {key, value} #=> Tuple with two elements

  ## Hygiene and context

  Elixir macros are hygienic via means of deferred resolution.
  This means variables, aliases and imports defined inside the
  quoted refer to the context that defined the macro and not
  the context where the macro is expanded.

  For this mechanism to work, every quoted code is attached
  to a context. Consider the following example:

      defmodule ContextSample do
        def hello do
          quote do: world
        end
      end

      ContextSample.hello
      #=> {:world,[],ContextSample}

  Notice how the third element of the returned tuple is the
  module name. This means that the variable is associated to the
  `ContextSample` module and only code generated by this module
  will be able to access that particular `world` variable.
  While this means macros from the same module could have
  conflicting variables, it also allows different quotes from
  the same module to access them.

  The context can be disabled or changed by explicitly setting
  the `context` option. All hygiene mechanisms are based on such
  context and we are going to explore each of them in the following
  subsections.

  ### Hygiene in variables

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
  in the macro does not affect the context the macro is executed.
  If you want to set or get a variable in the user context, you
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

  It is important to understand that quoted variables are scoped
  to the module they are defined. That said, even if two modules
  define the same quoted variable `a`, their values are going
  to be independent:

      defmodule Hygiene1 do
        defmacro var1 do
          quote do: a = 1
        end
      end

      defmodule Hygiene2 do
        defmacro var2 do
          quote do: a = 2
        end
      end

  Calling macros `var1` and `var2` are not going to change their
  each other values for `a`. This is useful because quoted
  variables from different modules cannot conflict. If you desire
  to explicitly access a variable from another module, we can once
  again use `var!` macro, but explicitly passing a second argument:

      # Access the variable a from Hygiene1
      quote do: var!(a, Hygiene1) = 2

  Hygiene for variables can be disabled overall as:

      quote hygiene: [vars: false], do: x

  ### Hygiene in aliases

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

  In some particular cases you may want to access an alias
  or a module defined in the caller. In such scenarios, you
  can access it by disabling hygiene with `hygiene: [aliases: false]`
  or by using the `alias!` macro inside the quote:

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
  function is not imported.

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

  One of Elixir's goals is to provide a proper stacktrace whenever there is an
  exception. In order to work properly with macros, the default behavior
  in quote is to not set a line. When a macro is invoked and the quoted
  expressions is expanded, the call site line is inserted.

  This is a good behavior for the majority of the cases, except if the macro
  is defining new functions. Consider this example:

      defmodule MyServer do
        use GenServer.Behaviour
      end

  `GenServer.Behaviour` defines new functions in our `MyServer` module.
  However, if there is an exception in any of these functions, we want
  the stacktrace to point to the `GenServer.Behaviour` and not the line
  that calls `use GenServer.Behaviour`. For this reason, there is an
  option called `:location` that when set to `:keep` keeps the original
  line and file lines instead of setting them to 0:

      quote location: :keep do
        def handle_call(request, _from, state) do
          { :reply, :undef, state }
        end
      end

  It is important to warn though that `location: :keep` evaluates the
  code as if it was defined inside `GenServer.Behaviour` file, in
  particular, the macro `__FILE__` and exceptions happening inside
  the quote will always point to `GenServer.Behaviour` file.

  ## Binding and unquote fragments

  Elixir quote/unquote mechanisms provides a functionality called
  unquote fragments. Unquote fragments provide an easy way to generate
  functions on the fly. Consider this example:

      kv = [foo: 1, bar: 2]
      Enum.each kv, fn { k, v } ->
        def unquote(k)(), do: unquote(v)
      end

  In the example above, we have generated the function `foo/0` and
  `bar/0` dynamically. Now, imagine that, we want to convert this
  functionality into a macro:

      defmacro defkv(kv) do
        Enum.each kv, fn { k, v } ->
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
  key-value at **compilation** time. Since in the example above
  we are passing the representation of the variable `kv`, our
  code fails.

  This is actually a common pitfall when developing macros. In
  practice, we want to avoid doing work at compilation time as
  much as we can. That said, let's attempt to improve our macro:

      defmacro defkv(kv) do
        quote do
          Enum.each unquote(kv), fn { k, v } ->
            def unquote(k)(), do: unquote(v)
          end
        end
      end

  If you try to run our new macro, you will notice it won't
  even compile, complaining that the variables `k` and `v`
  do not exist. This is because of the ambiguity: `unquote(k)`
  can either be an unquote fragment, as previously, or a regular
  unquote as in `unquote(kv)`.

  One solution for this problem is to disable unquoting in the
  macro, however, doing that would make it impossible to inject
  `kv` representation into the tree. That's when the `:bind_quoted`
  option comes to the rescue. By using `:bind_quoted`, we can
  automatically disable unquoting while still injecting the
  desired variables into the tree:

      defmacro defkv(kv) do
        quote bind_quoted: [kv: kv] do
          Enum.each kv, fn { k, v } ->
            def unquote(k)(), do: unquote(v)
          end
        end
      end

  In fact, the `:binding` option is recommended every time one
  desires to inject a value into the quote.
  """
  defmacro quote(opts, block)

  @doc """
  When used inside quoting, marks that the variable should
  not be hygienized. The argument can be either a variable
  node (i.e. a tuple with three elements where the last
  one is an atom) or an atom representing the variable name.
  Check `quote/2` for more information.
  """
  defmacro var!(var)

  @doc """
  Defines a variable in the given context.

  If the context is `false`, it is not stored in any particular
  context and the variable is not shared in between clauses.

  Check `quote/2` for more information.
  """
  defmacro var!(var, context)

  @doc """
  When used inside quoting, marks that the alias should not
  be hygienezed. This means the alias will be expanded when
  the macro is expanded.
  """
  defmacro alias!(alias)

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
  name = :unquote
  defmacro unquote(name)(expr)

  @doc """
  Unquotes the given list expanding its arguments. Similar
  to unquote.

  ## Examples

      values = [2, 3, 4]
      quote do: sum(1, unquote_splicing(values), 5)
      #=> { :sum, [], [1, 2, 3, 4, 5] }

  """
  name = :unquote_splicing
  defmacro unquote(name)(expr)

  @doc """
  List comprehensions allow you to quickly build a list from another list:

      iex> lc n inlist [1, 2, 3, 4], do: n * 2
      [2,4,6,8]

  A comprehension accepts many generators and also filters. Generators
  are defined using both `inlist` and `inbits` operators, allowing you
  to loop lists and bitstrings:

      # A list generator:
      iex> lc n inlist [1, 2, 3, 4], do: n * 2
      [2,4,6,8]

      # A bit string generator:
      iex> lc <<n>> inbits <<1, 2, 3, 4>>, do: n * 2
      [2,4,6,8]

      # A generator from a variable:
      iex> list = [1, 2, 3, 4]
      ...> lc n inlist list, do: n * 2
      [2,4,6,8]

      # A comprehension with two generators
      iex> lc x inlist [1, 2], y inlist [2, 3], do: x*y
      [2,3,4,6]

  Filters can also be given:

      # A comprehension with a generator and a filter
      iex> lc n inlist [1, 2, 3, 4, 5, 6], rem(n, 2) == 0, do: n
      [2,4,6]

  Bit string generators are quite useful when you need to
  organize bit string streams:

      iex> pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>
      iex> lc <<r::8, g::8, b::8>> inbits pixels, do: {r, g, b}
      [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

  Note: Unlike Erlang, Elixir comprehension filters
  never behave as guards when it comes to errors. Errors in
  list comprehensions will always be raised. Consider this
  Erlang example:

      erl> [I || I <- [1,2,3], hd(I)].
      []

  In Elixir, it will raise:

      iex> lc i inlist [1,2,3], hd(i), do: i
      ** (ArgumentError) argument error

  """
  defmacro lc(args)

  @doc """
  Defines a bit comprehension. It follows the same syntax as
  a list comprehension but expects each element returned to
  be a bitstring. For example, here is how to remove all
  spaces from a string:

      iex> bc <<c>> inbits " hello world ", c != ? , do: <<c>>
      "helloworld"

  """
  defmacro bc(args)

  @doc """
  This is the special form used whenever we have a block
  of expressions in Elixir. This special form is private
  and should not be invoked directly:

      iex> quote do: (1; 2; 3)
      { :__block__, [], [1, 2, 3] }

  """
  defmacro __block__(args)

  @doc """
  Captures a call as an anonymous function.

  The most common format to capture a function is via module,
  name and arity:

      iex> fun = &Kernel.is_atom/1
      iex> fun.(:atom)
      true
      iex> fun.("string")
      false

  Local functions, including private ones, and imported ones
  can also be captured by omitting the module name:

      &local_function/1

  A capture also allows the captured functions to be partially
  applied, for example:

      iex> fun = &atom_to_binary(&1, :utf8)
      iex> fun.(:hello)
      "hello"

  In the example above, we use &1 as a placeholder, generating
  a function with one argument. We could also use `&2` and `&3`
  to delimit more arguments:

      iex> fun = &atom_to_binary(&1, &2)
      iex> fun.(:hello, :utf8)
      "hello"

  Since operators are calls, they are also supported, although
  they require explicit parentheses around:

      iex> fun = &(&1 + &2)
      iex> fun.(1, 2)
      3

  And even more complex call expressions:

      iex> fun = &(&1 + &2 + &3)
      iex> fun.(1, 2, 3)
      6

  Record-like calls are also allowed:

      iex> fun = &(&1.first)
      iex> fun.(1..3)
      1

  Remember tuple and lists are represented as calls in the AST and
  therefore are also allowed:

      iex> fun = &{&1, &2}
      iex> fun.(1, 2)
      { 1, 2 }

      iex> fun = &[&1|&2]
      iex> fun.(1, 2)
      [1|2]

  Anything that is not a call is not allowed though, examples:

      # An atom is not a call
      &:foo

      # A var is not a call
      var = 1
      &var

      # A block expression is not a call
      &(foo(&1, &2); &3 + &4)

  """
  name = :&
  defmacro unquote(name)(expr)

  @doc """
  This is the special form used whenever we have to temporarily
  change the scope information of a block. Used when `quote` is
  invoked with `location: :keep` to execute a given block as if
  it belonged to another file.

      quote location: :keep, do: 1
      #=> { :__scope__, [line: 1], [[file: "iex"],[do: 1]] }

  Check `quote/1` for more information.
  """
  defmacro __scope__(opts, args)

  @doc """
  This is the special form used to hold aliases information.
  It is usually compiled to an atom:

      quote do: Foo.Bar #=>
      { :__aliases__, [], [:Foo,:Bar] }

  Elixir represents `Foo.Bar` as `__aliases__` so calls can be
  unambiguously identified by the operator `:.`. For example:

      quote do: Foo.bar #=>
      {{:.,[],[{:__aliases__,[],[:Foo]},:bar]},[],[]}

  Whenever an expression iterator sees a `:.` as the tuple key,
  it can be sure that it represents a call and the second argument
  in the list is an atom.

  On the other hand, aliases holds some properties:

  1) The head element of aliases can be any term;

  2) The tail elements of aliases are guaranteed to always be atoms;

  3) When the head element of aliases is the atom `:Elixir`, no expansion happen;

  4) When the head element of aliases is not an atom, it is expanded at runtime:

        quote do: some_var.Foo
        {:__aliases__,[],[{:some_var,[],:quoted},:Bar]}

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
end
