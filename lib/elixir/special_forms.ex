defmodule Elixir.SpecialForms do
  @moduledoc """
  In this module we define Elixir special forms. Those are called
  special forms because they cannot be overridden by the developer
  and sometimes have lexical scope (like alias, require, import, etc).
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
  """
  defmacro :<<>>.(args)

  @doc """
  `alias` is used to setup atom aliases, often useful with modules names.

  ## Examples

  `alias` can be used to setup an alias for any module:

      defmodule Math do
        alias MyKeyword, as: Keyword
      end

  In the example above, we have set up `MyOrdict` to be alias
  as `Keyword`. So now, any reference to `Keyword` will be
  automatically replaced by `MyKeyword`.

  In case one wants to access the original `Keyword`, it can be done
  by accessing __MAIN__:

      Keyword.values   #=> uses MyKeyword.values
      __MAIN__.Keyword.values #=> uses Keyword.values

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

  If you want to use the `values` function from `Keyword` several times
  in your module and you don't want to always type `Keyword.values`,
  you can simply import it:

      defmodule Math do
        import Keyword, only: [values: 1]

        def some_function do
          # call values(orddict)
        end
      end

  In this case, we are importing only the function `values` (with arity 1)
  from `Keyword`. Although `only` is optional, its usage is recommended.
  `except` could also be given as an option. If no option is given, all
  functions and macros are imported.

  In case you want to import only functions or macros, you can pass a
  first argument selecting the scope:

      import :macros, MyMacros

  And you can then use `only` or `except` to filter the macros being
  included.

  ## Lexical scope

  It is important to notice that `import` is lexical. This means you
  can import specific macros inside specific functions:

      defmodule Math do
        def some_function do
          # 1) Disable `if/2` from Elixir.Builtin
          import Elixir.Builtin, except: [if: 2]

          # 2) Require the new `if` macro from MyMacros
          import MyMacros

          # 3) Use the new macro
          if do_something, it_works
        end
      end

  In the example above, we imported macros from `MyMacros`, replacing
  the original `if/2` implementation by our own during that
  specific function. All other functions in that module will still
  be able to use the original one.

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
      a #=> 11

  Notice that aliases are not hygienic in Elixir, ambiguity
  must be solved by prepending __MAIN__:

      quote do
        __MAIN__.Foo #=> Access the root Foo
        Foo          #=> Access the Foo alias in the current module
                         (if any is set), then fallback to __MAIN__.Foo
      end

  ## Options

  `quote` also accepts some options as arguments. For example,
  hygiene can be turned off via `hygiene: false` which is useful
  when one is generating a code that should be inserted into
  some function.
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
  Returns an anonymous function based on the given arguments.

  ## Examples

      sum = fn(x, y) -> x + y end
      sum.(1, 2) #=> 3

  Notice that a function needs to be invoked using the dot between
  the function and the arguments.

  A function could also be defined using the `end` syntax, although
  it is recommend to use it only with the stab operator in order to
  avoid ambiguity. For example, consider this case:

      Enum.map [1,2,3], fn x ->
        x * 2
      end

  The example works fine because `->` binds to the closest function call,
  which is `fn`, but if we replace it by `do/end`, it will fail:

      Enum.map [1,2,3], fn(x) do
        x * 2
      end

  The reason it fails is because do/end always bind to the farthest
  function call.

  ## Function with multiple clauses

  One may define a function which expects different clauses as long
  as all clauses expects the same number of arguments:

      fun = fn do
        x, y when y < 0 ->
          x - y
        x, y ->
          x + y
      end

      fun.(10, -10) #=> 20
      fun.(10, 10)  #=> 20

  """
  defmacro fn(args)

  @doc """
  Handle annonymous recursive loops.

  ## Examples

      list = [1,2,3]

      loop list, [] do
        [h|t], acc ->
          recur t, [h*2|acc]
        [], acc ->
          acc
      end
      #=> [6,4,2]

  Notice that all match clauses expects the same ammount
  of arguments. Guards can also be given.

  Recursion happens by calling recur with the same number
  of arguments of each match clause. `recur` does not guarantee
  that it will be tail recursive.
  """
  defmacro loop(args)

  @doc """
  A function that forces the current loop to recur. See `loop/1`
  for more information.
  """
  defmacro recur(args)

  @doc """
  List comprehensions allow you to quickly build a list from another list:

      lc n in [1,2,3,4], do: n * 2
      #=> [2,4,6,8]

  A comprehension accepts many generators and also filters. Filters must be given after the when clause:

      # A comprehension with a generator and a filter
      lc n in [1,2,3,4,5,6] when rem(n, 2) == 0, do: n
      #=> [2,4,6]

      # A comprehension with two generators
      lc x in [1,2], y in [2,3], do: x*y
      #=> [2,3,4,6]

  Elixir provides generators for both lists and bitstrings:

      # A list generator:
      lc n in [1,2,3,4], do: n * 2
      #=> [2,4,6,8]

      # A bit string generator:
      lc <<n>> in <<1,2,3,4>>, do: n * 2
      #=> [2,4,6,8]

  Bit string generators are quite useful when you need to organize bit string streams:

      iex> pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>
      iex> lc <<r:8,g:8,b:8>> in pixels, do: {r,g,b}
      [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

  Elixir does its best to hide the differences between list and bit string generators.
  However, there is a special case due to Erlang limitation where we need to explicitly
  tell Erlang that a list is being given as argument:

      # This will fail because when Elixir sees that the left side
      # of the in expression is a bit string, it expects the right side
      # to be a bit string as well:
      lc <<n>> in [<<1>>,<<2>>,<<3>>], do: n*2
      #=> ** (ErlangError) erlang error {:bad_generator,[<<1>>,<<2>>,<<3>>]}

      # You need to be explicit and use inlist:
      lc inlist(<<n>>, [<<1>>,<<2>>,<<3>>]), do: n*2
      #=> [2,4,6]

      # For consistency, inbin is also available:
      lc inbin(<<n>>, <<1,2,3>>), do: n*2
      #=> [2,4,6]

  Notice that although comprehensions uses `when` to specify filters, filters are not
  guards and therefore accept any expression (they are not limited as guards).
  """
  defmacro lc(args)

  @doc """
  Defines a bit comprehension. It follows the same syntax as
  a list comprehension but expects each element returned to
  be a bitstring. For example, here is how to remove all
  spaces from a string:

      bc <<c>> in " hello world " when c != ?\s, do: <<c>>
      "helloworld"

  """
  defmacro bc(args)
end