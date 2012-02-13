defmodule Elixir::SpecialForms do
  # `import` allows one to easily access functions or macros from
  # others modules without using the qualified name.
  #
  # ## Examples
  #
  # If you want to use the `values` function from `Orddict` several times
  # in your module and you don't want to always type `Orddict.values`,
  # you can simply import it:
  #
  #     defmodule Math do
  #       import Orddict, only: [values: 1]
  #
  #       def some_function do
  #         # call values(orddict)
  #       end
  #     end
  #
  # In this case, we are importing only the function `values` (with arity 1)
  # from `Orddict`. Although `only` is optional, its usage is recommended.
  # `except` could also be given as an option. If no option is given, all
  # functions and macros are imported.
  #
  # In case you want to import only functions or macros, you can pass a
  # first argument selecting the scope:
  #
  #     import :macros, MyMacros
  #
  # And you can then use `only` or `except` to filter the macros being
  # included.
  #
  # ## Lexical scope
  #
  # It is important to notice that `import` is lexical. This means you
  # can import specific macros inside specific functions:
  #
  #     defmodule Math do
  #       def some_function do
  #         # 1) Disable `if/2` from Elixir::Builtin
  #         import Elixir::Builtin, except: [if: 2]
  #
  #         # 2) Require the new `if` macro from MyMacros
  #         import MyMacros
  #
  #         # 3) Use the new macro
  #         if do_something, it_works
  #       end
  #     end
  #
  # In the example above, we imported macros from `MyMacros`, replacing
  # the original `if/2` implementation by our own during that
  # specific function. All other functions in that module will still
  # be able to use the original one.
  #
  # ## Require shortcut
  #
  # You may optional pass `as:` an option to import and it will
  # automatically invoke `require` creating the desired shortcut.
  # Read `require` for more information.
  defmacro import(module, opts // [])

  # `require` is used to require the presence of external modules and
  # also to setup aliases.
  #
  # ## Aliases example
  #
  # `require` can be used to setup an alias for any module:
  #
  #     defmodule Math do
  #       require MyOrddict, as: Orddict
  #     end
  #
  # In the example above, we have set up `MyOrdict` to be referenced
  # as `Orddict`. So now, any reference to `Orddict` will be
  # automatically replaced by `MyOrddict`.
  #
  # In case one wants to access the original `Orddict`, it can be done
  # by prefixing the module name with `::`:
  #
  #     Orddict.values   #=> uses ::MyOrddict.values
  #     ::Orddict.values #=> uses ::Orddict.values
  #
  # ## Macros example
  #
  # Notice that usually modules should not be required before usage,
  # the only exception is if you want to use the macros from a module.
  # In such cases, you need to explicitly require them.
  #
  # Let's suppose you created your own `if` implementation in the module
  # `MyMacros`. If you want to invoke it, you need to first explicitly
  # require the `MyMacros`:
  #
  #     defmodule Math do
  #       require MyMacros
  #       MyMacros.if do_something, it_works
  #     end
  #
  # An attempt to call a macro that was not loaded will raise an error.
  #
  # ## Lexical scope
  #
  # As `import`, it is important to note that `require` is **lexical**.
  # This means you can set up aliases for specific functions not affecting
  # the overall scope.
  defmacro require(module, opts // [])

  # Returns the current module name as an atom or nil otherwise.
  defmacro __MODULE__

  # Returns the current file name as a char list.
  defmacro __FILE__

  # Returns the current line number as an integer.
  defmacro __LINE__

  # Allows you to get the representation of any expression.
  #
  # ## Examples
  #
  #     quote do: sum(1, 2, 3)
  #     #=> { :sum, 0, [1, 2, 3] }
  #
  # ## Homoiconicity
  #
  # Elixir is an homoiconic language. Any Elixir program can be
  # represented using its own data structures. The building block
  # of Elixir homoiconicity is a tuple with three elements, for example:
  #
  #     { :sum, 1, [1, 2, 3] }
  #
  # The tuple above represents a function call to sum passing 1, 2 and
  # 3 as arguments. The tuple elements are:
  #
  # * The first element of the tuple is always an atom or
  #   another tuple in the same representation;
  # * The second element of the tuple is always an integer
  #   representing the line number;
  # * The third element of the tuple are the arguments for the
  #   function call. The third argument may be an atom, meaning
  #   that it may be a variable.
  #
  # ## Macro literals
  #
  # Besides the tuple described above, Elixir has a few literals that
  # when quoted return themselves. They are:
  #
  #     :sum         #=> Atoms
  #     1            #=> Integers
  #     2.0          #=> Floats
  #     [1,2]        #=> Lists
  #     "binaries"   #=> Binaries
  #     {key, value} #=> Key-value pairs (i.e. a tuple with two elements)
  #
  # ## Hygiene
  #
  # Elixir macros are hygienic regarding to variables. This means
  # a variable defined in a macro cannot affect the scope where
  # the macro is included. Consider the following example:
  #
  #     defmodule Hygiene do
  #       defmacro no_interference do
  #         quote do: a = 1
  #       end
  #     end
  #
  #     require Hygiene
  #
  #     a = 10
  #     Hygiene.no_interference
  #     a # => 10
  #
  # In the example above, `a` returns 10 even if the macro
  # is apparently setting it to 1 because the variables defined
  # in the macro does not affect the context the macro is
  # executed. If you want to set or get a variable, you can do
  # it with the help of the `var!` macro:
  #
  #     defmodule NoHygiene do
  #       defmacro interference do
  #         quote do: var!(a) = 1
  #       end
  #     end
  #
  #     require NoHygiene
  #
  #     a = 10
  #     NoHygiene.interference
  #     a # => 11
  #
  # Notice that references are not hygienic in Elixir unless
  # you explicitly prepend :: to the reference name.
  #
  #     quote do
  #       ::Foo # => Access the root Foo
  #       Foo   # => Access the Foo reference in the current
  #                  module (if any is set), then fallback to root
  #     end
  #
  # ## Options
  #
  # `quote` also accepts some options as arguments. For example,
  # hygiene can be turned off via `hygiene: false` which is useful
  # when one is generating a code that should be inserted into
  # some function.
  defmacro quote(opts // [], do: contents)

  # Unquotes the given expression from inside a macro.
  #
  # ## Examples
  #
  # Imagine the situation you have a variable `name` and
  # you want to inject it inside some quote. The first attempt
  # would be:
  #
  #     value = 13
  #     quote do: sum(1, value, 3)
  #
  # Which would then return:
  #
  #     { :sum, 0, [1, { :value, 0, quoted }, 3] }
  #
  # Which is not the expected result. For this, we use unquote:
  #
  #     value = 13
  #     quote do: sum(1, unquote(value), 3)
  #     #=> { :sum, 0, [1, 13, 3] }
  #
  defmacro unquote(expr)

  # Unquotes the given list expanding its arguments. Similar
  # to unquote.
  #
  # ## Examples
  #
  #     values = [2,3,4]
  #     quote do: sum(1, unquote_splicing(values), 5)
  #     #=> { :sum, 0, [1, 2, 3, 4, 5] }
  #
  defmacro unquote_splicing(expr)

  # Returns an anonymous function based on the given arguments.
  #
  # ## Examples
  #
  #     sum = fn(x, y, do: x + y)
  #     sum.(1, 2) #=> 3
  #
  # Notice that a function needs to be invoked using the dot between
  # the function and the arguments.
  #
  # A function could also be defined using the `end` syntax, although
  # it is recommend to use it only with the stab operator in order to
  # avoid ambiguity. For example, consider this case:
  #
  #     Enum.map [1,2,3], fn(x) ->
  #       x * 2
  #     end
  #
  # The example works fine because `->` binds to the closest function call,
  # which is `fn`, but if we replace it by `do/end`, it will fail:
  #
  #     Enum.map [1,2,3], fn(x) do
  #       x * 2
  #     end
  #
  # The reason it fails is because do/end always bind to the farthest
  # function call.
  #
  # ## Function with multiple clauses
  #
  # One may define a function which expects different clauses as long
  # as all clauses expects the same number of arguments:
  #
  #     fun = fn do
  #     match: x, y when y < 0
  #       x - y
  #     match: x, y
  #       x + y
  #     end
  #
  #     fun.(10, -10) #=> 20
  #     fun.(10, 10)  #=> 20
  #
  defmacro fn(args)

  # Handle annonymous recursive loops.
  #
  # ## Examples
  #
  #     list = [1,2,3]
  #
  #     loop list, [] do
  #     match: [h|t], acc
  #       recur t, [h*2|acc]
  #     match: [], acc
  #       acc
  #     end
  #     #=> [6,4,2]
  #
  # Notice that all match clauses expects the same ammount
  # of arguments. Guards can also be given.
  #
  # Recursion happens by calling recur with the same number
  # of arguments of each match clause. `recur` does not guarantee
  # that it will be tail recursive.
  defmacro loop(args)

  # Receives a sequence of generators and filters and execute the
  # given block to the matching combination. This is a higher level
  # implementation of list comprehensions that follows the Enum::Iterator
  # protocol.
  #
  # ## Examples
  #
  # An example with one generator:
  #
  #     [2,4] = for x in [1,2], do: x * 2
  #
  # Generators can also pattern match:
  #
  #     list = [{1,2},{0,3},{1,4}]
  #     [4,8] = for {1,x} in list, do: x * 2
  #
  # Two generators:
  #
  #     [1,2,2,4] = for x in [1,2], y in [1,2], do: x * y
  #
  # Two generators and one filter:
  #
  #     [1,4] = for x in [1,2], y in [1,2], x == y do
  #       x * y
  #     end
  #
  # A filter can return any value. All values except nil and false
  # evalutes to true.
  #
  # ## Comprehensions
  #
  # Elixir also provides `lc` and `bc` as comprehensions. Both `lc`
  # and `bc` collect items in the stack, while `for` collects item
  # in a list that is reversed. `lc` and `bc` must be used when the
  # focus is performance and you don't expect different kinds of
  # enumerables as arguments.
  #
  # Also, binary generators are not supported by `for` since Elixir
  # doesn't treat binaries as enumerables. If you need to manipulate
  # binaries, you need to resort to comprehensions.
  defmacro for(args)

  # Keeps one of the given expressions depending in the context
  # of evaluation is a guard or not. This is useful when creating
  # macro that should work both inside and outside guards but
  # still hold some characteristics.
  #
  # ## Example
  #
  # A good example is the `is_exception/1` macro defined in Elixir:
  #
  #      defmacro is_exception(thing) do
  #        quote do
  #          in_guard do
  #            andalso(is_tuple(unquote(thing)), element(2, unquote(thing)) == __EXCEPTION__)
  #          else:
  #            result = unquote(thing)
  #            andalso(is_tuple(result), element(2, result) == __EXCEPTION__)
  #          end
  #        end
  #      end
  #
  # Notice that if inside a guard, we unquote the same element twice.
  # This will cause the same element to be evaluted twice, but this is
  # fine for guards since we cannot assign variables in guards and
  # we cannot call expressions inside guards. However, when outside
  # of a guard, evaluating the arguments twice can be harmful and
  # unexpected, for this reason, we save the result in a variable.
  #
  # In the example above, `in_guard` is allowing us to customize
  # the same macro to work inside and outside guards.
  defmacro is_guard(do: do_block, else: else_block)
end