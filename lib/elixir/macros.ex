defmodule Elixir::Macros do
  # Defines a module given by name with the given contents.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       def bar, do: :baz
  #     end
  #
  #     Foo.bar #=> :baz
  #
  # ## Nesting
  #
  # Nesting a module inside the other does not affect its name:
  #
  #     defmodule Foo do
  #       defmodule Bar do
  #       end
  #     end
  #
  # In the example above, two modules `Foo` and `Bar`. Notice that
  # the second module is **not** called `Foo::Bar`. In general,
  # nesting modules is discouraged in Elixir.
  #
  # ## Dynamic names
  #
  # Elixir module names can be dynamically generated. This is very
  # useful for macros. For instance, one could write:
  #
  #     defmodule binary_to_atom("Foo#{1}", :utf8) do
  #       # contents ...
  #     end
  #
  # Elixir will accept any module name as long as the expression
  # returns an atom.
  defmacro defmodule(name, do: contents)

  # Defines a function with the given name and contents.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       def bar, do: :baz
  #     end
  #
  #     Foo.bar #=> :baz
  #
  # A function that expects arguments can be defined as follow:
  #
  #     defmodule Foo do
  #       def sum(a, b) do
  #         a + b
  #       end
  #     end
  #
  # In the example above, we defined a function `sum` that receives
  # two arguments and sum them.
  #
  # ## Function invocation
  #
  # Elixir follows the same rule as Erlang when it comes to
  # function invocations. Calling a function is the same thing
  # as "invoking at atom". That said, one could write:
  #
  #     defmodule Foo do
  #       def bar do
  #         :sum.(1, 2)
  #       end
  #
  #       def sum(a, b), do: a + b
  #     end
  #
  # In the example above, sum is invoked by invoking an atom
  # passing arguments 1 and 2. Since this syntax is a bit verbose,
  # Elixir also support the more conventional:
  #
  #     sum(1, 2)
  #
  # Invoking a function in another module is equally easy:
  #
  #     Foo.sum(1, 2) #=> 3
  #
  # ## Dynamic function definition
  #
  # In macros it may be convenient to dynamically generate a function.
  # This can be done by explicitly passing the function name, arguments
  # and guard:
  #
  #     name = :some_function
  #
  #     def name, [first_arg, second_arg], [is_list(first_arg)] do
  #       # ...
  #     end
  #
  defmacro def(name, do: contents)

  # Defines a function that is private. Private functions
  # can only be accessible from the same module it is defined.
  #
  # Check `def/2` for more information
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       def bar do
  #         sum(1, 2)
  #       end
  #
  #       defp sum(a, b), do: a + b
  #     end
  #
  # In the example above, `sum` is private and accessing it
  # through `Foo.sum` will raise an error.
  defmacro defp(name, do: contents)

  # Define a record given by name and values.
  #
  # ## Examples
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # The line above will define a module named `FileInfo` which
  # contains a function named `new` that returns a new record
  # and other functions to read and set the values in the
  # record. Therefore, we can do:
  #
  #     file_info = FileInfo.new(atime: now())
  #     file_info.atime         #=> Returns the value of atime
  #     file_info.atime(now())  #=> Updates the value of atime
  #
  # Internally, a record is simply a tuple where the first element is
  # always the record module name. This can be noticed if we print
  # the record:
  #
  #     IO.puts FileInfo.new
  #     { ::FileInfo, nil, nil }
  #
  # ## Default based functions
  #
  # Depending on the default value, Elixir will define helpers to interact
  # with the record. For example, ExUnit defines a record which keeps
  # track of how many tests were executed and the failures that happened
  # The record definition is similar to:
  #
  #     defrecord Config, counter: 0, failures: []
  #
  # Since `counter` is an integer, Elixir automatically defines a helper
  # named `increment_counter` that will increase the counter value:
  #
  #     Config.new.increment_counter.counter #=> 1
  #
  # `increment_counter` also accepts a number of increment as argument:
  #
  #     Config.new.increment_counter(10).counter #=> 10
  #
  # Besides, if the default is a list, Elixir will define three helpers:
  #
  # * `merge_field` - Receives an orddict and merge it into the current value;
  # * `prepend_field` - Receives another list and prepend its values
  # * `append_field` - Receives another list and append its values
  #
  defmacro defrecord(name, values, opts // []) do
    Record.defrecord(name, values, opts)
  end

  # Defines the current module as a protocol and specifies the API
  # that should be implemented.
  #
  # ## Examples
  #
  # In Elixir, only `false` and `nil` are considered falsy values.
  # Everything else evaluates to true in `if` clauses. Depending
  # on the application, it may be important to specify a `blank?`
  # protocol that returns a boolean for other data types that should
  # be considered `blank?`. For instance, an empty list or an empty
  # binary could be considered blanks.
  #
  # We could implement this protocol as follow:
  #
  #     defprotocol Blank, [blank?(data)]
  #
  # Now that the protocol is defined, we can implement it. We need
  # to implement the protocol for each Elixir type. For example:
  #
  #     # Numbers are never blank
  #     defimpl Blank, for: Number do
  #       def blank?(number), do: false
  #     end
  #
  #     # Just empty list is blank
  #     defimpl Blank, for: List do
  #       def blank?([]), do: true
  #       def blank?(_),  do: false
  #     end
  #
  #     # Just the atoms false and nil are blank
  #     defimpl Blank, for: Atom do
  #       def blank?(false), do: true
  #       def blank?(nil),   do: true
  #       def blank?(_),     do: false
  #     end
  #
  # And we would have to define the implementation for all types.
  # The types available are:
  #
  # * Tuple
  # * Atom
  # * List
  # * BitString
  # * Number
  # * Function
  # * PID
  # * Port
  # * Reference
  #
  # ## Selecting implementations
  #
  # Implementing the protocol for all 9 types above can be cumbersome.
  # Even more if you consider that Number, Function, PID, Port and
  # Reference are never going to be blank. For this reason, Elixir
  # allows you to point out that you are going to implement the protocols
  # just for some types, as follows:
  #
  #     defprotocol Blank, [blank?(data)], only: [Atom, Tuple, List, BitString]
  #
  # And for all other types, Elixir will now dispatch to Any. That said,
  # the default behavior could be implemented as:
  #
  #     defimpl Blank, for: Any do
  #       def blank?(_), do: false
  #     end
  #
  # Now, all data types that we have not specified will be automatically
  # considered non blank.
  #
  # ## Protocols + Records
  #
  # The real benefit of protocols comes when mixed with records. For instance,
  # one may implement a custom dictionary as a Red-Black tree and this
  # dictionary should also be considered as blank in case it has no items.
  # That said, he just needs to implement the protocol for this dictionary:
  #
  #     defimpl Blank, for: RedBlack::Dict do
  #       def blank?(dict), do: RedBlack.empty?(dict)
  #     end
  #
  # In the example above, we have implemented `blank?` for the custom
  # dictionary that simply delegates to `RedBlack.empty?`.
  #
  # Since records are simply tuples, the default implementation for
  # records can be given in the tuple implementation. Similarly,
  # not passing `Tuple` to `only:` disables all records lookup.
  defmacro defprotocol(name, args, opts // []) do
    Protocol.defprotocol(name, args, opts)
  end

  # Defines an implementation for the given protocol. See
  # `defprotocol/2` for examples.
  defmacro defimpl(name, [for: for], [do: block]) do
    Protocol.defimpl(name, [for: for], [do: block])
  end

  # Defines that the tuples delegates to the given target.
  # Functions defined with defdelegate are available to be
  # invoked from external.
  #
  # Currently supports delegating only to functions (not macros).
  #
  # ## Examples
  #
  #     defmodule MyList do
  #       defdelegate [reverse: 1], to: Erlang.lists
  #     end
  #
  #     MyList.reverse([1,2,3])
  #     #=> [3,2,1]
  #
  defmacro defdelegate(tuples, to: target) do
    lc { name, arity } in tuples do
      args = lc i in :lists.seq(1, arity) do
        { binary_to_atom(<<?x, i + 64>>, :utf8), 0, :quoted }
      end

      quote {
        def unquote(name).(unquote_splicing(args)) do
          apply unquote(target), unquote(name), [unquote_splicing(args)]
        end
      }
    end
  end

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
  #         # 1) Disable `if/2` from Elixir::Macros
  #         import Elixir::Macros, except: [if: 2]
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

  # `use` is a simple mechanism for extending the current module with the
  # given module.
  #
  # == Examples
  #
  #     defmodule AssertionTest do
  #       use ExUnit::Case
  #
  #       def test_always_pass do
  #         true = true
  #       end
  #     end
  #
  # By calling `use`, a hook called `__using__` will be invoked in
  # `ExUnit::Case` which will then do the proper setup. In other words,
  # `use` is simply a translation to:
  #
  #     defmodule AssertionTest do
  #       require ExUnit::Case, as: false
  #       ExUnit::Case.__using__(::AssertionTest)
  #
  #       def test_always_pass do
  #         true = true
  #       end
  #     end
  #
  defmacro use(module, args // [])

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
  #     quote { sum(1, 2, 3) }
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
  #         quote { a = 1 }
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
  #         quote { var!(a) = 1 }
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
  defmacro quote(do: contents)

  # Unquotes the given expression from inside a macro.
  #
  # ## Examples
  #
  # Imagine the situation you have a variable `name` and
  # you want to inject it inside some quote. The first attempt
  # would be:
  #
  #     value = 13
  #     quote { sum(1, value, 3) }
  #
  # Which would then return:
  #
  #     { :sum, 0, [1, { :value, 0, quoted }, 3] }
  #
  # Which is not the expected result. For this, we use unquote:
  #
  #     value = 13
  #     quote { sum(1, unquote(value), 3) }
  #     #=> { :sum, 0, [1, 13, 3] }
  #
  defmacro unquote(expr)

  # Unquotes the given list expanding its arguments. Similar
  # to unquote.
  #
  # ## Examples
  #
  #     values = [2,3,4]
  #     quote { sum(1, unquote_splicing(values), 5) }
  #     #=> { :sum, 0, [1, 2, 3, 4, 5] }
  #
  defmacro unquote_splicing(expr)

  # Returns an anonymous function based on the given arguments.
  #
  # ## Examples
  #
  #     sum = fn(x, y) { x + y }
  #     sum.(1, 2) #=> 3
  #
  # Notice that a function needs to be invoked using the dot between
  # th function and the arguments.
  #
  # A function could also be defined using `do/end` syntax, although
  # this is not recommended in order to avoid ambiguity. For example,
  # consider this case:
  #
  #     Enum.map [1,2,3], fn(x){ x * 2 }
  #
  # The example works fine, but if we replace it by `do/end`, it will fail:
  #
  #     Enum.map [1,2,3], fn(x) do
  #       x * 2
  #     end
  #
  # The reason it fails is because do/end always bind to the farthest
  # function call. It is easy to see the problem if we add parentheis
  # to the outer call. For example, the example using curly brackets
  # would translate to:
  #
  #     Enum.map([1,2,3], fn(x){ x * 2 })
  #
  # Which is the expected result, however using `do/end` blocks:
  #
  #     Enum.map([1,2,3], fn(x)) do
  #       x * 2
  #     end
  #
  # Which is not what we expect.
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

  # Inspect the given arguments according to the String::Inspect protocol.
  #
  # ## Examples
  #
  #     inspect(:foo)
  #     #=> ":foo"
  #
  defmacro inspect(arg) do
    quote { ::String::Inspect.inspect(unquote(arg)) }
  end

  # Convert the argument to a string according to the String::Inspect protocol.
  # This is the function invoked when there is string interpolation.
  #
  # ## Examples
  #
  #     to_binary(:foo)
  #     #=> "foo"
  #
  defmacro to_binary(arg) do
    quote { ::String::Inspect.to_binary(unquote(arg)) }
  end

  # Convert the argument to a list according to the List::Inspect protocol.
  #
  # ## Examples
  #
  #     to_list(:foo)
  #     #=> 'foo'
  #
  defmacro to_list(arg) do
    quote { ::List::Inspect.to_list(unquote(arg)) }
  end

  # Define elem to get Tuple element according to Elixir conventions.
  # We need to implement it as a macro to it can be used in guards.
  #
  # ## Example
  #
  #    tuple = { :foo, :bar, 3 }
  #    elem(tuple, 1) #=> :foo
  #
  defmacro elem(tuple, index) do
    quote { element(unquote(index), unquote(tuple)) }
  end

  # Define setelem to set Tuple element according to Elixir conventions.
  # We need to implement it as a macro to it can be used in guards.
  #
  # ## Example
  #
  #    tuple = { :foo, :bar, 3 }
  #    setelem(tuple, 1, :baz) #=> { :baz, :bar, 3 }
  #
  defmacro setelem(tuple, index, value) do
    quote { setelement(unquote(index), unquote(tuple), unquote(value)) }
  end

  # Provides an integer division macro according to Erlang semantics.
  # Raises an error if one of the arguments is not an integer.
  # Can be used in guard tests.
  #
  # ## Examples
  #
  #     5 div 2 #=> 2
  #
  defmacro div(left, right), do:
    quote { __OP__ :div, unquote(left), unquote(right) }

  # Provides an integer remainder macro according to Erlang semantics.
  # Raises an error if one of the arguments is not an integer.
  # Can be used in guard tests.
  #
  # ## Examples
  #
  #     5 rem 2 #=> 1
  #
  defmacro rem(left, right), do:
    quote { __OP__ :rem, unquote(left), unquote(right) }

  # Matches the given condition against the match clauses.
  #
  # ## Examples
  #
  #     case thing do
  #     match: { :selector, i, value } when is_integer(i)
  #       value
  #     match: value
  #       value
  #     end
  #
  # In the example above, we compare `thing` with each given
  # match clause and execute the first one that matches. If no
  # clause matches, an error is raised.
  #
  # Since Elixir variables can be assigned more than once, variables
  # in a match clause will always be assigned instead of matching with
  # its previous values. For example:
  #
  #     i = 1
  #     case 10 do
  #     match: i
  #       i * 2
  #     end
  #
  # The example above will return 20, because `i` is assgined to 10
  # and then multiplied by 2. If you desire to match the value of `i`
  # against the given condition, you need to use the `^` operator:
  #
  #     i = 1
  #     case 10 do
  #     match: ^i
  #       i * 2
  #     end
  #
  # The example above will actually fail because 10 does not match 1.
  #
  # Finally, `case` accepts an `else:` branch as a fallback if none
  # of the clauses match:
  #
  #     case thing do
  #     match: { :selector, i, value } when is_integer(i)
  #       value
  #     else:
  #       thing
  #     end
  #
  defmacro case(condition, blocks)

  # Execute the given expressions and catch any error, exit
  # or throw that may have happened.
  #
  # ## Examples
  #
  #     try do
  #       do_something_that_may_fail(some_arg)
  #     catch: :error, :badarg
  #       IO.puts "Invalid argument given"
  #     catch: :throw, value
  #       IO.puts "caught #{value}"
  #     after:
  #       IO.puts "This is printed regardless if it failed or succeed"
  #     end
  #
  # Each `catch` clause must be followed by a tuple with three elements.
  # The first one is the kind of exception: `:error`, `:throw` or `:exit`.
  # The second one is the value given to error, throw or exit and the third
  # one is the stacktrace (which one usually wants to ignore).
  #
  # Note that calls inside `try` are not tail recursive.
  #
  # ## Variable visibility
  #
  # Since an expression inside `try` may not have been properly evaluted,
  # any variable created inside `try` cannot be accessed externaly.
  # For instance:
  #
  #     try do
  #       x = 1
  #       do_something_that_may_fail(same_arg)
  #       :ok
  #     catch: _, _
  #       :failed
  #     end
  #
  #     x #=> Cannot access `x`
  #
  # In the example above, `x` cannot be accessed since it was defined
  # inside the `try` clause.
  defmacro try(args)

  # The current process will hang until it receives a message
  # from other processes that matches the given clauses.
  #
  # ## Examples
  #
  #     receive
  #     match: { :selector, i, value } when is_integer(i)
  #       value
  #     match: value when is_atom(value)
  #       value
  #     else:
  #       IO.puts :standard_error, "Unexpected message received"
  #     end
  #
  # The match clauses above follows the same rules as `case/2`.
  #
  # An optional after clause can be given in case the message was not
  # received after the specified period of time:
  #
  #     receive
  #     match: { :selector, i, value } when is_integer(i)
  #       value
  #     match: value when is_atom(value)
  #       value
  #     else:
  #       IO.puts :standard_error, "Unexpected message received"
  #     after: 5000
  #       IO.puts :standard_error, "No message in 5 seconds"
  #     end
  #
  # The `after` clause can be specified even if there are no match clauses.
  # There are two special cases for the timout value given to after:
  #
  # * `:infinity` - The process should wait indefinitely for a matching
  # message, this is the same as not using a timeout.
  #
  # * 0 - if there is no matching message in the mailbox, the timeout
  # will occur immediately.
  defmacro receive(args)

  # Provides an `if` macro. The macro expects the first argument to
  # be a condition and the rest are key-value arguments.
  #
  # ## One-liner examples
  #
  #     if(foo, do: bar)
  #
  # In the example above, bar will be returned if foo evalutes to
  # true (i.e. it is not false nor nil). Otherwise, nil will be returned.
  #
  # An else option can be given to specify the opposite:
  #
  #     if(foo, do: bar, else: bar)
  #
  # ## Key-value blocks examples
  #
  # When several expressions must be passed to if, the most appropriate
  # form is thorugh key-value blocks. The first example above would then
  # be translated to:
  #
  #     if foo do
  #       bar
  #     end
  #
  # Notice that do/end becomes delimiters. The value given between
  # do/end becomes the expression given to as `do:`. The second example
  # would then translate do:
  #
  #     if foo do
  #       bar
  #     else:
  #       baz
  #     end
  #
  # Notice that extra keys follows the regular `else:` form. You can also
  # add extra `elsif:` clauses:
  #
  #
  #     if foo do
  #       bar
  #     elsif: some_condition
  #       bar + baz
  #     else:
  #       baz
  #     end
  #
  defmacro if(condition, [{:do,do_clause}|tail]) do
    # Transform the condition and the expressions in the
    # do_clause to a key-value block. Get the other values
    # from the tail orddict.
    if_clause   = { :__KVBLOCK__, 0, [ { [condition], do_clause } ] }
    else_clause = Orddict.get(tail, :else)

    # Merge if and elsif clauses, as they will all become match clauses.
    merged =
      case Orddict.get(tail, :elsif) do
      match: nil
        [match: if_clause]
      match: elsif_clause
        Erlang.elixir_kv_block.merge([match: if_clause], [match: elsif_clause])
      end

    # Decouple all if and elsif clauses into an array of tuples.
    # Those tuples are made of three elements, the key-block key,
    # the given condition and the block expressions
    all = Erlang.elixir_kv_block.decouple(merged)
    build_if_clauses(List.reverse(all), else_clause)
  end

  # Provide a unless macro that executes the expression
  # unless a value evalutes to true. Check `if` for examples
  # and documentation.
  defmacro unless(clause, options) do
    quote { if(!unquote(clause), unquote(options)) }
  end

  # Provide a short-circuit operator that executes the second
  # expression only if the first one evalutes to true (i.e. it is
  # not nil nor false). Returns the first expression otherwise.
  #
  # ## Examples
  #
  #     true && true         #=> true
  #     nil && true          #=> nil
  #     true && 1            #=> 1
  #     false && error(:bad) #=> false
  #
  # Notice that, differently from Erlang `and` operator, this operator
  # accepts any expression as arguments, not only booleans, but
  # cannot be used in macros.
  defmacro :&&.(left, right) do
    quote do
      case unquote(left) do
      match: false
        false
      match: nil
        nil
      match: _
        unquote(right)
      end
    end
  end

  # Provide a short-circuit operator that executes the second
  # expression only if the first one does not evalute to true (i.e. it
  # is not nil nor false). Returns the first expression otherwise.
  #
  # ## Examples
  #
  #     false || false       #=> false
  #     nil || true          #=> true
  #     false || 1           #=> 1
  #     true || error(:bad)  #=> true
  #
  # Notice that, differently from Erlang `or` operator, this operator
  # accepts any expression as arguments, not only booleans, but
  # cannot be used in macros.
  defmacro :||.(left, right) do
    quote {
      case !(oror = unquote(left)) do
      match: false
        oror
      else:
        unquote(right)
      end
    }
  end

  # Optimize !! to avoid generating case twice.
  # :nodoc:
  defmacro :!.({:!, _, [expr]}) do
    quote {
      case unquote(expr) do
      match: false
        false
      match: nil
        false
      else:
        true
      end
    }
  end

  # Implements the unary operator ! as a macro. It receives any
  # argument and returns true if it is false or nil. Returns false
  # otherwise.
  #
  # ## Examples
  #
  #   !1        #=> false
  #   ![1,2,3]  #=> false
  #   !false    #=> true
  #   !nil      #=> true
  #
  defmacro :!.(expr) do
    quote do
      case unquote(expr) do
      match: false
        true
      match: nil
        true
      else:
        false
      end
    end
  end

  ## Private functions

  # Build if clauses by nesting them recursively.
  # For instance, the following clause:
  #
  #     if foo do
  #       1
  #     elsif: bar
  #       2
  #     else:
  #       3
  #     end
  #
  # Becomes:
  #
  #     case !foo do
  #     match: false
  #       1
  #     match: true
  #       case !bar do
  #       match: false
  #         2
  #       match: true
  #         3
  #       end
  #     end
  #
  defp build_if_clauses([{ :match, [condition], clause }|t], acc) do
    new_acc = quote {
      case !unquote(condition) do
      match: false
        unquote(clause)
      else:
        unquote(acc)
      end
    }

    build_if_clauses(t, new_acc)
  end

  defp build_if_clauses([{ :match, [], _clause }|_], _) do
    error { :badarg, "No conditions given to elsif clause" }
  end

  defp build_if_clauses([], acc), do: acc
end