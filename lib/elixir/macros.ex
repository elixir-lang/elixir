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
  #    sum(1, 2)
  #
  # Invoking a function in another module is equally easy:
  #
  #    Foo.sum(1, 2) #=> 3
  #
  # ## Dynamic function definition
  #
  # In macros it may be convenient to dynamically generate a function.
  # The first argument that Elixir expects in `def` is always a function
  # invocation form. That said, imagine you have a variable called `name`
  # and you want to use it to dynamically generate a function, you can do
  # that as:
  #
  #     def atom.(first_arg, second_arg) do
  #       # ...
  #     end
  #
  # Notice that the `.` is important as the format above says that we want
  # to define a function with the name given in atom, while the format below:
  #
  #     def atom(first_arg, second_arg) do
  #       # ...
  #     end
  #
  # Means we are generating a new function called atom.
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
  # This macro will then define a module named FileInfo
  # which will contain getters and setters for each attribute
  # and initialization methods. Therefore, one can do:
  #
  #     file_info = FileInfo.new(atime: now())
  #     file_info.atime         #=> Returns the value of atime
  #     file_info.atime(now())  #=> Updates the value of atime
  #
  # FileInfo is simply a module with functions specific for
  # the record. Notice that the name of the module is sensitive
  # to the current context. For instance:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # ... will define a module named FileInfo. However, if invoked
  # inside a module, the name will be nested:
  #
  #     defmodule Foo::Bar do
  #       defrecord FileInfo, atime: nil, mtime: nil
  #     end
  #
  #     Foo::Bar::FileInfo.new # Nested
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
  # ## Record as tuples
  #
  # A record is nothing more than a tuple. That said, if you create a
  # record an inspect it, this is what you should see on console:
  #
  #     IO.puts FileInfo.new
  #     #=> { ::FileInfo, nil, nil }
  #
  # The first element of the tuple is always the record name.
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
  #     defmodule Blank do
  #       defprotocol [blank?(data)]
  #
  #       # The opposite of blank?
  #       def present?(data) do
  #         !blank?(data)
  #       end
  #     end
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
  # ## Selecting protocols
  #
  # Implement the protocol for all 9 types above can be cumbersome.
  # Even more if you consider that Number, Function, PID, Port and
  # Reference are never going to be blank. For this reason, Elixir
  # allows you to point out that you are going to implement the protocols
  # just for some types, as follows:
  #
  #     defprotocol [blank?(data)], only: [Atom, Tuple, List, BitString]
  #
  # And for all other types, Elixir will now dispatch to Any. That said,
  # the default behavior could be implemented as:
  #
  #     defimpl Blank, for: Any do
  #       def blank?(_), do: false
  #     end
  #
  # Now, all data types that we not specified in only will be automatically
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
  #       def blank?(dict), do: RedBlack.empty_dict?(dict)
  #     end
  #
  # In the example above, we have implemented `blank?` for the custom
  # dictionary.
  #
  # Finally, notice that since records are simply tuples, the default
  # implementation for records can be given in the tuple implementation.
  defmacro defprotocol(args, opts // []) do
    Protocol.defprotocol(args, opts)
  end

  # Defines an implementation for the given protocol. See
  # `defprotocol/2` for examples.
  defmacro defimpl(name, do: block, for: for) do
    Protocol.defimpl(name, do: block, for: for)
  end

  # Inspect the given arguments according to the Inspect protocol.
  #
  # ## Examples
  #
  #     inspect(:foo)
  #     #=> ":foo"
  #
  defmacro inspect(arg) do
    quote { ::Inspect.inspect(unquote(arg)) }
  end

  # Convert the argument to a string according to the Inspect protocol.
  # This is the function invoked when there is string interpolation.
  #
  # ## Examples
  #
  #     stringify(:foo)
  #     #=> "foo"
  #
  defmacro stringify(arg) do
    quote { ::Inspect.stringify(unquote(arg)) }
  end

  # Checks if the given argument is_any structure. Always returns true.
  defmacro is_any(_), do: true

  # Define elem to get Tuple element according to Elixir conventions.
  # We need to implement it as a macro to it can be used in guards.
  #
  # == Example
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
  # == Example
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
    quote { erlang_op :div, unquote(left), unquote(right) }

  # Provides an integer remainder macro according to Erlang semantics.
  # Raises an error if one of the arguments is not an integer.
  # Can be used in guard tests.
  #
  # ## Examples
  #
  #     5 rem 2 #=> 1
  #
  defmacro rem(left, right), do:
    quote { erlang_op :rem, unquote(left), unquote(right) }

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
    if_clause    = { :kv_block, 0, [ { [condition], do_clause } ] }
    else_clause  = Orddict.fetch(tail, :else, nil)

    # Merge if and elsif clauses, as they will all become match clauses.
    merged =
      case Orddict.fetch(tail, :elsif, nil) do
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
  # Notice that, differently from Erlang `and` and `andalso` operators,
  # this operator accepts any expression as arguments, not only booleans.
  # Unfortunately cannot be used in macros.
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
  # Notice that, differently from Erlang `or` and `orelse` operators,
  # this operator accepts any expression as arguments, not only booleans.
  # Unfortunately cannot be used in macros.
  defmacro :||.(left, right) do
    quote {
      case !(__oror_var = unquote(left)) do
      match: false
        __oror_var
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