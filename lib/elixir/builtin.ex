defmodule Elixir::Builtin do
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
  defmacro defrecord(name, values, opts // [], do_block // []) do
    Record.defrecord(name, values, Orddict.merge(opts, do_block))
  end

  # Defines an exception. It follows exactly the same API as record.
  # The defined record must implement `message/1` as API, otherwise
  # an error is raised. Check exception.ex for examples.
  defmacro defexception(name, values, opts // [], do_block // []) do
    opts   = Orddict.merge(opts, do_block)
    values = [{ :__exception__, __EXCEPTION__ }|values]

    record = Record.defrecord(name, values, opts)
    check  = quote do
      unless List.member?(unquote(name).__info__(:exports), { :message, 1 }), do:
        raise "Expected #{unquote(name)} to implement message/1"
    end

    [record, check]
  end

  # Check if the given structure is an exception.
  #
  # ## Examples
  #
  #     is_exception(Error.new) #=> true
  #     is_exception(1)         #=> false
  #
  defmacro is_exception(thing) do
    quote do
      in_guard do
        is_tuple(unquote(thing)) andalso element(2, unquote(thing)) == __EXCEPTION__
      else:
        result = unquote(thing)
        is_tuple(result) andalso element(2, result) == __EXCEPTION__
      end
    end
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

      quote do
        def unquote(name).(unquote_splicing(args)) do
          apply unquote(target), unquote(name), [unquote_splicing(args)]
        end
      end
    end
  end

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

  # Inspect the given arguments according to the String::Inspect protocol.
  #
  # ## Examples
  #
  #     inspect(:foo)
  #     #=> ":foo"
  #
  defmacro inspect(arg) do
    quote do: ::String::Inspect.inspect(unquote(arg))
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
    quote do: ::String::Inspect.to_binary(unquote(arg))
  end

  # Convert the argument to a list according to the List::Inspect protocol.
  #
  # ## Examples
  #
  #     to_char_list(:foo)
  #     #=> 'foo'
  #
  defmacro to_char_list(arg) do
    quote do: ::List::Inspect.to_char_list(unquote(arg))
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
    quote do: element(unquote(index), unquote(tuple))
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
    quote do: setelement(unquote(index), unquote(tuple), unquote(value))
  end

  # Provides an integer division macro according to Erlang semantics.
  # Raises an error if one of the arguments is not an integer.
  # Can be used in guard tests.
  #
  # ## Examples
  #
  #     5 div 2 #=> 2
  #
  defmacro div(left, right) do
    quote do: __OP__ :div, unquote(left), unquote(right)
  end

  # Provides an integer remainder macro according to Erlang semantics.
  # Raises an error if one of the arguments is not an integer.
  # Can be used in guard tests.
  #
  # ## Examples
  #
  #     5 rem 2 #=> 1
  #
  defmacro rem(left, right) do
    quote do: __OP__ :rem, unquote(left), unquote(right)
  end

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
  #     rescue: ArgumentError
  #       IO.puts "Invalid argument given"
  #     catch: value
  #       IO.puts "caught #{value}"
  #     after:
  #       IO.puts "This is printed regardless if it failed or succeed"
  #     end
  #
  # The rescue clause is used to handle errors, while the catch clause
  # can be used to catch throw values. The catch clause accepts the same
  # pattern matching rules as match.
  #
  # Note that calls inside `try` are not tail recursive since the VM
  # needs to keep the stacktrace in case an exception happens.
  #
  # ## Rescue clauses
  #
  # While `catch` is simply a pattern matching mechanism, rescue
  # provides a higher abstraction around exceptions that allows
  # one to rescue an exception by its name and not by its internal
  # contents. All the following formats are valid rescue expressions:
  #
  #     try do
  #       UndefinedModule.undefined_function
  #     rescue: UndefinedFunctionError
  #     end
  #
  #     try do
  #       UndefinedModule.undefined_function
  #     rescue: [UndefinedFunctionError]
  #     end
  #
  #     # rescue and assign to x
  #     try do
  #       UndefinedModule.undefined_function
  #     rescue: x in [UndefinedFunctionError]
  #     end
  #
  #     # rescue all and assign to x
  #     try do
  #       UndefinedModule.undefined_function
  #     rescue: x in _
  #     end
  #
  # ## Variable visibility
  #
  # Since an expression inside `try` may not have been evaluted
  # due to an exception, any variable created inside `try` cannot
  # be accessed externaly.
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
  #
  # ## Catching exits and errors
  #
  # The catch clause works exactly the same as in Erlang. Therefore,
  # one can also handle exits/errors coming from Erlang as below:
  #
  #     try do
  #       exit(1)
  #     catch: :exit, 1
  #       IO.puts "Exited with 1"
  #     end
  #
  #     try do
  #       error(:sample)
  #     catch: :error, :sample
  #       IO.puts "sample error"
  #     end
  #
  # Although the second form should be avoided in favor of raise/rescue
  # control mechanisms.
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

  # Provides a unless macro that executes the expression
  # unless a value evalutes to true. Check `if` for examples
  # and documentation.
  defmacro unless(clause, options) do
    quote do: if(!unquote(clause), unquote(options))
  end

  # Concatenates two binaries.
  #
  # ## Examples
  #
  #     "foo" <> "bar" #=> "foobar"
  #
  # The `<>` operator can also be used in guard clauses as
  # long as the first part is a literal binary:
  #
  #     "foo" <> x = "foobar"
  #     x #=> "bar"
  #
  defmacro :<>.(left, right) do
    concats = extract_concatenations({ :<>, 0, [left, right] })
    quote do: << unquote_splicing(concats) >>
  end

  # Provides a short-circuit operator that executes the second
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

  # Provides a short-circuit operator that executes the second
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
    quote do
      case !(oror = unquote(left)) do
      match: false
        oror
      else:
        unquote(right)
      end
    end
  end

  # Optimizes !! to avoid generating case twice.
  # :nodoc:
  defmacro :!.({:!, _, [expr]}) do
    quote do
      case unquote(expr) do
      match: false
        false
      match: nil
        false
      else:
        true
      end
    end
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

  # Raises an `Error` with the given message.
  #
  # ## Examples
  #
  #     raise "Given values do not match"
  #
  def raise(msg) when is_binary(msg) do
    error RuntimeError.new(message: msg)
  end

  # Receives a reference for an exception
  # and instantiates a new exception record
  #
  # ## Examples
  #
  #     raise ArgumentError
  #
  def raise(atom) when is_atom(atom) do
    error atom.new
  end

  # Receives an already existing exception and re-raises it.
  #
  # ## Examples
  #
  #     try do
  #       1 + :foo
  #     rescue: x in [BadargError]
  #       IO.puts "that was expected"
  #       raise x
  #     end
  #
  def raise(exception) when is_tuple(exception) andalso element(2, exception) == __EXCEPTION__ do
    error exception
  end

  # Receives a reference for an exception and
  # instantiates a new exception with the given args.
  #
  # ## Examples
  #
  #     raise ArgumentError, message: "Expected a protocol"
  #
  def raise(atom, args) when is_atom(atom) do
    error atom.new(args)
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
    { :|, 0, [other, :binary] }
  end

  # Builds if clauses by nesting them recursively.
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
    new_acc = quote do
      case !unquote(condition) do
      match: false
        unquote(clause)
      else:
        unquote(acc)
      end
    end

    build_if_clauses(t, new_acc)
  end

  defp build_if_clauses([{ :match, _, _clause }|_], _) do
    raise ArgumentError, message: "No or too many conditions given to elsif clause"
  end

  defp build_if_clauses([], acc), do: acc
end
