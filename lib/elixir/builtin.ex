defmodule Elixir::Builtin do
  @moduledoc """
  `Elixir::Builtin` provides the default macros and functions
  Elixir imports to your environment. Those macros and functions
  can be skipped or cherry-picked via the import function. For
  instance, if you want to tell Elixir to not import the `case`
  macro, you can do:

      import Elixir::Builtin, except: [case: 2]

  Elixir also has special forms that are always imported and
  cannot be skipped. These are described in `Elixir::SpecialForms`.
  """

  @doc """
  Defines a module given by name with the given contents.

  ## Examples

      defmodule Foo do
        def bar, do: :baz
      end

      Foo.bar #=> :baz

  ## Automatic aliases

  When defining modules, Elixir automatically sets up aliases
  for the defined module:

      defmodule MyNamespace::Foo do
        def value, do: 1
      end

      Foo.value #=> 1

  In the example above, Elixir automatically aliased `MyNamespace::Foo`
  to `Foo` in that lexical context. You can change or turn off the
  aliasing via the `:as` option:

      defmodule MyNamespace::Foo, as: Bar do
        def value, do: 1
      end

      Bar.value #=> 1

  Read about `require/2` below for more information.

  ## Nesting

  Nesting a module inside the other does not affect its name:

      defmodule Foo do
        defmodule Bar do
        end
      end

  In the example above, two modules `Foo` and `Bar`. Notice that
  the second module is **not** called `Foo::Bar`. In general,
  nesting modules is discouraged in Elixir.

  ## Dynamic names

  Elixir module names can be dynamically generated. This is very
  useful for macros. For instance, one could write:

      defmodule binary_to_atom("Foo#{1}", :utf8) do
        # contents ...
      end

  Elixir will accept any module name as long as the expression
  returns an atom.
  """
  defmacro defmodule(name, do: contents)

  @doc """
  Defines a function with the given name and contents.

  ## Examples

      defmodule Foo do
        def bar, do: :baz
      end

      Foo.bar #=> :baz

  A function that expects arguments can be defined as follow:

      defmodule Foo do
        def sum(a, b) do
          a + b
        end
      end

  In the example above, we defined a function `sum` that receives
  two arguments and sum them.

  ## Function invocation

  Elixir follows the same rule as Erlang when it comes to
  function invocations. Calling a function is the same thing
  as "invoking at atom". That said, one could write:

      defmodule Foo do
        def bar do
          :sum.(1, 2)
        end

        def sum(a, b), do: a + b
      end

  In the example above, sum is invoked by invoking an atom
  passing arguments 1 and 2. Since this syntax is a bit verbose,
  Elixir also support the more conventional:

      sum(1, 2)

  Invoking a function in another module is equally easy:

      Foo.sum(1, 2) #=> 3

  ## Dynamic function definition

  In macros it may be convenient to dynamically generate a function.
  This can be done by explicitly passing the function name, arguments
  and guard:

      name   = :some_function
      args   = quote(do: [first_arg, second_arg])
      guards = quote(do: is_list(first_arg))

      def name, args, guards do
        # ...
      end

  Notice that as arguments needs to be evaluated, they need to be
  properly quoted.
  """
  defmacro def(name, do: contents)

  @doc """
  Defines a function that is private. Private functions
  can only be accessible from the same module it is defined.

  Check `def/2` for more information

  ## Examples

      defmodule Foo do
        def bar do
          sum(1, 2)
        end

        defp sum(a, b), do: a + b
      end

  In the example above, `sum` is private and accessing it
  through `Foo.sum` will raise an error.
  """
  defmacro defp(name, do: contents)

  @doc """
  Define a record given by name and values.

  ## Examples

      defrecord FileInfo, atime: nil, mtime: nil

  The line above will define a module named `FileInfo` which
  contains a function named `new` that returns a new record
  and other functions to read and set the values in the
  record. Therefore, we can do:

      file_info = FileInfo.new(atime: now())
      file_info.atime         #=> Returns the value of atime
      file_info.atime(now())  #=> Updates the value of atime

  Internally, a record is simply a tuple where the first element is
  always the record module name. This can be noticed if we print
  the record:

      IO.puts FileInfo.new
      { ::FileInfo, nil, nil }

  ## Default based functions

  Depending on the default value, Elixir will define helpers to interact
  with the record. For example, ExUnit defines a record which keeps
  track of how many tests were executed and the failures that happened
  The record definition is similar to:

      defrecord Config, counter: 0, failures: []

  Since `counter` is an integer, Elixir automatically defines a helper
  named `increment_counter` that will increase the counter value:

      Config.new.increment_counter.counter #=> 1

  `increment_counter` also accepts a number of increment as argument:

      Config.new.increment_counter(10).counter #=> 10

  Besides, if the default is a list, Elixir will define three helpers:

  * `merge_field` - Receives an orddict and merge it into the current value;
  * `prepend_field` - Receives another list and prepend its values

  """
  defmacro defrecord(name, values, opts // [], do_block // []) do
    Record.defrecord(name, values, Orddict.merge(opts, do_block))
  end

  @doc """
  Defines an exception. It follows exactly the same API as record.
  The defined record must implement `message/1` as API, otherwise
  an error is raised. Check exception.ex for examples.
  """
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

  @doc """
  Check if the given structure is an exception.

  ## Examples

      is_exception(Error.new) #=> true
      is_exception(1)         #=> false

  """
  defmacro is_exception(thing) do
    quote do
      in_guard do
        is_tuple(unquote(thing)) and element(2, unquote(thing)) == __EXCEPTION__
      else:
        result = unquote(thing)
        is_tuple(result) and element(2, result) == __EXCEPTION__
      end
    end
  end

  @doc """
  Defines the current module as a protocol and specifies the API
  that should be implemented.

  ## Examples

  In Elixir, only `false` and `nil` are considered falsy values.
  Everything else evaluates to true in `if` clauses. Depending
  on the application, it may be important to specify a `blank?`
  protocol that returns a boolean for other data types that should
  be considered `blank?`. For instance, an empty list or an empty
  binary could be considered blanks.

  We could implement this protocol as follow:

      defprotocol Blank, [blank?(data)]

  Now that the protocol is defined, we can implement it. We need
  to implement the protocol for each Elixir type. For example:

      # Numbers are never blank
      defimpl Blank, for: Number do
        def blank?(number), do: false
      end

      # Just empty list is blank
      defimpl Blank, for: List do
        def blank?([]), do: true
        def blank?(_),  do: false
      end

      # Just the atoms false and nil are blank
      defimpl Blank, for: Atom do
        def blank?(false), do: true
        def blank?(nil),   do: true
        def blank?(_),     do: false
      end

  And we would have to define the implementation for all types.
  The types available are:

  * Record
  * Tuple
  * Atom
  * List
  * BitString
  * Number
  * Function
  * PID
  * Port
  * Reference
  * Any

  ## Selecting implementations

  Implementing the protocol for all default 9 types can be cumbersome.
  Even more if you consider that Number, Function, PID, Port and
  Reference are never going to be blank. For this reason, Elixir
  allows you to point out that you are going to implement the protocols
  just for some types, as follows:

      defprotocol Blank, [blank?(data)], only: [Atom, Tuple, List, BitString, Any]

  And for all other types, Elixir will now dispatch to Any. That said,
  the default behavior could be implemented as:

      defimpl Blank, for: Any do
        def blank?(_), do: false
      end

  Now, all data types that we have not specified will be automatically
  considered non blank.

  ## Protocols + Records

  The real benefit of protocols comes when mixed with records. For instance,
  one may implement a custom dictionary as a Red-Black tree and this
  dictionary should also be considered as blank in case it has no items.
  That said, he just needs to implement the protocol for this dictionary:

      defimpl Blank, for: RedBlack::Dict do
        def blank?(dict), do: RedBlack.empty?(dict)
      end

  In the example above, we have implemented `blank?` for the custom
  dictionary that simply delegates to `RedBlack.empty?`.
  """
  defmacro defprotocol(name, args, opts // []) do
    Protocol.defprotocol(name, args, opts)
  end

  @doc """
  Defines an implementation for the given protocol. See
  `defprotocol/2` for examples.
  """
  defmacro defimpl(name, [for: for], [do: block]) do
    Protocol.defimpl(name, [for: for], [do: block])
  end

  @doc """
  Defines the given functions in the current module that will
  delegate to the given `target`. Functions defined with defdelegate
  are public and should be purposedly allowed to be invoked from
  external. If you find yourself wishing to define a delegation
  as private, you should likely use import instead.

  Delegation only works with functions, delegating to macros
  is not supported.

  ## Examples

      defmodule MyList do
        defdelegate [reverse: 1], to: Erlang.lists
      end

      MyList.reverse([1,2,3])
      #=> [3,2,1]

  """
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

  @doc """
  Forwarding is a mechanism that allows a library or framework
  developer to provide a default implementation for some function.
  If the target module defines the forwarding function, `defforward`
  works as noop. However if not, a default implementation that
  forwards to the given module will be added.

  ## Examples

  First, let's define a custom MyLibrary code that will invoke
  `defforward` once used:

      defmodule MyLibrary do
        defmacro __using__(module, _) do
          quote do
            defforward [handle_failure: 1], to: MyLibrary
          end
        end

        # Default behavior for handling failure.
        # The callback function always receive the target
        # module and a set of callbacks as extra arguments.
        def handle_failure(_module, _callbacks, arg) do
          raise inspect(arg)
        end
      end

  Now, a module using `MyLibrary` can either use the default
  implementation for failure:

      defmodule MyModule do
        use MyLibrary
      end

  Or define its own:

      defmodule MyModule do
        use MyLibrary

        def handle_failure(arg) do
          raise "error was: #{inspect arg}"
        end
      end

  We call `MyModule.handle_failure/1` the **forwarding** function.

  ## defforward x defdelegate x import

  `defforward` is a callback mechanism used by library developers to
  provide a default behavior for user code. Since the default function
  receives the target module and some callbacks as argument, the default
  function is usually created with the defforward mechanism in mind.

  `defdelegate` however is simply a public API concern. For example, a
  developer providing his own helpers for handling lists called `MyList`
  may desire to delegate few functions to the original `List` module for
  convenience:

      defmodule MyList do
        defdelegate [reverse: 1], to: List
        # Other MyList related functions
      end

  Finally, `import` is an implementation concern. A developer or a library
  use import when they want to invoke functions without referencing their
  module every time. An imported function is never available from outside.

  ## Using super

  In some cases, a forwarding function specified by a developer may
  actually want to call the callback function. In such cases, to avoid
  coupling both functions, a developer can use the `super` macro:

      defmodule MyModule do
        use MyLibrary

        def handle_failure(arg) do
          super "error was: #{inspect arg}"
        end
      end

  `super` will automatically be replaced by a call to the callback function.

  Calling `super` without arguments implicitly passes the received arguments
  to the callback function:

      defmodule MyModule do
        use MyLibrary

        def handle_failure(arg) do
          IO.puts :standard_error, "Error: #{inspect arg}"
          super
        end
      end

  ## Forward chains

  It may happen that two different modules are defining a forwarding
  to the same function as a mechanism to decorate behavior. In this case,
  calling super should trigger all callback functions and not the immediate
  parent callback. Consider the `MyLibrary` example above:

      defmodule MyLibrary do
        defmacro __using__(module, _) do
          quote do
            defforward [handle_failure: 1], to: MyLibrary
          end
        end

        def handle_failure(_module, _callbacks, arg) do
          raise inspect(arg)
        end
      end

  Now let's assume we have a new module that is intended to decorate
  the handle failure to add logging:

      defmodule MyLibrary::Logging do
        defmacro __using__(module, _) do
          quote do
            defforward [handle_failure: 1], to: MyLibrary::Logging
          end
        end

        def handle_failure(module, callbacks, arg) do
          IO.puts :standard_error, "Error: #{inspect arg}"
          MyLibrary.handle_failure(module, callbacks, arg)
        end
      end

  And our module can now use both of them:

      defmodule MyModule do
        use MyLibrary
        use MyLibrary::Logging
      end

  The example above is bad because `MyLibrary::Logging` is actually
  hardcoding a call to `MyLibrary.handle_failure` ignoring the fact
  that other forwardings could be made between `MyLibrary` and itself.
  For this reason we have the `callbacks` arguments.

  The callbacks argument tell the forwarded function which is the parent
  module to invoke. The proper way to rewrite `handle_failure` for logging
  would be as follow:

      def handle_failure(module, [h|t], arg) do
        IO.puts :standard_error, "Error: #{inspect arg}"
        h.handle_failure(module, t, arg)
      end

  Since this pattern repeats many times, Elixir provides a `defcallback`
  macro that allows developers to write callback functions without worrying
  about callbacks and with proper super semantics. Using `defcallback`
  our `MyLibrary::Logging.handle_failure` could be rewritten as:

      defcallback handle_failure(arg) do
        IO.puts :standard_error, "Error: #{inspect arg} on #{__TARGET__}"
        super
      end

  Notice that now we don't need to explicitly receive the module and
  callbacks as arguments. Elixir will automatically wrap it for you
  and make the target module available under the variable `__TARGET__`.
  You can also implicitly invoke the callbacks by calling `super`.

  In general, it is recommended to use defcallback to define callbacks.

  ## Defining private forwardings

  `defforward` accepts another option called :via that allows you
  to define the visibility of the forwarding function.

      defforward [handle_failure: 1], to: ::MyLibrary, via: :defp

  Since the forwarded function must always be called from external,
  it needs to be necessarily a public function.
  """
  defmacro defforward(tuples, options) do
    quote do
      Module.add_forwarding __MODULE__, unquote(tuples), unquote(options)
    end
  end

  @doc """
  Defines a function to be forwarded to with defforward.
  See defforward/0 for more information.
  """
  defmacro defcallback(signature, contents)

  @doc """
  `use` is a simple mechanism for extending the current module with the
  given module.

  == Examples

      defmodule AssertionTest do
        use ExUnit::Case

        def test_always_pass do
          true = true
        end
      end

  By calling `use`, a hook called `__using__` will be invoked in
  `ExUnit::Case` which will then do the proper setup. In other words,
  `use` is simply a translation to:

      defmodule AssertionTest do
        require ExUnit::Case, as: false
        ExUnit::Case.__using__(::AssertionTest)

        def test_always_pass do
          true = true
        end
      end

  """
  defmacro use(module, args // [])

  @doc """
  Inspect the given arguments according to the String::Inspect protocol.

  ## Examples

      inspect(:foo)
      #=> ":foo"

  """
  defmacro inspect(arg) do
    quote do: ::String::Inspect.inspect(unquote(arg))
  end

  @doc """
  Convert the argument to a string according to the String::Inspect protocol.
  This is the function invoked when there is string interpolation.

  ## Examples

      to_binary(:foo)
      #=> "foo"

  """
  defmacro to_binary(arg) do
    quote do: ::String::Chars.to_binary(unquote(arg))
  end

  @doc """
  Convert the argument to a list according to the List::Chars protocol.

  ## Examples

      to_char_list(:foo)
      #=> 'foo'

  """
  defmacro to_char_list(arg) do
    quote do: ::List::Chars.to_char_list(unquote(arg))
  end

  @doc """
  Define elem to get Tuple element according to Elixir conventions.
  We need to implement it as a macro to it can be used in guards.

  ## Example

     tuple = { :foo, :bar, 3 }
     elem(tuple, 1) #=> :foo

  """
  defmacro elem(tuple, index) do
    quote do: element(unquote(index), unquote(tuple))
  end

  @doc """
  Define setelem to set Tuple element according to Elixir conventions.
  We need to implement it as a macro to it can be used in guards.

  ## Example

     tuple = { :foo, :bar, 3 }
     setelem(tuple, 1, :baz) #=> { :baz, :bar, 3 }

  """
  defmacro setelem(tuple, index, value) do
    quote do: setelement(unquote(index), unquote(tuple), unquote(value))
  end

  @doc """
  Provides an integer division macro according to Erlang semantics.
  Raises an error if one of the arguments is not an integer.
  Can be used in guard tests.

  ## Examples

      5 div 2 #=> 2

  """
  defmacro div(left, right) do
    quote do: __OP__ :div, unquote(left), unquote(right)
  end

  @doc """
  Provides an integer remainder macro according to Erlang semantics.
  Raises an error if one of the arguments is not an integer.
  Can be used in guard tests.

  ## Examples

      5 rem 2 #=> 1

  """
  defmacro rem(left, right) do
    quote do: __OP__ :rem, unquote(left), unquote(right)
  end

  @doc """
  A convenient macro that checks if the right side matches
  the left side. The left side is allowed to be a match pattern.

  ## Examples

      match?(1, 1) #=> true
      match?(1, 2) #=> false
      match?({1,_}, {1,2}) #=> true

  Match can also be used to filter or fina a value in an enumerable:

      list = [{:a,1},{:b,2},{:a,3}]
      Enum.filter list, match?({:a, _}, _)

  Guard clauses can also be given to the match:

      list = [{:a,1},{:b,2},{:a,3}]
      Enum.filter list, match?({:a, x } when x < 2, &1)

  """
  defmacro match?({ :_, _, atom }, _right) when is_atom(atom) do
    # Special case underscore since it always matches.
    true
  end

  defmacro match?(left, right) do
    quote do
      case unquote(right) do
      match: unquote(left)
        true
      else:
        false
      end
    end
  end

  @doc """
  Matches the given condition against the match clauses.

  ## Examples

      case thing do
      match: { :selector, i, value } when is_integer(i)
        value
      match: value
        value
      end

  In the example above, we compare `thing` with each given
  match clause and execute the first one that matches. If no
  clause matches, an error is raised.

  Since Elixir variables can be assigned more than once, variables
  in a match clause will always be assigned instead of matching with
  its previous values. For example:

      i = 1
      case 10 do
      match: i
        i * 2
      end

  The example above will return 20, because `i` is assgined to 10
  and then multiplied by 2. If you desire to match the value of `i`
  against the given condition, you need to use the `^` operator:

      i = 1
      case 10 do
      match: ^i
        i * 2
      end

  The example above will actually fail because 10 does not match 1.

  Finally, `case` accepts an `else:` branch as a fallback if none
  of the clauses match:

      case thing do
      match: { :selector, i, value } when is_integer(i)
        value
      else:
        thing
      end

  """
  defmacro case(condition, blocks)

  @doc """
  Execute the given expressions and catch any error, exit
  or throw that may have happened.

  ## Examples

      try do
        do_something_that_may_fail(some_arg)
      rescue: ArgumentError
        IO.puts "Invalid argument given"
      catch: value
        IO.puts "caught #{value}"
      after:
        IO.puts "This is printed regardless if it failed or succeed"
      end

  The rescue clause is used to handle errors, while the catch clause
  can be used to catch throw values. The catch clause accepts the same
  pattern matching rules as match.

  Note that calls inside `try` are not tail recursive since the VM
  needs to keep the stacktrace in case an exception happens.

  ## Rescue clauses

  While `catch` is simply a pattern matching mechanism, rescue
  provides a higher abstraction around exceptions that allows
  one to rescue an exception by its name and not by its internal
  contents. All the following formats are valid rescue expressions:

      try do
        UndefinedModule.undefined_function
      rescue: UndefinedFunctionError
      end

      try do
        UndefinedModule.undefined_function
      rescue: [UndefinedFunctionError]
      end

      # rescue and assign to x
      try do
        UndefinedModule.undefined_function
      rescue: x in [UndefinedFunctionError]
      end

      # rescue all and assign to x
      try do
        UndefinedModule.undefined_function
      rescue: x in _
      end

  ## Variable visibility

  Since an expression inside `try` may not have been evaluted
  due to an exception, any variable created inside `try` cannot
  be accessed externaly.
  For instance:

      try do
        x = 1
        do_something_that_may_fail(same_arg)
        :ok
      catch: _, _
        :failed
      end

      x #=> Cannot access `x`

  In the example above, `x` cannot be accessed since it was defined
  inside the `try` clause.

  ## Catching exits and errors

  The catch clause works exactly the same as in Erlang. Therefore,
  one can also handle exits/errors coming from Erlang as below:

      try do
        exit(1)
      catch: :exit, 1
        IO.puts "Exited with 1"
      end

      try do
        error(:sample)
      catch: :error, :sample
        IO.puts "sample error"
      end

  Although the second form should be avoided in favor of raise/rescue
  control mechanisms.
  """
  defmacro try(args)

  @doc """
  The current process will hang until it receives a message
  from other processes that matches the given clauses.

  ## Examples

      receive
      match: { :selector, i, value } when is_integer(i)
        value
      match: value when is_atom(value)
        value
      else:
        IO.puts :standard_error, "Unexpected message received"
      end

  The match clauses above follows the same rules as `case/2`.

  An optional after clause can be given in case the message was not
  received after the specified period of time:

      receive
      match: { :selector, i, value } when is_integer(i)
        value
      match: value when is_atom(value)
        value
      else:
        IO.puts :standard_error, "Unexpected message received"
      after: 5000
        IO.puts :standard_error, "No message in 5 seconds"
      end

  The `after` clause can be specified even if there are no match clauses.
  There are two special cases for the timout value given to after:

  * `:infinity` - The process should wait indefinitely for a matching
  message, this is the same as not using a timeout.

  * 0 - if there is no matching message in the mailbox, the timeout
  will occur immediately.
  """
  defmacro receive(args)

  @doc """
  Invokes the given `fun` with the array of arguments `args`.

  ## Examples

      apply fn(x) -> x * 2 end, [2]
      #=> 4

  """
  def apply(fun, args)

  @doc """
  Invokes the given `fun` from `module` with the array of arguments `args`.

  ## Examples

      apply List, reverse, [[1,2,3]]
      #=> [3,2,1]

  """
  def apply(module, fun, args)

  @doc """
  Provides an `if` macro. The macro expects the first argument to
  be a condition and the rest are key-value arguments.

  ## One-liner examples

      if(foo, do: bar)

  In the example above, bar will be returned if foo evalutes to
  true (i.e. it is not false nor nil). Otherwise, nil will be returned.

  An else option can be given to specify the opposite:

      if(foo, do: bar, else: bar)

  ## Key-value blocks examples

  When several expressions must be passed to if, the most appropriate
  form is thorugh key-value blocks. The first example above would then
  be translated to:

      if foo do
        bar
      end

  Notice that do/end becomes delimiters. The value given between
  do/end becomes the expression given to as `do:`. The second example
  would then translate do:

      if foo do
        bar
      else:
        baz
      end

  Notice that extra keys follows the regular `else:` form. You can also
  add extra `elsif:` clauses:


      if foo do
        bar
      elsif: some_condition
        bar + baz
      else:
        baz
      end

  """
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

  @doc """
  Provides a unless macro that executes the expression
  unless a value evalutes to true. Check `if` for examples
  and documentation.
  """
  defmacro unless(clause, options) do
    quote do: if(!unquote(clause), unquote(options))
  end

  @doc """
  Concatenates two binaries.

  ## Examples

      "foo" <> "bar" #=> "foobar"

  The `<>` operator can also be used in guard clauses as
  long as the first part is a literal binary:

      "foo" <> x = "foobar"
      x #=> "bar"

  """
  defmacro :<>.(left, right) do
    concats = extract_concatenations({ :<>, 0, [left, right] })
    quote do: << unquote_splicing(concats) >>
  end

  @doc """
  Provides a short-circuit operator that executes the second
  expression only if the first one evalutes to true (i.e. it is
  not nil nor false). Returns the first expression otherwise.

  ## Examples

      true && true         #=> true
      nil && true          #=> nil
      true && 1            #=> 1
      false && error(:bad) #=> false

  Notice that, differently from Erlang `and` and `and` operators,
  this operator accepts any expression as arguments, not only booleans.
  Unfortunately cannot be used in macros.
  """
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

  @doc """
  Provides a short-circuit operator that executes the second
  expression only if the first one does not evalute to true (i.e. it
  is not nil nor false). Returns the first expression otherwise.

  ## Examples

      false || false       #=> false
      nil || true          #=> true
      false || 1           #=> 1
      true || error(:bad)  #=> true

  Notice that, differently from Erlang `or` and `or` operators,
  this operator accepts any expression as arguments, not only booleans.
  Unfortunately cannot be used in macros.
  """
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

  @doc """
  Implements the unary operator ! as a macro. It receives any
  argument and returns true if it is false or nil. Returns false
  otherwise.

  ## Examples

    !1        #=> false
    ![1,2,3]  #=> false
    !false    #=> true
    !nil      #=> true

  """

  # Optimizes !! to avoid generating case twice.
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

  @doc """
  Raises an error.

  If the argument is a binary, it raises RuntimeError with the message.
  If an atom, it assumes the atom represents a record and instantiates
  a new record and raises it.
  If an exception, it simply re-raises the exception

  ## Examples

      raise ArgumentError

      raise "Given values do not match"

      try do
        1 + :foo
      rescue: x in [BadargError]
        IO.puts "that was expected"
        raise x
      end

  """
  def raise(msg) when is_binary(msg) do
    :erlang.error RuntimeError.new(message: msg)
  end

  def raise(atom) when is_atom(atom) do
    :erlang.error atom.new
  end

  def raise(exception) when is_tuple(exception) and element(2, exception) == __EXCEPTION__ do
    :erlang.error exception
  end

  @doc """
  Receives a reference for an exception and
  instantiates a new exception with the given args.

  ## Examples

      raise ArgumentError, message: "Expected a protocol"


  """
  def raise(atom, args) when is_atom(atom) do
    :erlang.error atom.new(args)
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
