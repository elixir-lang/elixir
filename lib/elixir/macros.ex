defmodule Elixir::Macros do
  # Define a record given by name and values. Example:
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
  # * `merge_field` - Receives an ordered dict and merge it into the current values;
  # * `prepend_field` - Receives another list and prepend its values
  # * `append_field` - Receives another list and append its values
  #
  defmacro defrecord(name, values, opts // []) do
    Record.defrecord(name, values, opts)
  end

  defmacro defprotocol(args, opts // []) do
    Protocol.defprotocol(args, opts)
  end

  defmacro defimpl(name, opts // []) do
    Protocol.defimpl(name, opts)
  end

  defmacro inspect(arg),   do: quote { ::Inspect.inspect(unquote(arg)) }
  defmacro stringify(arg), do: quote { ::Inspect.stringify(unquote(arg)) }

  # Checks if the given argument is_any structure.
  # Always returns true.
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