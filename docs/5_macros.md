## Meta-programming in Elixir

Elixir is an homoiconic language. Any Elixir program can be represented using its own data structures. This section describes the Elixir language specification for such data structures.

The building block of Elixir homoiconicity is a tuple with three elements, for example:

    { :sum, 1, [1, 2, 3] }

The tuple above represents a function call to sum passing 1, 2 and 3 as arguments. The tuple elements are:

* The first element of the tuple is always an atom or another tuple in the same representation;
* The second element of the tuple is always an integer representing the line number;
* The third element of the tuple are the arguments for the function call. The third argument may also be an atom (nil or quoted), meaning that it may be a variable call.

You can get the representation of any expression by using the quote macro:

    quote { sum(1, 2, 3) }
    #=> { :sum, 0, [1, 2, 3] }

Besides the tuple, Elixir has a few literals. Literals are data types that when quoted return themselves. They are:

    :sum         #=> Atoms
    1            #=> Integers
    2.0          #=> Floats
    [1,2]        #=> Lists
    "binaries"   #=> Binaries
    {key, value} #=> Key-value pairs (i.e. a tuple with two elements)

With those basic structures in mind, we are ready to define our own macro.

### Defining your own macro

A macro can be define using `defmacro`. For instance, we can define a macro called unless which works the same as Ruby's unless in just few lines of code:

    defmacro unless(clause, options) do
      quote { if(!unquote(clause), unquote(options)) }
    end

In the example above, unless will be called receiving two arguments: a `clause` and `options`. However, note that unless won't receive its values, but its expressions. For example, if one calls:

    unless 2 + 2 == 5, do: call_function()

Our `unless` macro will receive the following:

    unless({:==, 1, [{:+, 1, [2, 2]}, 5]}, { :call_function, 1, [] })

After being invoked, our `unless` macro will call `quote`, to return a tree representation of the `if` clause. This means we are transforming our `unless` in a `if`!

However, there is a common mistake when quoting expressions which is that developers usually forget to `unquote` the proper expression. In order to understand what `unquote` does, let's simply remove it:

    defmacro unless(clause, options) do
      quote { if(!clause, options) }
    end

When called as `unless 2 + 2 == 5, do: call_function()`, our `unless` would then return:

    { :if, 0, [{ :!, 0, [{:custom, 0, quoted}]}, do: {:options, 0, quoted}] }

Notice that the tree structure returned by unless is trying to access `custom` and `options` as variables instead of using the `2 + 2 == 5` and `call_function()` expressions we passed as parameters. This is because we forgot to unquote! If we add unquote back:

    defmacro unless(clause, options) do
      quote { if(!unquote(clause), unquote(options)) }
    end

It would return:

    { :if, 0, [{ :!, 0, [{:==, 1, [{:+, 1, [2, 2]}, 5]}]},
      do: { :call_function, 1, [] }] }

In other words, unquote is a mechanism to inject expressions into the tree being quoted and is essential to the meta-programming mechanism. Elixir also provides `unquote_splicing`.

### Locals and macros

In order to support recursion, macros cannot be called locally. For example, one cannot write:

    defmodule MyMacros
      defmacro delegate([h|t], to: target) do
        # ...
      end

      # Call the macro delegate just defined above
      delegate [values: 1], to: List
    end

In order to access the macro, it needs to be defined in an outer module:

    defmodule MyMacros::Support
      defmacro delegate([h|t], to: target) do
        # ...
      end
    end

    defmodule MyMacros
      require MyMacros::Support, import: true
      delegate [values: 1], to: List
    end
