# 6 Other topics

# 6.1 Partial application

Elixir supports partial application. Partial application is also sometimes called cut. Remembering from early chapters, the underscore `_` in Elixir can be assigned to but it never stores the given contents:

    iex> _ = 1
    1
    iex> _
    ** error {:unbound_var,:_}

When used inside function calls, the underscore means that we don't have the argument and that it should then be partially applied. For example, let's suppose we have a list of strings and we want to calculate the size for each them. We could do it as follow:

    iex> list = ["foo", "bar", "baz"]
    ["foo","bar","baz"]
    iex> Enum.map list, fn(x) { length x }
    [3,3,3]

However, with partial application, we could also do:

    iex> Enum.map list, length(_)
    [3,3,3]

In the example above, we have invoked the function `length` passing an underscore as argument meaning we still don't have the argument for `length`, so Elixir in this case automatically generates a function that is passed forward.

Since operators are also function calls they can also be partially applied:

    iex> Enum.map [1,2,3], _ * 2
    [2,4,6]

The only exception are data structures that cannot be partially applied:

    iex> { 1, _ }
    ** error {:unbound_var,:_}
    iex> [ 1 | _ ]
    ** error {:unbound_var,:_}

# 6.2 Comprehensions


# 6.3 Native compilation

Elixir can compile to native code using the Hipe compiler. All you need to do is to export the following before running your code:

    export ERL_COMPILER_OPTIONS=native

[Chapter 5: Macros](https://github.com/josevalim/elixir/blob/master/docs/5_macros.md) | [Index](https://github.com/josevalim/elixir/blob/master/docs/0_index.md)
