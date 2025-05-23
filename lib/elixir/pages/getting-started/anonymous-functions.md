<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Anonymous functions

Anonymous functions allow us to store and pass executable code around as if it was an integer or a string. Let's learn more.

## Identifying functions and documentation

Before we move on to discuss anonymous functions, let's talk about how Elixir identifies named functions.

Functions in Elixir are identified by both their name and their arity. The arity of a function describes the number of arguments that the function takes. From this point on we will use both the function name and its arity to describe functions throughout the documentation. `trunc/1` identifies the function which is named `trunc` and takes `1` argument, whereas `trunc/2` identifies a different (nonexistent) function with the same name but with an arity of `2`.

We can also use this syntax to access documentation. The Elixir shell defines the [`h`](`IEx.Helpers.h/1`) function, which you can use to access documentation for any function. For example, typing `h trunc/1` is going to print the documentation for the `trunc/1` function:

```elixir
iex> h trunc/1
                             def trunc(number)

Returns the integer part of number.
```

`h trunc/1` works because it is defined in the `Kernel` module. All functions in the `Kernel` module are automatically imported into our namespace. Most often you will also include the module name when looking up the documentation for a given function:

```elixir
iex> h Kernel.trunc/1
                             def trunc(number)

Returns the integer part of number.
```

You can use the module+function identifiers to lookup documentation for anything, including operators (try `h Kernel.+/2`). Invoking [`h`](`IEx.Helpers.h/1`) without arguments displays the documentation for `IEx.Helpers`, which is where `h` and other functionalities are defined.

## Defining anonymous functions

Anonymous functions in Elixir are delimited by the keywords `fn` and `end`:

```elixir
iex> add = fn a, b -> a + b end
#Function<12.71889879/2 in :erl_eval.expr/5>
```

In the example above, we defined an anonymous function that receives two arguments, `a` and `b`, and returns the result of `a + b`. The arguments are always on the left-hand side of `->` and the code to be executed on the right-hand side. The anonymous function is stored in the variable `add`. You can see it returns a value represented by `#Function<...>`. While its representation is opaque, the `:erl_eval.expr` bit tells us the function was defined in the shell (during evaluation).

We can invoke anonymous functions by passing arguments to it, using a dot (`.`) between the variable and the opening parenthesis:

```elixir
iex> add.(1, 2)
3
```

The dot makes it clear when you are calling an anonymous function, stored in the variable `add`, opposed to a function named `add/2`. For example, if you have an anonymous function stored in the variable `is_atom`, there is no ambiguity between `is_atom.(:foo)` and `is_atom(:foo)`. If both used the same `is_atom(:foo)` syntax, the only way to know the actual behavior of `is_atom(:foo)` would be by scanning all code thus far for a possible definition of the `is_atom` variable. This scanning hurts maintainability as it requires developers to track additional context in their head when reading and writing code.

Anonymous functions in Elixir are also identified by the number of arguments they receive. We can check if a value is function using `is_function/1` and also check its arity by using `is_function/2`:

```elixir
iex> is_function(add)
true
# check if add is a function that expects exactly 2 arguments
iex> is_function(add, 2)
true
# check if add is a function that expects exactly 1 argument
iex> is_function(add, 1)
false
```

## Closures

Anonymous functions can also access variables that are in scope when the function is defined. This is typically referred to as closures, as they close over their scope. Let's define a new anonymous function that uses the `add` anonymous function we have previously defined:

```elixir
iex> double = fn a -> add.(a, a) end
#Function<6.71889879/1 in :erl_eval.expr/5>
iex> double.(2)
4
```

A variable assigned inside a function does not affect its surrounding environment:

```elixir
iex> x = 42
42
iex> (fn -> x = 0 end).()
0
iex> x
42
```

## Clauses and guards

Similar to `case/2`, we can pattern match on the arguments of anonymous functions as well as define multiple clauses and guards:

```elixir
iex> f = fn
...>   x, y when x > 0 -> x + y
...>   x, y -> x * y
...> end
#Function<12.71889879/2 in :erl_eval.expr/5>
iex> f.(1, 3)
4
iex> f.(-1, 3)
-3
```

The number of arguments in each anonymous function clause needs to be the same, otherwise an error is raised.

```elixir
iex> f2 = fn
...>   x, y when x > 0 -> x + y
...>   x, y, z -> x * y + z
...> end
** (CompileError) iex:1: cannot mix clauses with different arities in anonymous functions
```

## The capture operator

Throughout this guide, we have been using the notation `name/arity` to refer to functions. It happens that this notation can actually be used to capture an existing function into a data-type we can pass around, similar to how anonymous functions behave.

```elixir
iex> fun = &is_atom/1
&:erlang.is_atom/1
iex> is_function(fun)
true
iex> fun.(:hello)
true
iex> fun.(123)
false
```

As you can see, once a function is captured, we can pass it as argument or invoke it using the anonymous function notation. The returned value above also hints we can capture functions defined in modules:

```elixir
iex> fun = &String.length/1
&String.length/1
iex> fun.("hello")
5
```

Since operators are functions in Elixir, you can also capture operators:

```elixir
iex> add = &+/2
&:erlang.+/2
iex> add.(1, 2)
3
```

The capture syntax can also be used as a shortcut for creating functions that wrap existing functions. For example, imagine you want to create an anonymous function that checks if a given function has arity 2. You could write it as:

```elixir
iex> is_arity_2 = fn fun -> is_function(fun, 2) end
#Function<8.71889879/1 in :erl_eval.expr/5>
iex> is_arity_2.(add)
true
```

But using the capture syntax, you can write it as:

```elixir
iex> is_arity_2 = &is_function(&1, 2)
#Function<8.71889879/1 in :erl_eval.expr/5>
iex> is_arity_2.(add)
true
```

The `&1` represents the first argument passed into the function. Therefore both `is_arity_2` anonymous functions defined above are equivalent.

Once again, given operators are function calls, the capture syntax shorthand also works with operators, or even string interpolation:

```elixir
iex> fun = &(&1 + 1)
#Function<6.71889879/1 in :erl_eval.expr/5>
iex> fun.(1)
2

iex> fun2 = &"Good #{&1}"
#Function<6.127694169/1 in :erl_eval.expr/5>
iex> fun2.("morning")
"Good morning"
```

`&(&1 + 1)` above is exactly the same as `fn x -> x + 1 end`. You can read more about the capture operator `&` in [its documentation](`&/1`).

Next let's revisit some of the data-types we learned in the past and dig deeper into how they work.
