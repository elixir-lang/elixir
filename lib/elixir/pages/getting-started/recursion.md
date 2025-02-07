<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Recursion

Elixir does not provide loop constructs. Instead we leverage recursion and high-level functions for working with collections. This chapter will explore the former.

## Loops through recursion

Due to immutability, loops in Elixir (as in any functional programming language) are written differently from imperative languages. For example, in an imperative language like C, one would write:

```c
for(i = 0; i < sizeof(array); i++) {
  array[i] = array[i] * 2;
}
```

In the example above, we are mutating both the array and the variable `i`. However, data structures in Elixir are immutable. For this reason, functional languages rely on recursion: a function is called recursively until a condition is reached that stops the recursive action from continuing. No data is mutated in this process. Consider the example below that prints a string an arbitrary number of times:

```elixir
defmodule Recursion do
  def print_multiple_times(msg, n) when n > 0 do
    IO.puts(msg)
    print_multiple_times(msg, n - 1)
  end

  def print_multiple_times(_msg, 0) do
    :ok
  end
end

Recursion.print_multiple_times("Hello!", 3)
# Hello!
# Hello!
# Hello!
:ok
```

Similar to `case`, a function may have many clauses. A particular clause is executed when the arguments passed to the function match the clause's argument patterns and its guards evaluate to `true`.

When `print_multiple_times/2` is initially called in the example above, the argument `n` is equal to `3`.

The first clause has a guard which says "use this definition if and only if `n` is more than `0`". Since this is the case, it prints the `msg` and then calls itself passing `n - 1` (`2`) as the second argument.

Now we execute the same function again, starting from the first clause. Given the second argument, `n`, is still more than 0, we print the message and call ourselves once more, now with the second argument set to `1`. Then we print the message one last time and call `print_multiple_times("Hello!", 0)`, starting from the top once again.

When the second argument is zero, the guard `n > 0` evaluates to false, and the first function clause won't execute. Elixir then proceeds to try the next function clause, which explicitly matches on the case where `n` is `0`. This clause, also known as the termination clause, ignores the message argument by assigning it to the `_msg` variable and returns the atom `:ok`.

Finally, if you pass an argument that does not match any clause, Elixir raises a `FunctionClauseError`:

```elixir
iex> Recursion.print_multiple_times("Hello!", -1)
** (FunctionClauseError) no function clause matching in Recursion.print_multiple_times/2

    The following arguments were given to Recursion.print_multiple_times/2:

        # 1
        "Hello!"

        # 2
        -1

    iex:1: Recursion.print_multiple_times/2
```

## Reduce and map algorithms

Let's now see how we can use the power of recursion to sum a list of numbers:

```elixir
defmodule Math do
  def sum_list([head | tail], accumulator) do
    sum_list(tail, head + accumulator)
  end

  def sum_list([], accumulator) do
    accumulator
  end
end

IO.puts Math.sum_list([1, 2, 3], 0) #=> 6
```

We invoke `sum_list` with the list `[1, 2, 3]` and the initial value `0` as arguments. We will try each clause until we find one that matches according to the pattern matching rules. In this case, the list `[1, 2, 3]` matches against `[head | tail]` which binds `head` to `1` and `tail` to `[2, 3]`; `accumulator` is set to `0`.

Then, we add the head of the list to the accumulator `head + accumulator` and call `sum_list` again, recursively, passing the tail of the list as its first argument. The tail will once again match `[head | tail]` until the list is empty, as seen below:

```elixir
sum_list([1, 2, 3], 0)
sum_list([2, 3], 1)
sum_list([3], 3)
sum_list([], 6)
```

When the list is empty, it will match the final clause which returns the final result of `6`.

The process of taking a list and _reducing_ it down to one value is known as a _reduce algorithm_ and is central to functional programming.

What if we instead want to double all of the values in our list?

```elixir
defmodule Math do
  def double_each([head | tail]) do
    [head * 2 | double_each(tail)]
  end

  def double_each([]) do
    []
  end
end

Math.double_each([1, 2, 3]) #=> [2, 4, 6]
```

Here we have used recursion to traverse a list, doubling each element and returning a new list. The process of taking a list and _mapping_ over it is known as a _map algorithm_.

Recursion and [tail call](https://en.wikipedia.org/wiki/Tail_call) optimization are an important part of Elixir and are commonly used to create loops. However, when programming in Elixir you will rarely use recursion as above to manipulate lists.

The `Enum` module, which we're going to see in the next chapter already provides many conveniences for working with lists. For instance, the examples above could be written as:

```elixir
iex> Enum.reduce([1, 2, 3], 0, fn x, acc -> x + acc end)
6
iex> Enum.map([1, 2, 3], fn x -> x * 2 end)
[2, 4, 6]
```

Or, using the [capture syntax](`Kernel.SpecialForms.&/1`):

```elixir
iex> Enum.reduce([1, 2, 3], 0, &+/2)
6
iex> Enum.map([1, 2, 3], &(&1 * 2))
[2, 4, 6]
```

Let's take a deeper look at `Enumerable` and, while we're at it, its lazy counterpart, `Stream`.
