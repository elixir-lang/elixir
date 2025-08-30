<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Pattern matching

In this chapter, we will learn why the [`=`](`=/2`) operator in Elixir is called the match operator and how to use it to pattern match inside data structures. We will learn about the pin operator [`^`](`^/1`) used to access previously bound values.

## The match operator

We have used the [`=`](`=/2`) operator a couple times to assign variables in Elixir:

```elixir
iex> x = 1
1
iex> x
1
```

In Elixir, the [`=`](`=/2`) operator is actually called *the match operator*. Let's see why:

```elixir
iex> x = 1
1
iex> 1 = x
1
iex> 2 = x
** (MatchError) no match of right hand side value: 1
```

Notice that `1 = x` is a valid expression, and it matched because both the left and right side are equal to `1`. When the sides do not match, a `MatchError` is raised.

A variable can only be assigned on the left side of [`=`](`=/2`):

```elixir
iex> 1 = unknown
** (CompileError) iex:1: undefined variable "unknown"
```

## Pattern matching

The match operator is not only used to match against simple values, but it is also useful for destructuring more complex data types. For example, we can pattern match on tuples:

```elixir
iex> {a, b, c} = {:hello, "world", 42}
{:hello, "world", 42}
iex> a
:hello
iex> b
"world"
```

A pattern match error will occur if the sides can't be matched, for example if the tuples have different sizes:

```elixir
iex> {a, b, c} = {:hello, "world"}
** (MatchError) no match of right hand side value: {:hello, "world"}
```

And also when comparing different types, for example if matching a tuple on the left side with a list on the right side:

```elixir
iex> {a, b, c} = [:hello, "world", 42]
** (MatchError) no match of right hand side value: [:hello, "world", 42]
```

More interestingly, we can match on specific values. The example below asserts that the left side will only match the right side when the right side is a tuple that starts with the atom `:ok`:

```elixir
iex> {:ok, result} = {:ok, 13}
{:ok, 13}
iex> result
13

iex> {:ok, result} = {:error, :oops}
** (MatchError) no match of right hand side value: {:error, :oops}
```

We can pattern match on lists:

```elixir
iex> [a, b, c] = [1, 2, 3]
[1, 2, 3]
iex> a
1
```

A list also supports matching on its own head and tail:

```elixir
iex> [head | tail] = [1, 2, 3]
[1, 2, 3]
iex> head
1
iex> tail
[2, 3]
```

Similar to the [`hd`](`hd/1`) and [`tl`](`tl/1`) functions, we can't match an empty list with a head and tail pattern:

```elixir
iex> [head | tail] = []
** (MatchError) no match of right hand side value: []
```

The `[head | tail]` format is not only used on pattern matching but also for prepending items to a list:

```elixir
iex> list = [1, 2, 3]
[1, 2, 3]
iex> [0 | list]
[0, 1, 2, 3]
```

In some cases, you don't care about a particular value in a pattern. It is a common practice to bind those values to the underscore, `_`. For example, if only the head of the list matters to us, we can assign the tail to underscore:

```elixir
iex> [head | _] = [1, 2, 3]
[1, 2, 3]
iex> head
1
```

The variable `_` is special in that it can never be read from. Trying to read from it gives a compile error:

```elixir
iex> _
** (CompileError) iex:1: invalid use of _. "_" represents a value to be ignored in a pattern and cannot be used in expressions
```

If a variable is mentioned more than once in a pattern, all references must bind to the same value:

```elixir
iex> {x, x} = {1, 1}
{1, 1}
iex> {x, x} = {1, 2}
** (MatchError) no match of right hand side value: {1, 2}
```

Although pattern matching allows us to build powerful constructs, its usage is limited. For instance, you cannot make function calls on the left side of a match. The following example is invalid:

```elixir
iex> length([1, [2], 3]) = 3
** (CompileError) iex:1: cannot invoke remote function :erlang.length/1 inside match
```

Pattern matching allows developers to easily destructure data types such as tuples and lists. As we will see in the following chapters, it is one of the foundations of recursion in Elixir and applies to other types as well, like maps and binaries.

## The pin operator

Variables in Elixir can be rebound:

```elixir
iex> x = 1
1
iex> x = 2
2
```

However, there are times when we don't want variables to be rebound.

Use the pin operator [`^`](`^/1`) when you want to pattern match against a variable's *existing value* rather than rebinding the variable.

```elixir
iex> x = 1
1
iex> ^x = 2
** (MatchError) no match of right hand side value: 2
```

Because we have pinned `x` when it was bound to the value of `1`, it is equivalent to the following:

```elixir
iex> 1 = 2
** (MatchError) no match of right hand side value: 2
```

Notice that we even see the exact same error message.

We can use the pin operator inside other pattern matches, such as tuples or lists:

```elixir
iex> x = 1
1
iex> [^x, 2, 3] = [1, 2, 3]
[1, 2, 3]
iex> {y, ^x} = {2, 1}
{2, 1}
iex> y
2
iex> {y, ^x} = {2, 2}
** (MatchError) no match of right hand side value: {2, 2}
```

Because `x` was bound to the value of `1` when it was pinned, this last example could have been written as:

```elixir
iex> {y, 1} = {2, 2}
** (MatchError) no match of right hand side value: {2, 2}
```

This finishes our introduction to pattern matching. As we will see in the next chapter, pattern matching is very common in many language constructs and they can be further augmented with guards.
