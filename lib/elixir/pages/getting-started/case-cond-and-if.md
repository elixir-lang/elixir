<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# case, cond, and if

In this chapter, we will learn about the [`case`](`case/2`), [`cond`](`cond/1`), and [`if`](`if/2`) control flow structures.

## case

[`case`](`case/2`) allows us to compare a value against many patterns until we find a matching one:

```elixir
iex> case {1, 2, 3} do
...>   {4, 5, 6} ->
...>     "This clause won't match"
...>   {1, x, 3} ->
...>     "This clause will match and bind x to 2 in this clause"
...>   _ ->
...>     "This clause would match any value"
...> end
"This clause will match and bind x to 2 in this clause"
```

If you want to pattern match against an existing variable, you need to use the [`^`](`^/1`) operator:

```elixir
iex> x = 1
1
iex> case 10 do
...>   ^x -> "Won't match"
...>   _ -> "Will match"
...> end
"Will match"
```

Clauses also allow extra conditions to be specified via guards:

```elixir
iex> case {1, 2, 3} do
...>   {1, x, 3} when x > 0 ->
...>     "Will match"
...>   _ ->
...>     "Would match, if guard condition were not satisfied"
...> end
"Will match"
```

The first clause above will only match when `x` is positive.

Keep in mind errors in guards do not leak but simply make the guard fail:

```elixir
iex> hd(1)
** (ArgumentError) argument error
iex> case 1 do
...>   x when hd(x) -> "Won't match"
...>   x -> "Got #{x}"
...> end
"Got 1"
```

If none of the clauses match, an error is raised:

```elixir
iex> case :ok do
...>   :error -> "Won't match"
...> end
** (CaseClauseError) no case clause matching: :ok
```

The documentation for the `Kernel` module lists all available guards in its sidebar. You can also consult the complete [Patterns and Guards](../references/patterns-and-guards.md#guards) reference for in-depth documentation.

## if

[`case`](`case/2`) builds on pattern matching and guards to destructure and match on certain conditions. However, patterns and guards are limited only to certain expressions which are optimized by the compiler. In many situations, you need to write conditions that go beyond what can be expressed with [`case`](`case/2`). For those, [`if`](`if/2`) is a useful alternative:

```elixir
iex> if true do
...>   "This works!"
...> end
"This works!"
iex> if false do
...>   "This will never be seen"
...> end
nil
```

If the condition given to [`if`](`if/2`) returns `false` or `nil`, the body given between `do`-`end` is not executed and instead it returns `nil`.

[`if`](`if/2`) also supports `else` blocks:

```elixir
iex> if nil do
...>   "This won't be seen"
...> else
...>   "This will"
...> end
"This will"
```

This is also a good opportunity to talk about variable scoping in Elixir. If any variable is declared or changed inside [`if`](`if/2`), [`case`](`case/2`), and similar constructs, the declaration and change will only be visible inside the construct. For example:

```elixir
iex> x = 1
1
iex> if true do
...>   x = x + 1
...> end
2
iex> x
1
```

In said cases, if you want to change a value, you must return the value from the [`if`](`if/2`):

```elixir
iex> x = 1
1
iex> x = if true do
...>   x + 1
...> else
...>   x
...> end
2
```

> #### `if` is a macro {: .info}
>
> An interesting note regarding [`if`](`if/2`) is that it is implemented as a macro in the language: it isn't a special language construct as it would be in many languages. You can check the documentation and its source for more information.

If you find yourself nesting several [`if`](`if/2`) blocks, you may want to consider using [`cond`](`cond/1`) instead. Let's check it out.

## cond

We have used `case` to find a matching clauses from many patterns. We have used `if` to check for a single condition. If you need to check across several conditions and find the first one that does not evaluate to `nil` or `false`, [`cond`](`cond/1`) is a useful construct:

```elixir
iex> cond do
...>   2 + 2 == 5 ->
...>     "This will not be true"
...>   2 * 2 == 3 ->
...>     "Nor this"
...>   1 + 1 == 2 ->
...>     "But this will"
...> end
"But this will"
```

This is equivalent to `else if` clauses in many imperative languages - although used less frequently in Elixir.

If all of the conditions return `nil` or `false`, an error (`CondClauseError`) is raised. For this reason, it may be necessary to add a final condition, equal to `true`, which will always match:

```elixir
iex> cond do
...>   2 + 2 == 5 ->
...>     "This is never true"
...>   2 * 2 == 3 ->
...>     "Nor this"
...>   true ->
...>     "This is always true (equivalent to else)"
...> end
"This is always true (equivalent to else)"
```

Similar to [`if`](`if/2`), [`cond`](`cond/1`) considers any value besides `nil` and `false` to be true:

```elixir
iex> cond do
...>   hd([1, 2, 3]) ->
...>     "1 is considered as true"
...> end
"1 is considered as true"
```

## Summing up

We have concluded the introduction to the most fundamental control-flow constructs in Elixir. Generally speaking, Elixir developers prefer pattern matching and guards, using [`case`](`case/2`) and function definitions (which we will explore in future chapters), as they are succinct and precise. When your logic cannot be outlined within patterns and guards, you may consider [`if`](`if/2`), falling back to [`cond`](`cond/1`) when there are several conditions to check.
