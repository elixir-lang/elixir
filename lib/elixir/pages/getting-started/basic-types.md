# Basic types

In this chapter we will learn more about Elixir basic types: integers, floats, booleans, atoms, and strings. Other data types, such as lists and tuples, will be explored in the next chapter.

```elixir
iex> 1          # integer
iex> 0x1F       # integer
iex> 1.0        # float
iex> true       # boolean
iex> :atom      # atom / symbol
iex> "elixir"   # string
iex> [1, 2, 3]  # list
iex> {1, 2, 3}  # tuple
```

## Basic arithmetic

Open up `iex` and type the following expressions:

```elixir
iex> 1 + 2
3
iex> 5 * 5
25
iex> 10 / 2
5.0
```

Notice that `10 / 2` returned a float `5.0` instead of an integer `5`. This is expected. In Elixir, the operator [`/`](`//2`) always returns a float. If you want to do integer division or get the division remainder, you can invoke the [`div`](`div/2`) and [`rem`](`rem/2`) functions:

```elixir
iex> div(10, 2)
5
iex> div 10, 2
5
iex> rem 10, 3
1
```

Notice that Elixir allows you to drop the parentheses when invoking functions that expect one or more arguments. This feature gives a cleaner syntax when writing declarations and control-flow constructs. However, Elixir developers generally prefer to use parentheses.

Elixir also supports shortcut notations for entering binary, octal, and hexadecimal numbers:

```elixir
iex> 0b1010
10
iex> 0o777
511
iex> 0x1F
31
```

Float numbers require a dot followed by at least one digit and also support `e` for scientific notation:

```elixir
iex> 1.0
1.0
iex> 1.0e-10
1.0e-10
```

Floats in Elixir are 64-bit precision.

You can invoke the [`round`](`round/1`) function to get the closest integer to a given float, or the [`trunc`](`trunc/1`) function to get the integer part of a float.

```elixir
iex> round(3.58)
4
iex> trunc(3.58)
3
```

Finally, we work with different data types, we will learn Elixir provides several predicate functions to check for the type of a value. For example, [`is_integer`](`is_integer/1`) can be used to check if a value is an integer or not:

```elixir
iex> is_integer(1)
true
iex> is_integer(2.0)
false
```

You can also use [`is_float`](`is_float/1`) or [`is_number`](`is_number/1`) to check, respectively, if an argument is a float, or either an integer or float.

## Booleans and `nil`

Elixir supports `true` and `false` as booleans:

```elixir
iex> true
true
iex> true == false
false
```

Elixir also provides three boolean operators: [`or`](`or/2`), [`and`](`and/2`), and [`not`](`not/1`). These operators are strict in the sense that they expect something that evaluates to a boolean (`true` or `false`) as their first argument:

```elixir
iex> true and true
true
iex> false or is_boolean(true)
true
```

Providing a non-boolean will raise an exception:

```elixir
iex> 1 and true
** (BadBooleanError) expected a boolean on left-side of "and", got: 1
```

`or` and `and` are short-circuit operators. They only execute the right side if the left side is not enough to determine the result:

```elixir
iex> false and raise("This error will never be raised")
false
iex> true or raise("This error will never be raised")
true
```

Elixir also provides the concept of `nil`, to indicate the absence of a value, and a set of logical operators that also manipulate `nil`: `||/2`, `&&/2`, and `!/1`. For these operators, `false` and `nil` are considered "falsy", all other values are considered "truthy":

```elixir
# or
iex> 1 || true
1
iex> false || 11
11

# and
iex> nil && 13
nil
iex> true && 17
17

# not
iex> !true
false
iex> !1
false
iex> !nil
true
```

Similarly, values like `0` and `""`, which some other programming languages consider to be "falsy", are also "truthy" in Elixir.

As a rule of thumb, use `and`, `or` and `not` when you are expecting booleans. If any of the arguments are non-boolean, use `&&`, `||` and `!`.

## Atoms

An atom is a constant whose value is its own name. Some other languages call these symbols. They are often useful to enumerate over distinct values, such as:

```elixir
iex> :apple
:apple
iex> :orange
:orange
iex> :watermelon
:watermelon
```

Atoms are equal if their names are equal.

```elixir
iex> :apple == :apple
true
iex> :apple == :orange
false
```

Often they are used to express the state of an operation, by using values such as `:ok` and `:error`.

The booleans `true` and `false` are also atoms:

```elixir
iex> true == :true
true
iex> is_atom(false)
true
iex> is_boolean(:false)
true
```

Elixir allows you to skip the leading `:` for the atoms `false`, `true` and `nil`.

## Strings

Strings in Elixir are delimited by double quotes, and they are encoded in UTF-8:

```elixir
iex> "hellö"
"hellö"
```

> Note: if you are running on Windows, there is a chance your terminal does not use UTF-8 by default. You can change the encoding of your current session by running `chcp 65001` before entering IEx.

You can concatenate two strings with the [`<>`](`<>/2`) operator:

```elixir
iex> "hello " <> "world!"
"hello world!"
```

Elixir also supports string interpolation:

```elixir
iex> string = "world"
iex> "hello #{string}!"
"hello world!"
```

String concatenation requires both sides to be strings but interpolation supports any data type that may be converted to a string:

```elixir
iex> number = 42
iex> "i am #{number} years old!"
"i am 42 years old!"
```

Strings can have line breaks in them. You can introduce them using escape sequences:

```elixir
iex> "hello
...> world"
"hello\nworld"
iex> "hello\nworld"
"hello\nworld"
```

You can print a string using the [`IO.puts`](`IO.puts/1`) function from the `IO` module:

```elixir
iex> IO.puts("hello\nworld")
hello
world
:ok
```

Notice that the [`IO.puts`](`IO.puts/1`) function returns the atom `:ok` after printing.

Strings in Elixir are represented internally by contiguous sequences of bytes known as binaries:

```elixir
iex> is_binary("hellö")
true
```

We can also get the number of bytes in a string:

```elixir
iex> byte_size("hellö")
6
```

Notice that the number of bytes in that string is 6, even though it has 5 graphemes. That's because the grapheme "ö" takes 2 bytes to be represented in UTF-8. We can get the actual length of the string, based on the number of graphemes, by using the [`String.length`](`String.length/1`) function:

```elixir
iex> String.length("hellö")
5
```

The `String` module contains a bunch of functions that operate on strings as defined in the Unicode standard:

```elixir
iex> String.upcase("hellö")
"HELLÖ"
```

## Structural comparison

Elixir also provides [`==`](`==/2`), [`!=`](`!=/2`), [`<=`](`<=/2`), [`>=`](`>=/2`), [`<`](`</2`) and [`>`](`>/2`) as comparison operators. We can compare numbers:

```elixir
iex> 1 == 1
true
iex> 1 != 2
true
iex> 1 < 2
true
```

But also atoms, strings, booleans, etc:

```elixir
iex> "foo" == "foo"
true
iex> "foo" == "bar"
false
```

Integers and floats compare the same if they have the same value:

```elixir
iex> 1 == 1.0
true
iex> 1 == 2.0
false
```

However, you can use the strict comparison operator [`===`](`===/2`) and [`!==`](`!==/2`) if you want to distinguish between integers and floats:

```elixir
iex> 1 === 1.0
false
```

The comparison operators in Elixir can compare across any data type. We say these operators perform _structural comparison_. For more information, you can read our documentation on [Structural vs Semantic comparisons](`Kernel#module-structural-comparison`).

Elixir also provides data-types for expressing collections, such as lists and tuples, which we learn next. When we talk about concurrency and fault-tolerance via processes, we will also discuss ports, pids, and references, but that will come on later chapters. Let's move forward.
