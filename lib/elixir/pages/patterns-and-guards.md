# Patterns and Guards

Elixir provides pattern matching, which allows us to assert on the shape or extract values from data-structures. Patterns are often augmented with guards, which give developers the ability to perform more complex checks, albeit limited.

This page describes the semantics of patterns and guards, where they are all allowed, and how to extend them.

## Patterns

Patterns in Elixir are made of variables, literals, and data-structure specific syntax. One of the most used constructs to perform pattern matching is the match operator (`=`):

```iex
iex> x = 1
1
iex> 1 = x
1
```

In the example above, `x` starts without a value and has `1` assigned to it. Then, we compare the value of `x` to the literal `1`, which succeeds as they are both `1`.

Matching `x` against 2 would raise:

```iex
iex> 2 = x
** (MatchError) no match of right hand side value: 1
```

Patterns are not bidirectional. If you have a variable `y` that was never assigned to (often called an unbound variable) and you write `1 = y`, an error will be raised:

```iex
iex> 1 = y
** (CompileError) iex:2: undefined function y/0
```

In other words, patterns are allowed only on the left side of `=`. The right side of `=` follows the regular evaluation semantics of the language.

Now let's cover the pattern matching rules for each construct and then for each relevant data-types.

### Variables

Variables in patterns are always assigned to:

```iex
iex> x = 1
1
iex> x = 2
2
iex> x
2
```

In other words, Elixir supports rebinding. In case you don't want the value of a variable to change, you can use the pin operator (`^`):

```iex
iex> x = 1
1
iex> ^x = 2
** (MatchError) no match of right hand side value: 2
```

If the same variable appears twice in the same pattern, then they must be bound to the same value:

```iex
iex> {x, x} = {1, 1}
{1, 1}
iex> {x, x} = {1, 2}
** (MatchError) no match of right hand side value: {1, 2}
```

The underscore variable (`_`) has a special meaning as it can never be bound to any value. It is especially useful when you don't care about certain value in a pattern:

```iex
iex> {_, integer} = {:not_important, 1}
{:not_important, 1}
iex> integer
1
iex> _
** (CompileError) iex:3: invalid use of _
```

### Literals (numbers and atoms)

Atoms and numbers (integers and floats) can appear in patterns and they are always represented as is. For example, an atom will only match an atom if they are the same atom:

```iex
iex> :atom = :atom
:atom
iex> :atom = :another_atom
** (MatchError) no match of right hand side value: :another_atom
```

Similar rule applies to numbers. Finally, note that numbers in patterns perform strict comparison. In other words, integers to do not match floats:

```iex
iex> 1 = 1.0
** (MatchError) no match of right hand side value: 1.0
```

### Tuples

Tuples may appear in patterns using the curly brackets syntax (`{}`). A tuple in a pattern will match only tuples of the same size, where each individual tuple element must also match:

```iex
iex> {:ok, integer} = {:ok, 13}
{:ok, 13}

# won't match due to different size
iex> {:ok, integer} = {:ok, 11, 13}
** (MatchError) no match of right hand side value: {:ok, 11, 13}

# won't match due to mismatch on first element
iex> {:ok, binary} = {:error, :enoent}
** (MatchError) no match of right hand side value: {:error, :enoent}
```

### Lists

Lists may appear in patterns using the square brackets syntax (`[]`). A list in a pattern will match only lists of the same size, where each individual list element must also match:

```iex
iex> [:ok, integer] = [:ok, 13]
[:ok, 13]

# won't match due to different size
iex> [:ok, integer] = [:ok, 11, 13]
** (MatchError) no match of right hand side value: [:ok, 11, 13]

# won't match due to mismatch on first element
iex> [:ok, binary] = [:error, :enoent]
** (MatchError) no match of right hand side value: [:error, :enoent]
```

Opposite to tuples, lists also allow matching on non-empty lists by using the `[head | tail]` notation, which matches on the `head` and `tail` of a list:

```iex
iex> [head | tail] = [1, 2, 3]
[1, 2, 3]
iex> head
1
iex> tail
[2, 3]
```

Multiple elements may prefix the `| tail` construct:

```iex
iex> [first, second | tail] = [1, 2, 3]
[1, 2, 3]
iex> tail
[3]
```

Note `[head | tail]` does not match empty lists:

```elixir
iex> [head | tail] = []
** (MatchError) no match of right hand side value: []
```

Given charlists are represented as a list of integers, one can also perform prefix matches on charlists using the list concatenation operator (`++`):

```elixir
iex> 'hello ' ++ world = 'hello world'
'hello world'
iex> world
'world'
```

Which is equivalent to matching on `[?h, ?e, ?l, ?l, ?o, ?\s | world]`. Suffix matches (`hello ++ ' world'`) are not valid patterns.

### Maps

Maps may appear in patterns using the percentage sign followed by the curly brackets syntax (`%{}`). Opposite to lists and tuples, maps perform a subset match. This means a map pattern will match any other map that has at least all of the keys in the pattern.

Here is an example where all keys match:

```iex
iex> %{name: name} = %{name: "meg"}
%{name: "meg"}
iex> name
"meg"
```

Here is when a subset of the keys match:

```iex
iex> %{name: name} = %{name: "meg", age: 23}
%{age: 23, name: "meg"}
iex> name
"meg"
```

If a key in the pattern is not available in the map, then they won't match:

```iex
iex> %{name: name, age: age} = %{name: "meg"}
** (MatchError) no match of right hand side value: %{name: "meg"}
```

Note that the empty map will match all maps, which is a contrast to tuples and lists, where an empty tuple or an empty list will only match empty tuples and empty lists respectively:

```iex
iex> %{} = %{name: "meg"}
%{name: "meg"}
```

Finally, note map keys in patterns must always be literals or previously bound variables matched with the pin operator.

### Binaries

Binaries may appear in patterns using the double less-than/greater-than syntax (`<<>>`). A binary in a pattern can match multiple segments at the same, each with different type, size, and unit:

```iex
iex> <<val::unit(8)-size(2)-integer>> = <<123, 56>>
"{8"
iex> val
31544
```

See the documentation for `<<>>` for a complete definition of pattern matching for binaries.

Finally, remember that strings in Elixir are UTF-8 encoded binaries. This means that, similar to charlists, prefix matches on strings are also possible with the binary concatenation operator (`<>`):

```elixir
iex> "hello " <> world = "hello world"
"hello world"
iex> world
"world"
```

Suffix matches (`hello <> " world"`) are not valid patterns.

## Guards

Guards are a way to augment pattern matching with more complex checks. They are allowed in a predefined set of constructs where pattern matching is allowed, such as function definitions, case clauses, and others.

Not all expressions are allowed in guard clauses, but only a handful of them. This is a deliberate choice. This way, Elixir (and Erlang) can make sure that nothing bad happens while executing guards and no mutations happen anywhere. It also allows the compiler to optimize the code related to guards efficiently.

### List of allowed functions and operators

You can find the built-in list of guards [in the `Kernel` module](Kernel.html#guards). Here is an overview:

  * comparison operators ([`==`](`==/2`), [`!=`](`!=/2`), [`===`](`===/2`), [`!==`](`!==/2`),
    [`<`](`</2`), [`<=`](`<=/2`), [`>`](`>/2`), [`>=`](`>=/2`))
  * strictly boolean operators ([`and`](`and/2`), [`or`](`or/2`), [`not`](`not/1`)). Note [`&&`](`&&/2`), [`||`](`||/2`), and [`!`](`!/1`) sibling operators are **not allowed** as they're not *strictly* boolean - meaning they don't require arguments to be booleans
  * arithmetic unary operators ([`+`](`+/1`), [`-`](`-/1`))
  * arithmetic binary operators [`+`](`+/2`), [`-`](`-/2`), [`*`](`*/2`), [`/`](`//2`))
  * [`in`](`in/2`) and [`not in`](`in/2`) operators (as long as the right-hand side is a list or a range)
  * "type-check" functions (`is_list/1`, `is_number/1`, and the like)
  * functions that work on built-in datatypes (`abs/1`, `hd/1`, `map_size/1`, and others)

The module `Bitwise` also includes a handful of [Erlang bitwise operations as guards](Bitwise.html#guards).

Macros constructed out of any combination of the above guards are also valid guards - for example, `Integer.is_even/1`. For more information, see the "Defining custom guard expressions" section shown below.

### Why guards

Let's see an example of a guard used in a function clause:

```elixir
def empty_map?(map) when map_size(map) == 0, do: true
def empty_map?(map) when is_map(map), do: false
```

Guards start with the `when` operator, followed by a guard expression. The clause will be executed if and only if the guard expression returns `true`. Multiple boolean conditions can be combined with the [`and`](`and/2`) and [`or`](`or/2`) operators.

Writing the `empty_map?/1` function by only using pattern matching would not be possible (as pattern matching on `%{}` would match *any* map, not only the empty ones).

### Failing guards

A function clause will be executed if and only if its guard expression evaluates to `true`. If any other value is returned, the function clause will be skipped. In particular, guards have no concept of "truthy" or "falsey".

For example, imagine a function that checks that the head of a list is not `nil`:

```elixir
def not_nil_head?([head | _]) when head, do: true
def not_nil_head?(_), do: false

not_nil_head?(["some_value", "another_value"])
#=> false
```

Even though the head of the list is not `nil`, the first clause for `not_nil_head?/1` fails because the expression does not evaluate to `true`, but to `"some_value"`, therefore triggering the second clause which returns `false`. To make the guard behave correctly, you must ensure that the guard evaluates to `true`, like so:

```elixir
def not_nil_head?([head | _]) when head != nil, do: true
def not_nil_head?(_), do: false

not_nil_head?(["some_value", "another_value"])
#=> true
```

### Errors in guards

In guards, when functions would normally raise exceptions, they cause the guard to fail instead.

For example, the `tuple_size/1` function only works with tuples. If we use it with anything else, an argument error is raised:

```elixir
iex> tuple_size("hello")
** (ArgumentError) argument error
```

However, when used in guards, the corresponding clause will fail to match instead of raising an error:

```elixir
iex> case "hello" do
...>   something when tuple_size(something) == 2 ->
...>     :worked
...>   _anything_else ->
...>     :failed
...> end
:failed
```

In many cases, we can take advantage of this. In the code above, we used `tuple_size/1` to both check that the given value is a tuple *and* check its size (instead of using `is_tuple(something) and tuple_size(something) == 2`).

However, if your guard has multiple conditions, such as checking for tuples or maps, it is best to call type-check functions like `is_tuple/1` before `tuple_size/1`, otherwise the whole guard will fail if a tuple is not given. Alternatively your function clause can use multiple guards as shown in the following section.

### Multiple guards in the same clause

There exists an additional way to simplify a chain of `or` expressions in guards: Elixir supports writing "multiple guards" in the same clause. The following code:

```elixir
def is_number_or_nil(term) when is_integer(term) or is_float(term) or is_nil(term),
  do: :maybe_number
def is_number_or_nil(_other),
  do: :something_else
```

can be alternatively written as:

```elixir
def is_number_or_nil(term)
    when is_integer(term)
    when is_float(term)
    when is_nil(term) do
  :maybe_number
end

def is_number_or_nil(_other) do
  :something_else
end
```

If each guard expression always returns a boolean, the two forms are equivalent. However, recall that if any function call in a guard raises an exception, the entire guard fails. To illustrate this, the following function will not detect empty tuples:

```elixir
defmodule Check do
  # If given a tuple, map_size/1 will raise, and tuple_size/1 will not be evaluated
  def empty?(val) when map_size(val) == 0 or tuple_size(val) == 0, do: true
  def empty?(_val), do: false
end

Check.empty?(%{})
#=> true

Check.empty?({})
#=> false # true was expected!
```

This could be corrected by ensuring that no exception is raised, either via type checks like `is_map(val) and map_size(val) == 0`, or by using multiple guards, so that if an exception causes one guard to fail, the next one is evaluated.

```elixir
defmodule Check do
  # If given a tuple, map_size/1 will raise, and the second guard will be evaluated
  def empty?(val)
      when map_size(val) == 0
      when tuple_size(val) == 0,
      do: true

  def empty?(_val), do: false
end

Check.empty?(%{})
#=> true

Check.empty?({})
#=> true
```

## Where patterns and guards can be used

In the examples above, we have used the match operator (`=`) and function clauses to showcase patterns and guards respectively. Here is the list of the built-in constructs in Elixir that support patterns and guards.

  * `match?/2`:

    ```elixir
    match?({:ok, value} when value > 0, {:ok, 13})
    ```

  * function clauses:

    ```elixir
    def type(term) when is_integer(term), do: :integer
    def type(term) when is_float(term), do: :float
    ```

  * [`case`](`case/2`) expressions:

    ```elixir
    case x do
      1 -> :one
      2 -> :two
      n when is_integer(n) and n > 2 -> :larger_than_two
    end
    ```

  * anonymous functions (`fn/1`):

    ```elixir
    larger_than_two? = fn
      n when is_integer(n) and n > 2 -> true
      n when is_integer(n) -> false
    end
    ```

  * [`for`](`for/1`) and [`with`](`with/1`) support patterns and guards on the left side of `<-`:

    ```elixir
    for x when x >= 0 <- [1, -2, 3, -4], do: x
    ```

    `with` also supports the `else` keyword, which supports patterns matching and guards.

  * [`try`](`try/1`) supports patterns and guards on `catch` and `else`

  * [`receive`](`receive/1`) supports patterns and guards to match on the received messages.

  * custom guards can also be defined with `defguard/1` and `defguardp/1`. A custom guard can only be defined based on existing guards.

Note that the match operator (`=`) does *not* support guards:

    ```elixir
    {:ok, binary} = File.read("some/file")
    ```

## Custom patterns and guards expressions

Only the constructs listed in this page are allowed in patterns and guards. However, we can take advantage of macros to write custom patterns guards that can simplify our programs or make them more domain-specific. At the end of the day, what matters is that the *output* of the macros boils down to a combination of the constructs above.

For example, the `Record` module in Elixir provides a series of macros to be used in patterns and guards that allows tuples to have named fields during compilation.

For defining your own guards, Elixir even provides conveniences in `defguard` and `defguardp`. Let's look at a quick case study: we want to check whether an argument is an even or an odd integer. With pattern matching this is impossible because there is an infinite number of integers, and therefore we can't pattern match on every single one of them. Therefore we must use guards. We will just focus on checking for even numbers since checking for the odd ones is almost identical.

Such a guard would look like this:

```elixir
def my_function(number) when is_integer(number) and rem(number, 2) == 0 do
  # do stuff
end
```

It would be repetitive to write every time we need this check. Instead you can use `defguard/1` and `defguardp/1` to create guard macros. Here's an example how:

```elixir
defmodule MyInteger do
  defguard is_even(term) when is_integer(term) and rem(term, 2) == 0
end
```

and then:

```elixir
import MyInteger, only: [is_even: 1]

def my_function(number) when is_even(number) do
  # do stuff
end
```

While it's possible to create custom guards with macros, it's recommended to define them using `defguard/1` and `defguardp/1` which perform additional compile-time checks.
