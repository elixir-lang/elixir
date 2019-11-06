# Guards

Guards are a way to augment pattern matching with more complex checks. They are allowed in a predefined set of constructs where pattern matching is allowed.

Not all expressions are allowed in guard clauses, but only a handful of them. This is a deliberate choice. This way, Elixir (and Erlang) can make sure that nothing bad happens while executing guards and no mutations happen anywhere. It also allows the compiler to optimize the code related to guards efficiently.

## List of allowed functions and operators

You can find the built-in list of guards [in the `Kernel` module](Kernel.html#guards). Here is an overview:

  * comparison operators ([`==`](`==/2`), [`!=`](`!=/2`), [`===`](`===/2`), [`!==`](`!==/2`),
    [`<`](`</2`), [`<=`](`<=/2`), [`>`](`>/2`), [`>=`](`>=/2`))
  * strictly boolean operators ([`and`](`and/2`), [`or`](`or/2`), [`not`](`not/1`)). Note [`&&`](`&&/2`), [`||`](`||/2`), and [`!`](`!/1`) sibling operators are **not allowed** as they're not *strictly* boolean - meaning they don't require arguments to be booleans
  * arithmetic unary operators ([`+`](`+/1`), [`-`](`-/1`))
  * arithmetic binary operators [`+`](`+/2`), [`-`](`-/2`), [`*`](`*/2`), [`/`](`//2`))
  * [`in`](`in/2`) and [`not in`](`in/2`) operators (as long as the right-hand side is a list or a range)
  * "type-check" functions (`is_list/1`, `is_number/1`, etc.)
  * functions that work on built-in datatypes (`abs/1`, `hd/1`, `map_size/1`, etc.)

The module `Bitwise` also includes a handful of [Erlang bitwise operations as guards](Bitwise.html#guards).

Macros constructed out of any combination of the above guards are also valid guards - for example, `Integer.is_even/1`. For more information, see the "Defining custom guard expressions" section shown below.

## Why guards

Let's see an example of a guard used in a function clause:

```elixir
def empty_map?(map) when map_size(map) == 0, do: true
def empty_map?(map) when is_map(map), do: false
```

Guards start with the `when` operator, followed by a guard expression. The clause will be executed if and only if the guard expression returns `true`. Multiple boolean conditions can be combined with the [`and`](`and/2`) and [`or`](`or/2`) operators.

Writing the `empty_map?/1` function by only using pattern matching would not be possible (as pattern matching on `%{}` would match *any* map, not only the empty ones).

## Where guards can be used

In the example above, we show how guards can be used in function clauses. There are several constructs that allow guards; for example:

  * function clauses:

    ```elixir
    def format(term) when is_integer(term), do: term
    def format(term) when is_float(term), do: round(term)
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

  * custom guards can also be defined with `defguard/1` and `defguardp/1`.
    A custom guard can only be defined based on existing guards.

Other constructs that support guards are [`for`](`for/1`), [`with`](`with/1`), [`try/rescue/catch/else`](`try/1`), and `match?/2`.

## Failing guards

As mentioned before, a function clause will be executed if and only if its guard expression evaluates to `true`. If any other value is returned, the function clause will be skipped. In particular, guards have no concept of "truthy" or "falsey". In a nutshell, when it comes to guards, *it's `true` or nothing*.

For example, imagine a function that checks that the head of a list is not `nil`:

```elixir
def not_nil_head?([head | _]) when head, do: true
def not_nil_head?(_), do: false

not_nil_head?(["some_value", "another_value"])
#=> false
```

Even though the head of the list is not `nil`, the first clause for `not_nil_head?/1` fails because the expression is evaluated to `"some_value"` and not to `true` as guards are expected to return, therefore triggering the second clause which returns `false`. To make the guard behave correctly, you must ensure that the guard evaluates to `true`, like so:

```elixir
def not_nil_head?(term) when head != nil, do: true
def not_nil_head?(_), do: false

not_nil_head?(["some_value", "another_value"])
#=> true
```

## Errors in guards

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
:worked
```

In many cases, we can take advantage of this. In the code above, we used `tuple_size/1` to both check that the given value is a tuple *and* check its size (instead of using `is_tuple(something) and tuple_size(something) == 2`).

Since `tuple_size/1` will make the whole guard fail if it is not given a tuple, we recommend to call type-check functions (such as `is_tuple/1`) before, provided your guard has multiple conditions. Additionally, this practice brings the benefit that when writing guard-safe macros, they will return `false` instead of raising an exception when used outside guards.

Alternatively your function clause can use multiple guards as shown in the following section.

## Multiple guards in the same clause

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

This could be corrected by ensuring that no exception is raised, either via type checks like `is_map(val) and map_size(val) == 0`, or by checking equality instead, like `val == %{}`.

It could also be corrected by using multiple guards, so that if an exception causes one guard to fail, the next one is still evaluated.

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

## Defining custom guard expressions

As mentioned before, only the functions and operators listed in this page are allowed in guards. However, we can take advantage of macros to write custom guards that can simplify our programs or make them more domain-specific. At the end of the day, what matters is that the *output* of the macros is boiled down to a combination of allowed expressions.

Let's look at a quick case study: we want to check whether a function argument is an even or an odd integer. With pattern matching this is impossible because there is an infinite number of integers, and therefore we can't pattern match on every single one of them. So, let's focus on checking for even numbers since checking for the odd ones is almost identical.

Such a guard would look like this:

```elixir
def my_function(number) when is_integer(number) and rem(number, 2) == 0 do
  # do stuff
end
```

It would be repetitive to write every time we need this check, so, as mentioned at the beginning of this section, we can abstract this away using a macro. Remember that defining a function that performs this check wouldn't work because we can't use custom functions in guards. Instead you can use `defguard/1` and `defguardp/1` to create guard-safe macros. Here's an example how:

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

While it's possible to create custom guards with macros, it's recommended to define them using `defguard/1` and `defguardp/1` which perform additional compile-time checks and optimizations.
