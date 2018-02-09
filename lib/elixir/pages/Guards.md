# Guards

Guards are a way to augment pattern matching with more complex checks. They are allowed in a predefined set of constructs where pattern matching is allowed.

Not all expressions are allowed in guard clauses, but only a handful of them. This is a deliberate choice. This way, Elixir (and Erlang) can make sure that nothing bad happens while executing guards and no mutations happen anywhere. It also allows the compiler to optimize the code related to guards efficiently.

## List of allowed expressions

For reference, the following is a comprehensive list of all expressions allowed in guards:

  * comparison operators ([`==`](`Kernel.==/2`), [`!=`](`Kernel.!=/2`), [`===`](`Kernel.===/2`), [`!==`](`Kernel.!==/2`),
    [`>`](`Kernel.>/2`), [`>=`](`Kernel.>=/2`), [`<`](`Kernel.</2`), [`<=`](`Kernel.<=/2`))
  * strictly boolean operators ([`and`](`Kernel.and/2`), [`or`](`Kernel.or/2`), [`not`](`Kernel.not/1`))
    - __NOTE__: [`&&`](`Kernel.&&/2`), [`||`](`Kernel.||/2`), and [`!`](`Kernel.!/1`) sibling operators are not allowed as they're not
    *strictly* boolean - meaning they don't require both sides to be booleans
  * arithmetic binary operators ([`+`](`Kernel.+/2`), [`-`](`Kernel.-/2`), [`*`](`Kernel.*/2`), [`/`](`Kernel.//2`))
  * arithmetic unary operators ([`+`](`Kernel.+/1`), [`-`](`Kernel.-/1`))
  * binary concatenation operator ([`<>`](`Kernel.<>/2`))
  * [`in`](`Kernel.in/2`) and [`not in`](`Kernel.in/2`) operators (as long as the right-hand side is a list or a range)
  * the following "type-check" functions (all documented in the `Kernel` module):
    * `is_atom/1`
    * `is_binary/1`
    * `is_bitstring/1`
    * `is_boolean/1`
    * `is_float/1`
    * `is_function/1`
    * `is_function/2`
    * `is_integer/1`
    * `is_list/1`
    * `is_map/1`
    * `is_nil/1`
    * `is_number/1`
    * `is_pid/1`
    * `is_port/1`
    * `is_reference/1`
    * `is_tuple/1`
  * the following guard-friendly functions (all documented in the `Kernel` module):
    * `abs/1`
    * `binary_part/3`
    * `bit_size/1`
    * `byte_size/1`
    * `div/2`
    * `elem/2`
    * `hd/1`
    * `length/1`
    * `map_size/1`
    * `node/0`
    * `node/1`
    * `rem/2`
    * `round/1`
    * `self/0`
    * `tl/1`
    * `trunc/1`
    * `tuple_size/1`
  * the following handful of Erlang bitwise operations, if imported from the `Bitwise` module:
    * [`band/2`](`Bitwise.band/2`) or the [`&&&`](`Bitwise.&&&/2`) operator
    * [`bor/2`](`Bitwise.bor/2`) or the [`|||`](`Bitwise.|||/2`) operator
    * [`bnot/1`](`Bitwise.bnot/1`) or the [`~~~`](`Bitwise.~~~/1`) operator
    * [`bsl/2`](`Bitwise.bsl/2`) or the [`<<<`](`Bitwise.<<</2`) operator
    * [`bsr/2`](`Bitwise.bsr/2`) or the [`>>>`](`Bitwise.>>>/2`) operator
    * [`bxor/2`](`Bitwise.bxor/2`) or the [`^^^`](`Bitwise.^^^/2`) operator

Macros constructed out of any combination of the above guards are also valid guards - for example, `Integer.is_even/1`. See the section "Defining custom guard expressions" below.

## Why guards

Let's see an example of a guard used in a function clause:

```elixir
def empty_map?(map) when map_size(map) == 0, do: true
def empty_map?(map) when is_map(map), do: false
```

Guards start with the `when` keyword, which is followed by a boolean expression (we will define the grammar of guards more formally later on).

Writing the `empty_map?/1` function by only using pattern matching would not be possible (as pattern matching on `%{}` would match *every* map, not empty maps).

## Where guards can be used

In the example above, we show how guards can be used in function clauses. There are several constructs that allow guards; for example:

  * function clauses:

  ```elixir
  def foo(term) when is_integer(term), do: term
  def foo(term) when is_float(term), do: round(term)
  ```

  * [`case`](`Kernel.SpecialForms.case/2`) expressions:

  ```elixir
  case x do
    1 -> :one
    2 -> :two
    n when is_integer(n) and n > 2 -> :larger_than_two
  end
  ```

  * anonymous functions ([`fn`](`Kernel.SpecialForms.fn/1`)s):

  ```elixir
  larger_than_two? = fn
    n when is_integer(n) and n > 2 -> true
    n when is_integer(n) -> false
  end
  ```

  * custom guards can also be defined with `Kernel.defguard/1` and `Kernel.defguardp/1`.
    A custom guard is always defined based on existing guards.

Other constructs are [`for`](`Kernel.SpecialForms.for/1`), [`with`](`Kernel.SpecialForms.with/1`), [`try/rescue/catch/else`](`Kernel.SpecialForms.try/1`), and the `Kernel.match?/2`.

## Failing guards

In guards, when functions would normally raise exceptions, they cause the guard to fail instead.
For example, the `length/1` function only works with lists. If we use it with anything else, a runtime error is raised:

```elixir
iex> length("hello")
** (ArgumentError) argument error
```

However, when used in guards, the corresponding clause simply fails to match:

```elixir
iex> case "hello" do
...>   something when length(something) > 0 ->
...>     :length_worked
...>   _anything_else ->
...>     :length_failed
...> end
:length_failed
```

In many cases, we can take advantage of this. In the code above, we used `length/1` to both check that the given thing is a list *and* check some properties of its length (instead of using `is_list(something) and length(something) > 0`).

## Defining custom guard expressions

As mentioned before, only the expressions listed in this page are allowed in guards. However, we can take advantage of macros to write custom guards that can simplify our programs or make them more domain-specific. At the end of the day, what matters is that the *output* of the macros (which is what will be compiled) boils down to a combinations of the allowed expressions.

Let's look at a quick case study: we want to check that a function argument is an even or odd integer. With pattern matching, this is impossible to do since there are infinite integers, and thus we can't pattern match on the single even/odd numbers. Let's focus on checking for even numbers since checking for odd ones is almost identical.

Such a guard would look like this:

```elixir
def my_function(number) when is_integer(number) and rem(number, 2) == 0 do
  # do stuff
end
```

This would be repetitive to write every time we need this check, so, as mentioned at the beginning of this section, we can abstract this away using a macro. Remember that defining a function that performs this check wouldn't work because we can't use custom functions in guards. Our macro would look like this:

```elixir
defmodule MyInteger do
  defmacro is_even(number) do
    quote do
      is_integer(unquote(number)) and rem(unquote(number), 2) == 0
    end
  end
end
```

and then:

```elixir
import MyInteger, only: [is_even: 1]

def my_function(number) when is_even(number) do
  # do stuff
end
```

While it's possible to create custom guards with macros, it's recommended to define them using `defguard` and `defguardp` which perform additional compile-time checks. Here's an example:

```elixir
defmodule MyInteger do
  defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
end
```

## Multiple guards in the same clause

There exists an additional way to simplify a chain of `or`s in guards: Elixir supports writing "multiple guards" in the same clause. This:

```elixir
def foo(term) when is_integer(term) or is_float(term) or is_nil(term),
  do: :maybe_number
def foo(_other),
  do: :something_else
```

can be alternatively written as:

```elixir
def foo(term)
    when is_integer(term)
    when is_float(term)
    when is_nil(term) do
  :maybe_number
end

def foo(_other) do
  :something_else
end
```

For most cases, the two forms are exactly the same. However, there exists a subtle difference in the case of failing guards, as discussed in the section above.
In case of a boolean expression guard, a failed element means the whole guard fails. In case of multiple guards it means the next one will be evaluated.
The difference can be highlighted with an example:

```elixir
def multiguard(value)
    when map_size(value) < 1
    when tuple_size(value) < 1 do
  :guard_passed
end
def multiguard(_value) do
  :guard_failed
end

def boolean(value) when map_size(value) < 1 or tuple_size(value) < 1 do
  :guard_passed
end
def boolean(value) do
  :guard_failed
end

multiguard(%{}) #=> :guard_passed
multiguard({})  #=> :guard_passed

boolean(%{}) #=> :guard_passed
boolean({})  #=> :guard_failed
```

For cases where guards do not rely on the failing guard behavior the two forms are exactly the same semantically but there are cases where multiple guard clauses may be more aesthetically pleasing.
