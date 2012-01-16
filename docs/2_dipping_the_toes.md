# 2 Dipping the toes

In this section we are going a bit deeper into the basic data-types, learn some control flow mechanisms and basic functions.

## 2.1 Lists and tuples

Elixir provides both lists and tuples:

    iex> is_list [1,2,3]
    true
    iex> is_tuple {1,2,3}
    true

While both are used to store items, they differ on how those items are stored in memory. Lists are implemented as linked lists (each item in the list simply points to the next item in memory) and tuples are stored contiguously in memory.

This means that accessing a tuple element is very fast (constant time) and can be achieved using the `elem` function (notice that indexes in Elixir data-types start with `1`):

    iex> elem { :a, :b, :c }, 1
    :a

On the other hand, updating a tuple is expensive as it needs to duplicate the tuple contents in memory. Updating a tuple can be done with the `setelem` function:

    iex> setelem { :a, :b, :c }, 1, :d
    {:d, :b, :c}

> If you are an Erlang developer, you will notice that we used the `elem` and `setelem` functions instead of Erlang's `element` and `setelement`. The reason for this choice is that Elixir attempts to normalize Erlang API's to always receive the `subject` of the function as the first argument.

Since updating a tuple is expensive, when we want to iterate, add or remove elements, we usually use lists. Since lists are linked, it means accessing the first element of the list is very cheap, however, accessing the n-th element will require the algorithm to pass to n-1 nodes before reaching the n-th. For this reason, Elixir allows you to easily retrieve the first element of the list (called head):

    iex> [h | t] = [1,2,3]
    [1, 2, 3]
    iex> h
    1
    iex> t
    [2, 3]
    iex> [h | t]
    [1, 2, 3]
    iex> length [h | t]
    2

In the example above, we have assigned the head of the list to `h` and the tail of the list to `t`. The [`Enum` module](https://github.com/josevalim/elixir/blob/master/lib/enum.ex) provides several helpers to manipulate lists (and other enumerables in general that we will discuss later) while the [List module](https://github.com/josevalim/elixir/blob/master/lib/list.ex) provides several helpers specific to lists:

    iex> Enum.map [1,2,3], fn(x) { x * 2 }
    [4,5,6]
    iex> List.flatten [1,[2],3]
    [4,5,6]

## 2.2 Calling Erlang functions

Elixir's plans is to provide a small standard library responsible for handling most basic structures (lists, ordered dicts, strings and so forth) and IO. That said, complex applications will require the developer to use Erlang's libraries.

Erlang ships with a group of libraries called OTP (Open Telecom Platform). Besides an standard library, OTP provides several facilities to build OTP applications with supervisors that are robust, distributed and fault-tolerant. Invoking those libraries from Elixir is quite straight-forward, for example, we can call the [function `flatten` from the module `lists`](www.erlang.org/doc/man/lists.html) as follow:

    iex> Erlang.lists.flatten [1,[2],3]
    [1,2,3]

Erlang's OTP is very well documented and a developer should not have problems going around it:

* [OTP docs](http://www.erlang.org/doc/)
* [Standard library docs](http://www.erlang.org/doc/man/STDLIB_app.html)

## 2.3 Pattern matching

When discussing lists, we saw the following example:

    iex> [h | t] = [1,2,3]
    [1, 2, 3]
    iex> h
    1
    iex> t
    [2, 3]

In Elixir, `=` does not mean assignment as in programming languages like Java and Ruby. `=` is actually a match operator which will check if the expressions on both left and right side match. Consider this example:

    iex> { 1, 2, 3 } = { 1, 2, 3 }
    { 1, 2, 3 }
    iex> { 1, 2, 3 } = { 1, 4, 3 }
    ** error {:badmatch, {1, 4, 3}}

If the tuples given on the left and right side do not match, an error is raised. If any of the tuples contain a variable, this variable will always be assigned:

    iex> { 1, x, 3 } = { 1, 2, 3 }
    { 1, 2, 3 }
    iex> x
    2
    iex> { 1, x, 3 } = { 1, 4, 3 }
    { 1, 4, 3 }
    iex> x
    4

This is exactly what happened in our list example:

    iex> [h | t] = [1,2,3]
    [1, 2, 3]

We have assigned the head of the list to `h` and the tail to `t`. In fact, we could check if the head of the list is `1` and just then assign by doing:

    iex> [1 | t] = [1,2,3]
    [1, 2, 3]
    iex> [0 | t] = [1,2,3]
    ** error {:badmatch, [1, 2, 3]}

In case you want to pattern match against the value of a variable, you can use the `^` operator:

    iex> x = 1
    1
    iex> ^x = 1
    1
    iex> ^x = 2
    ** error {:badmatch, 2}
    iex> x = 2
    2

In Elixir, it is a common practice to assign a variable to underscore `_` if we don't intend to use it. For example, if the only the head of the list matters to us, we can assign the tail to underscore:

    iex> [h | _] = [1,2,3]
    [1, 2, 3]
    iex> h
    1

The variable `_` in Elixir is special in the sense it can never be assigned. Trying to read from it gives an unbound variable error:

    iex> _
    ** error {:unbound_var, :_}

Although pattern matching allow powerful constructs, its usage is limited. For instance, you cannot make function calls on the left side of the match. The following example is invalid:

    iex> Erlang.lists.flatten([1,[2],3]) = [1,2,3]
    ** error :illegal_pattern

## 2.4 Key-values

One of the first control flow constructs we usually learn is the conditional `if`. In Elixir, we can write `if` in three different ways, all equivalent:

    iex> if true, do: 1 + 2
    3
    iex> if(true) { 1 + 2 }
    3
    iex> if true do
    ...>   1 + 2
    ...> end
    3

All those three formats are simply different ways of expressing key-value arguments. Key-value arguments are simply a list of two-item tuples, where the first element is an atom representing the key and the second is the value. Elixir provides a syntax-shortcut for creating such key-values:

    iex> [a: 1, b: 2]
    [{:a, 1}, {:b, 2}]

Notice that a key-value argument is a special kind of Ordered Dictionary where the keys are always atoms. For this reason we usually use the [`Orddict` module](https://github.com/josevalim/elixir/blob/master/lib/orddict.ex) to manipulate key-value arguments:

    iex> x = [a: 1, b: 2]
    [{:a, 1}, {:b, 2}]
    iex> Orddict.get x, :a
    1
    iex> Orddict.get x, :c
    nil

Going back to the `if` example, we invoked it passing a condition (`true`) and a key-value argument (`do: 1 + 2`):

    iex> if true, do: 1 + 2
    3

Since the key-value argument is the last argument, the brackets are optional. Those are all equivalent:

    iex> if true, do: 1 + 2
    3
    iex> if true, [do: 1 + 2]
    3
    iex> if(true, [do: 1 + 2])
    3

Besides, we can also pass an `else` clause to `if`:

    iex> if false, do: 1 + 2, else: 10 + 3
    13

However, most of the times if clauses are much longer than the examples above. In such cases, we usually use the block format:

    iex> if true do
    ...>   1 + 2
    ...> end

Internally, this is converted to the same key-value arguments as above. This feature is called key-value blocks. You can pass `else:` as option as well:

    if false do
      1 + 2
    else:
      10 + 3
    end

Those key-value blocks are similar to Ruby blocks. Parenthesis can be added as follow:

    if(false) do
      1 + 2
    else:
      10 + 3
    end

Key-value blocks are an important feature that allow developers to create their own control structures as if they were part of the language itself. For instance, none of the control structures we are going to see in the next section are keywords. They are all implemented using key-values blocks.

Elixir supports two types of key-value blocks, one using `do`/`end` and another one using curly brackets `{`/`}`. Their only difference is function call precedence. For example, consider this:

    Enum.map [1,2,3], fn(x) do
      x * 2
    end

This would be parsed as:

    Enum.map([1,2,3], fn(x)) do
      x * 2
    end

Which is not what we want since `do` is applying to the `Enum.map` call. We can fix this by using curly brackets:

    Enum.map [1,2,3], fn(x) {
      x * 2
    }

Which is then parsed as:

    Enum.map([1,2,3], fn(x) {
      x * 2
    })

A good rule of thumb is: when providing arguments to a function, always use curly brackets.

## 2.5 Control flow structures

In this section we are going to describe Elixir main control structures.

### 2.5.1 If

Refreshing from the section above, all those calls are equivalent:

   if false, do: 1 + 2, else: 10 + 3

   if(false) {
     1 + 2
   else:
     10 + 3
   }

   if false do
     1 + 2
   else:
     10 + 3
   end

`if` also accepts many `elsif:` clauses:

    if 1 + 1 == 3 do
      IO.puts "Impossible"
    elsif: 1 + 1 == 2
      IO.puts "This will match"
    elsif: true
      IO.puts "This won't because the one above matched"
    else:
      IO.puts "This won't"
    end

In Elixir, all values except `false` and `nil` evaluates to true. So there is no need to convert them to false.

### 2.5.2 Other boolean operators

In the previous chapter, we discussed the boolean operators `and`, `or`, `not`, `andalso` and `orelse`. Those operators are strict in the sense they only accept booleans as arguments.

To work around this limitation, Elixir provides three operators with similar functionality but that accept any argument: `||`, `&&` and `!`. For those operators, all values except `false` and `nil` will evaluate to true.

    # Short-circuit or
    iex> 1 || true
    1
    iex> false || 11
    11

    # Short-circuit and
    iex> nil && 13
    nil
    iex> true && 17
    17

    # Short-circuit !
    iex> !true
    false
    iex> !1
    false
    iex> !nil
    true

### 2.5.3 Case

In this section we have introduced pattern matching via the `=` operator. Sometimes however it is convenient to match an expression against several expressions until we find a matching one. For such cases, we use `case` (pun intended):

    case { 1, 2, 3 } do
    match: { 4, 5, 6 }
      IO.puts "This won't match"
    match: { 1, x, 3 }
      IO.puts "This will match and assign x"
    else:
      IO.puts "No match"
    end

As in the `=` operator, any assigned variable will be overridden in the match clause. In case you want to pattern match against a variable, you need to use the `^` operator:

    x = 1
    case 10 do
    match: ^x
      IO.puts "Won't match"
    else:
      IO.puts "Will match"
    end

Each match clause also supports special conditions to be given via guards:

    case { 1, 2, 3 } do
    match: { 4, 5, 6 }
      IO.puts "This won't match"
    match: { 1, x, 3 } when x > 0
      IO.puts "This will match and assign x"
    else:
      IO.puts "No match"
    end

In the example above, the second clause will only match if x is positive. The Erlang VM machine only allows few expressions as guards, they are:

* comparison operators (`==`, `!=`, `===`, `!===`, `>`, `<`, `<=`, `>=`);
* strict boolean operators (`and`, `or`, `not`, `andalso`, `orelse`);
* arithmetic operators (`+`, `-`, `*`, `/`);
* all the following type check functions:

    is_atom/1
    is_binary/1
    is_bitstring/1
    is_boolean/1
    is_float/1
    is_function/1
    is_function/2
    is_integer/1
    is_list/1
    is_number/1
    is_pid/1
    is_port/1
    is_record/2
    is_record/3
    is_reference/1
    is_tuple/1

* plus these functions:

    abs(Number)
    bit_size(Bitstring)
    byte_size(Bitstring)
    elem(Tuple, n)
    float(Term)
    hd(List)
    length(List)
    node()
    node(Pid|Ref|Port)
    round(Number)
    self()
    size(Tuple|Bitstring)
    tl(List)
    trunc(Number)
    tuple_size(Tuple)

Custom macros can be invoked in guard clauses as long as they expand to expressions that are a subset of the ones described above.

### 2.5.4 Functions and loops

### 2.5.5 Try

### 2.5.6 Receive

## 2.6 Default Erlang functions