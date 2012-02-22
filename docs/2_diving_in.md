# 2 Diving in

In this section we are going a bit deeper into the basic data-types, learn some control flow mechanisms and basic functions.

## 2.1 Lists and tuples

Elixir provides both lists and tuples:

    iex> is_list [1,2,3]
    true
    iex> is_tuple {1,2,3}
    true

While both are used to store items, they differ on how those items are stored in memory. Lists are implemented as linked lists (where each item in the list points to the next item) while tuples are stored contiguously in memory.

This means that accessing a tuple element is very fast (constant time) and can be achieved using the `elem` function (notice that indexes in Elixir data-types start with `1`):

    iex> elem { :a, :b, :c }, 1
    :a

On the other hand, updating a tuple is expensive as it needs to duplicate the tuple contents in memory. Updating a tuple can be done with the `setelem` function:

    iex> setelem { :a, :b, :c }, 1, :d
    {:d,:b,:c}

> Note: If you are an Erlang developer, you will notice that we used the `elem` and `setelem` functions instead of Erlang's `element` and `setelement`. The reason for this choice is that Elixir attempts to normalize Erlang API's to always receive the `subject` of the function as the first argument.

Since updating a tuple is expensive, when we want to iterate, add or remove elements, we use lists. Since lists are linked, it means accessing the first element of the list is very cheap, however, accessing the n-th element will require the algorithm to pass to n-1 nodes before reaching the n-th. We can access the `head` of the list as follow:

    iex> [head | tail] = [1,2,3]
    [1,2,3]
    iex> head
    1
    iex> tail
    [2,3]
    iex> [head | tail]
    [1,2,3]
    iex> length [head | tail]
    3

In the example above, we have assigned the head of the list to the variable `head` and the tail of the list to the variable `tail`. The [`Enum` module](https://github.com/josevalim/elixir/blob/master/lib/enum.ex) provides several helpers to manipulate lists (and other enumerables in general) while the [List module](https://github.com/josevalim/elixir/blob/master/lib/list.ex) provides several helpers specific to lists:

    iex> Enum.map [1,2,3], fn(x) -> x * 2 end
    [4,5,6]
    iex> List.flatten [1,[2],3]
    [1,2,3]

## 2.2 Lists and binaries

In the previous chapter we have discussed double- and single-quoted strings. Double quoted strings are binaries while single-quoted strings are lists:

    iex> "string" == 'string'
    false
    iex> is_binary "string"
    true
    iex> is_list 'string'
    true

In fact, both double-quoted and single-quoted representations are just a shorter representation of binaries and lists. Considering that `?a` in Elixir returns the ASCII integer for the letter `a`, we could also write:

    iex> <<?a, ?b, ?c>>
    "abc"
    iex> [?a, ?b, ?c]
    'abc'

In such cases, Elixir is detects all characters in the list and in the binary are printable and returns the quoted representation. However, adding a non-printable character forces them to be printed differently:

    iex> <<?a, ?b, ?c, 1>>
    <<97,98,99,1>>

    iex> [?a, ?b, ?c, 1]
    [97,98,99,1]

Since lists are implemented as linked lists, it means a string represented as list usually takes a lot of space in memory (in ASCII, it would be one byte for each character and another byte to point to the next character). For this reason, binary (double-quoted) strings is preferred unless you want to explicitly iterate over the string as a list.

Currently Elixir does not ship with any library for doing string manipulation, but this will be amended soon.

## 2.3 Calling Erlang functions

Elixir's plans is to provide a small standard library responsible for handling most basic structures (lists, ordered dicts, strings and so forth) and IO. That said, complex applications will require the developer to use Erlang's libraries.

Erlang ships with a group of libraries called OTP (Open Telecom Platform). Besides an standard library, OTP provides several facilities to build OTP applications with supervisors that are robust, distributed and fault-tolerant. Invoking those libraries from Elixir is quite straight-forward, for example, we can call the [function `flatten` from the module `lists`](www.erlang.org/doc/man/lists.html) as follow:

    iex> Erlang.lists.flatten [1,[2],3]
    [1,2,3]

Erlang's OTP is very well documented and a developer should not have problems going around it:

* [OTP docs](http://www.erlang.org/doc/)
* [Standard library docs](http://www.erlang.org/doc/man/STDLIB_app.html)

## 2.4 Pattern matching

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
    ** (::MatchError) no match of right hand side value: {1,4,3}

If the tuples given on the left and right side do not match, an error is raised. If any of the tuples contain a variable, this variable will always be assigned:

    iex> { 1, x, 3 } = { 1, 2, 3 }
    { 1, 2, 3 }
    iex> x
    2
    iex> { 1, x, 3 } = { 1, 4, 3 }
    { 1, 4, 3 }
    iex> x
    4

This is exactly what happened in the list example:

    iex> [h | t] = [1,2,3]
    [1, 2, 3]

We have assigned the head of the list to `h` and the tail to `t`. In fact, if we want to check if the head of the list is `1` and assign the tail, we could do:

    iex> [1 | t] = [1,2,3]
    [1, 2, 3]
    iex> [0 | t] = [1,2,3]
    ** (::MatchError) no match of right hand side value: [1,2,3]

In case you want to pattern match against the value of a variable, you can use the `^` operator:

    iex> x = 1
    1
    iex> ^x = 1
    1
    iex> ^x = 2
    ** (::MatchError) no match of right hand side value: 2
    iex> x = 2
    2

In Elixir, it is a common practice to assign a variable to underscore `_` if we don't intend to use it. For example, if the only the head of the list matters to us, we can assign the tail to underscore:

    iex> [h | _] = [1,2,3]
    [1, 2, 3]
    iex> h
    1

The variable `_` in Elixir is special in the sense it can never be assigned. Trying to read from it gives an unbound variable error:

    iex> _
    ** (ErlangError) erlang error {:unbound_var, :_}

Although pattern matching allow powerful constructs, its usage is limited. For instance, you cannot make function calls on the left side of the match. The following example is invalid:

    iex> Erlang.lists.flatten([1,[2],3]) = [1,2,3]
    ** (ErlangError) erlang error :illegal_pattern

## 2.5 Key-values

One of the first control flow constructs we usually learn is the conditional `if`. In Elixir, we can write `if` in those two equivalent ways:

    iex> if true, do: 1 + 2
    3
    iex> if true do
    ...>   1 + 2
    ...> end
    3

Both examples above are simply different ways of expressing key-value arguments. Key-value arguments are a list of two-item tuples, where the first element is an atom representing the key and the second is the value. Elixir provides a syntax-shortcut for creating such key-values:

    iex> [a: 1, b: 2]
    [{:a, 1}, {:b, 2}]

In order to manipulate those key-value arguments, we can use the [`Orddict` module](https://github.com/josevalim/elixir/blob/master/lib/orddict.ex):

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

However, most of the times `if` clauses are longer than the examples above. In such cases, we usually use the block format:

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

Elixir supports two syntaxes for key-value blocks: `do`/`end` and `->`/`end`. The first one always binds to the farthest function call, while the latter to the closest. For example, the following expression:

    Enum.map [1,2,3], fn(x) do
      x * 2
    end

Would be parsed as:

    Enum.map([1,2,3], fn(x)) do
      x * 2
    end

Which is not what we want since `do` is binding to the farthest function call, in this case: `Enum.map`. We can fix this by using `->`, forcing the key-value block to bind to the `fn`:

    Enum.map [1,2,3], fn(x) ->
      x * 2
    end

Which is then parsed as:

    Enum.map([1,2,3], fn(x) ->
      x * 2
    end)

A good rule of thumb is: always use `->/end` when defining functions with `fn`, use `do/end` for all other structures. If we follow this rule, everything works transparently.

## 2.6 Control flow structures

In this section we are going to describe Elixir main control structures.

### 2.6.1 If

Refreshing from the section above, all those calls are equivalent:

    if false, do: 1 + 2, else: 10 + 3

    if false do
      1 + 2
    else:
      10 + 3
    end

    # Although this is valid, its usage is discouraged.
    if(false) ->
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

### 2.6.2 Other boolean operators

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

### 2.6.3 Case

In this section we have introduced pattern matching via the `=` operator. Sometimes however it is convenient to match an expression against several expressions until we find a matching one. For such cases, we use `case`:

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

In the example above, the second clause will only match when x is positive. The Erlang VM machine only allows few expressions as guards, they are:

* comparison operators (`==`, `!=`, `===`, `!===`, `>`, `<`, `<=`, `>=`);
* strict boolean operators (`and`, `or`, `not`, `andalso`, `orelse`);
* arithmetic operators (`+`, `-`, `*`, `/`);
* `<>` and `++` as long as the left side is a literal;
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
    is_exception/1

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

Many independent guards clauses can also be given at the same time. For example, consider a function that checks if the first element of a tuple or a list is zero. It could be written as:

    def first_is_zero?(tuple_or_list) when
      elem(tuple_or_list, 1) == 0 or hd(tuple_or_list) == 0 do
      true
    end

However, the example above will always fail because, if the argument is a list, calling `elem` in a list will raise an error. On the other hand, if the element is a tuple, calling `hd` in a tuple will also raise an error. That said, we can rewrite it to become two different clauses:

    def first_is_zero?(tuple_or_list) when
      elem(tuple_or_list, 1) == 0 when
      hd(tuple_or_list) == 0 do
      true
    end

In such cases, if there is an error in one of the guards, it won't affect the next one.

### 2.6.4 Functions

Throughout this guide, we have created many functions in examples. The syntax for creating functions is:

    fn(a, b) -> a + b end

But it could also be written as (the previous example is preferred though):

    fn(a, b, do: a + b)

    fn(a, b) do
      a + b
    end

As an immutable language, the binding of the function is also immutable. This means that setting a variable inside the function does not affect its outer scope:

    x = 1
    (fn -> x = 2 end).()
    x #=> 1

### 2.6.5 Loops

Due to data structure immutability, loops in Elixir (and in functional programming languages) are written differently from conventional imperative languages. For example, in an imperative language, one would write:

    for(i = 0; i < array.length; i++) {
      array[i] = array[i] * 2
    }

In the example above, we are mutating the array which is not possible in Elixir. Therefore, in functional languages recursion happens by calling an anonymous or a named function recursively, until we reach a condition. Consider the example below that manually sums all the items in the list:

    iex> loop [1,2,3], 0 do
    ...> match: [h|t], acc
    ...>   recur(t, h + acc)
    ...> match: [], acc
    ...>   acc
    ...> end
    6

In the example above, we pass  a list `[1,2,3]` and the initial value `0` as arguments to loop. The list `[1,2,3]` is then matched against `[h|t]` which assigns `h = 1` and `t = [2,3]` and 0 is assigned to `acc`.

Then, we add the head of the list to the accumulator `h + acc` and call the loop again using the recur function, passing the tail of the list as argument. The tail will once again match the `[h|t]` until the list empty, matching the final clause which returns the final result of `6`. In other words, the loop is called 4 times until the list is empty and the recursion stops:

    loop [1,2,3], 0
    loop [2,3], 1
    loop [3], 3
    loop [], 6

> Note: `loop/recur` is also a Clojure idiom, although differently from Clojure, `recur` in Elixir does not ensure a tail call was made.

### 2.6.6 Try

The next control-flow mechanism is `try/catch/after`:

    iex> try do
    ...>   throw 13
    ...> catch: number
    ...>   number
    ...> end
    13

`try/catch` is the main mechanism for catching values thrown by Elixir runtime. It also supports an `after` clause that is invoked regardless if the value was caught or not:

    iex> try do
    ...>   throw 13
    ...> catch: nan when not is_number(nan)
    ...>   nan
    ...> after:
    ...>   IO.puts "Didn't catch"
    ...> end
    Didn't catch
    ** throw 13
        erl_eval:expr/3

There is one particularity that applies to `try/catch/after` when compared to other control-flow expressions. The Erlang VM machine considers such clauses unsafe (since they may fail or not) and do not allow variables defined inside `try/catch/after` to be accessed from the outer scope:

    iex> try do
    ...>   new_var = 1
    ...> catch: value
    ...>   value
    ...> end
    1
    iex> new_var
    ** error :undef

The common strategy then is to explicitly all arguments that are required after the `try`:

    { x, y } = try do
      x = calculate_some_value()
      y = some_other_value()
      { x, y }
    catch: _
      { nil, nil }
    end

    x #=> returns the value of x or nil for failures

### 2.6.7 Rescue

While `catch` clauses inside `try` are simply a pattern matching mechanism, `rescue` provides a higher abstraction around exceptions. `rescue` allows a developer to rescue an exception by its name and not by its internal contents. Consider the following examples:

    try do
      raise "some error"
    rescue: RuntimeError
      "rescued"
    end

    try do
      raise "some error"
    rescue: [RuntimeError]
      "rescued"
    end

    # rescue and assign to x
    try do
      raise "some error"
    rescue: x in [RuntimeError]
      # all exceptions respond to message
      x.message
    end

    # rescue all (discouraged) and assign to x
    try do
      raise ArgumentError, message: "unexpected argument"
    rescue: x in _
      x.message
    end

Custom exceptions can be defined using the `defexception` macro. Check [the exceptions file for some examples](https://github.com/josevalim/elixir/tree/master/lib/exception.ex).

### 2.6.8 Receive

The last control-flow mechanism we are going to discuss is essential to Elixir's and Erlang's actor mechanism. In Elixir, every code run in processes that exchange messages between them. Those processes are not Operating System processes (they are actually quite light-weight) but called so since they do not share state with each other.

In order to exchange messages, each process has a mailbox where the received messages are stored. The `receive` mechanism allows us to go throw this mailbox searching for a message that matches the given pattern. Here is an example that uses the arrow operator `<-` to send a message to the current process and then collects this message from this mailbox:

    # Get the current process id
    iex> current_pid = self()

    # Spawn another process that will send a message to current_pid
    iex> spawn fn(do: current_pid <- { :hello, self() })
    <0.36.0>

    # Collect the message
    iex> receive do
    ...> match: { :hello, pid }
    ...>   IO.puts "Hello from #{pid}"
    ...> end
    Hello from <0.36.0>

You may not see exactly `<0.36.0>` back, but something similar. If there are no messages in the mailbox, the current process will hang until a matching message arrives, unless an after clause is given:

    iex> receive do
    ...> match: :waiting
    ...>   IO.puts "This may never come"
    ...> after: 1000 # 1 second
    ...>   IO.puts "Too late"
    ...> end
    Too late

In most cases, we don't send messages directly with `<-` nor write `receive` control expressions. Instead, we use many of the abstractions provided by OTP which will be discussed later.

## 2.7 Default functions

Elixir ships with many default functions automatically available in the current scope. Besides all the control flow expressions seen above, Elixir also adds: `elem` and `setelem` to read and set values in tuples, `inspect` that returns the representation of a given data type as string and many others. [Many of these functions with documentation and examples are available in `Elixir::Builtin`](https://github.com/josevalim/elixir/tree/master/lib/elixir/builtin.ex) and [Elixir special forms are available in `Elixir::SpecialForms`](https://github.com/josevalim/elixir/tree/master/lib/elixir/special_forms.ex).

Besides the functions provided by Elixir, most of the root functions from Erlang are also available. The function `length`, `is_list`, `is_number` and many others we discussed above comes from Erlang. [The full documented list is available on the OTP documentation page](http://www.erlang.org/doc/man/erlang.html).

All those functions and control flow expressions are essential for building Elixir programs. The next chapter will then discuss how to organize our code into modules, so it can be easily re-used between different components.

[Chapter 1: Introduction](https://github.com/josevalim/elixir/blob/master/docs/1_introduction.md) | [Index](https://github.com/josevalim/elixir/blob/master/docs/0_index.md) | [Chapter 3: Modules](https://github.com/josevalim/elixir/blob/master/docs/3_modules.md)
