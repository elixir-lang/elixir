# Elixir

Elixir is a programming language built on top of Erlang. As Erlang, it is a functional language with strict evaluation, single assignment and dynamic typing built to support distributed, fault-tolerant, non-stop applications with hot swapping. Elixir allows you to invoke Erlang modules without a need to convert data types, therefore there is no hit in performance when invoking existing Erlang code.

The main difference between Elixir and Erlang is its syntax inspired by Ruby and method dispatching and metaprogramming on top of modules.

# Usage

Elixir is still in development. If you want to help building it or are just looking for some fun, you can get started now! First, you need to clone this repository to your machine, compile and test it:

    $ git clone https://github.com/josevalim/elixir.git
    $ cd elixir
    $ make test

    $ bin/elixir -v
    Elixir 0.3.1.dev

If tests fail, it is likely you have an outdated Erlang version. You can check your Erlang version by calling ``erl`` in the command line. You will see some information as follow:

    Erlang R14B01 (erts-5.8.2) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Elixir requires Erlang R14B01 or later version to execute (R14A and R14B **do not work**). If you have the correct version and tests still fail, feel free to open an issue in the issues tracker on Github. If all tests pass, you are ready to play with Elixir!

This README provides a length explanation about Elixir in the Learning Elixir section below. There are also some examples [in the examples folder](https://github.com/josevalim/elixir/tree/master/examples/) that you can run by executing the `bin/elixir EXAMPLE` and an interactive Elixir available as `bin/iex`. Feel free to build your own examples and study the language better.

## Contributing & Roadmap

Currently, there is an effort to improve Elixir Standard Library. As much of Elixir's STDLIB is written in Elixir and tested in Elixir, you don't need to be an advanced Erlang user to improve the language, just know the [OTP](http://www.erlang.org/doc/) a bit. As an example, you may take a look at the [List implementation](https://github.com/josevalim/elixir/tree/master/lib/list.ex) and [its tests](https://github.com/josevalim/elixir/tree/master/test/elixir/list_test.ex) to check how simple it is.

If you want to contribute to Elixir, the code is organized as follow:

* `include`, `src` - Both directories contain the part of the source code written in Erlang. `leex` and `yecc` were used as tokenizer and parser respectively;

* `lib` - Contains Elixir's STDLIB, written in Elixir;

* `test/elixir` - Tests for Elixir's STDLIB, written in Elixir. For this purpose, Elixir ships with a small unit test library called `ExUnit`;

* `test/erlang` - Contains tests for Elixir, written in Erlang. Usually, just internal stuff is tested here. The preferred way to test is in Elixir itself.

If you are interested, check out the ROADMAP.md file in the repository or keep reading this README to find items to be implemented.

## Important links

* [Mailing list](http://groups.google.com/group/elixir-lang-core)
* #elixir-lang on freenode IRC
* [Textmate Bundle for Elixir](https://github.com/josevalim/elixir-tmbundle)

# Learning Elixir

This is a basic introduction into Elixir. Some sections have a paragraph called "To be implemented", they represent parts of Elixir that was not implemented yet and that are under discussion.

This introduction borrowed its guidelines from [Learn You Some Erlang](http://learnyousomeerlang.com/), a great resource to learn Erlang which will be referenced several times during this introduction.

## Hello World

Let's start with a simple hello world. The first step is to create a new file called "hello.ex" inside Elixir repository with the following contents:

    module Hello
      def world
        IO.puts "Hello World"
      end
    end

Now, we can compile this file to the current directory:

    bin/elixirc hello.ex -o .

Notice that a .beam file was added to the current directory with the compiled code. We can execute it by invoking the method `world` in the module `Hello` in the same directory:

    bin/elixir -e "Hello.world"

And you will see "Hello World" printed! This example works because Elixir automatically loads the compiled files in the current directory. If your compiled files are in other directories, you can pass those new directories to `bin/elixir` using `-pa` and `-pz` as options. Type `bin/elixir` with no arguments for more information.

When you are building libraries in Elixir, those are the main steps you should take. Write your code, compile it and run it! However, sometimes it is nice to just put some code together and run it, without a explicit compilation step. For that, elixir allows you to easily create scripts. Let's create a new file "hello.exs" with the following contents:

    IO.puts "Hello World"

And now run it:

    bin/elixir hello.exs

And it works again! Notice we used the extension `.exs` instead of `.ex` here. This is just a convention, Elixir does not treat `.exs` files differently from `.ex` files in any way! In fact, you could even try to compile the `.exs` file:

    bin/elixirc hello.exs -o .

When you do that, you can see that "Hello World" is printed as well. This is because Elixir actually executes the files to compile them. This is the key to many Elixir features, as we are going to see later.

Also notice that Elixir ships with an interactive console that you can use for most examples in this tutorial, you can start it with:

    bin/iex

Enjoy!

## Some notation

Before we start, notice that comments in Elixir are, as in Erlang, done with %.

    % This is a commented line

Throughout this introduction, `% =>` represents the result of an expression:

    1 + 1 % => 2

## Basic Types

### Numbers

Elixir supports both Integer and Floats:

    2 + 15       % => 17
    - 13 * 10    % => -130
    1986 - 1985  % => 1
    5 / 2        % => 2.5
    4 / 2        % => 2.0

Notice that, as Erlang, "/" always returns a Float. If you want to have integer-to-integer division and the modulo operator, you should use `div` and `rem`:

    5 div 2  % => 2
    5 rem 2  % => 1

Several operations can also be done in a single expression, obeying the normal precedence rules:

    50 * 10 - 490     % => 10
    (50 * 10) - 490   % => 10
    -(50 * 10) - 490  % => -990

In Elixir, you can dispatch methods to data types, this is further explained later:

    -1.abs    % => 1
    5.div(2)  % => 2

It comes as no surprise that + is also a method:

    1.+(2)  % => 3

Finally, notice that Elixir allows you to include "_" in numbers (as in Ruby). This improves the readability when working with large numbers:

    1_052_672

#### Documentation:

* <https://github.com/josevalim/elixir/tree/master/lib/integer.ex>
* <https://github.com/josevalim/elixir/tree/master/lib/float.ex>
* <https://github.com/josevalim/elixir/tree/master/lib/numeric.ex>

> #### To be implemented
>
> Currently, there is no support to enter numbers in bases other than base 10. This is the current API in Erlang (although the best API for Elixir is under discussion):
>
>     2#101010.  % => 42
>     8#0677.    % => 447
>     16#AE.     % => 174
>

### Atoms

Elixir also has Atoms, called Symbols in other languages like Ruby. Although its syntax was borrowed from Lisp:

    'atom
    'Atom
    'atom_without_spaces

Atoms are literals, with their own value as name. An atom 'symbol is an atom 'symbol everywhere, with exactly the same value. Atoms start with a single quote and should not have spaces (spaces delimit the atom end). Atoms with spaces are represented by wrapping them in quotes:

    '"Atom with Spaces"

As in Erlang and Ruby, Atoms are not garbage collected, so remember to not generate atoms dynamically, otherwise you will run out of memory sooner rather than later.

#### Documentation:

* <https://github.com/josevalim/elixir/tree/master/lib/atom.ex>

### Booleans

As in Erlang, the boolean values are simply atoms named true and false. However, to avoid writing `'true` and `'false`, Elixir also allows you to simply write `true` or `false`. The following are all equivalent and will yield `1` as result:

    if 'true
      1
    else
      2
    end

    if true
      1
    else
      2
    end

    if 'false
      2
    else
      1
    end

    if false
      2
    else
      1
    end

Besides those two boolean values, Elixir also has a `nil` value which is simply an atom as well. `nil` also evaluates to false in conditionals.

### Tuples

Tuples are used to organize many terms together when you know how many terms there are. As in Erlang, a tuple is written in the following form:

    % A tuple containing all boolean values
    { true, false }

    % A tuple that may represent a point with coordinates X and Y
    { 10, 20 }

    % An empty tuple
    Tuple.empty

Tuples and lists (which are going to see next), are zero-indexed in Elixir while they are one-indexed in Erlang. You can retrieve a specific element using []:

    {'a,'b,'c}[1]  % => 'b
    {'a,'b,'c}[2]  % => 'c

#### Documentation:

* <https://github.com/josevalim/elixir/tree/master/lib/tuple.ex>

### Lists

Lists are the main structure in Elixir (as in any other functional language) and can contain anything:

    % Some list with elements
    ['atom, 1, 2, 3, { 'some, 'tuple }]

    % An empty list
    []

Elixir Standard Library has a bunch of methods to interact with lists:

    [1, 2, 3].length       % => 3
    ['a, 'b, 'c][1]        % => 'b

As in Elixir `+` is simply a method like any other (and not an arithmetic operator as in Erlang), it can also be used to add arrays:

    [1, 2, 3] + [4, 5, 6]  % => [1,2,3,4,5,6]

Lists in Erlang and Elixir are implemented as linked lists. This means prepending an item to the list is quite fast, but appending is much slower. Therefore we have a special syntax to prepend one or more items to a list:

    list = [2,3,4]

    % Don't do this:
    [1]   + [2,3,4]  % => [1,2,3,4]
    [0,1] + [2,3,4]  % => [0,1,2,3,4]

    % Do this instead:
    [1|list]    % => [1,2,3,4]
    [0,1|list]  % => [0,1,2,3,4]

Most of the power in lists comes when used together with functions:

    [1, 2, 3].map do (x)
      x * 2
    end  % => [2, 4, 6]

    [1, 2, 3].foldl 0, do (x, acc)
      acc + x
    end  % => 6

The examples above uses functions using the `do/end` syntax. Don't worry about them now, we are going to take a better look at them later.

### Ordered Dicts

Elixir provides a first-class syntax to deal with ordered dictionaries (similar to Hashes in Ruby).

    % A dict with 'a and 'b as keys and 1 and 2 as their respective values
    { 'a: 1, 'b: 2 }

    % An empty dict
    {}

Elixir dictionary implementation is backed up by [the orddict module](http://www.erlang.org/doc/man/orddict.html) in OTP. Notice that Erlang ordered dicts are **not** ordered in the order items are added, but rather using Erlang ordering of terms. You can learn more about Erlang ordering by [reading this section from Learn You Some Erlang](http://learnyousomeerlang.com/starting-out-for-real#bool-and-compare).

Ordered Dicts are recommended to deal with small amount of data. Other data structures are recommended to deal with a huge amount and you can [read more about others key-value store](http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#key-value-stores), but remember that most of them are not implemented in Elixir yet.

#### Documentation:

* <https://github.com/josevalim/elixir/tree/master/lib/ordered_dict.ex>

### Bit strings

Elixir has a similar syntax to Erlang for handling bit strings:

    % A bit string with three elements
    <<1, 17, 42>>

    % Converting a bit string to a list
    <<1, 17, 42>>.to_list  % => [1, 17, 42]

Elixir also allows to specify the size for bit strings, using the same syntax as Erlang:

    % A bit string with size 4, because we specify that 42 is a 16-bits segment
    <<1, 17, 42:16>>

By default, the bit string type in both Elixir and Erlang is integer. That said, the following is invalid:

    <<3.14>>

Instead, you need explicitly specify it as a float:

    <<3.14|float>>

Notice the syntax above is a bit different from Erlang. Erlang uses `/` to specify the type, Elixir uses `|`. This allows Elixir, differently from Erlang, to have expressions inside bit string:

    <<1+2>>

In general, everything that applies to Erlang bit string applies to Elixir bit string. You can [read more about them on Erlang's documentation](http://www.erlang.org/doc/programming_examples/bit_syntax.html).

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/bit_string.ex>
* <http://www.erlang.org/doc/programming_examples/bit_syntax.html>

### Strings

In Erlang, strings are a list of chars:

    "hello" == [104, 101, 108, 108, 111]

This is expensive because each character uses 8 bytes of memory, not 8 bits! Erlang stores each character as a 32-bit integer, with a 32-bit pointer for the next item in the list.

Elixir takes a different approach to strings. Strings in Elixir are handled as UTF-8 binaries. Since a binary is nothing more than a bit string, where the number of bits is a multiple of 8, we can create strings using the bit string syntax:

    <<72, 73, 74>>  % => "HIJ"

When a bit string with multiple of 8 bits is created, it is automatically mapped to a string. However, you will rarely use the syntax above as Elixir provides the more traditional quote syntax to handle strings:

    % The famous "hello world" string
    "hello world"

    % A string converted to a char list:
    "hello".to_char_list  % => [104, 101, 108, 108, 111]

    % Convert a char list back to a binary/string:
    [104, 101, 108, 108, 111].to_bin % => "hello"

    % Notice that to_s in a list is not the same as to_bin.
    % It returns the list represented as a string instead:
    [104, 101, 108, 108, 111].to_s % => "[104,101,108,108,111]"

    % Strings are UTF-8
    "Arrow ⇧ up".length  % => 10

Keep in mind that, as Elixir strings are different from Erlang strings, sometimes you may need to convert Elixir strings to a char list and vice-versa when invoking Erlang methods using the methods `to_char_list` and `to_bin` as seen above.

Finally, strings also support interpolation:

    "string #{'with} interpolation"  % => "string with interpolation"
    "1 + 1 = #{1 + 1}"               % => "1 + 1 = 2"

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/string.ex>

### Functions

Functions are an important aspect of Elixir as in any functional programming language. Functions are created in Elixir with the keywords `->` or `do`:

    my_function = do
      1 + 2
    end

    my_function.call() % => 3

Some functions expect arguments:

    my_function = do (x, y)
      x + y
    end

    my_function.call(1, 2) % => 3

The stab operator is used to represent inline functions:

    another_function = -> (x, y) x * y
    another_function.call(1, 2) % => 2

Notice that, whenever using one-line functions, if you need parentheses inside the expression, you are required to give empty parentheses arguments, for example:

    % This works as expected:
    my_function = -> 1 + 2
    my_function.call % => 3

    % This won't work and it raises a syntax error as it is ambiguous
    my_function = -> (1 + 2)

    % This works as well:
    my_function = -> () (1 + 2)

Inline functions are convenient for method calls:

    [1,2,3].map(-> (x) x * 2)   % => [2,4,6]

As the parenthesis are not required, we can remove them:

    [1,2,3].map -> (x) x * 2    % => [2,4,6]

Other examples using the multiline syntax:

    [1,2,3].foldl(0, do (x, acc)
      acc + x
    end) % => 6

Again, removing the parentheses, improves readability:

    [1,2,3].foldl 0, do (x, acc)
      acc + x
    end % => 6

Besides using call, we can also invoke functions using `apply`, brackets `[]` and the `.()` syntax:

    my_function = -> (x, y) x + y
    my_function.apply([1, 2])   % => pass arguments in a list (just like JavaScript)
    my_function[1, 2]
    my_function.(1, 2) % => 3

Another useful extension Elixir adds to functions is the easy generation of anonymous functions. For instance, suppose you have a list of cars and you want to get their names. A way to do that would be:

    cars.map -> (c) c.name

However, you can easily generate an anonymous functions that does the same:

    cars.map _.name

Anonymous functions can also be generated with arguments, so the map expressions we saw above:

    [1,2,3].map -> (x) x * 2   % => [2,4,6]

Could actually be rewritten as:

    [1,2,3].map _.*(2)

Currently, functions do not support partial applications or pipes, but such features will be added down the road.

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/function.ex>

## Variables and Pattern Matching

Variables in Elixir work differently from Erlang. You can assign values to the same variable several times:

    x = 1
    x = 2

You can force a match to happen prefixing `~` to the variable name:

    ~x = 3  % => Raises a bad match error, because x was last bound to 2

In Erlang/Elixir terms, a variable that was not assigned yet is called *unbound variable*. Let's see more examples:

    % Let's bound the variable x to 'foo
    x = 'foo

    % Now let's match a tuple with other tuple.
    % Since x is already bound, we are comparing x with 'baz and it will fail:
    { ~x, y } = { 'baz, 'bar }

    % In this case, we compare 'x with 'foo and it matches.
    % Since y is unbound, we assign 'bar to it:
    { ~x, y } = { 'foo, 'bar }

    x  % => 'foo
    y  % => 'bar

For lists, we can use the same syntax to prepend an item on pattern matching, easily retrieving the head and tail:

    [h|t] = [1,2,3]
    h  % => 1
    t  % => [2,3]

    % Raises an error because h was already assigned to 1 and 1 does not match 2
    [~h|t1] = [2,3,4]

Elixir will often complain if you bound a value to a variable but never use it. For instance, imagine that you want to get just the first element of a tuple with three items:

    {x, y, z} = {1, 2, 3}

If you don't use the `y` and `z` variables, Elixir will show you some warnings. For this reason, you could use `_` instead:

    {x, _, _} = {1, 2, 3}

The variable `_` is always unbound:

    _ = 1
    _   % => Raises that variable '_' is unbound

Sometimes having several occurrences of `_` in the same expression is confusing, so you can do this instead:

    {x, _y, _z} = {1, 2, 3}

The values 2 and 3 will be bound to the variables `_y` and `_z`, but Elixir won't complain if you eventually don't use them.

Keep in mind that the number of expressions allowed in pattern matching are limited. You cannot invoke methods, use interpolated strings, retrieve constants and so on. Therefore, this is invalid:

    1.abs = -1

Ordered dicts are also allowed in pattern matching but there is one important restriction: you are responsible to make their order match. Therefore, this won't work:

    dict = { 2: 4, 1: 2 }
    { 2: 4, 1: 2 } = dict

This fails because the `dict` variable is ordered, so it is actually represented as `{1: 2, 2: 4}`. Remember that `OrderedDict`s are ordered according to Elixir ordering of terms and not the order new items are added. This ordering rule is important to allow us to bound variables to key-values:

    dict = { 2: 4, 1: 2 }

    % This matches as the left expression is in the correct order
    { 1: 2, 2: 4 } = dict

    % This matches and bound x and y to 2 and 4
    { 1: 2, x: y } = dict

### Method signatures

Finally, pattern matching can also be implemented in methods signatures. Here is the classic Fibonacci example:

    module Math
      def fibonacci(0)
        0
      end

      def fibonacci(1)
        1
      end

      def fibonacci(n)
        fibonacci(n - 1) + fibonacci(n - 2)
      end
    end

    Math.fibonacci(0)   % => 0
    Math.fibonacci(1)   % => 1
    Math.fibonacci(3)   % => 2
    Math.fibonacci(10)  % => 55

Notice the example above is not tail call optimized. We will discuss modules, methods and optimizations later.

## Operators

Operators can be binary or unary operators:

    (expression) binary_op (expression)
    unary_op (expression)

### Term comparisons

Elixir term comparisons operators are close to Erlang ones, except `!=`, `===`, `!==` and `<=` which maps to Erlang `/=`, `=:=`, `=/=` and `=<`.

<table>
<tr>
  <td><strong>Operator</strong></td>
  <td><strong>Description</strong></td>
</tr>
<tr>
  <td>==</td>
  <td>equal to</td>
</tr>
<tr>
  <td>!=</td>
  <td>not equal to</td>
</tr>
<tr>
  <td>&lt;=</td>
  <td>less than or equal to</td>
</tr>
<tr>
  <td>&lt;</td>
  <td>less than</td>
</tr>
<tr>
  <td>&gt;=</td>
  <td>greater than or equal to</td>
</tr>
<tr>
  <td>&gt;</td>
  <td>greater than</td>
</tr>
<tr>
  <td>===</td>
  <td>exactly equal to</td>
</tr>
<tr>
  <td>!==</td>
  <td>exactly not equal to</td>
</tr>
</table>

As in Erlang, Elixir can order different types according to the following order:

    number < atom < reference < fun < port < pid < tuple < modules < blank slates < list < bit string

Lists are compared element by element. Tuples are ordered by size, two tuples with the same size are compared element by element. If one of the compared terms is an integer and the other a float, the integer is first converted into a float, unless the operator is one of `===` and `!==`.

All term comparison operators return a boolean expression.

### Arithmetic operators

<table>
<tr>
  <td><strong>Operator</strong></td>
  <td><strong>Description</strong></td>
  <td><strong>Argument</strong></td>
</tr>
<tr>
  <td>+</td>
  <td>unary +</td>
  <td>number</td>
</tr>
<tr>
  <td>-</td>
  <td>unary -</td>
  <td>number</td>
</tr>
<tr>
  <td>+</td>
  <td></td>
  <td>anything</td>
</tr>
<tr>
  <td>-</td>
  <td></td>
  <td>anything</td>
</tr>
<tr>
  <td>*</td>
  <td></td>
  <td>anything</td>
</tr>
<tr>
  <td>/</td>
  <td>returns a float</td>
  <td>anything</td>
</tr>
<tr>
  <td>div</td>
  <td>returns an integer</td>
  <td>anything</td>
</tr>
<tr>
  <td>rem</td>
  <td>returns an integer</td>
  <td>anything</td>
</tr>
</table>

Except by the two unary operators, all other operators can be overloaded. For instance, we can concatenate two lists by using the `+` operator:

    [1,2,3] + [4,5,6]  % => [1,2,3,4,5,6]

This is the same as:

    [1,2,3].+([4,5,6]) % => [1,2,3,4,5,6]

Notice however that we cannot add a list with a number:

    [1,2,3] + 1  % => Raises an error

Also, Elixir keeps the same semantics as Erlang in the sense the `/` operator always returns a float when numbers are given as argument. The `div` and `rem` operators are used to deal with integers:

    2 / 1    % => 2.0
    6 div 4  % => 1
    6 rem 4  % => 2

### Bitwise operators

To be implemented/written.

### Strict boolean operators

Elixir provides the following operators to deal strictly with booleans:

<table>
<tr>
  <td><strong>Operator</strong></td>
  <td><strong>Erlang equivalent</strong></td>
  <td><strong>Description</strong></td>
</tr>
<tr>
  <td>and</td>
  <td>and</td>
  <td>Both expressions must return boolean</td>
</tr>
<tr>
  <td>or</td>
  <td>or</td>
  <td>Both expressions must return boolean</td>
</tr>
<tr>
  <td>andalso</td>
  <td>andalso</td>
  <td>First expression must return boolean, short-circuit operator</td>
</tr>
<tr>
  <td>orelse</td>
  <td>orelse</td>
  <td>First expression must return boolean, short-circuit operator</td>
</tr>
<tr>
  <td>not</td>
  <td>not</td>
  <td>Unary operators, expression must be a boolean</td>
</tr>
</table>

### Logical operators and control-flow

Elixir provides three operators general purposes operators:

<table>
<tr>
  <td><strong>Operator</strong></td>
  <td><strong>Description</strong></td>
</tr>
<tr>
  <td>&&</td>
  <td>and</td>
</tr>
<tr>
  <td>||</td>
  <td>or</td>
</tr>
<tr>
  <td>!</td>
  <td>not</td>
</tr>
</table>

Remember that everything, except `false` and `nil`, evaluates to `true`:

    !false       % => true
    !true        % => false
    !Object.new  % => false

Both `&&` and `||` are actually control structures. They do not return a boolean but the last evaluated expression:

    1 && 2       % => 2

    true || false       % => true
    'atom || 'another   % => 'atom
    false || 'another   % => 'another

    false && IO.puts("I will never be executed")

    1 || IO.puts("I will never be executed")
    true || IO.puts("I will never be executed")

### Precedence

Operator precedence in falling priority:

<table>
<tr>
  <td><strong>Operator</strong></td>
  <td><strong>Associativity</strong></td>
</tr>
<tr>
  <td>. #</td>
  <td>Left</td>
</tr>
<tr>
  <td>+ - ! not</td>
  <td>Non associative (unary operators)</td>
</tr>
<tr>
  <td>/ * div rem</td>
  <td>Left</td>
</tr>
<tr>
  <td>== != &lt; &lt;= &gt; &gt;= === !==</td>
  <td>Left</td>
</tr>
<tr>
  <td>&lt;-</td>
  <td>Right</td>
</tr>
<tr>
  <td>and andalso</td>
  <td>Left</td>
</tr>
<tr>
  <td>or orelse</td>
  <td>Left</td>
</tr>
<tr>
  <td>&&</td>
  <td>Left</td>
</tr>
<tr>
  <td>||</td>
  <td>Left</td>
</tr>
</table>

## if/else and case/match

Elixir, differently from Erlang, has a more conventional if/else structure:

    list = [1,2,3]

    if list.include?(4)
      IO.puts "it includes 4"
    elsif list.include?(5)
      IO.puts "it includes 5"
    else
      IO.puts "it does not include 4 or 5"
    end

Everything in Elixir, except `false` and `nil`, evaluates to `true`.

On the other hand, the case/match structure from Elixir is quite similar to Erlang's:

    case {1,2,3}
    match {3,2,x}
      x * 2
    match {1,2,x}
      x * 2
    end

As you can notice,`case/match` uses pattern matching. If no case expression matches, an error is raised. Elixir also allows an `else` clause in case/match, which is the same as `match _`:

    case {4,5,6}
    match {3,2,x}
      x * 2
    match {1,2,x}
      x * 2
    else
      10
    end

Finally, `case/match` expressions can be inlined and grouped, providing a more compact syntax:

    case {4,5,6}
    match {3,2,x}, {1,2,x} then x * 2
    else 10
    end

Currently there is no support for guard expressions as in Erlang, although it may be implemented at some point.

## Exceptions

Similarly to Erlang, Elixir has three kinds of exceptions. They are raised with the methods (and not keywords!) `throw`, `error` and `exit`. You can [read more about each type on Learn You Some Erlang](http://learnyousomeerlang.com/errors-and-exceptions#raising-exceptions).

To handle these exceptions, Elixir uses a syntax similar to Ruby:

    try
      self.throw {1,2}
    catch {1,2}
      IO.puts "Rescued {1,2}"
    end

Similar to the `match` syntax, you can catch different values in the same clause:

    try
      self.throw {1,2}
    catch {1,2}, {3,4}
      IO.puts "Rescued a tuple"
    end

In order to catch an `error` or an `exit`, you need to be explicit:

    try
      self.error {1,2}
    catch {1,2}
      IO.puts "I will never get a tuple {1,2}"
    catch 'error: {1,2}
      IO.puts "Rescue an error with {1,2}"
    end

You must use the keyword `after` if you want to execute some code regardless if there was an exception or not:

    try
      self.error {1,2}
    catch {1,2}
      IO.puts "I will never get a tuple {1,2}"
    after
      IO.puts "I am always executed"
    end

It is important to keep in mind that **tail calls are not optimized** inside try blocks. This is expected as the runtime needs to keep the backtrace in case an exception occur. Also, notice that variables created inside try/catch/after clauses do not leak to the outer scope.

    try
      foo = 13
    end

    foo % => raises undefined variable or local method foo error

    try
      foo = 13
    after
      IO.puts "I am always executed"
    end

    foo % => raises undefined variable or local method foo error

When used inside methods, the `try/end` can be omitted:

    def some_method
      self.error {1,2}
    catch {1,2}
      IO.puts "I will never get a tuple {1,2}"
    after
      IO.puts "I am always executed"
    end

Again, be careful when using this pattern with tail calls, as the try block is not optimized. For instance, consider this method:

    def some_method([h|t], value)
      value = method_that_may_raise_error(h)
      some_method(t, value)
    catch {1,2}
      IO.puts "I will never get a tuple {1,2}"
    after
      IO.puts "I am always executed"
    end

It should actually be written as:

    def some_method([h|t], value)
      value = try
        method_that_may_raise_error(h)
      catch {1,2}
        IO.puts "I will never get a tuple {1,2}"
      after
        IO.puts "I am always executed"
      end
      
      some_method(t, value)
    end

### List of errors

Here is a list of runtime errors that can be raised by Elixir:

*   `{ 'builtin_not_allowed, { method, builtin } }`

    Invoking `method` not allowed on the `builtin` structure. Builtins are all structures that comes directly from Erlang, they are: String, Integer, Float, Tuple, List, OrderedDict and so forth. Binding and setting instance variables are currently disabled on builtins;

*   `{ 'module_defined, { name, file, line } }`

    An module with `name` was already defined on `file` at `line`. This is a common error to appear during compilation time as the following valid Ruby pattern is not valid in Elixir:

        module Foo
          module Bar
          end
        end

        module Foo
          module Baz
          end
        end

    In the example above, we are reopening `Foo` to add a `Baz` module. This is invalid in Elixir as modules cannot be reopened. To handle this, just define `Baz` directly as `Foo::Baz`:

        module Foo::Baz
        end

*   `{ 'no_local_method, { name, arity, module } }`

    There isn't a local method with the given `name` and `arity` in `module`;

*   `{ 'not_a_module, { method, other } }`

    `method` failed because `other` is not a module;

*   `{ 'no_module, name }`

    A module with `name` could not be found;

*   `{ 'no_callback, { name, arity, structure } }`

    The callback `name` with `arity` was not implemented in `structure`. Raised when a structure is given as callback but does not comply to all conditions;

*   `{ 'bad_ivar, name }`

    The `name` given is not an atom and cannot be given as internal variable name;

*   `{ 'bad_binding, { module, actual } }`

    Could not bind to module `module` as `__bound__` callback returned `actual`;

*   `{ 'bad_ivars, value }`

    `value` given to `@()` or `set_ivars` is not an `OrderedDict` or it is an `OrderedDict` but not all keys are atoms;

*   `{ 'internal_method_overridden, { method, arity } }`

    The `method` with arity `arity` is defined automatically by Elixir and cannot be overridden.

## Strings, Atoms, Regular Expressions, Interpolation and Sigils

In Elixir, we have the following basic types related to Strings:

    % Strings (utf8 by default and represented as binaries)
    "string"
    "string #{'with} interpolation"    % => "string with interpolation"

    % Integer representation of a character
    $a    % => 97
    $b    % => 98
    $\\   % => 92
    $\(   % => 40

    % A string represented as a list of chars (all four expressions below allow interpolation)
    $"string"    % => [115,116, 114, 105, 110, 103]
    $(string)    % => [115,116, 114, 105, 110, 103]
    $[string]    % => [115,116, 114, 105, 110, 103]
    ${string}    % => [115,116, 114, 105, 110, 103]

    % A binary representing the list of chars above
    <<115, 116, 114, 105, 110, 103>>

    % Erlang Atoms or Ruby Symbols
    'atom
    '"atom with space and interpolation"
    '(atom with space and interpolation)
    '[atom with space and interpolation]
    '{atom with space and interpolation}

Besides these basic types, we also have string sigils. Here is one example:

    % Regular expressions
    %% Without interpolation
    ~r(regexp)
    ~r[regexp]
    ~r{regexp}
    ~r"regexp"

    %% With interpolation
    %% It also accepts [], {} and "" as separators as above
    ~R(regexp #{1 + 1} interpolation)

    %% With regexp operators
    ~r(foo)im

All string sigils follow the same set of rules. They start with a ~ followed by a letter and the string is delimited by a separator. The available separators are (), [], {} and "". If the letter after ~ is lowercased, no interpolation is allowed, if uppercased, interpolation is allowed. A couple more examples:

    % Another way to create strings
    ~q(string without interpolation)
    ~Q{string without interpolation}

    % Another way to create atoms
    ~a"atom without interpolation"
    ~A[atom with interpolation]

    % Another way to create a list of chars
    ~l(string)  % => [115,116, 114, 105, 110, 103]
    ~L{string with interpolation}

    % A list of words (to be implemented)
    ~w(foo bar baz)        % => ["foo", "bar", "baz"]
    ~W{foo #{'bar} baz}    % => ["foo", "bar", "baz"]

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/string.ex>
* <https://github.com/josevalim/elixir/tree/master/lib/atom.ex>
* <https://github.com/josevalim/elixir/tree/master/lib/regexp.ex>

## Heredoc

Elixir also has HEREDOCs to make easier to handle big strings:

    string = ~~
      This is a string which
      preserves whitespace at
      the beginning and also
      handles #{'interpolation}
    ~~

Similar to Ruby, HEREDOCs allow an identifier right after the initial three quotes:

    string = ~~HTML
      <p>Nice!</p>
    ~~

This allows to identify the content and most text editor uses it to properly syntax highlight it. Besides, you can add Elixir code after the HEREDOC and they still are properly evaluated:

    string = ~~STRING + "123"
    abc
    ~~

    string % => "abc\n123"

Consequently, this feature allows multiple HEREDOCs:

    list = [~~ONE, ~~TWO, ~~THREE]
    this is the first string
    ~~
    this is another one
    ~~
    this is the third. cool, isn't?
    ~~

    list[0] % => "this is the first string\n"
    list[1] % => "this is another one\n"
    list[2] % => "this is the third. cool, isn't?\n"

## Invoking Erlang Methods

Invoking Erlang methods with elixir is quite trivial:

    % Accessing the is_atom BIF from Erlang.
    % This is the same as `is_atom(foo)` in Erlang.
    Erlang.is_atom('foo)  % => true

    % Accessing the function delete from module lists.
    % This is the same as `lists:member(1, [1,2,3])` in Erlang.
    Erlang.lists.member(1, [1,2,3]) % => true

As there is no conversion between most Erlang data types and Elixir ones, there is no performance hit in invoking Erlang methods. The only exception are strings that are binaries in Elixir and may need to be converted to char lists in some specific erlang modules. More details were outline in the BitString and String sections above.

Finally, notice that `Erlang` is just a proxy that is converted to erlang calls at compile time.

## List and Bit string comprehensions

List comprehensions allow you to quickly build a list from another list:

    [n*2 for n in [1,2,3,4]]  % => [2,4,6,8]

The comprehension is defined with the `for` keyword which accepts several expressions. Those expressions can be generators, as in `x in [1,2,3,4]`, or filters:

    % A comprehension with a generator and a filter
    [n for n in [1,2,3,4,5,6], X rem 2 == 0]  % => [2,4,6]

    % A comprehension with two generators
    [x*y for x in [1,2], y in [2,3]]  % => [2,3,4,6]

There are two types of generators in Elixir/Erlang: list and bit string generator:

    % A list generator:
    [n*2 for n in [1,2,3,4]]  % => [2,4,6,8]

    % A bit string generator:
    [n*2 for <<n>> in <<1,2,3,4>>]  % => [2,4,6,8]

Bit string generators are quite useful when you need to organize bit string streams:

    pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>
    [{r,g,b} for <<r:8,g:8,b:8>> in pixels ]  % => [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

Remember, as strings are binaries and a binary is a special kind of bit string where the number of bit is a multiple of 8, we can also use strings on comprehensions. For instance, the example below removes all white space characters from a string:

    <<c for <<c>> in " hello world ", c != $\s>> % => "helloworld"

Elixir does its best to hide the differences between list and bit string generators from you. However, there is a special case due to Erlang limitation that you need to explicitly tell Erlang that a list is being given as argument:

    % This will fail because when Elixir sees that the left side
    % of the in expression is a bit string, it expects the right side
    % to be a bit string as well:
    [n*2 for <<n>> in [<<1>>,<<2>>,<<3>>]  % => [2,4,6]

    % You need to be explicit and use inlist:
    [n*2 for <<n>> inlist [<<1>>,<<2>>,<<3>>]  % => [2,4,6]

    % inbin is also available:
    [n*2 for <<n>> inbin <<1,2,3>>]  % => [2,4,6]

You can [read more about list and bit string comprehensions in Learn You Some Erlang](http://learnyousomeerlang.com/starting-out-for-real#list-comprehensions).

## Modules

As Erlang, Elixir code is mainly organized around modules. Here is a very simple example:

    module Hello
      def world
        IO.puts "Hello World"
      end
    end
    
    Hello.world % => "Hello World"

### Method Visibility

One important aspect of modules is the method visibility. Elixir provides two different visibilities: *public* and *private*. All methods are public by default, this means that a method can be called from anywhere, at any time:

    module Example
      def public_method
        13
      end
      
      def calling_public_method
        public_method
      end
      
      def calling_public_method2
        self.public_method
      end
    end
    
    Example.public_method           % => 13
    Example.calling_public_method   % => 13
    Example.calling_public_method2  % => 13

Private methods are internal and therefore cannot be accessed in the `self.method` format:

    module Example
      def calling_private_method
        private_method
      end
      
      def calling_private_method2
        self.private_method
      end
      
      private
      
      def private_method
        13
      end
    end
    
    % Won't work, the method is private.
    Example.private_method
    
    % It works because calling_private_method is calling private_method without self.
    Example.calling_private_method   % => 13
    
    % It won't work because calling_private_method is calling private_method with self.
    Example.calling_private_method2

### Local and remote calls

In Erlang, it is very important to make a difference between local calls and remote calls, as they affect how hot code swapping works. You can read this section from [Learn You Some Erlang](http://learnyousomeerlang.com/designing-a-concurrent-application#hot-code-loving) for more information.

In Elixir, every time you call a private method, it is doing a local call. This means that private methods are always called locally (i.e. in the same module) and can't be overridden in mixins. Consider the following example:

    module A
      def foo
        1 + bar + baz
      end
      
      def bar
        1
      end
      
      private
      
      def baz
        1
      end
    end
    
    module B
      mixin A
      
      def bar
        2
      end
      
      def baz
        3
      end
    end
    
    A.foo % => 3
    B.foo % => 4

Notice how `B.foo` returns 4 and not `6`. This is because we can only override `bar` in mixins, we can't access private methods as they can only be called locally (i.e. from the same module).

Finally, notice that if a variable is defined with the same name as method, the variable is given higher preference:

    module AnotherExample
      def value
        some_value = 11
        some_value
      end
      
      private
      
      def some_value
        13
      end
    end
    
    AnotherExample.value % => 11

If in the example above you wanted to call do a local call, you could have used `some_value()` to disambiguate.

### Tail call optimization

In the "Variables and Pattern Matching" section above, we have showed a simple Fibonacci example using Pattern Matching in the method signature. However, that example was not properly optimized:

    module Math
      def fibonacci(0)
        0
      end
      
      def fibonacci(1)
        1
      end
      
      def fibonacci(n)
        fibonacci(n - 1) + fibonacci(n - 2)
      end
    end

As Erlang, Elixir does tail call optimization. We can rewrite the fibonacci method with a version that will use tail call optimization like below:

    module OptimizedMath
      def fibonacci(n)
        fibonacci(n, 1, 0)
      end
      
      def fibonacci(0, _, result)
        result
      end
      
      def fibonacci(n, next, result)
        fibonacci(n - 1, next + result, next)
      end
    end
    
    OptimizedMath.fibonacci(0)   % => 0
    OptimizedMath.fibonacci(1)   % => 1
    OptimizedMath.fibonacci(3)   % => 2
    OptimizedMath.fibonacci(10)  % => 55

The third fibonacci method in `OptimizedMath` is optimized because the last method calls itself. In order to understand the difference between both versions and how tail call optimization works, we recommend reading more about it on the Recursion chapter from [Learn You Some Erlang](http://learnyousomeerlang.com/recursion).

### Pattern matching in methods

As we mentioned earlier and saw in the examples above, pattern matching is also allowed in method signatures. If the given args does not match a given method, it will try the next one until it succeeds or none is found, raising an error. Below, is an example that checks if a list is the prefix of another, relying solely on pattern matching:

    module Prefix
      % This won't match if the first element of each list is not equal
      def is?([i|prefix], [i|list])
        is?(prefix, list)
      end
      
      % If prefix is empty or gets empty, it matches
      def is?([], _list)
        true
      end
      
      % Anything else is false
      def is?(_prefix, _list)
        false
      end
    end
    
    prefix = "foo".to_char_list
    full   = "foobar".to_char_list
    Prefix.is?(prefix, full) % => true
    Prefix.is?(full, prefix) % => false

The fact `OrderedDict`s are allowed in pattern matching and pattern matching is allowed in methods, makes it possible to use key-value arguments:

    def do_something(value, 'special: true)
      % Do something special
    end
    
    def do_something(value, 'special: false)
      % Do something not that special
    end

### Default arguments in methods

Besides supporting pattern matching in methods, Elixir also supports default arguments. You can specify a default argument using the `:=` operator. Example:

    module Default
      def sum(a := 1, b := 2)
        a + b
      end
    end
    
    Default.sum        % => 3
    Default.sum(2)     % => 4
    Default.sum(2, 3)  % => 5

Default arguments work by implicitly defining methods that accepts less arguments. The code above generates exactly the same module as follow:

    module Default
      def sum()
        sum(1, 2)
      end
      
      def sum(a)
        sum(a, 2)
      end
      
      def sum(a, b)
        a + b
      end
    end

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/module.ex>

## Module binding, refinements and mixins

Elixir provides a way to bind modules to structures in order to provide method dispatching. A couple of features are built on top of this functionality and are going to be described next.

### Binding

Binding is the ability to bind modules to data types. Method dispatching is done by simply invoking a method in the module bound to the structure. For example, when the method `+` is invoked in a string, it is simply invoking the method `+` defined in `String::Behavior` as below:

    module String::Behavior
      def +(another)
        <<self|binary, another|binary>>
      end
    end

All built-in data types (like integers, tuples, lists, etc) are already bound to a given module at runtime and cannot be changed. However, Elixir provides a data structure called **blank slate** that is more flexible and can be bound to any module:

    module Car
      def engine
        IO.puts "VROOOM"
      end
    end
    
    % Create a blank slate and bind it to `Car` module:
    car = Module.blank_slate#Car()
    car.engine % => "VROOOM"

The `Module.blank_slate` expression above returns an empty data type that is then bound to the module `Car` using the operator `#`. Since the expression `Module.blank_slate#Car()` is too long, the form commonly used is:

    car = #Car()
    car.engine % => "VROOOM"

Notice the `#` operator has the same precedence as `.`, so the next expressions are equivalent and will all print `"VROOOM"`:

     Module.blank_slate#Car.engine
     (Module.blank_slate#Car).engine
     Module.blank_slate#Car().engine

#### Internal variables

Elixir allows us to store information inside blank slates. This is done with internal variables. In the example below, we are going to store the color of a car by using the `__bound__` callback and then read it:

    module Car
      % Callback invoked whenever this module is bound to a structure.
      % All the argument passed to the bind operator are accessible here.
      def __bound__(color)
        % Set the internal variable color to the given color.
        @('color, color)
      end
      
      def color
        % Read the internal variable @color.
        % @internal_variables are always relative to self.
        @color
      end
    end
    
    car = #Car('green)
    car.color % => 'green

Whenever a module is bound, the callback `__bound__` in the module is invoked. All the values given on binding are accessible in the callback. Data can be added to the blank slate through **internal variables** (for example, `@color` above).

Besides blank slates, modules also have internal variables:

    module Car
      set_ivar('color, 'red)
      
      def color
        @color
      end
    end
    
    Car.color % => 'red

In the example above, we are setting and reading the internal variable of the module Car. However, keep in mind that if we bind the module, `@color` will point to the bind object as seen above:

    #Car().color % => nil

#### Mutability

Almost everything in Elixir is immutable. For example, every time we set the internal variable of a new structure, it returns a new structure instead of modifying the first one in place:

    car = #Car('green)
    new_car = car.set_ivar('color, 'red)
    
    car.color     % => 'green
    new_car.color % => 'red

The only exception to mutability are modules:

    module Car
      set_ivar('color, 'red)
    
      def color
        @color
      end
    end
    
    Car.color % => 'red

In the example above, `set_ivar` is setting the value of the internal variable `@color` but it is not returning a new object, it is modifying the value of the module in place! However, notice that modules are only mutable during definition. After a module is defined, we can no longer modify it:

    new_module = Car.set_ivar 'color, 'green
    Car.color        % => 'red
    new_module.color % =>'green

Notice in the example above `Car`'s internal variable `@color` is still `'red` even after calling `set_ivar`. Immutability is important to ensure we won't have race conditions or deadlocks on runtime. However, it is ok for modules to be mutable as long as it happens only during compilation time, adding great extensibility.

#### Best practices

Libraries must hide the blank slate binding as most as possible. For instance, the `Car` example above, requires the developer to manually bind a method, leading to coupling. Ideally, the `Car` should provide an API for that as below:

    module Car
      def new(color)
        #Car::Behavior(color)
      end
      
      module Behavior
        def __bound__(color)
          @('color, color)
        end
    
        def color
          @color
        end
      end
    end

### Refinements (to be implemented)

Refinements is the ability to change an already defined module at compile time avoiding the need to always bind the module to the same data types:

    :refine Integer::Instance with DurationExtensions
    1.day_in_seconds % => 3600

### Mixins

Mixins are the ability to mix one module into another module **still in definition**:

    module SimpleMath
      def one
        1
      end
      
      def two
        2
      end
    end
    
    module AdvancedMath
      mixin SimpleMath
      
      def one_plus_two
        one + two
      end
    end
    
    AdvancedMath.one_plus_two % => 3

Different from refinements that only alter method lookup, mixins provide a copy mechanism that copy all the methods defined in the mixed in module into the target providing faster behavior at run-time.

Note that mixed in methods are available straight away, while methods defined in the module are just available after the module is defined:

    module AdvancedMath
      mixin SimpleMath
      
      one % => 1
      two % => 2
      
      def one_plus_two
        one + two
      end
      
      % Fails because AdvancedMath is still
      % in definition.
      one_plus_two % => ERROR
    end

Finally, notice that methods that are not available at compile time cannot be called locally. For instance, imagine you have module `RequiresName` that requires name to be implemented in the target:

    module RequiresName
      def shout
        IO.puts "HELLO #{self.name}"
      end
    end
    
    module Person
      mixin RequiresName
      
      def name
        "John Doe"
      end
    end
    
    Person.shout % => "HELLO John Doe"

In the example above, `RequiresName` uses `self.name` instead of `name`. This is required because `name` is not known locally.

#### Temporary mixins

Sometimes it is also convenient to include a mixin only during the
module definition. This is achieved with temporary mixins:

    module SimpleMath
      def one
        1
      end
      
      def two
        2
      end
    end
    
    module AdvancedMath
      using SimpleMath
      
      one + two % => 3
      
      def one_plus_two
        one + two
      end
    end
    
    % Will fail because SimpleMath methods are
    % available only during the module definition.
    AdvancedMath.one_plus_two % => ERROR

#### Summary

* Binding allows us to change dispatch on a given data type at runtime;
* Refinements allows us to change dispatch on an existing module at compile time;
* Mixins allows us to mix behavior from existing modules during module definition.

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/module.ex>

## Code and load paths

Loading code in Elixir happens by automatically loading modules inside the compilation directory. For instance, if you are building a library and have the compiled code inside the exbin/ directory, you can access any of the modules in it using:

    bin/elixir -pa exbin/ -e "SomeCompiledModule.method"

You can find more documentation by typing "bin/elixir". You may also add and remove paths programatically

When scripting, it may be convenient to load another specific script file, you can do that using `Code.load_file` or `Code.require_file` in which the second assures the file is being loaded just once.

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/code.ex>

## Processes, Behavior and Callbacks

Elixir provides the same facilities to deal with processes as Erlang. Messages are sent using `<-` and the same `receive/after` syntax is available. You can learn more about it by checking the `process.ex` file in the examples folder: <https://github.com/josevalim/elixir/tree/master/examples/process.ex>

Besides, Elixir also imports behaviors from Erlang OTP. Currently, just `GenServer` is implemented and support for others will come as needed. Once again, you can learn more in the examples folder: <https://github.com/josevalim/elixir/tree/master/examples/gen_server.ex>

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/process.ex>
* <https://github.com/josevalim/elixir/tree/master/lib/gen_server.ex>

# Advanced Topics

Some advanced topics related to Elixir.

## Variable scopes

As explained at the beginning of this README, Elixir allows the same variable to be assigned more than once. However, keep in mind that variables assignment inside functions do not change the original binding. For example:

    a = 1
    b = -> a = 2
    b()
    a % => 1

As everything is immutable, when the function assigns a new variable, it creates a new binding with the new variable value and the original binding is never modified. This is important to avoid side-effects when passing functions to different processes.

Also, Elixir has much more flexible rules when it comes to variables inside control-flow expressions. For instance, the following works:

    x = 1
    
    if true
      x = 2
    end
    
    x % => 2

The same is also true for `receive/after` and `case/match` expressions. The only exception comes to `try/catch` scenarios, where a variable defined inside such blocks is never accessible from the outside. For example:

    x = 1
    
    try
      x = 2
    catch _:_
      % Do nothing
    end
    
    x % => 1

## Guards

Elixir has basic support for guards. They can be used on method declaration, `receive/match` clauses, `case/match` clauses and `catch` clauses. In all cases, they are declared using the keyword `when`. For instance, you could implement a method that returns the absolute value of a number as follow:

    def abs(x) when x < 0
      - x
    end
    
    def abs(x)
      x
    end

In a receive/case match clause, we would do instead:

    case y
    match x when x < 0 then - x
    match x then x
    end

Finally, in catch expressions it works as follow:

    try
      throw y
    catch 'throw:x when x < 0
      - x
    catch 'throw:x
      x
    end

Guards only supports arithmetic operators on numbers, comparison operators and the following boolean operators: `or`, `orelse`, `and`, `andalso` and `not`.

## Dynamic Dispatch and Metaprogramming

Elixir allows you to dynamically dispatch methods using `send`:

    [1,2,3].send 'head   % => 1
    {}.send 'empty?      % => true

Elixir also allows you to dynamically define methods. For example, below we can define attribute readers for both "title" and "author" attributes dynamically:

    module Person
      def __bound__(name, age)
        @('name: name, 'age: age)
      end

      ["name", "age"].each do (method)
        module_eval __FILE__, __LINE__ + 1, ~~METHOD
      def #{method}
        @#{method}
      end
    ~~
      end
    end
    
    person = #Person("John Doe", 24)
    person.name % => "John Doe"

The real benefit is when you encapsulate it inside a method. For example, the definition above is inside Elixir, so you can actually call:

    module Person
      attr_reader ['name, 'age]
      
      def initialize(name, age)
        @('name: name, 'age: age)
      end
    end

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/module.ex>

## Performance

The focus in Elixir so far has not been in performance, but there are a few things you can do right now.

### Compilation to Native Code

Elixir can compile to native code using the Hipe compiler. All you need to do is to export the following before running your code:

    export ERL_COMPILER_OPTIONS=native

## Records

Elixir allows you to import records from Erlang code. Here is an example that imports the `file_info` record available in the `kernel` module:

    module FileInfo
      mixin Record
      record 'file_info, 'from_lib: "kernel/include/file.hrl"
    end
    
    % Manually access the Erlang file:read_file_info method
    % passing the current file as a char list.
    { 'ok, info } = Erlang.file.read_file_info(__FILE__.to_char_list)
    
    % Create a new FileInfo object based on the tuple returned above
    record = #FileInfo(info)
    
    % Profit by accessing the record info
    record.access % => 'read_write

#### Documentation

* <https://github.com/josevalim/elixir/tree/master/lib/record.ex>

# License

Copyright (c) 2011 José Valim

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
