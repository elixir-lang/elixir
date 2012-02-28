# 1 Introduction

Welcome! In this tutorial we are going to show you how to get started with Elixir. In later sections, we are even going to discuss more advanced subjects as macros, protocols and other features provided by Elixir.

Keep in mind that Elixir is still in development and sometimes you may get poorly written error messages. If, at any point, you receive an error message and you are not sure on how to proceed, [please let us know in the issues tracker](https://github.com/elixir-lang/elixir/issues). Having explanative and consistent error messages is one of the many features we aim for Elixir.

## 1.1 Installation

Elixir isn't available in any distribution yet, but you can download and compile it in few steps:

    $ git clone https://github.com/elixir-lang/elixir.git
    $ cd elixir
    $ make test

    $ bin/elixir -v
    Elixir 0.4.0.dev

If tests pass, we are ready to go. But if tests fail it is likely you have an outdated Erlang version (Elixir requires Erlang R15B or later). You can check your Erlang version by calling `erl` in the command line. You will see some information as follow:

    Erlang R15B (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

If you have the correct version and tests still fail, feel free to open an issue in the issues tracker on Github.

> Note: if you are planning to develop projects with Elixir, we recommend adding the bin directory in Elixir git repository to your PATH for easier access to binaries.
>
>     export PATH="$PATH:/path/to/elixir/bin"
>

## 1.2 Interactive mode

We can start Elixir interactive mode by running `bin/iex` in the same directory you compiled Elixir. In interactive mode, we can type any Elixir expression. Let's warm up with some basic arithmetic expressions:

    iex> 1 + 1
    2
    iex> 10 - 5
    5
    iex> 10 / 2
    5.0

Notice `10 / 2` returned a float `5.0` instead of an integer. This is expected, in Elixir the operator `/` always returns a float. In case you want to do integer division or get the division remainder, you can invoke the `div` and `rem` functions:

    iex> div(10, 2)
    5
    iex> div 10, 2
    5
    iex> rem 10, 3
    1

In the example above, we called two functions called `div` and `rem`. Notice that parenthesis are not required in order to invoke a function. We are going to discuss more about it later. Let's move forward and see which other data types we have in Elixir:

## 1.3 Basic types

Some basic types are:

    iex> 1          # integer
    iex> 1.0        # float
    iex> :atom      # atom / symbol
    iex> {1,2,3}    # tuple
    iex> [1,2,3]    # list

Elixir also provides functions (note the dot between the variable and arguments when calling a function):

    # function
    iex> x = fn(a, b) -> a + b end
    #Fun<erl_eval.12.111823515>
    iex> x.(1, 2)
    3

And double-quoted strings:

    iex> "string"
    "string"

Strings in Elixir are UTF-8 binaries delimited by double quotes. A single-quoted string in Elixir is called a char list and is simply a list of characters:

    iex> 'string'
    'string'

We can use the `is_binary` and `is_list` helpers to detect if a given string is a binary (double-quoted) or a list (single-quoted):

    iex> is_binary "string"
    true
    iex> is_list "string"
    false

    iex> is_binary 'string'
    false
    iex> is_list 'string'
    true

Although they represent the same thing, double-quoted and single-quoted strings are different and best suited for different scenarios:

    iex> "string" == 'string'
    false

Most of the cases, developers should use double-quoted strings as their representation is more compact. We are going to discuss this subject with more detail in the next chapter.

Elixir also provides `true` and `false` as booleans:

    iex> true
    true
    iex> is_boolean false
    true

Booleans are represented internally as atoms:

    iex> is_atom(true)
    true

Elixir also provides Port, References and PIDs as data types (usually used in process communication) but they are out of the scope of a getting started tutorial. For now, let's take a look at the basic operators in Elixir before we move on to the next chapter.

## 1.4 Operators

As we saw earlier, Elixir provides `+`, `-`, `*`, `/` as arithmetic operators.

Elixir also provides `++` and `--` to manipulate lists:

    iex> [1,2,3] ++ [4,5,6]
    [1,2,3,4,5,6]
    iex> [1,2,3] -- [2]
    [1,3]

Since single-quoted strings are lists, we can also use `++` and `--` as operators to manipulate them:

    iex> 'some' ++ ' example'
    'some example'
    iex> 'some' -- 'oe'
    'sm'

Notice that `++` and `--` cannot be used to manipulate double quoted strings, as they are binaries. That said, string concatenation is done via `<>`:

    iex> "foo" <> "bar"
    "foobar"

Elixir also provides three boolean operators: `or`, `and` and `not`. Those operators are strict in the sense those operators expects only booleans (true or false) as arguments:

    iex> true and true
    true
    iex> false or is_atom(:example)
    true

Giving a non-boolean as argument will raise an exception:

    iex> 1 and true
    ** (::ArgumentError) argument error

`or` and `and` are short-circuit operators. They just execute the right side in case the left side is not enough to determine the result:

    iex> false and error("This error will never be raised")
    false

    iex> true or error("This error will never be raised")
    true

> Note: If you are an Erlang developer, `and` and `or` in Elixir actually map to the `andalso` and `orelse` operators in Erlang.

Elixir also provides `==`, `!=`, `===`, `!===`, `<=`, `>=`, `<` and `>` as comparison operators:

    iex> 1 == 1
    true
    iex> 1 != 2
    true
    iex> 1 < 2
    true

The difference between `==` and `===` is that the latter is more strict when comparing integers and floats:

    iex> 1 == 1.0
    true
    iex> 1 === 1.0
    false

In Elixir, we can compare two different data types:

    iex> 1 < :atom
    true

The reason we can compare different data types is for pragmatism. Sorting algorithms don't need to worry about different data types in order to sort. The overall sorting order is defined below:

    number < atom < reference < functions < port < pid < tuple < list < bit string

You actually don't need to memorize this ordering, it is important just to know an order exists.

Well, that is it for the introduction. In the next chapter, we are going to discuss some basic functions, data types conversions and a bit of control-flow.

[Index](https://github.com/elixir-lang/elixir/blob/master/docs/0_index.md) | [Chapter 2: Diving in](https://github.com/elixir-lang/elixir/blob/master/docs/2_diving_in.md)
