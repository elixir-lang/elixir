# Elixir

Elixir is a programming language built on top of Erlang. As Erlang, it is a functional language with strict evaluation, single assignment and dynamic typing built to support distributed, fault-tolerant, non-stop applications with hot swapping. Elixir allows you to invoke Erlang modules without a need to convert data types, therefore there is no hit in performance when invoking existing Erlang code.

The main difference between Elixir and Erlang is syntax and object orientation. Elixir provides a very simple Object Model based on prototype languages with most of its syntax based on Ruby.

# Usage

Elixir is not ready for use yet, though this milestone will be achieved soon. If you want to contribute to Elixir, you can do that by cloning the repository and running ``make test``.

The tests are organized in two directories: `test/erlang` and `test/elixir`. The first are written in Erlang and the second in Elixir. Much of Elixir's standard library is written in Elixir and tested in Elixir. This makes fairly easy to improve the language.

# Roadmap

* Add case/match expressions
* Implement missing types on STDLIB
* Add $"foo" as shortcut to create a list of integers
* Add generators
* Add metaprogramming
* Add partial application, pipeline f1 + f2, and 1#add and Integer##add 
* Add _.foo
* Improve STDLIB
* Add load paths
* Add JIT on module compilation
* Extending builtin types (like inheriting from Integer)
* Add method cache table
* Allow classes to be reopened
* Improve constant lookup

# Learning Elixir

This is a basic introduction into Elixir basic objects and object model. Some sections have a paragraph called "To be implemented", they represent parts of Elixir that was not implemented yet and that are under discussion.

This introduction borrowed its guidelines from [Learn You Some Erlang](http://learnyousomeerlang.com/), a great resource to learn Erlang which will be referenced several times during this introduction.

## Some notation

Comments in Elixir are, as in Erlang, done with %.

    % This is a commented line

Throughout this introduction, `% =>` represents the result of an expression:

    1 + 1 % => 2

## Basic Types

### Numbers

Elixir supports both Integer and Floats:

    2 + 15       % => 17
    - 13 * 10    % => 130
    1986 - 1985  % => 1
    5 / 2        % => 2.5
    4 / 2        % => 2.0

Notice that, as Erlang, "/" always returns a Float. If you want to have integer-to-integer division and the modulo operator, you should use `div` and `rem`:

    5 div 2  % => 2
    5 rem 2  % => 1

Several operations can also be done in a single expression, obeying the normal precedence rules:

    50 * 100 - 490     % => 10
    (50 * 100) - 490   % => 10
    -(50 * 100) - 490  % => -990

As in Ruby, everything is an object, so we can call methods numbers:

    -1.abs    % => 1
    5.div(2)  % => 2

It comes as no surprise that + is also a method:

    1.+(2)  % => 3

#### Documentation:

* https://github.com/josevalim/elixir/tree/master/lib/integer.ex
* https://github.com/josevalim/elixir/tree/master/lib/float.ex
* https://github.com/josevalim/elixir/tree/master/lib/numeric.ex

> #### To be implemented
>
> Currently, there is no support to enter numbers in other bases than base 10. This is the current API in Erlang:
>
>     2#101010.  % => 42
>     8#0677.    % => 447
>     16#AE.     % => 174
>
> Another feature that will likely be included is the ability to include "_" in numbers as in Ruby. This improves the readability when working with large numbers:
>
>     1_052_672

### Atoms

Elixir also has Atoms, called Symbols in other languages like Ruby. Although its syntax was borrowed from Lisp:

    'atom
    'Atom
    'atom_without_spaces

Atoms are literals, with their own value as name. An atom 'symbol is an atom 'symbol everywhere, with exactly the same value. Atoms start with a single quote and should not have spaces (spaces delimit the atom end). Atoms with spaces are represented by wrapping them in quotes:

    '"Atom with Spaces"

As in Erlang and Ruby, Atoms are not garbage collected, so remember to not generate atoms dynamically, otherwise you will run out of memory sooner than later.

#### Documentation:

* https://github.com/josevalim/elixir/tree/master/lib/atom.ex

### Booleans

As in Erlang, the boolean values are simply atoms named true and false. However, to avoid writing 'true and 'false, Elixir also allows you to simply write true or false. The following are all equivalent and will yield `1` as result:

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

> #### To be implemented
>
> Elixir currently lacks much of boolean algebra. None of the boolean operators were implemented yet:  `and`, `or`, `xor` nor `not`.
> Equality and match operators were not implemented as well: `==`, `/=`, `=:=`, `>`, `<`, etc.

### Tuples

Tuples are used to organize many terms together when you know how many terms there are. As in Erlang, a tuple is written in the following form:

    % A tuple containing all boolean values
    { true, false }

    % A tuple that may represent a point with coordinates X and Y
    { 10, 20 }

#### Documentation:

* https://github.com/josevalim/elixir/tree/master/lib/tuple.ex

> #### To be implemented
>
> Currently there is no way to interact with tuples elements. The following API is proposed:
>
>   { 'a, 'b, 'c }[0]  % => 'a
>   { 'a, 'b, 'c }[1]  % => 'b
>   { 'a, 'b, 'c }[2]  % => 'c
>
> Setting an element will be done through the set method:
>
>   { 'a, 'b, 'c }.set(0, 'd)  % => { 'd, 'b, 'c }
>
> This same note is also valid for lists (which we are going to see next).

### Lists

Lists are the main object in Elixir (as in any other functional language) and can contain anything:

    ['atom, 1, 2, 3, { 'some, 'tuple }]

Elixir Standard Library has a bunch of methods to interact with lists:

    [1, 2, 3].head    % => 1
    [1, 2, 3].tail    % => [2,3]
    [1, 2, 3].length  % => 3

Although, most of the power in lists comes when used together with functions:

    [1, 2, 3].map do (x)
      x * 2
    end  % => [2, 4, 6]

    [1, 2, 3].foldl 0, do (x, acc)
      acc + x
    end  % => 6

The examples above uses functions using the `do/end` syntax. Don't worry about them now, we are going to take a better look at them later.

#### Documentation:

* https://github.com/josevalim/elixir/tree/master/lib/tuple.ex

> #### To be implemented
>
> Currently there is no way to interact with tuples elements. The following API is proposed:
>
>   { 'a, 'b, 'c }[0]  % => 'a
>   { 'a, 'b, 'c }[1]  % => 'b
>   { 'a, 'b, 'c }[2]  % => 'c
>
> Setting an element will be done through the set method:
>
>   { 'a, 'b, 'c }.set(0, 'd)  % => { 'd, 'b, 'c }
>


# Examples

Imagine that we have a dictionary and we want to represent it as a string in the following format:

    { k1: v1, k2: v2, ... }
    
In Elixir, the implementation would be as follow:

    object Dict
      def to_s
        transformer = -> (key, value, acc) ["#{key.inspect}: #{value.inspect}"|acc]
        "{" + fold([], transformer).join(", ") + "}"
      end
    end

In Erlang, we would need the following:

    -module(dict).

    to_s(Dict) ->
      Transformer = fun(Key, Value, Acc) -> [string_format(Key) ++ ": " ++ string_format(Value)|Acc] end,
      List = dict:fold(Transformer, [], Dict),
      "{" ++ string:join(List, ", ") ++ "}".

    string_format(Thing) ->
      lists:flatten(io_lib:format("~p", [Thing])).

The Object Oriented aspect of Elixir brings several benefits. For example, we don't need to explicitly call *dict:fold()* because the *to_s* method in Elixir is already in the Dictionary scope, so we just call the *fold* method directly. The same applies to calling the function *join*. In Elixir, *join* is a method implemented in the List object, which is returned as result of the *fold* call. So we can simply call *join(", ")* in the List object instead of calling *string:join(List, ", ")* passing the List as argument.

# String and Sigils

In Elixir, we have the following basic types composed of a list of characters:

    % Strings (they are utf8 by default)
    "string"
    "string #{1 + 1} interpolation"    % => "string 2 interpolation"

    % Integer representation of a character
    $a    % => 97
    $b    % => 98
    $\\   % => 92
    $\(   % => 40

    % A string represented as a list of chars (all allow interpolation)
    $"string"    % => [115,116, 114, 105, 110, 103]
    $(string)    % => [115,116, 114, 105, 110, 103]
    $[string]    % => [115,116, 114, 105, 110, 103]
    ${string}    % => [115,116, 114, 105, 110, 103]

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
    ~l(string)
    ~L{string with interpolation}

    % A list of words
    ~w(foo bar baz)        % => ["foo", "bar", "baz"]
    ~W{foo #{'bar} baz}    % => ["foo", "bar", "baz"]
