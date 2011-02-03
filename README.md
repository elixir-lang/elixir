# Elixir

Nothing interesting here yet. Try again later.

# Roadmap

* Add case/match expressions
* Add missing types (regexp, ranges, bitstream)
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
