# Keyword lists and maps

Now let's talk about associative data structures. Associative data structures are able to associate a key to a certain value. Different languages call these different names like dictionaries, hashes, associative arrays, etc.

In Elixir, we have two main associative data structures: keyword lists and maps.

## Keyword lists

Keyword lists are a data-structure used to pass options to functions. Imagine you want to split a string of numbers. We can use `String.split/2`:

```elixir
iex> String.split("1 2 3", " ")
["1", "2", "3"]
```

However, what happens if there is an additional space between the numbers:

```elixir
iex> String.split("1  2  3", " ")
["1", "", "2", "", "3"]
```

As you can see, there are now empty strings in our results. Luckily, the `String.split/3` function allows the `trim` option to be set to true:

```elixir
iex> String.split("1  2  3", " ", [trim: true])
["1", "2", "3"]
```

`[trim: true]` is a keyword list. Furthermore, when a keyword list is the last argument of a function, we can skip the brackets and write:

```elixir
iex> String.split("1  2  3", " ", trim: true)
["1", "2", "3"]
```

As shown in the example above, keyword lists are mostly used as optional arguments to functions.

As the name implies, keyword lists are simply lists. In particular, they are lists consisting of 2-item tuples where the first element (the key) is an atom and the second element can be any value. Both representations are the same:

```elixir
iex> [{:trim, true}] == [trim: true]
true
```

Since keyword lists are lists, we can use all operations available to lists. For example, we can use `++` to add new values to a keyword list:

```elixir
iex> list = [a: 1, b: 2]
[a: 1, b: 2]
iex> list ++ [c: 3]
[a: 1, b: 2, c: 3]
iex> [a: 0] ++ list
[a: 0, a: 1, b: 2]
```

You can read the value of a keyword list using the brackets syntax. This is also known as the access syntax, as it is defined by the `Access` module:

```elixir
iex> list[:a]
1
iex> list[:b]
2
```

In case of duplicate keys, values added to the front are the ones fetched:

```elixir
iex> new_list = [a: 0] ++ list
[a: 0, a: 1, b: 2]
iex> new_list[:a]
0
```

Keyword lists are important because they have three special characteristics:

  * Keys must be atoms.
  * Keys are ordered, as specified by the developer.
  * Keys can be given more than once.

For example, [the Ecto library](https://github.com/elixir-lang/ecto) makes use of these features to provide an elegant DSL for writing database queries:

```elixir
query =
  from w in Weather,
    where: w.prcp > 0,
    where: w.temp < 20,
    select: w
```

Although we can pattern match on keyword lists, it is not done in practice since pattern matching on lists requires the number of items and their order to match:

```elixir
iex> [a: a] = [a: 1]
[a: 1]
iex> a
1
iex> [a: a] = [a: 1, b: 2]
** (MatchError) no match of right hand side value: [a: 1, b: 2]
iex> [b: b, a: a] = [a: 1, b: 2]
** (MatchError) no match of right hand side value: [a: 1, b: 2]
```

Furthermore, given keyword lists are often used as optional arguments, they are used in situations where not all keys may be present, which would make it impossible to match on them. In a nutshell, do not pattern match on keyword lists.

In order to manipulate keyword lists, Elixir provides the `Keyword` module. Remember, though, keyword lists are simply lists, and as such they provide the same linear performance characteristics as them: the longer the list, the longer it will take to find a key, to count the number of items, and so on. If you need to store a large amount of keys in a key-value data structure, Elixir offers maps, which we will soon learn.

### `do`-blocks and keywords

As we have seen, keywords are mostly used in the language to pass optional values. In fact, we have used keywords in earlier chapters. For example, we have seen:

```elixir
iex> if true do
...>   "This will be seen"
...> else
...>   "This won't"
...> end
"This will be seen"
```

It happens that `do` blocks are nothing more than a syntax convenience on top of keywords. We can rewrite the above to:

```elixir
iex> if true, do: "This will be seen", else: "This won't"
"This will be seen"
```

Pay close attention to both syntaxes. In the keyword list format, we separate each key-value pair with commas, and each key is followed by `:`. In the `do`-blocks, we get rid of the colons, the commas, and separate each keyword by a newline. They are useful exactly because they remove the verbosity when writing blocks of code. Most of the time, you will use the block syntax, but it is good to know they are equivalent.

This plays an important role in the language as it allows Elixir syntax to stay small but still expressive. We only need few data structures to represent the language, a topic we will come back to when talking about [optional syntax](optional-syntax.md) and go in-depth when discussing [meta-programming](../meta-programming/quote-and-unquote.md).

With this out of the way, let's talk about maps.

## Maps as key-value pairs

Whenever you need to store key-value pairs, maps are the "go to" data structure in Elixir. A map is created using the `%{}` syntax:

```elixir
iex> map = %{:a => 1, 2 => :b}
%{2 => :b, :a => 1}
iex> map[:a]
1
iex> map[2]
:b
iex> map[:c]
nil
```

Compared to keyword lists, we can already see two differences:

  * Maps allow any value as a key.
  * Maps' keys do not follow any ordering.

In contrast to keyword lists, maps are very useful with pattern matching. When a map is used in a pattern, it will always match on a subset of the given value:

```elixir
iex> %{} = %{:a => 1, 2 => :b}
%{2 => :b, :a => 1}
iex> %{:a => a} = %{:a => 1, 2 => :b}
%{2 => :b, :a => 1}
iex> a
1
iex> %{:c => c} = %{:a => 1, 2 => :b}
** (MatchError) no match of right hand side value: %{2 => :b, :a => 1}
```

As shown above, a map matches as long as the keys in the pattern exist in the given map. Therefore, an empty map matches all maps.

The `Map` module provides a very similar API to the `Keyword` module with convenience functions to add, remove, and update maps keys:

```elixir
iex> Map.get(%{:a => 1, 2 => :b}, :a)
1
iex> Map.put(%{:a => 1, 2 => :b}, :c, 3)
%{2 => :b, :a => 1, :c => 3}
iex> Map.to_list(%{:a => 1, 2 => :b})
[{2, :b}, {:a, 1}]
```

## Maps of predefined keys

In the previous section, we have used maps as a key-value data structure where keys can be added or removed at any time. However, it is also common to create maps with a pre-defined set of keys. Their values may be updated, but new keys are never added nor removed. This is useful when we know the shape of the data we are working with and, if we get a different key, it likely means a mistake was done elsewhere.

We define such maps using the same syntax as in the previous section, except that all keys must be atoms:

```elixir
iex> map = %{:name => "John", :age => 23}
%{name: "John", age: 23}
```

As you can see from the printed result above, Elixir also allows you to write maps of atom keys using the same `key: value` syntax as keyword lists.

When the keys are atoms, in particular when working with maps of predefined keys, we can also access them using the `map.key` syntax:

```elixir
iex> map = %{name: "John", age: 23}
%{name: "John", age: 23}

iex> map.name
"John"
iex> map.agee
** (KeyError) key :agee not found in: %{name: "John", age: 23}
```

There is also syntax for updating keys, which also raises if the key has not yet been defined:

```elixir
iex> %{map | name: "Mary"}
%{name: "Mary", age: 23}
iex> %{map | agee: 27}
** (KeyError) key :agee not found in: %{name: "John", age: 23}
```

These operations have one large benefit in that they raise if the key does not exist in the map and the compiler may even detect and warn when possible. This makes them useful to get quick feedback and spot bugs and typos early on. This is also the syntax used to power another Elixir feature called "Structs", which we will learn later on.

Elixir developers typically prefer to use the `map.key` syntax and pattern matching instead of the functions in the `Map` module when working with maps because they lead to an assertive style of programming. [This blog post by José Valim](https://dashbit.co/blog/writing-assertive-code-with-elixir) provides insight and examples on how you get more concise and faster software by writing assertive code in Elixir.

## Nested data structures

Often we will have maps inside maps, or even keywords lists inside maps, and so forth. Elixir provides conveniences for manipulating nested data structures via the `get_in/1`, `put_in/2`, `update_in/2`, and other macros giving the same conveniences you would find in imperative languages while keeping the immutable properties of the language.

Imagine you have the following structure:

```elixir
iex> users = [
  john: %{name: "John", age: 27, languages: ["Erlang", "Ruby", "Elixir"]},
  mary: %{name: "Mary", age: 29, languages: ["Elixir", "F#", "Clojure"]}
]
[
  john: %{age: 27, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
  mary: %{age: 29, languages: ["Elixir", "F#", "Clojure"], name: "Mary"}
]
```

We have a keyword list of users where each value is a map containing the name, age and a list of programming languages each user likes. If we wanted to access the age for john, we could write:

```elixir
iex> users[:john].age
27
```

It happens we can also use this same syntax for updating the value:

```elixir
iex> users = put_in users[:john].age, 31
[
  john: %{age: 31, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
  mary: %{age: 29, languages: ["Elixir", "F#", "Clojure"], name: "Mary"}
]
```

The `update_in/2` macro is similar but allows us to pass a function that controls how the value changes. For example, let's remove "Clojure" from Mary's list of languages:

```elixir
iex> users = update_in users[:mary].languages, fn languages -> List.delete(languages, "Clojure") end
[
  john: %{age: 31, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
  mary: %{age: 29, languages: ["Elixir", "F#"], name: "Mary"}
]
```

There is more to learn about `get_in/1`, `pop_in/1` and others, including the `get_and_update_in/2` that allows us to extract a value and update the data structure at once. There are also `get_in/3`, `put_in/3`, `update_in/3`, `get_and_update_in/3`, `pop_in/2` which allow dynamic access into the data structure.

## Summary

There are two different data structures for working with key-value stores in Elixir. Alongside the `Access` module and pattern matching, they provide a rich set of tools for manipulating complex, potentially nested, data structures.

As we conclude this chapter, remember that you should:

  * Use keyword lists for passing optional values to functions

  * Use maps for general key-value data structures

  * Use maps when working with data that has a predefined set of keys

Now let's talk about modules and functions.
