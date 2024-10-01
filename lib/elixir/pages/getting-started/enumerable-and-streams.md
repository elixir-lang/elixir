# Enumerables and Innumerables

While Elixir allows us to write recursive code, most operations we perform on collections is done with the help of the `Enum` and `Stream` modules. Let's learn how.

## Enumerables

Elixir provides the concept of enumerables and the `Enum` module to work with them. We have already learned two enumerables: lists and maps.

```elixir
iex> Enum.map([1, 2, 3], fn x -> x * 2 end)
[2, 4, 6]
iex> Enum.map(%{1 => 2, 3 => 4}, fn {k, v} -> k * v end)
[2, 12]
```

The `Enum` module provides a huge range of functions to transform, sort, group, filter and retrieve items from enumerables. It is one of the modules developers use frequently in their Elixir code. For a general overview of all functions in the `Enum` module, see [the `Enum` cheatsheet](enum-cheat.cheatmd).

Elixir also provides ranges (see `Range`), which are also enumerable:

```elixir
iex> Enum.map(1..3, fn x -> x * 2 end)
[2, 4, 6]
iex> Enum.reduce(1..3, 0, &+/2)
6
```

The functions in the `Enum` module are limited to, as the name says, enumerating values in data structures. For specific operations, like inserting and updating particular elements, you may need to reach for modules specific to the data type. For example, if you want to insert an element at a given position in a list, you should use the `List.insert_at/3` function, as it would make little sense to insert a value into, for example, a range.

We say the functions in the `Enum` module are polymorphic because they can work with diverse data types. In particular, the functions in the `Enum` module can work with any data type that implements the `Enumerable` protocol. We are going to discuss Protocols in a later chapter, for now we are going to move on to a specific kind of enumerable called a stream.

## Eager vs Lazy

All the functions in the `Enum` module are eager. Many functions expect an enumerable and return a list back:

```elixir
iex> odd? = fn x -> rem(x, 2) != 0 end
#Function<6.80484245/1 in :erl_eval.expr/5>
iex> Enum.filter(1..3, odd?)
[1, 3]
```

This means that when performing multiple operations with `Enum`, each operation is going to generate an intermediate list until we reach the result:

```elixir
iex> 1..100_000 |> Enum.map(&(&1 * 3)) |> Enum.filter(odd?) |> Enum.sum()
7500000000
```

The example above has a pipeline of operations. We start with a range and then multiply each element in the range by 3. This first operation will now create and return a list with `100_000` items. Then we keep all odd elements from the list, generating a new list, now with `50_000` items, and then we sum all entries.

## The pipe operator

The `|>` symbol used in the snippet above is the **pipe operator**: it takes the output from the expression on its left side and passes it as the first argument to the function call on its right side. Its purpose is to highlight the data being transformed by a series of functions. To see how it can make the code cleaner, have a look at the example above rewritten without using the `|>` operator:

```elixir
iex> Enum.sum(Enum.filter(Enum.map(1..100_000, &(&1 * 3)), odd?))
7500000000
```

Find more about the pipe operator [by reading its documentation](`|>/2`).

## Streams

As an alternative to `Enum`, Elixir provides the `Stream` module which supports lazy operations:

```elixir
iex> 1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?) |> Enum.sum()
7500000000
```

Streams are lazy, composable enumerables.

In the example above, `1..100_000 |> Stream.map(&(&1 * 3))` returns a data type, an actual stream, that represents the `map` computation over the range `1..100_000`:

```elixir
iex> 1..100_000 |> Stream.map(&(&1 * 3))
#Stream<[enum: 1..100000, funs: [#Function<34.16982430/1 in Stream.map/2>]]>
```

Furthermore, they are composable because we can pipe many stream operations:

```elixir
iex> 1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?)
#Stream<[enum: 1..100000, funs: [...]]>
```

Instead of generating intermediate lists, streams build a series of computations that are invoked only when we pass the underlying stream to the `Enum` module. Streams are useful when working with large, *possibly infinite*, collections.

Many functions in the `Stream` module accept any enumerable as an argument and return a stream as a result. It also provides functions for creating streams. For example, `Stream.cycle/1` can be used to create a stream that cycles a given enumerable infinitely. Be careful to not call a function like `Enum.map/2` on such streams, as they would cycle forever:

```elixir
iex> stream = Stream.cycle([1, 2, 3])
#Function<15.16982430/2 in Stream.unfold/2>
iex> Enum.take(stream, 10)
[1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
```

On the other hand, `Stream.unfold/2` can be used to generate values from a given initial value:

```elixir
iex> stream = Stream.unfold("hełło", &String.next_codepoint/1)
#Function<39.75994740/2 in Stream.unfold/2>
iex> Enum.take(stream, 3)
["h", "e", "ł"]
```

Another interesting function is `Stream.resource/3` which can be used to wrap around resources, guaranteeing they are opened right before enumeration and closed afterwards, even in the case of failures. For example, `File.stream!/1` builds on top of `Stream.resource/3` to stream files:

```elixir
iex> stream = File.stream!("path/to/file")
%File.Stream{
  line_or_bytes: :line,
  modes: [:raw, :read_ahead, :binary],
  path: "path/to/file",
  raw: true
}
iex> Enum.take(stream, 10)
```

The example above will fetch the first 10 lines of the file you have selected. This means streams can be very useful for handling large files or even slow resources like network resources.

The `Enum` and `Stream` modules provide a wide range of functions, but you don't have to know all of them by heart. Familiarize yourself with `Enum.map/2`, `Enum.reduce/3` and other functions with either `map` or `reduce` in their names, and you will naturally build an intuition around the most important use cases. You may also focus on the `Enum` module first and only move to `Stream` for the particular scenarios where laziness is required, to either deal with slow resources or large, possibly infinite, collections.

Next, we'll look at a feature central to Elixir, Processes, which allows us to write concurrent, parallel and distributed programs in an easy and understandable way.
