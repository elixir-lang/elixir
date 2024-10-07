# Lists and tuples

In this chapter we will learn two of the most used collection data-types in Elixir: lists and tuples.

## (Linked) Lists

Elixir uses square brackets to specify a list of values. Values can be of any type:

```elixir
iex> [1, 2, true, 3]
[1, 2, true, 3]
iex> length([1, 2, 3])
3
```

Two lists can be concatenated or subtracted using the [`++`](`++/2`) and [`--`](`--/2`) operators respectively:

```elixir
iex> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
iex> [1, true, 2, false, 3, true] -- [true, false]
[1, 2, 3, true]
```

List operators never modify the existing list. Concatenating to or removing elements from a list returns a new list. We say that Elixir data structures are *immutable*. One advantage of immutability is that it leads to clearer code. You can freely pass the data around with the guarantee no one will mutate it in memory - only transform it.

Throughout the tutorial, we will talk a lot about the head and tail of a list. The head is the first element of a list and the tail is the remainder of the list. They can be retrieved with the functions [`hd`](`hd/1`) and [`tl`](`tl/1`). Let's assign a list to a variable and retrieve its head and tail:

```elixir
iex> list = [1, 2, 3]
iex> hd(list)
1
iex> tl(list)
[2, 3]
```

Getting the head or the tail of an empty list throws an error:

```elixir
iex> hd([])
** (ArgumentError) argument error
```

Sometimes you will create a list and it will return a quoted value preceded by `~c`. For example:

```elixir
iex> [11, 12, 13]
~c"\v\f\r"
iex> [104, 101, 108, 108, 111]
~c"hello"
```

When Elixir sees a list of printable ASCII numbers, Elixir will print that as a charlist (literally a list of characters). Charlists are quite common when interfacing with existing Erlang code. Whenever you see a value in IEx and you are not quite sure what it is, you can use [`i`](`IEx.Helpers.i/1`) to retrieve information about it:

```elixir
iex> i ~c"hello"
Term
  i ~c"hello"
Data type
  List
Description
  ...
Raw representation
  [104, 101, 108, 108, 111]
Reference modules
  List
Implemented protocols
  ...
```

We will talk more about charlists in the ["Binaries, strings, and charlists"](binaries-strings-and-charlists.md) chapter.

> #### Single-quoted strings {: .info}
>
> In Elixir, you can also use `'hello'` to build charlists, but this notation has been soft-deprecated in Elixir v1.15 and will emit warnings in future versions. Prefer to write `~c"hello"` instead.

## Tuples

Elixir uses curly brackets to define tuples. Like lists, tuples can hold any value:

```elixir
iex> {:ok, "hello"}
{:ok, "hello"}
iex> tuple_size({:ok, "hello"})
2
```

Tuples store elements contiguously in memory. This means accessing a tuple element by index or getting the tuple size is a fast operation. Indexes start from zero:

```elixir
iex> tuple = {:ok, "hello"}
{:ok, "hello"}
iex> elem(tuple, 1)
"hello"
iex> tuple_size(tuple)
2
```

It is also possible to put an element at a particular index in a tuple with [`put_elem`](`put_elem/3`):

```elixir
iex> tuple = {:ok, "hello"}
{:ok, "hello"}
iex> put_elem(tuple, 1, "world")
{:ok, "world"}
iex> tuple
{:ok, "hello"}
```

Notice that [`put_elem`](`put_elem/3`) returned a new tuple. The original tuple stored in the `tuple` variable was not modified. Like lists, tuples are also immutable. Every operation on a tuple returns a new tuple, it never changes the given one.

## Lists or tuples?

What is the difference between lists and tuples?

Lists are stored in memory as linked lists, meaning that each element in a list holds its value and points to the following element until the end of the list is reached. This means accessing the length of a list is a linear operation: we need to traverse the whole list in order to figure out its size.

Similarly, the performance of list concatenation depends on the length of the left-hand list:

```elixir
iex> list = [1, 2, 3]
[1, 2, 3]

# This is fast as we only need to traverse `[0]` to prepend to `list`
iex> [0] ++ list
[0, 1, 2, 3]

# This is slow as we need to traverse `list` to append 4
iex> list ++ [4]
[1, 2, 3, 4]
```

Tuples, on the other hand, are stored contiguously in memory. This means getting the tuple size or accessing an element by index is fast. On the other hand, updating or adding elements to tuples is expensive because it requires creating a new tuple in memory:

```elixir
iex> tuple = {:a, :b, :c, :d}
{:a, :b, :c, :d}
iex> put_elem(tuple, 2, :e)
{:a, :b, :e, :d}
```

Note, however, the elements themselves are not copied. When you update a tuple, all entries are shared between the old and the new tuple, except for the entry that has been replaced. This rule applies to most data structures in Elixir. This reduces the amount of memory allocation the language needs to perform and is only possible thanks to the immutable semantics of the language.

Those performance characteristics dictate the usage of those data structures. In a nutshell, lists are used when the number of elements returned may vary. Tuples have a fixed size. Let's see two examples from the `String` module:

```elixir
iex> String.split("hello world")
["hello", "world"]
iex> String.split("hello beautiful world")
["hello", "beautiful", "world"]
```

The [`String.split`](`String.split/1`) function breaks a string into a list of strings on every whitespace character. Since the amount of elements returned depends on the input, we use a list.

On the other hand, [`String.split_at`](`String.split_at/2`) splits a string in two parts at a given position. Since it always returns two entries, regardless of the input size, it returns tuples:

```elixir
iex> String.split_at("hello world", 3)
{"hel", "lo world"}
iex> String.split_at("hello world", -4)
{"hello w", "orld"}
```

It is also very common to use tuples and atoms to create "tagged tuples", which is a handy return value when an operation may succeed or fail. For example, [`File.read`](`File.read/1`) reads the contents of a file at a given path, which may or may not exist. It returns tagged tuples:

```elixir
iex> File.read("path/to/existing/file")
{:ok, "... contents ..."}
iex> File.read("path/to/unknown/file")
{:error, :enoent}
```

If the path given to [`File.read`](`File.read/1`) exists, it returns a tuple with the atom `:ok` as the first element and the file contents as the second. Otherwise, it returns a tuple with `:error` and the error description. As we will soon learn, Elixir allows us to *pattern match* on tagged tuples and effortlessly handle both success and failure cases.

Given Elixir consistently follows those rules, the choice between lists and tuples get clearer as you learn and use the language. Elixir often guides you to do the right thing. For example, there is an [`elem`](`elem/2`) function to access a tuple item:

```elixir
iex> tuple = {:ok, "hello"}
{:ok, "hello"}
iex> elem(tuple, 1)
"hello"
```

However, given you often don't know the number of elements in a list, there is no built-in equivalent for accessing arbitrary entries in a lists, apart from its head.

## Size or length?

When counting the elements in a data structure, Elixir also abides by a simple rule: the function is named `size` if the operation is in constant time (the value is pre-calculated) or `length` if the operation is linear (calculating the length gets slower as the input grows). As a mnemonic, both "length" and "linear" start with "l".

For example, we have used 4 counting functions so far: [`byte_size`](`byte_size/1`) (for the number of bytes in a string), [`tuple_size`](`tuple_size/1`) (for tuple size), [`length`](`length/1`) (for list length) and [`String.length`](`String.length/1`) (for the number of graphemes in a string). We use [`byte_size`](`byte_size/1`) to get the number of bytes in a string, which is a cheap operation. Retrieving the number of Unicode graphemes, on the other hand, uses [`String.length`](`String.length/1`), and may be expensive as it relies on a traversal of the entire string.

Now that we are familiar with the basic data-types in the language, let's learn important constructs for writing code, before we discuss more complex data structures.
