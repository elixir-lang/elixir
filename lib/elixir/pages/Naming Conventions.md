# Naming Conventions

This document covers some naming conventions in Elixir code, from casing to punctuation characters.

## Casing

Elixir developers must use `snake_case` when defining variables, function names, module attributes, etc:

    some_map = %{this_is_a_key: "and a value"}
    is_map(some_map)

Aliases, commonly used as module names, are an exception as they must be capitalized and written in `CamelCase`, like `OptionParser`. For aliases, capital letters are kept in acronyms, like `ExUnit.CaptureIO` or `Mix.SCM`.

Atoms can be written either in `:snake_case` or `:CamelCase`, although the convention is to use the snake case version throughout Elixir.

Generally speaking, filenames follow the `snake_case` convention of the module they define. For example, `MyApp` should be defined inside the `my_app.ex` file. However, this is only a convention. At the end of the day, any filename can be used as they do not affect the compiled code in any way.

## Underscore (`_foo`)

Elixir relies on underscores in different situations.

For example, a value that is not meant to be used must be assigned to `_` or to a variable starting with underscore:

    iex> {:ok, _contents} = File.read("README.md")

Function names may also start with an underscore. Such functions are never imported by default:

    iex> defmodule Example do
    ...>   def _wont_be_imported do
    ...>     :oops
    ...>   end
    ...> end

    iex> import Example
    iex> _wont_be_imported()
    ** (CompileError) iex:1: undefined function _wont_be_imported/0

Due to this property, Elixir relies on functions starting with underscore to attach compile-time metadata to modules. Such functions are most often in the `__foo__` format. For example, every module in Elixir has an `__info__/1` function:

    iex> String.__info__(:functions)
    [at: 2, capitalize: 1, chunk: 2, ...]

Elixir also includes five special forms that follow the double underscore format: `__CALLER__/0`, `__DIR__/0`, `__ENV__/0`and `__MODULE__/0` retrieve compile-time information about the current environment, while `__STACKTRACE__/0` retrieves the stacktrace for the current exception.

## Trailing bang (`foo!`)

A trailing bang (exclamation mark) signifies a function or macro where failure cases raise an exception.

Many functions come in pairs, such as `File.read/1` and `File.read!/1`. `File.read/1` will return a success or failure tuple, whereas `File.read!/1` will return a plain value or else raise an exception:

    iex> File.read("file.txt")
    {:ok, "file contents"}
    iex> File.read("no_such_file.txt")
    {:error, :enoent}

    iex> File.read!("file.txt")
    "file contents"
    iex> File.read!("no_such_file.txt")
    ** (File.Error) could not read file no_such_file.txt: no such file or directory

The version without `!` is preferred when you want to handle different outcomes using pattern matching:

    case File.read(file) do
      {:ok, body}      -> # do something with the `body`
      {:error, reason} -> # handle the error caused by `reason`
    end

However, if you expect the outcome to always to be successful (e.g. if you expect the file always to exist), the bang variation can be more convenient and will raise a more helpful error message (than a failed pattern match) on failure.

More examples of paired functions: `Base.decode16/2` and `Base.decode16!/2`, `File.cwd/0` and `File.cwd!/0`

There are also some non-paired functions, with no non-bang variant. The bang still signifies that it will raise an exception on failure. Examples: `Mix.Config.validate!/1`, `Protocol.assert_protocol!/1`

In macro code, the bang on `Kernel.alias!/1` and `Kernel.var!/2` signifies that [macro hygiene](https://elixir-lang.org/getting-started/meta/macros.html#macros-hygiene) is set aside.

## Trailing question mark (`foo?`)

Functions that return a boolean are named with a trailing question mark.

Examples: `Keyword.keyword?/1`, `Mix.debug?/0`, `String.contains?/2`

However, functions that return booleans and are valid in guards follow another convention, described next.

## `is_` prefix (`is_foo`)

Type checks and other boolean checks that are allowed in guard clauses are named with an `is_` prefix.

Examples: `Integer.is_even/1`, `Kernel.is_list/1`

These functions and macros follow the Erlang convention of an `is_` prefix, instead of a trailing question mark, precisely to indicate that they are allowed in guard clauses.

Note that type checks that are not valid in guard clauses do not follow this convention. Examples: `Keyword.keyword?/1`, `Regex.regex?/1`

## Special names

Some names have specific meaning in Elixir. We detail those cases below.

### length and size

When you see `size` in a function name, it means the operation runs in constant time (also written as "O(1) time") because the size is stored alongside the data structure.

Examples: `Kernel.map_size/1`, `Kernel.tuple_size/1`

When you see `length`, the operation runs in linear time ("O(n) time") because the entire data structure has to be traversed.

Examples: `Kernel.length/1`, `String.length/1`

In other words, functions using the word "size" in its name will take the same amount of time whether the data structure is tiny or huge. Conversely, functions having "length" in its name will take more time as the data structure grows in size.
