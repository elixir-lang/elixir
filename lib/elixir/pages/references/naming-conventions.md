# Naming conventions

This document is a reference of the naming conventions in Elixir, from casing to punctuation characters.

The naming convention is, by definition, a subset of the Elixir syntax. A convention aims to
follow and set best practices for language and the community. If instead you want a complete reference into the Elixir syntax, beyond its conventions, see [the Syntax reference](syntax-reference.md).

## Casing

Elixir developers must use `snake_case` when defining variables, function names, module attributes, and the like:

    some_map = %{this_is_a_key: "and a value"}
    is_map(some_map)

Aliases, commonly used as module names, are an exception as they must be capitalized and written in `CamelCase`, like `OptionParser`. For aliases, capital letters are kept in acronyms, like `ExUnit.CaptureIO` or `Mix.SCM`.

Atoms can be written either in `:snake_case` or `:CamelCase`, although the convention is to use the snake case version throughout Elixir.

Generally speaking, filenames follow the `snake_case` convention of the module they define. For example, `MyApp` should be defined inside the `my_app.ex` file. However, this is only a convention. At the end of the day any filename can be used as they do not affect the compiled code in any way.

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

Due to this property, Elixir relies on functions starting with underscore to attach compile-time metadata to modules. Such functions are most often in the `__foo__` format. For example, every module in Elixir has an [`__info__/1`](`c:Module.__info__/1`) function:

    iex> String.__info__(:functions)
    [at: 2, capitalize: 1, chunk: 2, ...]

Elixir also includes five special forms that follow the double underscore format: `__CALLER__/0`, `__DIR__/0`, `__ENV__/0`and `__MODULE__/0` retrieve compile-time information about the current environment, while `__STACKTRACE__/0` retrieves the stacktrace for the current exception.

## Trailing bang (`foo!`)

A trailing bang (exclamation mark) signifies a function or macro where failure cases raise an exception. They most often exist as a "raising variant" of a function that returns `:ok`/`:error` tuples (or `nil`).

One example is `File.read/1` and `File.read!/1`. `File.read/1` will return a success or failure tuple, whereas `File.read!/1` will return a plain value or else raise an exception:

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
      {:ok, body} -> # do something with the `body`
      {:error, reason} -> # handle the error caused by `reason`
    end

However, if you expect the outcome to always be successful (for instance, if you expect the file always to exist), the bang variation can be more convenient and will raise a more helpful error message (than a failed pattern match) on failure.

When thinking about failure cases, we are often thinking about semantic errors related to the operation being performed, such as failing to open a file or trying to fetch key from a map. Errors that come from invalid argument types, or similar, must always raise regardless if the function has a bang or not. In such cases, the exception is often an `ArgumentError` or a detailed `FunctionClauseError`:

    iex(1)> File.read(123)
    ** (FunctionClauseError) no function clause matching in IO.chardata_to_string/1

        The following arguments were given to IO.chardata_to_string/1:

            # 1
            123

        Attempted function clauses (showing 2 out of 2):

            def chardata_to_string(string) when is_binary(string)
            def chardata_to_string(list) when is_list(list)


More examples of paired functions: `Base.decode16/2` and `Base.decode16!/2`, `File.cwd/0` and `File.cwd!/0`. In some situations, you may have bang functions without a non-bang counterpart. They also imply the possibility of errors, such as: `Protocol.assert_protocol!/1` and `PartitionSupervisor.resize!/2`. This can be useful if you foresee the possibility of adding a non-raising variant in the future.

## Trailing question mark (`foo?`)

Functions that return a boolean are named with a trailing question mark.

Examples: `Keyword.keyword?/1`, `Mix.debug?/0`, `String.contains?/2`

However, functions that return booleans and are valid in guards follow another convention, described next.

## `is_` prefix (`is_foo`)

Type checks and other boolean checks that are allowed in guard clauses are named with an `is_` prefix.

Examples: `Integer.is_even/1`, `is_list/1`

These functions and macros follow the Erlang convention of an `is_` prefix, instead of a trailing question mark, precisely to indicate that they are allowed in guard clauses.

Note that type checks that are not valid in guard clauses do not follow this convention. For example: `Keyword.keyword?/1`.

## Special names

Some names have specific meaning in Elixir. We detail those cases below.

### length and size

When you see `size` in a function name, it means the operation runs in constant time (also written as "O(1) time") because the size is stored alongside the data structure.

Examples: `map_size/1`, `tuple_size/1`

When you see `length`, the operation runs in linear time ("O(n) time") because the entire data structure has to be traversed.

Examples: `length/1`, `String.length/1`

In other words, functions using the word "size" in its name will take the same amount of time whether the data structure is tiny or huge. Conversely, functions having "length" in its name will take more time as the data structure grows in size.

### get, fetch, fetch!

When you see the functions `get`, `fetch`, and `fetch!` for key-value data structures, you can expect the following behaviours:

  * `get` returns a default value (which itself defaults to `nil`) if the key is not present, or returns the requested value.
  * `fetch` returns `:error` if the key is not present, or returns `{:ok, value}` if it is.
  * `fetch!` *raises* if the key is not present, or returns the requested value.

Examples: `Map.get/2`, `Map.fetch/2`, `Map.fetch!/2`, `Keyword.get/2`, `Keyword.fetch/2`, `Keyword.fetch!/2`

### compare

The function `compare/2` should return `:lt` if the first term is less than the second, `:eq` if the two
terms compare as equivalent, or `:gt` if the first term is greater than the second.

Examples: `DateTime.compare/2`

Note that this specific convention is important due to the expectations of `Enum.sort/2`
