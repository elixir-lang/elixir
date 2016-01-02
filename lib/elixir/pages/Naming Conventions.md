# Naming Conventions

## Naming modules

Elixir module names are written in "upper camel case", like `OptionParser`.

Capital letters are kept in abbreviations, like `ExUnit.CaptureIO` or `Mix.SCM`.

When you refer to Erlang modules from Elixir code, you follow their Erlang naming, which is usually "lower snake case". For example: `:file_sorter`, `:io_lib`

Behind the scenes, Erlang modules are represented as plain atoms (e.g. `:binary`) and Elixir modules are represented as prefixed and uppercased atoms (e.g. `:"Elixir.String"`).

## Naming variables, module attributes and functions

Variables, module attributes and function names are written in "lower snake case":

    def MyModule do
      @my_attribute 123

      def my_function(my_argument) do
        my_argument + @my_attribute
      end
    end

There are rare exceptions, like uppercase sigils (for example `Kernel.sigil_S/2`) or current-environment functions like `Kernel.__CALLER__/0`.

## Function and macro conventions

Knowing these conventions can help you to understand some properties of a function or macro by its name alone, and to follow the conventions in your own naming.

### Trailing bang (`foo!`)

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

More examples of paired functions: `Base.decode_16/2` and `Base.decode_16!/2`, `File.cwd/0` and `File.cwd!/0`

There are also some non-paired functions, with no non-bang variant. The bang still signifies that it will raise an exception on failure. Examples: `Mix.Config.validate!/1`, `Protocol.assert_protocol!/1`

In macro code, the bang on `Kernel.alias!/1` and `Kernel.var!/1` signifies that [macro hygiene](http://elixir-lang.org/getting-started/meta/macros.html#macros-hygiene) is set aside.

### Trailing question mark (`foo?`)

Functions that return a boolean are named with a trailing question mark.

Examples: `Keyword.keyword?/1`, `Mix.debug?/0`, `String.contains?/2`

However, functions that are valid in guards follow another convention, described next.

### `is_` prefix (`is_foo`)

Type checks and other boolean checks that are allowed in guard clauses are named with an `is_` prefix.

Examples: `Integer.is_even/1`, `Kernel.is_list/1`

These functions and macros follow the Erlang convention of an `is_` prefix, instead of a trailing question mark, precisely to indicate that they are allowed in guard clauses.

Note that type checks that are not valid in guard clauses do not follow this convention. Examples: `Keyword.keyword?/1`, `Regex.regex?/1`

### `length` and `size`

When you see `size` in a function name, it means the operation runs in constant time (also written as "O(1) time") because the size is stored alongside the data structure.

Examples: `Kernel.map_size/1`, `Kernel.tuple_size/1`

When you see `length`, the operation runs in linear time ("O(n) time") because the entire data structure has to be traversed.

Examples: `Kernel.length/1`, `String.length/1`

In other words, `size` functions will take the same amount of time whether the data structure is tiny or huge. `length` functions will take more time as the data structure grows in size.
