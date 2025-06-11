<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Typespecs reference

> #### Typespecs are not set-theoretic types {: .warning}
>
> Elixir is in the process of implementing its
> [own type system](./gradual-set-theoretic-types.md) based on set-theoretic types.
> Typespecs, which are described in the following document, are a distinct notation
> for declaring types and specifications based on Erlang.
> Typespecs may be phased out as the set-theoretic type effort moves forward.

Elixir is a dynamically typed language, and as such, type specifications are never used by the compiler to optimize or modify code. Still, using type specifications is useful because:

  * they provide documentation (for example, tools such as [`ExDoc`](https://hexdocs.pm/ex_doc/) show type specifications in the documentation)
  * they're used by tools such as [Dialyzer](`:dialyzer`), that can analyze code with typespecs to find type inconsistencies and possible bugs

Type specifications (most often referred to as *typespecs*) are defined in different contexts using the following attributes:

  * `@type`
  * `@opaque`
  * `@typep`
  * `@spec`
  * `@callback`
  * `@macrocallback`

In addition, you can use `@typedoc` to document a custom `@type` definition.

See the "User-defined types" and "Defining a specification" sub-sections below for more information on defining types and typespecs.

## A simple example

    defmodule StringHelpers do
      @typedoc "A word from the dictionary"
      @type word() :: String.t()

      @spec long_word?(word()) :: boolean()
      def long_word?(word) when is_binary(word) do
        String.length(word) > 8
      end
    end

In the example above:

  * We declare a new type (`word()`) that is equivalent to the string type (`String.t()`).

  * We describe the type using a `@typedoc`, which will be included in the generated documentation.

  * We specify that the `long_word?/1` function takes an argument of type `word()` and
    returns a boolean (`boolean()`), that is, either `true` or `false`.

## Types and their syntax

The syntax Elixir provides for type specifications is similar to [the one in Erlang](https://www.erlang.org/doc/reference_manual/typespec.html). Most of the built-in types provided in Erlang (for example, `pid()`) are expressed in the same way: `pid()` (or simply `pid`). Parameterized types (such as `list(integer)`) are supported as well and so are remote types (such as [`Enum.t()`](`t:Enum.t/0`)). Integers and atom literals are allowed as types (for example, `1`, `:atom`, or `false`). All other types are built out of unions of predefined types. Some types can also be declared using their syntactical notation, such as `[type]` for lists, `{type1, type2, ...}` for tuples and `<<_ * _>>` for binaries.

The notation to represent the union of types is the pipe `|`. For example, the typespec `type :: atom() | pid() | tuple()` creates a type `type` that can be either an `atom`, a `pid`, or a `tuple`. This is usually called a [sum type](https://en.wikipedia.org/wiki/Tagged_union) in other languages

> #### Differences with set-theoretic types {: .warning}
>
> While they do share some similarities, the types below do not map one-to-one
> to the new types from the set-theoretic type system.
>
> For example, there is no plan to support subsets of the `integer()` type such
> as positive, ranges or literals.
>
> Furthermore, set-theoretic types support the full range of set operations,
> including intersections and negations.

### Basic types

    type ::
          any()                     # the top type, the set of all terms
          | none()                  # the bottom type, contains no terms
          | atom()
          | map()                   # any map
          | pid()                   # process identifier
          | port()                  # port identifier
          | reference()
          | tuple()                 # tuple of any size

                                    ## Numbers
          | float()
          | integer()
          | neg_integer()           # ..., -3, -2, -1
          | non_neg_integer()       # 0, 1, 2, 3, ...
          | pos_integer()           # 1, 2, 3, ...

                                                                          ## Lists
          | list(type)                                                    # proper list ([]-terminated)
          | nonempty_list(type)                                           # non-empty proper list
          | maybe_improper_list(content_type, termination_type)           # proper or improper list
          | nonempty_improper_list(content_type, termination_type)        # improper list
          | nonempty_maybe_improper_list(content_type, termination_type)  # non-empty proper or improper list

          | Literals                # Described in section "Literals"
          | BuiltIn                 # Described in section "Built-in types"
          | Remotes                 # Described in section "Remote types"
          | UserDefined             # Described in section "User-defined types"

### Literals

The following literals are also supported in typespecs:

    type ::                               ## Atoms
          :atom                           # atoms: :foo, :bar, ...
          | true | false | nil            # special atom literals

                                          ## Bitstrings
          | <<>>                          # empty bitstring
          | <<_::size>>                   # size is 0 or a positive integer
          | <<_::_*unit>>                 # unit is an integer from 1 to 256
          | <<_::size, _::_*unit>>

                                          ## (Anonymous) Functions
          | (-> type)                     # zero-arity, returns type
          | (type1, type2 -> type)        # two-arity, returns type
          | (... -> type)                 # any arity, returns type

                                          ## Integers
          | 1                             # integer
          | 1..10                         # integer from 1 to 10

                                          ## Lists
          | [type]                        # list with any number of type elements
          | []                            # empty list
          | [...]                         # shorthand for nonempty_list(any())
          | [type, ...]                   # shorthand for nonempty_list(type)
          | [key: value_type]             # keyword list with optional key :key of value_type

                                                  ## Maps
          | %{}                                   # empty map
          | %{key: value_type}                    # map with required key :key of value_type
          | %{key_type => value_type}             # map with required pairs of key_type and value_type
          | %{required(key_type) => value_type}   # map with required pairs of key_type and value_type
          | %{optional(key_type) => value_type}   # map with optional pairs of key_type and value_type
          | %SomeStruct{}                         # struct with all fields of any type
          | %SomeStruct{key: value_type}          # struct with required key :key of value_type

                                          ## Tuples
          | {}                            # empty tuple
          | {:ok, type}                   # two-element tuple with an atom and any type

### Built-in types

The following types are also provided by Elixir as shortcuts on top of the basic and literal types described above.

Built-in type           | Defined as
:---------------------- | :---------
`term()`                | `any()`
`arity()`               | `0..255`
`as_boolean(t)`         | `t`
`binary()`              | `<<_::_*8>>`
`nonempty_binary()`     | `<<_::8, _::_*8>>`
`bitstring()`           | `<<_::_*1>>`
`nonempty_bitstring()`  | `<<_::1, _::_*1>>`
`boolean()`             | `true` \| `false`
`byte()`                | `0..255`
`char()`                | `0..0x10FFFF`
`charlist()`            | `[char()]`
`nonempty_charlist()`   | `[char(), ...]`
`fun()`                 | `(... -> any)`
`function()`            | `fun()`
`identifier()`          | `pid()` \| `port()` \| `reference()`
`iodata()`              | `iolist()` \| `binary()`
`iolist()`              | `maybe_improper_list(byte() \| binary() \| iolist(), binary() \| [])`
`keyword()`             | `[{atom(), any()}]`
`keyword(t)`            | `[{atom(), t}]`
`list()`                | `[any()]`
`nonempty_list()`       | `nonempty_list(any())`
`maybe_improper_list()` | `maybe_improper_list(any(), any())`
`nonempty_maybe_improper_list()` | `nonempty_maybe_improper_list(any(), any())`
`mfa()`                 | `{module(), atom(), arity()}`
`module()`              | `atom()`
`no_return()`           | `none()`
`node()`                | `atom()`
`number()`              | `integer()` \| `float()`
`struct()`              | `%{:__struct__ => atom(), optional(atom()) => any()}`
`timeout()`             | `:infinity` \| `non_neg_integer()`

`as_boolean(t)` exists to signal users that the given value will be treated as a boolean, where `nil` and `false` will be evaluated as `false` and everything else is `true`. For example, `Enum.filter/2` has the following specification: `filter(t, (element -> as_boolean(term))) :: list`.

### Remote types

Any module is also able to define its own types and the modules in Elixir are no exception. For example, the `Range` module defines a `t/0` type that represents a range: this type can be referred to as `t:Range.t/0`. In a similar fashion, a string is `t:String.t/0`, and so on.

### Maps

The key types in maps are allowed to overlap, and if they do, the leftmost key takes precedence.
A map value does not belong to this type if it contains a key that is not in the allowed map keys.

If you want to denote that keys that were not previously defined in the map are allowed,
it is common to end a map type with `optional(any) => any`.

Note that the syntactic representation of `map()` is `%{optional(any) => any}`, not `%{}`. The notation `%{}` specifies the singleton type for the empty map.

### Keyword Lists

Beyond `keyword()` and `keyword(t)`, it can be helpful to compose a spec for an expected keyword list.
For example:

```elixir
@type option :: {:name, String.t} | {:max, pos_integer} | {:min, pos_integer}
@type options :: [option()]
```

This makes it clear that only these options are allowed, none are required, and order does not matter.

It also allows composition with existing types.
For example:

```elixir
@type option :: {:my_option, String.t()} | GenServer.option()

@spec start_link([option()]) :: GenServer.on_start()
def start_link(opts) do
  {my_opts, gen_server_opts} = Keyword.split(opts, [:my_option])
  GenServer.start_link(__MODULE__, my_opts, gen_server_opts)
end
```

The following spec syntaxes are equivalent:

```elixir
@type options [{:name, String.t} | {:max, pos_integer} | {:min, pos_integer}]

@type options [name: String.t, max: pos_integer, min: pos_integer]
```

### User-defined types

The `@type`, `@typep`, and `@opaque` module attributes can be used to define new types:

    @type type_name :: type
    @typep type_name :: type
    @opaque type_name :: type

A type defined with `@typep` is private. An opaque type, defined with `@opaque` is a type where the internal structure of the type will not be visible, but the type is still public.

Types can be parameterized by defining variables as parameters; these variables can then be used to define the type.

    @type dict(key, value) :: [{key, value}]

## Defining a specification

A specification for a function can be defined as follows:

    @spec function_name(type1, type2) :: return_type

Guards can be used to restrict type variables given as arguments to the function.

    @spec function(arg) :: [arg] when arg: atom

If you want to specify more than one variable, you separate them by a comma.

    @spec function(arg1, arg2) :: {arg1, arg2} when arg1: atom, arg2: integer

Type variables with no restriction can also be defined using `var`.

    @spec function(arg) :: [arg] when arg: var

This guard notation only works with `@spec`, `@callback`, and `@macrocallback`.

You can also name your arguments in a typespec using `arg_name :: arg_type` syntax. This is particularly useful in documentation as a way to differentiate multiple arguments of the same type (or multiple elements of the same type in a type definition):

    @spec days_since_epoch(year :: integer, month :: integer, day :: integer) :: integer
    @type color :: {red :: integer, green :: integer, blue :: integer}

Specifications can be overloaded, just like ordinary functions.

    @spec function(integer) :: atom
    @spec function(atom) :: integer

## Behaviours

Behaviours in Elixir (and Erlang) are a way to separate and abstract the generic part of a component (which becomes the *behaviour module*) from the specific part (which becomes the *callback module*).

A behaviour module defines a set of functions and macros (referred to as *callbacks*) that callback modules implementing that behaviour must export. This "interface" identifies the specific part of the component. For example, the `GenServer` behaviour and functions abstract away all the message-passing (sending and receiving) and error reporting that a "server" process will likely want to implement from the specific parts such as the actions that this server process has to perform.

Say we want to implement a bunch of parsers, each parsing structured data: for example, a JSON parser and a MessagePack parser. Each of these two parsers will *behave* the same way: both will provide a `parse/1` function and an `extensions/0` function. The `parse/1` function will return an Elixir representation of the structured data, while the `extensions/0` function will return a list of file extensions that can be used for each type of data (e.g., `.json` for JSON files).

We can create a `Parser` behaviour:

```elixir
defmodule Parser do
  @doc """
  Parses a string.
  """
  @callback parse(String.t) :: {:ok, term} | {:error, atom}

  @doc """
  Lists all supported file extensions.
  """
  @callback extensions() :: [String.t]
end
```

As seen in the example above, defining a callback is a matter of defining a specification for that callback, made of:

  * the callback name (`parse` or `extensions` in the example)
  * the arguments that the callback must accept (`String.t`)
  * the *expected* type of the callback return value

Modules adopting the `Parser` behaviour will have to implement all the functions defined with the `@callback` attribute. As you can see, `@callback` expects a function name but also a function specification like the ones used with the `@spec` attribute we saw above.

### Implementing behaviours

Implementing a behaviour is straightforward:

```elixir
defmodule JSONParser do
  @behaviour Parser

  @impl Parser
  def parse(str), do: {:ok, "some json " <> str} # ... parse JSON

  @impl Parser
  def extensions, do: [".json"]
end
```

```elixir
defmodule CSVParser do
  @behaviour Parser

  @impl Parser
  def parse(str), do: {:ok, "some csv " <> str} # ... parse CSV

  @impl Parser
  def extensions, do: [".csv"]
end
```

If a module adopting a given behaviour doesn't implement one of the callbacks required by that behaviour, a compile-time warning will be generated.

Furthermore, with `@impl` you can also make sure that you are implementing the **correct** callbacks from the given behaviour in an explicit manner. For example, the following parser implements both `parse` and `extensions`. However, thanks to a typo, `BADParser` is implementing `parse/0` instead of `parse/1`.

```elixir
defmodule BADParser do
  @behaviour Parser

  @impl Parser
  def parse, do: {:ok, "something bad"}

  @impl Parser
  def extensions, do: ["bad"]
end
```

This code generates a warning letting you know that you are mistakenly implementing `parse/0` instead of `parse/1`.
You can read more about `@impl` in the [module documentation](`Module#module-impl`).

### Using behaviours

Behaviours are useful because you can pass modules around as arguments and you can then *call back* to any of the functions specified in the behaviour. For example, we can have a function that receives a filename, several parsers, and parses the file based on its extension:

```elixir
@spec parse_path(Path.t(), [module()]) :: {:ok, term} | {:error, atom}
def parse_path(filename, parsers) do
  with {:ok, ext} <- parse_extension(filename),
       {:ok, parser} <- find_parser(ext, parsers),
       {:ok, contents} <- File.read(filename) do
    parser.parse(contents)
  end
end

defp parse_extension(filename) do
  if ext = Path.extname(filename) do
    {:ok, ext}
  else
    {:error, :no_extension}
  end
end

defp find_parser(ext, parsers) do
  if parser = Enum.find(parsers, fn parser -> ext in parser.extensions() end) do
    {:ok, parser}
  else
    {:error, :no_matching_parser}
  end
end
```

You could also invoke any parser directly: `CSVParser.parse(...)`.

Note you don't need to define a behaviour in order to dynamically dispatch on a module, but those features often go hand in hand.

### Optional callbacks

Optional callbacks are callbacks that callback modules may implement if they want to, but are not required to. Usually, behaviour modules know if they should call those callbacks based on configuration, or they check if the callbacks are defined with `function_exported?/3` or `macro_exported?/3`.

> ### Unloaded modules {: .warning}
>
> `function_exported?/3` (and `macro_exported?/3`) do *not* load the module in case it is not loaded and Elixir lazily loads modules by default (except on releases). So in practice you will want to invoke `Code.ensure_loaded?/1` before checking if the function/macro is exported. See the documentation for `function_exported?/3` for examples.

Optional callbacks can be defined through the `@optional_callbacks` module attribute, which has to be a keyword list with function or macro name as key and arity as value. For example:

    defmodule MyBehaviour do
      @callback vital_fun() :: any
      @callback non_vital_fun() :: any
      @macrocallback non_vital_macro(arg :: any) :: Macro.t
      @optional_callbacks non_vital_fun: 0, non_vital_macro: 1
    end

One example of optional callback in Elixir's standard library is `c:GenServer.format_status/1`.

### Inspecting behaviours

The `@callback` and `@optional_callbacks` attributes are used to create a `behaviour_info/1` function available on the defining module. This function can be used to retrieve the callbacks and optional callbacks defined by that module.

For example, for the `MyBehaviour` module defined in "Optional callbacks" above:

    MyBehaviour.behaviour_info(:callbacks)
    #=> [vital_fun: 0, "MACRO-non_vital_macro": 2, non_vital_fun: 0]
    MyBehaviour.behaviour_info(:optional_callbacks)
    #=> ["MACRO-non_vital_macro": 2, non_vital_fun: 0]

When using `iex`, the `IEx.Helpers.b/1` helper is also available.

## Pitfalls

There are some known pitfalls when using typespecs, they are documented next.

## The `string()` type

Elixir discourages the use of the `string()` type. The `string()` type refers to Erlang strings, which are known as "charlists" in Elixir. They do not refer to Elixir strings, which are UTF-8 encoded binaries. To avoid confusion, if you attempt to use the type `string()`, Elixir will emit a warning. You should use `charlist()`, `nonempty_charlist()`, `binary()` or `String.t()` accordingly, or any of the several literal representations for these types.

Note that `String.t()` and `binary()` are equivalent to analysis tools. Although, for those reading the documentation, `String.t()` implies it is a UTF-8 encoded binary.

## Functions which raise an error

Typespecs do not need to indicate that a function can raise an error; any function can fail any time if given invalid input.
In the past, the Elixir standard library sometimes used `no_return()` to indicate this, but these usages have been removed.

The `no_return()` type also should not be used for functions which do return but whose purpose is a "side effect", such as `IO.puts/1`.
In these cases, the expected return type is `:ok`.

Instead, `no_return()` should be used as the return type for functions which can never return a value.
This includes functions which loop forever calling `receive`, or which exist specifically to raise an error, or which shut down the VM.
