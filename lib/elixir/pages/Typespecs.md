# Typespecs

Elixir comes with a notation for declaring types and specifications. Elixir is dynamically typed, and as such, typespecs are never used by the compiler to optimize or modify code. Still, using typespecs is useful as documentation and tools such as [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html) can analyze code with typespecs to find bugs.

The attributes `@type`, `@opaque`, `@typep`, `@spec`, `@callback` and `@macrocallback` are the main mechanism for defining typespecs. See sub-sections "Defining a type" and "Defining a specification" below.

## Types and their syntax

The type syntax provided by Elixir is fairly similar to [the one in Erlang](http://www.erlang.org/doc/reference_manual/typespec.html).

Most of the built-in types provided in Erlang (for example, `pid()`) are expressed the same way: `pid()` or simply `pid`. Parameterized types are also supported (`list(integer)`) and so are remote types (`Enum.t`).

Integers and atom literals are allowed as types (ex. `1`, `:atom` or `false`). All other types are built of unions of predefined types. Certain shorthands are allowed, such as `[...]`, `<<>>` and `{...}`.

### Basic types

    type :: any()                   # the top type, the set of all terms
          | none()                  # the bottom type, contains no terms
          | pid()
          | port()
          | reference()
          | tuple()
          | atom()
          | integer()
          | non_neg_integer()       # 0, 1, 2, 3, ...
          | pos_integer()           # 1, 2, 3, ...
          | neg_integer()           # ..., -3, -2, -1
          | float()
          | map()
          | struct()
          | list(type)
          | nonempty_list(type)
          | improper_list(type1, type2)
          | maybe_improper_list(type1, type2)
          | Literals                # Described in section "Literals"
          | Builtin                 # Described in section "Builtin-types"
          | Remotes                 # Described in section "Remotes"

### Literals

The following literals are also supported in typespecs:

    type :: :atom                         ## Atoms
          | 1                             ## Integers
          | 1..10                         ## Integers from 1 to 10
          | 1.0                           ## Floats

          | <<>>                          ## Bitstrings
          | <<_::size>>                 # size is 0 or a positive integer
          | <<_::_ * unit>>             # unit is an integer from 1 to 256
          | <<_::size * unit>>

          | [type]                        ## Lists
          | []                            # empty list
          | [...]                         # shorthand for nonempty_list(any())
          | [type, ...]                   # shorthand for nonempty_list(type)
          | [key: type]                   # keyword lists

          | (... -> type)                 ## Functions
          | (... -> type)                 # any arity, returns type
          | (() -> type)                  # 0-arity, returns type
          | (type1, type2 -> type)        # 2-arity, returns type

          | %{}                           ## Maps
          | %{key: type}                  # map with key :key with value of type
          | %{type1 => type2}             # map with keys of type1 with values of type2
          | %SomeStruct{}
          | %SomeStruct{key: type}

          | {}                            ## Tuples
          | {:ok, type}                   # two element tuple with an atom and any type

### Built-in types

These types are also provided by Elixir as shortcuts on top of the basic and literal types.

Built-in type           | Defined as
:---------------------- | :---------
`term()`                | `any()`
`binary()`              | `<<_::_ * 8>>`
`bitstring()`           | `<<_::_ * 1>>`
`boolean()`             | `false` \| `true`
`byte()`                | `0..255`
`char()`                | `0..0x10ffff`
`number()`              | `integer()` \| `float()`
`char_list()`           | `[char()]`
`list()`                | `[any()]`
`maybe_improper_list()` | `maybe_improper_list(any(), any())`
`nonempty_list()`       | `nonempty_list(any())`
`iolist()`              | `maybe_improper_list(byte() \| binary() \| iolist(), binary() \| [])`
`iodata()`              | `iolist()` \| `binary()`
`module()`              | `atom()` \| `tuple()`
`arity()`               | `0..255`
`mfa()`                 | `{atom(), atom(), arity()}`
`identifier()`          | `pid()` \| `port()` \| `reference()`
`node()`                | `atom()`
`timeout()`             | `:infinity` \| `non_neg_integer()`
`no_return()`           | `none()`
`fun()`                 | `(... -> any)`
`struct()`              | `%{__struct__: atom()}`
`as_boolean(t)`         | `t`
`keyword()`             | `[{atom(), any()}]`
`keyword(t)`            | `[{atom(), t}]`

### Remote types

Any module is also able to define its own type and the modules in Elixir are no exception. For example, a string is `String.t`, a range is `Range.t`, any enumerable can be `Enum.t` and so on.

## Defining a type

    @type type_name :: type
    @typep type_name :: type
    @opaque type_name :: type

A type defined with `@typep` is private. An opaque type, defined with `@opaque` is a type where the internal structure of the type will not be visible, but the type is still public.

Types can be parameterized by defining variables as parameters, these variables can then be used to define the type.

    @type dict(key, value) :: [{key, value}]

## Defining a specification

    @spec function_name(type1, type2) :: return_type
    @callback function_name(type1, type2) :: return_type
    @macrocallback macro_name(type1, type2) :: Macro.t

Callbacks are used to define the callbacks functions of behaviours (see `Behaviour`).

Guards can be used to restrict type variables given as arguments to the function.

    @spec function(arg) :: [arg] when arg: atom

Type variables with no restriction can also be defined.

    @spec function(arg) :: [arg] when arg: var

Specifications can be overloaded just like ordinary functions.

    @spec function(integer) :: atom
    @spec function(atom)    :: integer

## Notes

Elixir discourages the use of type `string` as it might be confused with binaries which are referred to as "strings" in Elixir (as opposed to character lists). In order to use the type that is called `string` in Erlang, one has to use the `char_list` type which is a synonym for `string`. If you use `string`, you'll get a warning from the compiler.

If you want to refer to the "string" type (the one operated on by functions in the `String` module), use `String.t` type instead.

In map and struct type declarations such as `%{key: value}` or `%Struct{key: value}`, the key-value pair type information is not used by the current version of dialyzer.
