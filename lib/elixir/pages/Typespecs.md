# Typespecs

Elixir comes with a notation for declaring types and specifications. Elixir is a dynamically typed language, and as such, type specifications are never used by the compiler to optimize or modify code. Still, using type specifications is useful because

  * they provide documentation (for example, tools such as [ExDoc](https://github.com/elixir-lang/ex_doc) show type specifications in the documentation)
  * they're used by tools such as [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html), that can analyze code with typespec to find type inconsistencies and possible bugs

Type specifications (sometimes referred to as *typespecs*) are defined in different contexts using the following attributes:

  * `@type`
  * `@opaque`
  * `@typep`
  * `@spec`
  * `@callback`
  * `@macrocallback`

See the "Defining a type" and "Defining a specification" sub-sections below for more information on defining types and typespecs.

## Types and their syntax

The syntax Elixir provides for type specifications is similar to [the one in Erlang](http://www.erlang.org/doc/reference_manual/typespec.html). Most of the built-in types provided in Erlang (for example, `pid()`) are expressed in the same way: `pid()` (or simply `pid`). Parametrized types (such as `list(integer)`) are supported as well and so are remote types (such as `Enum.t`). Integers and atom literals are allowed as types (e.g., `1`, `:atom`, or `false`). All other types are built out of unions of predefined types. Some shorthands are allowed, such as `[...]`, `<<>>`, and `{...}`.

### Basic types

    type :: any()                   # the top type, the set of all terms
          | none()                  # the bottom type, contains no terms
          | atom()
          | map()                   # any map
          | pid()                   # process identifier
          | port()
          | reference()
          | struct()                # any struct
          | tuple()                 # tuple of any size

                                    ## Numbers
          | float()
          | integer()
          | neg_integer()           # ..., -3, -2, -1
          | non_neg_integer()       # 0, 1, 2, 3, ...
          | pos_integer()           # 1, 2, 3, ...

                                                        ## Lists
          | list(type)                                  # proper list ([]-terminated)
          | nonempty_list(type)                         # non-empty proper list
          | maybe_improper_list(type1, type2)           # proper or improper list
          | nonempty_improper_list(type1, type2)        # improper list
          | nonempty_maybe_improper_list(type1, type2)  # non-empty proper or improper list

          | Literals                # Described in section "Literals"
          | Builtin                 # Described in section "Built-in types"
          | Remotes                 # Described in section "Remote types"
          | UserDefined             # Described in section "User-defined types"

### Literals

The following literals are also supported in typespecs:

    type ::                               ## Atoms
            :atom                         # atoms: :foo, :bar, ...
          | true | false | nil            # special atom literals

                                          ## Bitstrings
          | <<>>                          # empty bitstring
          | <<_::size>>                   # size is 0 or a positive integer
          | <<_::_*unit>>                 # unit is an integer from 1 to 256
          | <<_::size, _::_*unit>>

                                          ## Functions
          | (... -> type)                 # any arity, returns type
          | (() -> type)                  # 0-arity, returns type
          | (type1, type2 -> type)        # 2-arity, returns type

                                          ## Integers
          | 1                             # integer
          | 1..10                         # integer from 1 to 10

                                          ## Lists
          | [type]                        # list with any number of type elements
          | []                            # empty list
          | [...]                         # shorthand for nonempty_list(any())
          | [type, ...]                   # shorthand for nonempty_list(type)
          | [key: value_type]             # keyword list with key :key of value_type

                                                  ## Maps
          | %{}                                   # empty map
          | %{key: value_type}                    # map with required key :key of value_type
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
`bitstring()`           | `<<_::_*1>>`
`boolean()`             | `false` \| `true`
`byte()`                | `0..255`
`char()`                | `0..0x10FFFF`
`charlist()`            | `[char()]`
`nonempty_charlist()`   | `[char(), ...]`
`fun()`                 | `(... -> any)`
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

### Remote types

Any module is also able to define its own types and the modules in Elixir are no exception. For example, the `Range` module defines a `t/0` type that represents a range: this type can be referred to as `t:Range.t/0`. In a similar fashion, a string is `t:String.t/0`, any enumerable can be `t:Enum.t/0`, and so on.

### Maps

The key types in maps are allowed to overlap, and if they do, the leftmost key takes precedence.
A map value does not belong to this type if it contains a key that is not in the allowed map keys.

If you want to denote that keys that were not previously defined in the map are allowed,
it is common to end a map type with `optional(any) => any`.

Notice that the syntactic representation of `map()` is `%{optional(any) => any}`, not `%{}`. The notation `%{}` specifies the singleton type for the empty map.

### User-defined types

The `@type`, `@typep`, and `@opaque` module attributes can be used to define new types:

    @type type_name :: type
    @typep type_name :: type
    @opaque type_name :: type

A type defined with `@typep` is private. An opaque type, defined with `@opaque` is a type where the internal structure of the type will not be visible, but the type is still public.

Types can be parameterized by defining variables as parameters; these variables can then be used to define the type.

    @type dict(key, value) :: [{key, value}]

## Defining a specification

    @spec function_name(type1, type2) :: return_type
    @callback function_name(type1, type2) :: return_type
    @macrocallback macro_name(type1, type2) :: Macro.t

Callbacks are used to define the callbacks functions of behaviours (see the ["Behaviours"](behaviours.html) page in the documentation for more information on behaviours).

Guards can be used to restrict type variables given as arguments to the function.

    @spec function(arg) :: [arg] when arg: atom

If you want to specify more than one variable, you separate them by a comma.

    @spec function(arg1, arg2) :: [arg1, arg2] when arg1: atom, arg2: integer

Type variables with no restriction can also be defined.

    @spec function(arg) :: [arg] when arg: var

You can also name your arguments in a typespec using `arg_name :: arg_type` syntax. This is particularly useful in documentation as a way to differentiate multiple arguments of the same type (or multiple elements of the same type in a type definition):

    @spec days_since_epoch(year :: integer, month :: integer, day :: integer) :: integer
    @type color :: {red :: integer, green :: integer, blue :: integer}

Specifications can be overloaded just like ordinary functions.

    @spec function(integer) :: atom
    @spec function(atom) :: integer

## Notes

Elixir discourages the use of type `t:string/0` as it might be confused with binaries which are referred to as "strings" in Elixir (as opposed to character lists). In order to use the type that is called `t:string/0` in Erlang, one has to use the `t:charlist/0` type which is a synonym for `string`. If you use `string`, you'll get a warning from the compiler.

If you want to refer to the "string" type (the one operated on by functions in the `String` module), use `t:String.t/0` type instead.
