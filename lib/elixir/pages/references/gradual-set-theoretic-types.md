<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Gradual set-theoretic types

Elixir is in the process of incorporating set-theoretic types into the compiler. This document outlines the current stage of our implementation for this Elixir version. Elixir's type system is:

  * **sound** - the inferred and assigned by the type system align with the behaviour of the program

  * **gradual** - Elixir's type system includes the `dynamic()` type, which can be used when the type of a variable or expression is checked at runtime. However, instead of simply discarding all typing information, Elixir's `dynamic()` type works as a range. For example, if you write `dynamic(integer() or binary())`, Elixir's type system will still emit violations if none of those types are accepted. Furthermore, in the absence of `dynamic()`, Elixir's type system behaves as a static one

  * **developer friendly** - the types are described, implemented, and composed using basic set operations: unions, intersections, and negation (hence it is a set-theoretic type system)

The current milestone aims to infer types from existing programs and use them for type checking, enabling the Elixir compiler to find faults and bugs in codebases without requiring changes to existing software. User provided type signatures are planned for future releases. The underlying principles, theory, and roadmap of our work have been outlined in ["The Design Principles of the Elixir Type System" by Giuseppe Castagna, Guillaume Duboc, José Valim](https://arxiv.org/abs/2306.06391).

## A gentle introduction

Types in Elixir are written using the type named followed by parentheses, such as `integer()` or `list(integer())`.

The basic types are:

```elixir
atom()
binary()
bitstring()
empty_list()
integer()
float()
function()
map()
non_empty_list(elem_type, tail_type)
pid()
port()
reference()
tuple()
```

Many of the types above can also be written more precisely. We will discuss their syntax in the next sections, but here are two examples:

  * While `atom()` represents all atoms, the atom `:ok` can also be represented in the type system as `:ok`

  * While `tuple()` represents all tuples, you can specify the type of a two-element tuple where the first element is the atom `:ok` and the second is an integer as `{:ok, integer()}`

There are also three special types: `none()` (represents an empty set), `term()` (represents all types), `dynamic()` (represents a range of the given types).

Given the types are set-theoretic, we can compose them using unions (`or`), intersections (`and`), and negations (`not`). For example, to say a function returns either atoms or integers, one could write: `atom() or integer()`.

Intersections will find the elements in common between the operands. For example, `atom() and integer()`, which in this case is the empty set `none()`. You can combine intersections and negations to perform difference, for example, to say that a function expects all atoms, except `nil` (which is an atom), you could write: `atom() and not nil`.

You can find a complete reference in the [set-theoretic types cheatsheet](../cheatsheets/types-cheat.cheatmd).

## The syntax of data types

In this section we will cover the syntax of all data types. At the moment, developers will interact with those types mostly through compiler warnings and diagnostics.

### Broad types

These types are broad in that they cannot represent individual elements, only the whole set. For example, the numbers `1` and `42` are both represented by the type `integer()`.

They are: `binary()`, `bitstring()`, `integer()`, `float()`, `pid()`, `port()`, `reference()`.

The `binary()` type is a subtype of the less frequently used `bitstring()` type, as binaries are bitstrings where the number of bits is divisible by 8.

### Atoms

You can represent all atoms as `atom()`. You can also represent each individual atom using their literal syntax. For instance, the atom `:foo` and `:hello_world` are also valid (distinct) types.

`nil`, `true`, and `false` are also atoms and can be written as is. `boolean()` is a convenience type alias that represents `true or false`.

### Tuples

You can represent all tuples as `tuple()`. Tuples may also be written using the curly brackets syntax, such as `{:ok, binary()}`.

You may use `...` at the end of the tuple to imply the overall size of the tuple is unknown. For example, the following tuple has at least two elements: `{:ok, binary(), ...}`.

### Lists

You can represent all _proper_ lists as `list()`, which also includes the empty list.

You can also specify the type of the list element as argument. For example, `list(integer())` represents the values `[]` and `[1, 2, 3]`, but not `[1, "two", 3]`.

Internally, Elixir represents the type `list(a)` as the union two distinct types, `empty_list()` and `not_empty_list(a)`. In other words, `list(integer())` is equivalent to `empty_list() or non_empty_list(integer())`.

#### Improper lists

While most developers will simply use `list(a)`, the type system can express all different representations of lists in Elixirby passing a second argument to `non_empty_list`, which represents the type of the tail.

A proper list is one where the tail is the empty list itself. The type `non_empty_list(integer())` is equivalent to `non_empty_list(integer(), empty_list())`.

If the `tail_type` is anything but a list, then we have an improper list. For example, the value `[1, 2 | 3]` would have the type `non_empty_list(integer(), integer())`.

If you pass a list type as the tail, then the list type is merged into the element type. For example, `non_empty_list(integer(), list(binary()))` is the same as `non_empty_list(integer() or binary(), empty_list())`.

### Maps

You can represent all maps as `map()`.

Maps may also be written using their literal syntax:

```elixir
%{name: binary(), age: integer()}
```

which outlines a map with exactly two keys, `:name` and `:age`, and values of type `binary()` and `integer()` respectively. We say the map above is "closed": it only supports the keys explicitly defined. We can also mark a map as "open", by including `...` as its first element:

```elixir
%{..., name: binary(), age: integer()}
```

The type above says the keys `:name` and `:age` must exist, with their respective types, but other keys may be present. The `map()` type is the same as `%{...}`. For the empty map, you may write `%{}`, although we recommend using `empty_map()` for clarity.

#### Optional keys

A key may be marked as optional using the `if_set/1` operation on its value type:

```elixir
%{name: binary(), age: if_set(integer())}
```

is a map that certainly has the `:name` key but it may have the `:age` key (and if it has such key, its value type is `integer()`).

You can also use `not_set()` to denote a key cannot be present:

```elixir
%{..., age: not_set()}
```

The type above says the map may have any key, except the `:age` one. This is, for instance, the type returned by `Map.delete(map, :age)`.

#### Domain types

In the examples above, all map keys were atoms, but we can also use other types as map keys. For example:

```elixir
# Closed map
%{binary() or atom() => integer()}

# Open map
%{..., binary() or atom() => integer()}
```

Currently, the type system only tracks the top of each individual type as the domain keys. For example, if you say:

```elixir
%{list(integer()) => integer(), list(binary()) => binary()}
```

That's the same as specifying all lists:

```elixir
%{list() => integer() or binary()}
```

The supported domain keys are `atom()`, `bitstring()`, `binary()`, `integer()`, `float()`, `fun()`, `list()`, `map()`, `pid()`, `port()`, `reference()`, and `tuple()`. In the case of maps, the `bitstring()` domain stores exclusively keys which are not binary. The ones which are `binary()` are stored under the `binary()` domain.

Furthermore, it is important to note that domain keys are, by definition, optional. Whenever you have a `%{integer() => integer()}`and you try to fetch a key, we must assume the key may not exist (after all, it is not possible to store all integers as map keys as they are infinite).

#### Mixed keys

It is also possible to mix domain and atom keys. For example, the following map says that all atom keys are of type `binary()`, except the `:root` key, which has type `integer()`:

```elixir
# Closed map
%{atom() => binary(), root: integer()}

# Open map
%{..., atom() => binary(), root: integer()}
```

The order of the keys is of increasing precision. `:root` is more precise than `atom()`, therefore it comes later. This mirrors the runtime semantics of maps, where duplicate keys override the value of earlier ones.

### Functions

You can represent all functions as `function()`.  However, in practice, most functions are represented as arrows. For example, a function that receives an integer and return boolean would be written as `(integer() -> boolean())`. A function that receives two integers and return a string (i.e. a binary) would be written as `(integer(), integer() -> binary())`.

When representing functions with multiple clauses, which may take different input types, we use intersections. For example, imagine the following function:

```elixir
def negate(x) when is_integer(x), do: -x
def negate(x) when is_boolean(x), do: not x
```

If you give it an integer, it negates it. If you give it a boolean, it negates it.

We can say this function has the type `(integer() -> integer())` because it is capable of receiving an integer and returning an integer. In this case, `(integer() -> integer())` is a set that represents all functions that can receive an integer and return an integer. Even though this function can receive other arguments and return other values, it is still part of the `(integer() -> integer())` set.

This function also has the type `(boolean() -> boolean())`, because it also receives booleans and returns booleans. If you pass the function above to another function that expects `(boolean() -> boolean())`, type checking will succeed. Therefore, we can say the overall type of the function is `(integer() -> integer()) and (boolean() -> boolean())`. The intersection means the function belongs to both sets.

At this point, you may ask, why not a union? As a real-world example, take a t-shirt with green and yellow stripes. We can say the t-shirt belongs to the set of "t-shirts with green color". We can also say the t-shirt belongs to the set of "t-shirts with yellow color". Let's see the difference between unions and intersections:

  * `(t_shirts_with_green() or t_shirts_with_yellow())` - contains t-shirts with either green or yellow, such as green, green and red, green and yellow, but also only yellow, yellow and red, etc.

  * `(t_shirts_with_green() and t_shirts_with_yellow())` - contains t-shirts with both green and yellow (and maybe other colors)

Since the t-shirt has both colors, we could say it belongs to the union of green and yellow t-shirts, but doing so would not capture the fact it is both green and yellow. Therefore it is more precise to say it belongs to the intersection of both sets. The same way that a function that goes from `(integer() -> integer())` and `(boolean() -> boolean())` is also an intersection. In practice, it is not useful to define the union of two functions in Elixir, so the compiler will point you to the right direction if you specify the wrong one.

## The `dynamic()` type

Existing Elixir programs do not have type declarations, but we still want to be able to type check them. This is done with the introduction of the `dynamic()` type.

When Elixir sees the following function:

```elixir
def negate(x) when is_integer(x), do: -x
def negate(x) when is_boolean(x), do: not x
```

Elixir type checks it as if the function had the type `(dynamic() -> dynamic())`. Then, based on patterns and guards, we can refine the value of the variable `x` to be `dynamic() and integer()` and `dynamic() and boolean()` for each clause respectively. We say `dynamic()` is a gradual type, which leads us to _gradual set-theoretic types_.

The simplest way to reason about `dynamic()` in Elixir is that it is a range of types. If you have a type `atom() or integer()`, the underlying code needs to work with both `atom() or integer()`. For example, if you call `Integer.to_string(var)`, and `var` has type `atom() or integer()`, the type system will emit a warning, because `Integer.to_string/1` does not accept atoms.

However, by intersecting a type with `dynamic()`, we make the type gradual and therefore only a subset of the type needs to be valid. For instance, if you call `Integer.to_string(var)`, and `var` has type `dynamic() and (atom() or integer())`, the type system will not emit a warning, because `Integer.to_string/1` works with at least one of the types. For convenience, most programs will write `dynamic(atom() or integer())` instead of the intersection. They are equivalent.

Compared to other gradually typed languages, the `dynamic()` type in Elixir is quite powerful: it restricts our program to certain types, via intersections, while still emitting warnings once it is certain the code will fail. This makes `dynamic()` an excellent tool for typing existing Elixir code with meaningful warnings.

If the user provides their own types, and those types are not `dynamic()`, then Elixir's type system behaves as a statically typed one. This brings us to one last property of dynamic types in Elixir: dynamic types are always at the root. For example, when you write a tuple of type `{:ok, dynamic()}`, Elixir will rewrite it to `dynamic({:ok, term()})`. While this has the downside that you cannot make part of a tuple/map/list gradual, only the whole tuple/map/list, it comes with the upside that dynamic is always explicitly at the root, making it harder to accidentally sneak `dynamic()` in a statically typed program.

## Type inference

Type inference (or reconstruction) is the ability of a type system to automatically deduce, either partially or fully, the type of an expression at compile time. Type inference may occur at different levels. For example, many programming languages can automatically infer the types of variables, also known "local type inference", but not all can infer type signatures of functions.

Inferring type signatures comes with a series of trade-offs:

  * Speed - type inference algorithms are often more computationally intensive than type checking algorithms.

  * Expressiveness - in any given type system, the constructs that support inference are always a subset of those that can be type-checked. Therefore, if a programming language is restricted to fully reconstructed types, it is less expressive than a solely type checked counterpart.

  * Incremental compilation - type inference complicates incremental compilation. If module A depends on module B, which depends on module C, a change to C may require the type signature in B to be reconstructed, which may then require A to be recomputed (and so on). This dependency chain may require large projects to explicitly add type signatures for stability and compilation efficiency.

  * Cascading errors - when a user accidentally makes type errors or the code has conflicting assumptions, type inference may lead to less clear error messages as the type system tries to reconcile diverging type assumptions across code paths.

On the other hand, type inference offers the benefit of enabling type checking for functions and codebases without requiring the user to add type annotations. To balance these trade-offs, Elixir aims to provide "module type inference": our goal is to infer the types of functions considering the current module, Elixir's standard library and your dependencies, while calls to modules within the same project are assumed to be `dynamic()`. Once types are inferred, then the whole project is type checked considering all modules and all types (inferred or otherwise).

Type inference in Elixir is best-effort: it doesn't guarantee it will find all possible type incompatibilities, only that it may find bugs where all combinations of a type _will_ fail, even in the absence of explicit type annotations. It is meant to be an efficient routine that brings developers some benefits of static typing without requiring any effort from them.

In the long term, Elixir developers who want typing guarantees must explicitly add type signatures to their functions (see "Roadmap"). Any function with an explicit type signature will be typed checked against the user-provided annotations, as in other statically typed languages, without performing type inference. In summary, type checking will rely on type signatures and only fallback to inferred types when no signature is available.

## Roadmap

The current milestone is to implement type inference of existing codebases, as well as type checking of all language constructs, without changes to the Elixir language. At this stage, we want to collect feedback on the quality of error messages and performance, and therefore the type system has no user facing API. Full type inference of patterns was released in Elixir v1.18, and complete inference is expected as part of Elixir v1.20.

If the results are satisfactory, the next milestone will include a mechanism for defining typed structs. Elixir programs frequently pattern match on structs, which reveals information about the struct fields, but it knows nothing about their respective types. By propagating types from structs and their fields throughout the program, we will increase the type system’s ability to find errors while further straining our type system implementation. Proposals including the required changes to the language surface will be sent to the community once we reach this stage.

The third milestone is to introduce set-theoretic type signatures for functions. Unfortunately, the existing Erlang Typespecs are not precise enough for set-theoretic types and they will be phased out of the language and have their postprocessing moved into a separate library once this stage concludes.

## Acknowledgements

The type system was made possible thanks to a partnership between [CNRS](https://www.cnrs.fr/) and [Remote](https://remote.com/). The development work is currently sponsored by [Fresha](https://www.fresha.com/), and [Tidewave](https://tidewave.ai/).
