# Gradual set-theoretic types

Elixir is in the process of incorporating set-theoretic types into the compiler. This document outlines the current stage of our implementation for this Elixir version. Elixir's type system is:

  * **sound** - the inferred and assigned by the type system align with the behaviour of the program

  * **gradual** - Elixir's type system includes the `dynamic()` type, which can be used when the type of a varible or expression is checked at runtime. In the absense of `dynamic()`, Elixir's type system behaves as a static one

  * **set-theoretic** - the types are described, implemented, and composed using basic set operations: unions, intersections, and negation

The current milestone aims to infer types from patterns and guards and use them to type check programs, enabling the Elixir compiler to find faults and bugs in codebases without requiring changes to existing software. User provided type signatures are planned for future releases. The underlying principles, theory, and roadmap of our work have been outlined in ["The Design Principles of the Elixir Type System" by Giuseppe Castagna, Guillaume Duboc, José Valim](https://arxiv.org/abs/2306.06391).

## Supported types

At the moment, Elixir developers interact with set-theoretic types through warnings found by the type system. These warnings will represent tyoes using the following notation:

  * `binary()`, `integer()`, `float()`, `pid()`, `port()`, `reference()` - these types are indivisible. This means both `1` and `13` get the same `integer()` type.

  * `atom()` - it represents all atoms and it is divisible. For instance, the atom `:foo` and `:hello_world` are also valid (distinct) types.

  * `tuple()` - it represents all tuples. Tuples may also be written using the curly brackets syntax, such as `{:ok, binary()}`. A `...` at the end of the tuple means the overall size of the tuple is unknown. For example, the following tuple has at least two elements: `{:ok, binary(), ...}`.

  * `list(type)` - it represents a list of `type`. More precisely, it can be written as `empty_list() or non_empty_list(type, empty_list())`. Improper lists, which are lists which do not end with an empty list, such as `[1, 2 | 3]`, can be written as `list(integer(), integer())`.

  * `map()` and structs - maps can be "closed" or "open". Closed maps only allow the specified keys, such as `%{key: atom(), value: integer()}`. Open maps support any other keys in addition to the ones listed and their definition starts with `...`, such as `%{..., key: atom(), value: integer()}`. Structs are closed maps with the `__struct__` key.

  * `function()` - it represents anonymous functions (which may be closures)

## Set operations

We compose set-theoretic types by using set operations (hence the name). For example, to say a function returns either atoms or integers, one could write: `atom() or integer()`.

Intersections are available via the `and` operator, such as `atom() and integer()`, which in this case it becomes the empty set `none()`. `term()` is the union of all types, also known as the "top" type.

Intersections are useful when modelling functions. For example, imagine the following function:

```elixir
def negate(x) when is_integer(x), do: -x
def negate(x) when is_boolean(x), do: not x
```

If you give it an integer, it negates it. If you give it a boolean, it negates it.

We can say this function has the type `(integer() -> integer())` because it is capable of receiving an integer and returning an integer. In this case, `(integer() -> integer())` is a set that represents all functions that can receive an integer and return an integer. Even though this function can receive other arguments and return other values, it is still part of the `(integer() -> integer())` set.

This function also has the type `(boolean() -> boolean())`, because it receives booleans and returns booleans. Therefore, we can say the overall type of the function is `(integer() -> integer()) and (boolean() -> boolean())`. The intersection means the function belongs to both sets.

At this point, you may ask, why not a union? As a real-world example, take a t-shirt with green and yellow stripes. We can say the t-shirt belongs to the set of "t-shirts with green color". We can also say the t-shirt belongs to the set of "t-shirts with yellow color". Let's see the difference between unions and intersections:

  * `(t_shirts_with_green() or t_shirts_with_yellow())` - contains t-shirts with either green or yellow, such as green, green and red, green and yellow, yellow, yellow and red, etc.

  * `(t_shirts_with_green() and t_shirts_with_yellow())` - contains t-shirts with both green and yellow (and also other colors)

Since the t-shirt has both colors, we say it belongs to the intersection of both sets. The same way that a function that goes from `(integer() -> integer())` and `(boolean() -> boolean())` is also an intersection. In practice, it does not make sense to define the union of two functions in Elixir, so the compiler will always point to the right direction.

Finally, we can also negate types by using `not`. For example, to express all atoms, except the atoms `:foo` and `:bar`, one can write: `atom() and not (:foo or :bar)`.

## The `dynamic()` type

Existing Elixir programs do not have type declarations, but we still want to be able to type check them. This is done with the introduction of the `dynamic()` type.

When Elixir sees the following function:

```elixir
def negate(x) when is_integer(x), do: -x
def negate(x) when is_boolean(x), do: not x
```

Elixir type checks it as if the function had the type `(dynamic() -> dynamic())`. Then, based on patterns and guards, we can refine the value of the variable `x` to be `dynamic() and integer()` and `dynamic() and boolean()` for each clause respectively. We say `dynamic()` is a gradual type, which leads us to *gradual set-theoretic types*.

The simplest way to reason about `dynamic()` in Elixir is that it is a range of types. If you have a type `atom() or integer()`, the underlying code needs to work with both `atom() or integer()`. For example, if you call `Integer.to_string(var)`, and `var` has type `atom() or integer()`, the type system will emit a warning, because `Integer.to_string/1` does not accept atoms.

However, by intersecting a type with `dynamic()`, we make the type gradual and therefore only a subset of the type needs to be valid. For instance, if you call `Integer.to_string(var)`, and `var` has type `dynamic() and (atom() or integer())`, the type system will not emit a warning, because `Integer.to_string/1` works with at least one of the types. For convenience, most programs will write `dynamic(atom() or integer())` instead of the intersection. They are equivalent.

Compared to other gradually typed languages, the `dynamic()` type in Elixir is quite powerful: it restricts our program to certain types, via intersections, while still emitting warnings once it is certain the code will fail. This makes `dynamic()` an excellent tool for typing existing Elixir code with meaningful warnings.

If the user provides their own types, and those types are not `dynamic()`, then Elixir's type system behaves as a statically typed one. This brings us to one last property of dynamic types in Elixir: dynamic types are always at the root. For example, when you write a tuple of type `{:ok, dynamic()}`, Elixir will rewrite it to `dynamic({:ok, term()})`. While this has the downside that you cannot make part of a tuple/map/list gradual, only the whole tuple/map/list, it comes with the upside that dynamic is always explicitly at the root, making it harder to accidentally sneak `dynamic()` in a statically typed program.

## Type inference

Type inference (or reconstruction) is the ability of a type system automatically deduce, either partially or fully, the type of an expression at compile time. Type inference may occur at different levels. For example, many programming languages can automatically infer the types of variables, also known "local type inference", but not all can infer type signatures. In other words, they may not reconstruct the arguments types and return types of a function.

Inferring type signatures comes with a series of trade-offs:

  * Speed - type inference algorithms are often more computationally intensive than type checking algorithms.

  * Expressiveness - in any given type system, the constructs that support inference are always a subset of those that can be type-checked. Therefore, if a programming language is restricted to fully reconstructed types, it is less expressive than a solely type checked counterpart.

  * Incremental compilation - type inference complicates incremental compilation. If module A depends on module B, which depends on module C, a change to C may require the type signature in B to be reconstructed, which may then require A to be recomputed (and so on). This dependency chain may require large projects to explicitly add type signatures for stability and compilation efficiency.

  * Cascading errors - when a user accidentally makes type errors or the code has conflicting assumptions, type inference may lead to less clear error messages as the type system tries to reconcile diverging type assumptions across code paths.

On the other hand, type inference offers the benefit of enabling type checking for functions and codebases without requiring the user to add type annotations. To balance these trade-offs, Elixir’s type system provides the following type reconstruction capabilities:

  * Local type inference - the type system automatically infer the types of variables, at the place those variables are defined.

  * Type inference of patterns (and guards in future releases) - the argument types of a function are automatically inferred based on patterns and guards, which capture and narrow types based on common Elixir constructs.

  * Module-local inference of return types - the gradual return types of functions are computed considering all of the functions within the module itself. Any call to a function in another module is conservatively assumed to return `dynamic()`.

The last two items offer gradual reconstruction of type signatures. Our goal is to provide an efficient type reconstruction algorithm that can detect definite bugs in dynamic codebases, even in the absence of explicit type annotations. The gradual system focuses on proving cases where all combinations of a type *will* fail, rather than issuing warnings for cases where some combinations *might* error.

Once Elixir introduces typed function signatures (see "Roadmap"), any function with an explicit type signature will be checked against the user-provided type, as in other statically typed languages, without performing type inference of the function signature.

## Roadmap

The current milestone is to implement type inference of patterns and guards, as well as type checking of all language contructs, without changes to the Elixir language. At this stage, we want to collect feedback on the quality of error messages and performance, and therefore the type system has no user facing API. Full type inference of patterns was released in Elixir v1.18, and inference of guards is expected as part of Elixir v1.19.

If the results are satisfactory, the next milestone will include a mechanism for defining typed structs. Elixir programs frequently pattern match on structs, which reveals information about the struct fields, but it knows nothing about their respective types. By propagating types from structs and their fields throughout the program, we will increase the type system’s ability to find errors while further straining our type system implementation. Proposals including the required changes to the language surface will be sent to the community once we reach this stage.

The third milestone is to introduce set-theoretic type signatures for functions. Unfortunately, the existing Erlang Typespecs are not precise enough for set-theoretic types and they will be phased out of the language and have their postprocessing moved into a separate library once this stage concludes.

## Acknowledgements

The type system was made possible thanks to a partnership between  [CNRS](https://www.cnrs.fr/) and [Remote](https://remote.com/). The research was partially supported by [Supabase](https://supabase.com/) and [Fresha](https://www.fresha.com/). The development work is sponsored by [Fresha](https://www.fresha.com/), [Starfish*](https://starfish.team/), and [Dashbit](https://dashbit.co/).
