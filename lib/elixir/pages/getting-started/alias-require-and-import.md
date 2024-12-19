# alias, require, import, and use

In order to facilitate software reuse, Elixir provides three directives (`alias`, `require`, and `import`) plus a macro called `use` summarized below:

```elixir
# Alias the module so it can be called as Bar instead of Foo.Bar
alias Foo.Bar, as: Bar

# Require the module in order to use its macros
require Foo

# Import functions from Foo so they can be called without the `Foo.` prefix
import Foo

# Invokes the custom code defined in Foo as an extension point
use Foo
```

We are going to explore them in detail now. Keep in mind the first three are called directives because they have *lexical scope*, while `use` is a common extension point that allows the used module to inject code.

## alias

`alias` allows you to set up aliases for any given module name.

Imagine a module uses a specialized list implemented in `Math.List`. The `alias` directive allows referring to `Math.List` just as `List` within the module definition:

```elixir
defmodule Stats do
  alias Math.List, as: List
  # In the remaining module definition List expands to Math.List.
end
```

The original `List` can still be accessed within `Stats` by the fully-qualified name `Elixir.List`.

> All modules defined in Elixir are defined inside the main `Elixir` namespace, such as `Elixir.String`. However, for convenience, you can omit "Elixir." when referencing them.

Aliases are frequently used to define shortcuts. In fact, calling `alias` without an `:as` option sets the alias automatically to the last part of the module name, for example:

```elixir
alias Math.List
```

Is the same as:

```elixir
alias Math.List, as: List
```

Note that `alias` is *lexically scoped*, which allows you to set aliases inside specific functions:

```elixir
defmodule Math do
  def plus(a, b) do
    alias Math.List
    # ...
  end

  def minus(a, b) do
    # ...
  end
end
```

In the example above, since we are invoking `alias` inside the function `plus/2`, the alias will be valid only inside the function `plus/2`. `minus/2` won't be affected at all.

## require

Elixir provides macros as a mechanism for meta-programming (writing code that generates code). Macros are expanded at compile time.

Public functions in modules are globally available, but in order to use macros, you need to opt-in by requiring the module they are defined in.

```elixir
iex> Integer.is_odd(3)
** (UndefinedFunctionError) function Integer.is_odd/1 is undefined or private. However, there is a macro with the same name and arity. Be sure to require Integer if you intend to invoke this macro
    (elixir) Integer.is_odd(3)
iex> require Integer
Integer
iex> Integer.is_odd(3)
true
```

In Elixir, `Integer.is_odd/1` is defined as a macro so that it can be used as a guard. This means that, in order to invoke `Integer.is_odd/1`, we need to first require the `Integer` module.

Note that like the `alias` directive, `require` is also lexically scoped. We will talk more about macros in a later chapter.

## import

We use `import` whenever we want to access functions or macros from other modules without using the fully-qualified name. Note we can only import public functions, as private functions are never accessible externally.

For example, if we want to use the `duplicate/2` function from the `List` module several times, we can import it:

```elixir
iex> import List, only: [duplicate: 2]
List
iex> duplicate(:ok, 3)
[:ok, :ok, :ok]
```

We imported only the function `duplicate` (with arity 2) from `List`. Although `:only` is optional, its usage is recommended in order to avoid importing all the functions of a given module inside the current scope. `:except` could also be given as an option in order to import everything in a module except a list of functions.

Note that `import` is *lexically scoped* too. This means that we can import specific macros or functions inside function definitions:

```elixir
defmodule Math do
  def some_function do
    import List, only: [duplicate: 2]
    duplicate(:ok, 10)
  end
end
```

In the example above, the imported `List.duplicate/2` is only visible within that specific function. `duplicate/2` won't be available in any other function in that module (or any other module for that matter).

While `import`s can be useful for frameworks and libraries to build abstractions, developers should generally prefer `alias` to `import` on their own codebases, as aliases make the origin of the function being invoked clearer.

## use

The `use` macro is frequently used as an extension point. This means that, when you `use` a module `FooBar`, you allow that module to inject *any* code in the current module, such as importing itself or other modules, defining new functions, setting a module state, etc.

For example, in order to write tests using the ExUnit framework, a developer should use the `ExUnit.Case` module:

```elixir
defmodule AssertionTest do
  use ExUnit.Case, async: true

  test "always pass" do
    assert true
  end
end
```

Behind the scenes, `use` requires the given module and then calls the `__using__/1` callback on it allowing the module to inject some code into the current context. Some modules (for example, the above `ExUnit.Case`, but also `Supervisor` and `GenServer`) use this mechanism to populate your module with some basic behaviour, which your module is intended to override or complete.

Generally speaking, the following module:

```elixir
defmodule Example do
  use Feature, option: :value
end
```

is compiled into

```elixir
defmodule Example do
  require Feature
  Feature.__using__(option: :value)
end
```

Since `use` allows any code to run, we can't really know the side-effects of using a module without reading its documentation. Therefore use this function with care and only if strictly required. Don't use `use` where an `import` or `alias` would do.

## Understanding Aliases

At this point, you may be wondering: what exactly is an Elixir alias and how is it represented?

An alias in Elixir is a capitalized identifier (like `String`, `Keyword`, etc) which is converted to an atom during compilation. For instance, the `String` alias translates by default to the atom `:"Elixir.String"`:

```elixir
iex> is_atom(String)
true
iex> to_string(String)
"Elixir.String"
iex> :"Elixir.String" == String
true
```

By using the `alias/2` directive, we are changing the atom the alias expands to.

Aliases expand to atoms because in the Erlang Virtual Machine (and consequently Elixir) modules are always represented by atoms:

```elixir
iex> List.flatten([1, [2], 3])
[1, 2, 3]
iex> :"Elixir.List".flatten([1, [2], 3])
[1, 2, 3]
```

That's the mechanism we use to call Erlang modules:

```elixir
iex> :lists.flatten([1, [2], 3])
[1, 2, 3]
```

## Module nesting

Now that we have talked about aliases, we can talk about nesting and how it works in Elixir. Consider the following example:

```elixir
defmodule Foo do
  defmodule Bar do
  end
end
```

The example above will define two modules: `Foo` and `Foo.Bar`. The second can be accessed as `Bar` inside `Foo` as long as they are in the same lexical scope.

If, later, the `Bar` module is moved outside the `Foo` module definition, it must be referenced by its full name (`Foo.Bar`) or an alias must be set using the `alias` directive discussed above.

**Note**: in Elixir, you don't have to define the `Foo` module before being able to define the `Foo.Bar` module, as they are effectively independent. The above could also be written as:

```elixir
defmodule Foo.Bar do
end

defmodule Foo do
  alias Foo.Bar
  # Can still access it as `Bar`
end
```

Aliasing a nested module does not bring parent modules into scope. Consider the following example:

```elixir
defmodule Foo do
  defmodule Bar do
    defmodule Baz do
    end
  end
end

alias Foo.Bar.Baz
# The module `Foo.Bar.Baz` is now available as `Baz`
# However, the module `Foo.Bar` is *not* available as `Bar`
```

As we will see in later chapters, aliases also play a crucial role in macros, to guarantee they are hygienic.

## Multi alias/import/require/use

It is possible to `alias`, `import`, `require`, or `use` multiple modules at once. This is particularly useful once we start nesting modules, which is very common when building Elixir applications. For example, imagine you have an application where all modules are nested under `MyApp`, you can alias the modules `MyApp.Foo`, `MyApp.Bar` and `MyApp.Baz` at once as follows:

```elixir
alias MyApp.{Foo, Bar, Baz}
```

With this, we have finished our tour of Elixir modules. The next topic to cover is module attributes.
