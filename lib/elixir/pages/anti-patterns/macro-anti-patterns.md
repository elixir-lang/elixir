# Meta-programming anti-patterns

This document outlines anti-patterns related to meta-programming.

## Unnecessary macros

#### Problem

`Macros` are powerful meta-programming mechanisms that can be used in Elixir to extend the language. While implementing `macros` is not an anti-pattern in itself, this meta-programming mechanism should only be used when absolutely necessary. Whenever a `macros` is implemented, and it was possible to solve the same problem using functions or other pre-existing Elixir structures, the code becomes unnecessarily more complex and less readable. Because `macros` are more difficult to implement and understand, their indiscriminate use can compromise the evolution of a system, reducing its maintainability.

#### Example

The `MyMath` module implements the `sum/2` macro to perform the sum of two numbers received as parameters. While this code has no syntax errors and can be executed correctly to get the desired result, it is unnecessarily more complex. By implementing this functionality as a macro rather than a conventional function, the code became less clear and less objective:

```elixir
defmodule MyMath do
  defmacro sum(v1, v2) do
    quote do
      unquote(v1) + unquote(v2)
    end
  end
end
```
```elixir
iex> require MyMath
MyMath
iex> MyMath.sum(3, 5)
8
iex> MyMath.sum(3+1, 5+6)
15
```

#### Refactoring

To remove this anti-pattern, the developer must replace the unnecessary macro with structures that are simpler to write and understand, such as named functions. The code shown below is the result of the refactoring of the previous example. Basically, the `sum/2` macro has been transformed into a conventional named function. Note that the `require` command is no longer needed:

```elixir
defmodule MyMath do
  def sum(v1, v2) do   # <= macro became a named function!
    v1 + v2
  end
end
```
```elixir
iex> MyMath.sum(3, 5)
8
iex> MyMath.sum(3+1, 5+6)
15
```

## Large code generation by macros

TODO.

## `use` instead of `import`

#### Problem

Elixir has mechanisms such as `import/1`, `alias/1`, and `use/1` to establish dependencies between modules. Code implemented with these mechanisms does not characterize a smell by itself. However, while the `import/1` and `alias/1` directives have lexical scope and only facilitate a module calling functions of another, the `use/1` directive has a *broader scope*, which can be problematic.

The `use/1` directive allows a module to inject any type of code into another, including propagating dependencies. In this way, using the `use/1` directive makes code harder to read, because to understand exactly what will happen when it references a module, it is necessary to have knowledge of the internal details of the referenced module.

#### Example

The code shown below is an example of this anti-pattern. It defines three modules -- `ModuleA`, `Library`, and `ClientApp`. `ClientApp` is reusing code from the `Library` via the `use/1` directive, but is unaware of its internal details. This makes it harder for the author of `ClientApp` to visualize which modules and functionality are now available within its module. To make matters worse, `Library` also imports `ModuleA`, which defines a `foo/0` function that conflicts with a local function defined in `ClientApp`:

```elixir
defmodule ModuleA do
  def foo do
    "From Module A"
  end
end
```

```elixir
defmodule Library do
  defmacro __using__(_opts) do
    quote do
      import Library
      import ModuleA  # <= propagating dependencies!
    end
  end

  def from_lib do
    "From Library"
  end
end
```

```elixir
defmodule ClientApp do
  use Library

  def foo do
    "Local function from client app"
  end

  def from_client_app do
    from_lib() <> " - " <> foo()
  end
end
```

When we try to compile `ClientApp`, Elixir detects the conflict and throws the following error:

```
error: imported ModuleA.foo/0 conflicts with local function
  └ client_app.ex:4:
```

#### Refactoring

To remove this anti-pattern, we recommend library authors to avoid providing `__using__/1` callbacks whenever it can be replaced by `alias/1` or `import/1` directives. In the following code, we assume `use Library` is no longer available and `ClientApp` was refactored in this way, and with that, the code is clearer and the conflict as previously shown no longer exists:

```elixir
defmodule ClientApp do
  import Library

  def foo do
    "Local function from client app"
  end

  def from_client_app do
    from_lib() <> " - " <> foo()
  end
end
```

```elixir
iex> ClientApp.from_client_app()
"From Library - Local function from client app"
```

**Additional remarks:** In situations where you need to do more than importing and aliasing modules, providing `use MyModule` may be necessary, as it provides a common extension point within the Elixir ecosystem.

Therefore, to provide guidance and clarity, we recommend library authors to include an admonition block in their `@moduledoc` that explains how `use MyModule` impacts the developer's code. As an example, the `GenServer` documentation outlines:

> #### `use GenServer` {: .info}
>
> When you `use GenServer`, the `GenServer` module will
> set `@behaviour GenServer` and define a `child_spec/1`
> function, so your module can be used as a child
> in a supervision tree.

Think of this summary as a ["Nutrition facts label"](https://en.wikipedia.org/wiki/Nutrition_facts_label) for code generation. Make sure to only list changes made to the public API of the module. For example, if `use Library` sets an internal attribute called `@_some_module_info` and this attribute is never meant to be public, avoid documenting it in the nutrition facts.

For convenience, the markup notation to generate the admonition block above is this:

```markdown
> #### `use GenServer` {: .info}
>
> When you `use GenServer`, the `GenServer` module will
> set `@behaviour GenServer` and define a `child_spec/1`
> function, so your module can be used as a child
> in a supervision tree.
```
