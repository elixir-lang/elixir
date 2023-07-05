# Meta-programming anti-patterns

This document outlines anti-patterns related to meta-programming.

## Unnecessary macros

TODO.

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
