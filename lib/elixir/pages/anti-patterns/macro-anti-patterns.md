<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Meta-programming anti-patterns

This document outlines potential anti-patterns related to meta-programming.

## Compile-time dependencies

#### Problem

This anti-pattern is related to dependencies between files in Elixir. Because macros are used at compile-time, the use of any macro in Elixir adds a compile-time dependency to the module that defines the macro.

However, when macros are used in the body of a module, the arguments to the macro themselves may become compile-time dependencies. These dependencies may lead to dependency graphs where changing a single file causes several files to be recompiled.

#### Example

Let's take the [`Plug`](https://github.com/elixir-plug/plug) library as an example. The `Plug` project allows you to specify several modules, also known as plugs, which will be invoked whenever there is a request. As a user of `Plug`, you would use it as follows:

```elixir
defmodule MyApp do
  use Plug.Builder

  plug MyApp.Authentication
end
```

And imagine `Plug` has the following definitions of the macros above (simplified):

```elixir
defmodule Plug.Builder do
  defmacro __using__(_opts) do
    quote do
      Module.register_attribute(__MODULE__, :plugs, accumulate: true)
      @before_compile Plug.Builder
    end
  end

  defmacro plug(mod) do
    quote do
      @plugs unquote(mod)
    end
  end

  ...
end
```

The implementation accumulates all modules inside the `@plugs` module attribute. Right before the module is compiled, `Plug.Builder` will reads all modules stored in `@plugs` and compile them into a function, like this:

```elixir
def call(conn, _opts) do
  MyApp.Authentication.call(conn)
end
```

The trouble with the code above is that, because the `plug MyApp.Authentication` was invoked at compile-time, the module `MyApp.Authentication` is now a compile-time dependency of `MyApp`, even though `MyApp.Authentication` is never used at compile-time. If `MyApp.Authentication` depends on other modules, even at runtime, this can now lead to a large recompilation graph in case of changes.

#### Refactoring

To address this anti-pattern, a macro can expand literals within the context they are meant to be used, as follows:

```elixir
  defmacro plug(mod) do
    mod = Macro.expand_literals(mod, %{__CALLER__ | function: {:call, 2}})

    quote do
      @plugs unquote(mod)
    end
  end
```

In the example above, since `mod` is used only within the `call/2` function, we prematurely expand module reference as if it was inside the `call/2` function. Now `MyApp.Authentication` is only a runtime dependency of `MyApp`, no longer a compile-time one.

Note, however, the above must only be done if your macros do not attempt to invoke any function, access any struct, or any other metadata of the module at compile-time. If you interact with the module given to a macro anywhere outside of definition of a function, then you effectively have a compile-time dependency. And, even though you generally want to avoid them, it is not always possible.

In actual projects, developers may use `mix xref trace path/to/file.ex` to execute a file and have it print information about which modules it depends on, and if those modules are compile-time, runtime, or export dependencies. See `mix xref` for more information.

## Large code generation

#### Problem

This anti-pattern is related to macros that generate too much code. When a macro generates a large amount of code, it impacts how the compiler and/or the runtime work. The reason for this is that Elixir may have to expand, compile, and execute the code multiple times, which will make compilation slower and the resulting compiled artifacts larger.

#### Example

Imagine you are defining a router for a web application, where you could have macros like `get/2`. On every invocation of the macro (which could be hundreds), the code inside `get/2` will be expanded and compiled, which can generate a large volume of code overall.

```elixir
defmodule Routes do
  defmacro get(route, handler) do
    quote do
      route = unquote(route)
      handler = unquote(handler)

      if not is_binary(route) do
        raise ArgumentError, "route must be a binary"
      end

      if not is_atom(handler) do
        raise ArgumentError, "handler must be a module"
      end

      @store_route_for_compilation {route, handler}
    end
  end
end
```

#### Refactoring

To remove this anti-pattern, the developer should simplify the macro, delegating part of its work to other functions. As shown below, by encapsulating the code inside `quote/1` inside the function `__define__/3` instead, we reduce the code that is expanded and compiled on every invocation of the macro, and instead we dispatch to a function to do the bulk of the work.

```elixir
defmodule Routes do
  defmacro get(route, handler) do
    quote do
      Routes.__define__(__MODULE__, unquote(route), unquote(handler))
    end
  end

  def __define__(module, route, handler) do
    if not is_binary(route) do
      raise ArgumentError, "route must be a binary"
    end

    if not is_atom(handler) do
      raise ArgumentError, "handler must be a module"
    end

    Module.put_attribute(module, :store_route_for_compilation, {route, handler})
  end
end
```

## Unnecessary macros

#### Problem

*Macros* are powerful meta-programming mechanisms that can be used in Elixir to extend the language. While using macros is not an anti-pattern in itself, this meta-programming mechanism should only be used when absolutely necessary. Whenever a macro is used, but it would have been possible to solve the same problem using functions or other existing Elixir structures, the code becomes unnecessarily more complex and less readable. Because macros are more difficult to implement and reason about, their indiscriminate use can compromise the evolution of a system, reducing its maintainability.

#### Example

The `MyMath` module implements the `sum/2` macro to perform the sum of two numbers received as parameters. While this code has no syntax errors and can be executed correctly to get the desired result, it is unnecessarily more complex. By implementing this functionality as a macro rather than a conventional function, the code became less clear:

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
iex> MyMath.sum(3 + 1, 5 + 6)
15
```

#### Refactoring

To remove this anti-pattern, the developer must replace the unnecessary macro with structures that are simpler to write and understand, such as named functions. The code shown below is the result of the refactoring of the previous example. Basically, the `sum/2` macro has been transformed into a conventional named function. Note that the `require/2` call is no longer needed:

```elixir
defmodule MyMath do
  def sum(v1, v2) do # <= The macro became a named function
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

```text
error: imported ModuleA.foo/0 conflicts with local function
  └ client_app.ex:4:
```

#### Refactoring

To remove this anti-pattern, we recommend library authors avoid providing `__using__/1` callbacks whenever it can be replaced by `alias/1` or `import/1` directives. In the following code, we assume `use Library` is no longer available and `ClientApp` was refactored in this way, and with that, the code is clearer and the conflict as previously shown no longer exists:

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

#### Additional remarks

In situations where you need to do more than importing and aliasing modules, providing `use MyModule` may be necessary, as it provides a common extension point within the Elixir ecosystem.

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

## Untracked compile-time dependencies

#### Problem

This anti-pattern is the opposite of ["Compile-time dependencies"](#compile-time-dependencies) and it happens when a compile-time dependency is accidentally bypassed, making the Elixir compiler unable track dependencies and recompile files correctly. This happens when building aliases (in other words, module names) dynamically, either within a module or within a macro.

#### Example

For example, imagine you invoke a module at compile-time, you could write it as such:

```elixir
defmodule MyModule do
  SomeOtherModule.example()
end
```

In this case, Elixir knows `MyModule` is invoked `SomeOtherModule.example/0` outside of a function, and therefore at compile-time.

Elixir can also track module names even during dynamic calls:

```elixir
defmodule MyModule do
  mods = [OtherModule.Foo, OtherModule.Bar]

  for mod <- mods do
    mod.example()
  end
end
```

In the previous example, even though Elixir does not know which modules the function `example/0` was invoked on, it knows the modules `OtherModule.Foo` and `OtherModule.Bar` are referred outside of a function and therefore they become compile-time dependencies. If any of them change, Elixir will recompile `MyModule` itself.

However, you should not programatically generate the module names themselves, as that would make it impossible for Elixir to track them. More precisely, do not do this:

```elixir
defmodule MyModule do
  parts = [:Foo, :Bar]

  for part <- parts do
    Module.concat(OtherModule, part).example()
  end
end
```

In this case, because the whole module was generated, Elixir sees a dependency only to `OtherModule`, never to `OtherModule.Foo` and `OtherModule.Bar`, potentially leading to inconsistencies when recompiling projects.

A similar bug can happen when abusing the property that aliases are simply atoms, defining the atoms directly. In the case below, Elixir never sees the aliases, leading to untracked compile-time dependencies:

```elixir
defmodule MyModule do
  mods = [:"Elixir.OtherModule.Foo", :"Elixir.OtherModule.Bar"]

  for mod <- mods do
    mod.example()
  end
end
```

#### Refactoring

To address this anti-pattern, you should avoid defining module names programatically. For example, if you need to dispatch to multiple modules, do so by using full module names.

Instead of:

```elixir
defmodule MyModule do
  parts = [:Foo, :Bar]

  for part <- parts do
    Module.concat(OtherModule, part).example()
  end
end
```

Do:

```elixir
defmodule MyModule do
  mods = [OtherModule.Foo, OtherModule.Bar]

  for mod <- mods do
    mod.example()
  end
end
```

If you really need to define modules dynamically, you can do so via meta-programming, building the whole module name at compile-time:

```elixir
defmodule MyMacro do
  defmacro call_examples(parts) do
    for part <- parts do
      quote do
        # This builds OtherModule.Foo at compile-time
        OtherModule.unquote(part).example()
      end
    end
  end
end

defmodule MyModule do
  import MyMacro
  call_examples [:Foo, :Bar]
end
```

In actual projects, developers may use `mix xref trace path/to/file.ex` to execute a file and have it print information about which modules it depends on, and if those modules are compile-time, runtime, or export dependencies. This can help you debug if the dependencies are being properly tracked in relation to external modules. See `mix xref` for more information.
