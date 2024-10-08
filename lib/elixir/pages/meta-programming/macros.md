# Macros

Even though Elixir attempts its best to provide a safe environment for macros, most of the responsibility of writing clean code with macros falls on developers. Macros are harder to write than ordinary Elixir functions, and it's considered to be bad style to use them when they're not necessary. Write macros responsibly.

Elixir already provides mechanisms to write your everyday code in a simple and readable fashion by using its data structures and functions. Macros should only be used as a last resort. Remember that **explicit is better than implicit**. **Clear code is better than concise code.**

## Our first macro

Macros in Elixir are defined via `defmacro/2`.

> For this guide, we will be using files instead of running code samples in IEx. That's because the code samples will span multiple lines of code and typing them all in IEx can be counter-productive. You should be able to run the code samples by saving them into a `macros.exs` file and running it with `elixir macros.exs` or `iex macros.exs`.

In order to better understand how macros work, let's create a new module where we are going to implement `unless` (which does the opposite of `if/2`), as a macro and as a function:

```elixir
defmodule Unless do
  def fun_unless(clause, do: expression) do
    if(!clause, do: expression)
  end

  defmacro macro_unless(clause, do: expression) do
    quote do
      if(!unquote(clause), do: unquote(expression))
    end
  end
end
```

The function receives the arguments and passes them to `if/2`. However, as we learned in the [previous guide](quote-and-unquote.md), the macro will receive quoted expressions, inject them into the quote, and finally return another quoted expression.

Let's start `iex` with the module above:

```console
$ iex macros.exs
```

and play with those definitions:

```elixir
iex> require Unless
iex> Unless.macro_unless(true, do: IO.puts("this should never be printed"))
nil
iex> Unless.fun_unless(true, do: IO.puts("this should never be printed"))
"this should never be printed"
nil
```

In our *macro* implementation, the sentence was not printed, although it was printed in our *function* implementation. That's because the arguments to a function call are evaluated before calling the function. However, macros do not evaluate their arguments. Instead, they receive the arguments as quoted expressions which are then transformed into other quoted expressions. In this case, we have rewritten our `unless` macro to become an `if/2` behind the scenes.

In other words, when invoked as:

```elixir
Unless.macro_unless(true, do: IO.puts("this should never be printed"))
```

Our `macro_unless` macro received the following:

```elixir
macro_unless(true, [do: {{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}, [], ["this should never be printed"]}])
```

and it then returned a quoted expression as follows:

```elixir
{:if, [],
 [{:!, [], [true]},
  [do: {{:., [],
     [{:__aliases__,
       [], [:IO]},
      :puts]}, [], ["this should never be printed"]}]]}
```

We can actually verify that this is the case by using `Macro.expand_once/2`:

```elixir
iex> expr = quote do: Unless.macro_unless(true, do: IO.puts("this should never be printed"))
iex> res  = Macro.expand_once(expr, __ENV__)
iex> IO.puts(Macro.to_string(res))
if(!true) do
  IO.puts("this should never be printed")
end
:ok
```

`Macro.expand_once/2` receives a quoted expression and expands it according to the current environment. In this case, it expanded/invoked the `Unless.macro_unless/2` macro and returned its result. We then proceeded to convert the returned quoted expression to a string and print it (we will talk about `__ENV__` later in this chapter).

That's what macros are all about. They are about receiving quoted expressions and transforming them into something else.
In fact, `if/2` in Elixir is implemented as a macro:

```elixir
defmacro if(clause, do: expression) do
  quote do
    case clause do
      x when x in [false, nil] -> nil
      _ -> unquote(expression)
  end
end
```

Constructs such as `if/2`, `defmacro/2`, `def/2`, `defprotocol/2`, and many others used throughout the Elixir standard library are written in pure Elixir, often as a macro. This means that the constructs being used to build the language can be used by developers to extend the language to the domains they are working on.

We can define any function and macro we want, including ones that override the built-in definitions provided by Elixir. The only exceptions are Elixir special forms which are not implemented in Elixir and therefore cannot be overridden. The full list of special forms is available in `Kernel.SpecialForms`.

## Macro hygiene

Elixir macros have "late resolution". This guarantees that a variable defined inside a quote won't conflict with a variable defined in the context where that macro is expanded. For example:

```elixir
defmodule Hygiene do
  defmacro no_interference do
    quote do: a = 1
  end
end

defmodule HygieneTest do
  def go do
    require Hygiene
    a = 13
    Hygiene.no_interference()
    a
  end
end

HygieneTest.go()
# => 13
```

In the example above, even though the macro injects `a = 1`, it does not affect the variable `a` defined by the `go/0` function. If a macro wants to explicitly affect the context, it can use `var!/1`:

```elixir
defmodule Hygiene do
  defmacro interference do
    quote do: var!(a) = 1
  end
end

defmodule HygieneTest do
  def go do
    require Hygiene
    a = 13
    Hygiene.interference()
    a
  end
end

HygieneTest.go()
# => 1
```

The code above will work but issue a warning: `variable "a" is unused`. The macro is overriding the original value and the original value is never used.

Variable hygiene only works because Elixir annotates variables with their **context**. For example, a variable `x` defined on line 3 of a module would be represented as:

```elixir
{:x, [line: 3], nil}
```

However, a quoted variable would be represented as:

```elixir
defmodule Sample do
  def quoted do
    quote do: x
  end
end

Sample.quoted() #=> {:x, [line: 3], Sample}
```

Notice that the *third element* in the quoted variable is the atom `Sample`, instead of `nil`, which marks the variable as coming from the `Sample` module. Therefore, Elixir considers these two variables as coming from different contexts and handles them accordingly.

Elixir provides similar mechanisms for imports and aliases too. This guarantees that a macro will behave as specified by its source module rather than conflicting with the target module where the macro is expanded. Hygiene can be bypassed under specific situations by using macros like `var!/2` and `alias!/1`, although one must be careful when using those as they directly change the user environment.

Sometimes variable names might be dynamically created. In such cases, `Macro.var/2` can be used to define new variables:

```elixir
defmodule Sample do
  defmacro initialize_to_char_count(variables) do
    Enum.map(variables, fn name ->
      var = Macro.var(name, nil)
      length = name |> Atom.to_string() |> String.length()

      quote do
        unquote(var) = unquote(length)
      end
    end)
  end

  def run do
    initialize_to_char_count([:red, :green, :yellow])
    [red, green, yellow]
  end
end

> Sample.run() #=> [3, 5, 6]
```

Take note of the second argument to `Macro.var/2`. This is the **context** being used and will determine hygiene as described in the next section. Check out also `Macro.unique_var/2`, for cases when you need to generate variables with unique names.

## The environment

When calling `Macro.expand_once/2` earlier in this chapter, we used the special form `__ENV__/0`.

`__ENV__/0` returns a `Macro.Env` struct which contains useful information about the compilation environment, including the current module, file, and line, all variables defined in the current scope, as well as imports, requires, and more:

```elixir
iex> __ENV__.module
nil
iex> __ENV__.file
"iex"
iex> __ENV__.requires
[IEx.Helpers, Kernel, Kernel.Typespec]
iex> require Integer
nil
iex> __ENV__.requires
[IEx.Helpers, Integer, Kernel, Kernel.Typespec]
```

Many of the functions in the `Macro` module expect a `Macro.Env` environment. You can read more about these functions in `Macro` and learn more about the compilation environment in the `Macro.Env`.

## Private macros

Elixir also supports **private macros** via `defmacrop`. Like private functions, these macros are only available inside the module that defines them, and only at compilation time.

It is important that a macro is defined before its usage. Failing to define a macro before its invocation will raise an error at runtime, since the macro won't be expanded and will be translated to a function call:

```elixir
iex> defmodule Sample do
...>  def four, do: two() + two()
...>  defmacrop two, do: 2
...> end
** (CompileError) iex:2: function two/0 undefined
```

## Write macros responsibly

Macros are a powerful construct and Elixir provides many mechanisms to ensure they are used responsibly.

  * Macros are **hygienic**: by default, variables defined inside a macro are not going to affect the user code. Furthermore, function calls and aliases available in the macro context are not going to leak into the user context.

  * Macros are **lexical**: it is impossible to inject code or macros globally. In order to use a macro, you need to explicitly `require` or `import` the module that defines the macro.

  * Macros are **explicit**: it is impossible to run a macro without explicitly invoking it. For example, some languages allow developers to completely rewrite functions behind the scenes, often via parse transforms or via some reflection mechanisms. In Elixir, a macro must be explicitly invoked in the caller during compilation time.

  * Macros' language is clear: many languages provide syntax shortcuts for `quote` and `unquote`. In Elixir, we preferred to have them explicitly spelled out, in order to clearly delimit the boundaries of a macro definition and its quoted expressions.

Even with such guarantees, the developer plays a big role when writing macros responsibly. If you are confident you need to resort to macros, remember that macros are not your API. Keep your macro definitions short, including their quoted contents. For example, instead of writing a macro like this:

```elixir
defmodule MyModule do
  defmacro my_macro(a, b, c) do
    quote do
      do_this(unquote(a))
      # ...
      do_that(unquote(b))
      # ...
      and_that(unquote(c))
    end
  end
end
```

write:

```elixir
defmodule MyModule do
  defmacro my_macro(a, b, c) do
    quote do
      # Keep what you need to do here to a minimum
      # and move everything else to a function
      MyModule.do_this_that_and_that(unquote(a), unquote(b), unquote(c))
    end
  end

  def do_this_that_and_that(a, b, c) do
    do_this(a)
    ...
    do_that(b)
    ...
    and_that(c)
  end
end
```

This makes your code clearer and easier to test and maintain, as you can invoke and test `do_this_that_and_that/3` directly. It also helps you design an actual API for developers that do not want to rely on macros.

With this guide, we finish our introduction to macros. The next guide is a brief discussion on **DSLs** that shows how we can mix macros and module attributes to annotate and extend modules and functions.
