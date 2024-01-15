# Writing Documentation

Elixir elevates documentation to first-class status, emphasizing ease of writing as well as readability. This guide will teach you to craft effective documentation in Elixir, covering essential constructs like module attributes, typespecs, style practices, and doctests.
## Markdown

Elixir documentation is written in Markdown. Some commonly used elements are: 

  * `[link text](URL)` for hyperlinks.

  * ` ```elixir ` and ` ``` ` for code blocks.
  
  * `#` for H1 headers, `##` for H2 headers, etc.
  
  * `*` for unordered list items, or escape numbered list items with a backslash (`1\.`).

  * `` ` `` for inline code, or double backticks ` `` ` for inline code with backticks, e.g., ``` `` `elixir` `` ```.

We created an [ExDoc Cheatsheet](https://hexdocs.pm/ex_doc/cheatsheet.html) as a supplemental resource.

You can read about admonition blocks and other advanced features at [ExDoc documentation](https://hexdocs.pm/ex_doc/readme.html#admonition-blocks). 
 
Just getting started? We recommend first reading [Basic Writing and Formatting Syntax](https://help.github.com/articles/basic-writing-and-formatting-syntax/) by GitHub.

### Module Attributes

The module attributes `@moduledoc` and `@doc` attach documentation to the module `MyApp.Hello`.

```elixir 
defmodule MyApp.Hello do
  @moduledoc """
  This is the Hello module.
  """
  @moduledoc since: "1.0.0"

  @doc """
  Says hello to the given `name`.

  Returns `:ok`.

  ## Examples

      iex> MyApp.Hello.world(:john)
      :ok

  """
  @doc since: "1.3.0"
  def world(name) do
    IO.puts("hello #{name}")
  end
end
```

The `@moduledoc` attribute attaches documentation for the current module.

The `@doc` attribute provides documentation for the function `world/1`.

## Function Arguments

When documenting a function, argument names are inferred by the compiler. For example:

```elixir
def size(%{size: size}) do
  size
end
```

Sometimes the inference will be suboptimal, especially if the function contains multiple clauses with the argument matching on different values each time. You can specify the proper names for documentation by declaring only the function head at any moment before the implementation.

```elixir
def size(map_with_size)
def size(%{size: size}) do
  size
end
```

The function head `def size(map_with_size)` is declared before its implementation and the argument name is set to `map_with_size.`

## Typespecs 

Type specification (most often referred to as typespecs) is the notation for declaring types and specifications in Elixir. ExDoc can use that notation to create documentation. 

### Custom Types

Developers use `@type` and `@typedoc` together to define and describe custom types. Developers can also pass any metadata to `@typedoc` that they might to `@moduledoc` and `@doc`.

```elixir 
@typedoc """
A user entity, represented as a tuple of user_id and username.
"""
@type user :: {user_id, username}
```

In this example, `@type user :: {user_id, username}` defines a custom type named `user`. `@typedoc` provides a descriptive comment, explaining what the user type represents.

### Function Signatures

The primary goal of `@spec` is to convey the types of arguments a function accepts and the types of the values it can return.  ExDoc uses @spec to show the expected input and output of functions, making it easier for developers to understand how to use a function. 

```elixir
@spec sum([integer]) :: integer
def sum(numbers), do: Enum.reduce(numbers, 0, &(&1 + &2))
```

In this example, `@spec` is the module attribute indicating that a type specification for a function will follow. `@spec` should always immediately precede the function name.``

`[integer]` is the function's parameter type list. The square brackets denote a list and the integer inside the brackets means this function expects a list of integers.

 The `::` symbol separates the parameter type list from the return type. It can be read as "yields" or "returns."

The function `sum/1` has a return type of `integer` so we know to expect a single integer when working with this function.

You can read more about types and specifications at our [Typespecs Reference](https://hexdocs.pm/elixir/1.16.0/typespecs.html#defining-a-specification).

## Metadata

Elixir allows developers to attach arbitrary metadata to module attributes. Metadata is added to documentation by passing a keyword list to the relevant attribute, like `@doc`, `@moduledoc`, or `@typedoc`.

### `:since` Version MetaData

A commonly used metadata key is `:since`. It annotates in which version that particular module, function, or type was added. 
 
In the module example, the developer passes the keyword list `[:since, "1.3.0"]` to `@doc` (and `[since: "1.1.0"]` to `@moduledoc`). The `@doc` attaches the version information along with the text string, argument name and other captured data to the documentation.

### `:deprecated` Deprecation MetaData

Another common metadata is `:deprecated`. It emits a warning in the documentation, discouraging the use of its associated entity.

```elixir
@doc deprecated: "Use MyApp.Hello.world/1 instead"
  def hello_world(name) do
    IO.puts("hello #{name}")
  end
```

Note that the `:deprecated` key only affects documentation and does not warn when a developer invokes the functions. If you want the code to also emit a warning, you can use the `@deprecated` attribute.

```elixir
@deprecated "Use MyApp.Hello.world/1 instead"
  def hello_world(name) do
    IO.puts("hello #{name}")
  end
```

## Recommendations

When writing documentation, we recommend you follow these guidelines:

  * Keep the first paragraph of the documentation a concise description, ideally one line. Tools like [ExDoc](https://github.com/elixir-lang/ex_doc/) use the first line to generate a summary.

  * Reference modules by their full name and wrap them in backticks (`` ` ``). Elixir builds on top of Markdown to automatically generate links for modules and functions, so reference `MyApp.Hello` as `` `MyApp.Hello` ``, never as `` `Hello` ``.
  
  * Reference local functions by their name and arity, as in `` `world/1` ``.

  * Reference external functions by their module name followed by the function name and arity, e.g., `` `MyApp.Hello.world/1` ``.

  * Reference a `@callback` by prepending `c:`, as in `` `c:world/1` ``.

  * Reference a `@type` by prepending `t:`, as in `` `t:values/0` ``.

  * Start new sections with H2 Markdown syntax `##`. Top level headers `#` are reserved for module and function names.

  * Place documentation immediately before the head of a multi-clause function. If the function head doesn't exist,  place it before the first clause. Documentation is always per function and arity and not per clause.
  
  * Add code comments to private functions. Elixir will discard  a private function's `@doc` documentation.

  * Document as you go. Review and revise documentation regularly to ensure it is accurate and complete.

  * Make use of `@impl true` to document callbacks.

  * Use doctests and include code examples in your documentation.

## Incorporating Doctests in Documentation

We recommend developers include examples in their documentation, ideally under a ## Examples heading. To ensure examples remain up-to-date, Elixir's ExUnit test framework provides a feature called doctests. 

Doctests work by parsing code samples starting with iex> from the documentation. This feature automatically verifies the correctness of these code samples. Here's an example doctest:

```elixir
@doc """
## Examples

    iex> MyApp.Hello.world(:john)
    :ok
"""
@doc since: "1.3.0"
def world(name) do
  IO.puts("hello #{name}")
end
```

You can read more about doctests at `ExUnit.DocTest`.

## Intentional Documentation Design

Elixir treats documentation and code comments as fundamentally different by design. Documentation is intended for public consumption and serves as an implicit contract with your API users, including third-party developers, co-workers, and your future self.

Code comments, on the other hand, are meant for developers reading the source code. They are useful for annotating improvements, explaining workarounds, or providing other relevant insights. Unlike documentation, code comments are tied to the source code and can be altered or removed without impacting the API user experience.

Note: Add code comments to your private functions. Elixir will issue a warning and ignore the @doc attribute on all private functions since they aren't externally accessible.

## Managing Visibility of Internal Modules and Functions

Many libraries contain modules and functions that are critical internally but not intended for public use. Elixir allows you to control their visibility:

### Internal Module Visibility

To hide an entire module, use `@moduledoc false`.

```elixir
defmodule MyApp.Hidden do
  @moduledoc false

  @doc """
  This function is now hidden.
  """
  @doc since: "1.3.0"
  def world(name) do
    IO.puts("hello #{name}")
  end
end
```

### Internal Function Visibility

To hide individual functions within a visible module, use `@doc false`.

```elixir
defmodule MyApp.Sample do
  @doc false
  def add(a, b), do: a + b
end
```

To document a function in a hidden module (`@moduledoc false`), use `@doc` as usual. The module won't appear in the generated documentation, but documented functions will.

Remember, `@moduledoc false` and `@doc false` don't make a function private. The `add/2` function in `MyApp.Sample` remains callable and importable. Use these options cautiously: 

  * Move undocumented functions to a module with `@moduledoc false`, like `MyApp.Hidden`. This hides the module but allows each function to be documented with `@doc`.

  * Prefix functions with underscores (e.g., __add__/2). Underscore-prefixed functions are treated as hidden, and you can also explicitly add `@doc false`. The compiler does not import functions with leading underscores, signaling their intended private use.

## `Code.fetch_docs/1`

Documentation in Elixir is stored in predefined chunks within the module's bytecode and is not loaded into memory with the module. Instead, it can be accessed from the bytecode on disk using `Code.fetch_docs/1`. This function retrieves the documentation from disk in a structured format. For example, to access the documentation for the `Enum` module:

```elixir
{docs, _} = Code.fetch_docs(Enum)
```

The docs variable now contains the Enum module's documentation. The downside to this approach is that in-memory modules, such as those in an IEx session, don't have their documentation accessible. This lack of access is because these in-memory modules do not write their bytecode, and thus documentation, to disk.