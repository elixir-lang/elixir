# Changelog for Elixir v1.19

## Type system improvements

### More type inference

Elixir now performs inference of whole functions. The best way to show the new capabilities are with examples. Take the following code:

```elixir
def add_foo_and_bar(data) do
  data.foo + data.bar
end
```

Elixir now infers that the function expects a `map` as first argument, and the map must have the keys `.foo` and `.bar` which values of either `integer()` or `float()`. The return type will be either `integer()` or `float()`.

Here is another example:

```elixir
def sum_to_string(a, b) do
  Integer.to_string(a + b)
end
```

Even though the `+` operator works with both integers and floats, Elixir infers that `a` and `b` must be both integers, as the result of `+` is given to a function that expects an integer. The inferred type information is then used during type checking to find possible typing errors.

### Type checking of protocol dispatch and implementations

This release also adds type checking when dispatching and implementing protocols.

For example, string interpolation in Elixir uses the `String.Chars` protocol. If you pass a value that does not implement said protocol, Elixir will now emit a warning accordingly.

Here is an example passing a range, which cannot be converted into a string, to an interpolation:

```elixir
defmodule Example do
  def my_code(first..last//step = range) do
    "hello #{range}"
  end
end
```

the above emits the following warnings:

```
warning: incompatible value given to string interpolation:

    data

it has type:

    %Range{first: term(), last: term(), step: term()}

but expected a type that implements the String.Chars protocol, it must be one of:

    dynamic(
      %Date{} or %DateTime{} or %NaiveDateTime{} or %Time{} or %URI{} or %Version{} or
        %Version.Requirement{}
    ) or atom() or binary() or float() or integer() or list(term())
```

Warnings are also emitted if you pass a data type that does not implement the `Enumerable` protocol as a generator to for-comprehensions:

```elixir
defmodule Example do
  def my_code(%Date{} = date) do
    for(x <- date, do: x)
  end
end
```

will emit:

```
warning: incompatible value given to for-comprehension:

    x <- date

it has type:

    %Date{year: term(), month: term(), day: term(), calendar: term()}

but expected a type that implements the Enumerable protocol, it must be one of:

    dynamic(
      %Date.Range{} or %File.Stream{} or %GenEvent.Stream{} or %HashDict{} or %HashSet{} or
        %IO.Stream{} or %MapSet{} or %Range{} or %Stream{}
    ) or fun() or list(term()) or non_struct_map()
```

## v1.19.0-dev

### 1. Enhancements

#### Elixir

  * [Code] Add `:migrate_call_parens_on_pipe` formatter option
  * [Date] Support durations as the second element in `Date.range/3`
  * [Enum] Provide more information on `Enum.OutOfBoundsError`
  * [Kernel] Support `min/2` and `max/2` as guards
  * [Protocol] Type checking of protocols dispatch and implementations
  * [Protocol] Add `Protocol.impl_for/2` and `Protocol.impl_for!/2`

#### IEx

  * [IEx.Autocomplete] Functions annotated with `@doc group: "Name"` metadata will appear within their own groups in autocompletion

#### Mix

  * [Mix.Tasks.Compile] Add `Mix.Tasks.Compile.reenable/1`

### 2. Bug fixes

#### Mix

  * [mix cmd] Preserve argument quoting in subcommands

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Node] `Node.start/2-3` is deprecated in favor of `Node.start/2` with a keyword list

#### Mix

  * [mix compile] `--no-protocol-consolidation` is deprecated in favor of `--no-consolidate-protocols` for consistency with `mix.exs` configuration
  * [mix compile.protocols] Protocol consolidation is now part of `compile.elixir` and has no effect

### 4. Hard deprecations

#### Elixir

  * [Code] The `on_undefined_variable: :warn` is deprecated. Relying on undefined variables becoming function calls will not be supported in the future
  * [File] Passing a callback as third argument to `File.cp/3` is deprecated, pass it as a `on_conflict: callback` option instead
  * [File] Passing a callback as third argument to `File.cp_r/3` is deprecated, pass it as a `on_conflict: callback` option instead
  * [Kernel] The struct update syntax, such as `%URI{uri | path: "/foo/bar"}` is deprecated in favor of pattern matching on the struct when the variable is defined and then using the map update syntax `%{uri | path: "/foo/bar"}`. Thanks to the type system, pattern matching on structs can find more errors, more reliably
  * [Kernel.ParallelCompiler] Passing `return_diagnostics: true` as an option is required on `compile`, `compile_to_path` and `require`

#### Logger

  * [Logger] The `:backends` configuration is deprecated, either set the `:default_handler` to false or start backends in your application start callback

#### Mix

  * [mix] The `:default_task`, `:preferred_cli_env`, and `:preferred_cli_target` configuration inside `def project` in your `mix.exs` has been deprecated in favor of `:default_task`, `:preferred_envs` and `:preferred_targets` inside the `def cli` function
  * [mix do] Using commas as task separator in `mix do` (such as `mix do foo, bar`) is deprecated, use `+` instead (as in `mix do foo + bar`)

## v1.18

The CHANGELOG for v1.18 releases can be found [in the v1.18 branch](https://github.com/elixir-lang/elixir/blob/v1.18/CHANGELOG.md).
