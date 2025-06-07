<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Changelog for Elixir v1.20

## Type system improvements

### Full type inference

Elixir now performs inference of whole functions. The best way to show the new capabilities are with examples. Take the following code:

```elixir
def add_foo_and_bar(data) do
  data.foo + data.bar
end
```

Elixir now infers that the function expects a `map` as first argument, and the map must have the keys `.foo` and `.bar` whose values are either `integer()` or `float()`. The return type will be either `integer()` or `float()`.

Here is another example:

```elixir
def sum_to_string(a, b) do
  Integer.to_string(a + b)
end
```

Even though the `+` operator works with both integers and floats, Elixir infers that `a` and `b` must be both integers, as the result of `+` is given to a function that expects an integer. The inferred type information is then used during type checking to find possible typing errors.

### Acknowledgements

The type system was made possible thanks to a partnership between [CNRS](https://www.cnrs.fr/) and [Remote](https://remote.com/). The development work is currently sponsored by [Fresha](https://www.fresha.com/), [Starfish*](https://starfish.team/), and [Dashbit](https://dashbit.co/).

## v1.20.0-dev

### 1. Enhancements

### 2. Bug fixes

### 3. Soft deprecations (no warnings emitted)

### 4. Hard deprecations

## v1.19

The CHANGELOG for v1.19 releases can be found [in the v1.19 branch](https://github.com/elixir-lang/elixir/blob/v1.19/CHANGELOG.md).
