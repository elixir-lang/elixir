# Changelog for Elixir v1.4

## v1.4.0-dev

### 1. Enhancements

#### Elixir

  * [Integer] `digits/2` now accepts negative integers

#### IEx

  * [IEx.Helpers] `c/1` now compiles in memory by default to avoid common issue where `.beam` files remain at projects root directory
  * [IEx.Autocomplete] Stop appending a trailing dot when autocompleting modules in IEx

#### Mix

  * [Mix] Add warning for an invalid path on deps.clean task

### 2. Bug fixes


### 3. Soft deprecations (no warnings emitted)


### 4. Deprecations

#### Elixir

  * [Kernel] Deprecate support for making private functions overridable. Overridable functions must always be public as they must be contracts.

#### IEx

  * [IEx.Helpers] `import_file/2` is deprecated in favor of `import_file_if_available/1`

