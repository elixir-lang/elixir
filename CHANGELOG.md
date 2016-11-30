# Changelog for Elixir v1.4

Elixir v1.4 brings new features, enhancements and bug fixes into Elixir. The most notable changes are the addition of the `Registry` module and the `Task.async_stream/3` and `Task.async_stream/5` which aid developers in writing concurrent software. Those two features and a couple other improvements are described in detail below followed by the complete list of changes.

## Registry

The registry is a local, decentralized and scalable key-value process storage:

  * Local because keys and values are only accessible to the current node (opposite to distributed)
  * Decentralized because there is no single entity responsible for managing the registry
  * Scalable because performance scales linearly with the addition of more cores upon partitioning

A registry is chosen upon start to have unique or duplicate keys. Every key-value pair is associated to the process registering the key. Keys are automatically removed once the owner process terminates.

    iex> Registry.start_link(:unique, MyRegistry)
    iex> {:ok, _} = Registry.register(MyRegistry, "hello", 1)
    iex> Registry.lookup(MyRegistry, "hello")
    [{self(), 1}]

With the registry, developers can provide dynamic process names, module-function dispatch or even a local pubsub system. See the `Registry` documentation for more information.

## Syntax coloring

Elixir v1.4 introduces the ability to syntax color inspected data structures:

    iex> IO.puts inspect([hello: 1, world: "!"], syntax_colors: [atom: :cyan])
    [hello: 1, world: "!"]

Coloring is done with ANSI colors as specified in the `IO.ANSI` module.

IEx automatically relies on this feature to provide syntax coloring for evaluated shell results. This behaviour can be configured via the `:syntax_colors` coloring option:

    IEx.configure [colors: [syntax_colors: [atom: :cyan, string: :green]]]

To disable coloring altogether, pass an empty list to `:syntax_colors`.

## Calendar

Elixir v1.3 introduced new calendar types. This release continues evolving the Calendar APIs by adding functions for comparing, adding and calculating the difference between types, retrieve the `day_of_week/1`, check if the current date is a `leap_year?/1` and more.

## Task.async_stream

When there is a need to traverse a collection of items concurrently, Elixir developers often resort to tasks:

    collection
    |> Enum.map(&Task.async(SomeMod, :function, [&1]))
    |> Enum.map(&Task.await/1)

While the snippet above works fine in many occasions, for large collections it will spawn and run concurrently as many tasks as there are items in the collection.

`Task.async_stream/3` and `Task.async_stream/5` allows developers to process collections concurrently while controlling the maximum amount of concurrent tasks:

    collection
    |> Task.async_stream(SomeMod, :function, [], max_concurrency: System.schedulers_online)

The `Task.async_stream` functions are also lazy, allowing developers to partially consume the stream until a condition is reached. Furthermore, `Task.Supervisor.async_stream/4` and `Task.Supervisor.async_stream/6` can be used to ensure the concurrent tasks are spawned under a given supervisor.

## Application inference

Mix v1.4 now automatically infers the list of applications that are required on runtime from your dependencies list.

In previous Mix versions, most of your dependencies had to be added both to your dependencies list and applications list. Here is how a `mix.exs` would look like:

    def application do
      [applications: [:logger, :plug, :postgrex]]
    end

    def deps do
      [{:plug, "~> 1.2"},
       {:postgrex, "~> 1.0"}]
    end

This was error prone as many developers would not list their dependencies in their applications list.

Mix v1.4 now automatically infers your applications list as long as you leave the `:applications` key empty. The `mix.exs` above can be rewritten to:

    def application do
      [extra_applications: [:logger]]
    end

    def deps do
      [{:plug, "~> 1.2"},
       {:postgrex, "~> 1.0"}]
    end

With the above, Mix will automatically build your application list based on your dependencies. Applications that are part of Erlang or Elixir that are required at runtime, such as `:logger`, must be added to the `:extra_applications` list. All extra applications will be included in the application list.

Finally, if there is a dependency you don't want to include in the application runtime list, you can do so by specifying the `runtime: false` option:

    {:distillery, "> 0.0.0", runtime: false}

We hope this feature provides a more streamlined workflow for developers who are building releases for their Elixir projects.

## Mix install from SCM

Mix v1.4 can now install escripts and archives from both Git and Hex, providing you with even more options for distributing Elixir code.

This makes it possible to distribute CLI applications written in Elixir by publishing a package which builds an escript to Hex. [`ex_doc`](https://hex.pm/packages/ex_doc) has been updated to serve as an example of how to use this new functionality.

Simply running:

    mix escript.install hex ex_doc

will fetch `ex_doc` and its dependencies, build them, and then install `ex_doc` to `~/.mix/escripts` (by default). After adding `~/.mix/escripts` to your `PATH`, running `ex_doc` is as simple as:

    ex_doc

You can now also install archives from Hex in this way. Since they are fetched and built on the user's machine, they do not have the same limitations as pre-built archives. However, keep in mind archives run alongside every Mix project, which may lead to conflicts. For this reason, escripts is the preferred format.

It is also possible to install escripts and archives by providing a Git/GitHub repo. See `mix help escript.install` and `mix help archive.install` for more details.

## v1.4.0-rc.1 (2016-12-05)

### 1. Enhancements

#### Elixir

  * [Calendar] Add `Date.compare/2`, `Time.compare/2`, `NaiveDateTime.compare/2` and `DateTime.compare/2`
  * [Calendar] Support `NaiveDateTime.add/3` and `NaiveDateTime.diff/3` for adding seconds (up to microseconds) as well as the difference between two NaiveDateTimes in seconds (up to microseconds)
  * [Calendar] Add `Date.leap_year?/1` and `Date.day_of_week/1`
  * [Calendar] Ensure `Date`, `Time` and `NaiveDateTime` APIs work with any struct that provides the same set of fields as their respective struct. For example, a `NaiveDateTime` can be given to `Date` since it contains a superset of the fields in the `Date` struct
  * [Enum] Add `Enum.map_every/2` that invokes the given function with every nth item
  * [Enum] Add `min/2`, `max/2`, `min_max/2`, `min_by/3`, `max_by/3`, and `min_max_by/3` that allow a function specifying the default value when the enumerable is empty
  * [Enum] Introduce `Enum.zip/1` to zip multiple entries at once
  * [Float] Introduce `Float.ratio/1` that returns a tuple with the numerator and denominator as integers to retrieve the given float
  * [GenServer] Log error on default `handle_info/2` implementation
  * [Inspect] Support syntax coloring via the `:syntax_color` option
  * [Integer] `Integer.digits/2` now accepts negative integers
  * [Integer] Add `Integer.mod/2` and `Integer.floor_div/2`
  * [Kernel] Recognize merge conflict markers in source and provide a readable error message
  * [Kernel] Warn on unused module attributes
  * [Kernel] Improve compiler message on unexpected end of line
  * [Kernel] Raise `BadBooleanError` when a non-boolean is given on the left-hand side of `and`/`or`
  * [List] Add `List.pop_at/3`
  * [List] Add `List.myers_difference/2`
  * [OptionParser] Expand multi-letter aliases in `OptionParser`
  * [Process] Add `Process.send_after/4`
  * [Process] Improve error messages on `Process.register/2` errors
  * [Registry] Add a local, decentralized and scalable key-value process storage
  * [Stream] Add `Stream.map_every/2` that invokes the given function with every nth item
  * [Stream] Introduce `Stream.zip/1` to lazily zip multiple entries at once
  * [String] Update to Unicode 9.0.0
  * [Task] Add `Task.async_stream/3` and `Task.async_stream/5` as well as the supervised versions `Task.Supervisor.async_stream/4` and `Task.Supervisor.async_stream/6`
  * [URI] Allow 0 as URI scheme default port

#### ExUnit

  * [ExUnit.Diff] Use red or green background for whitespace-only diffs
  * [ExUnit.Doctest] Allow inspected structures with multiples lines and unicode characters in the doctest result
  * [ExUnit.Formatter] Replace lhs/rhs with left/right in the formatter for clarity

#### IEx

  * [IEx.Autocomplete] Stop appending a trailing dot when autocompleting modules in IEx
  * [IEx.Autocomplete] Support autocompletion for structs
  * [IEx.Autocomplete] Improve IEx autocomplete to support navigating map atom keys
  * [IEx.Helpers] `c/1` now compiles in memory by default to avoid common issue where `.beam` files remain at projects root directory
  * [IEx.Helpers] Add info about protocols in `i/1`
  * [IEx.Server] Support interrupting IEx evaluation through the Ctrl+G prompt

#### Mix

  * [mix archive] Compress archive files built by `mix archive` as they are now unzipped during installation
  * [mix archive] Install from SCM
  * [mix compile] Automatically infer the list of applications for Mix projects
  * [mix cmd] Add the ability to specify one or more apps in `mix cmd`
  * [mix deps] Warn if there are non-applications in the `apps` directory for umbrella projects
  * [mix deps] Add warning for invalid paths on `mix deps.clean`
  * [mix deps] Add `Mix.Project.apps_paths` that returns the paths to children applications in umbrella projects
  * [mix deps] Add `MIX_REBAR` environment variable for overriding local rebar
  * [mix escript] Install from SCM
  * [mix new] Check directory existence in `mix new` and ask how to proceed if one exists
  * [mix new] Applications built with the `--sup` flag now have an individual module to work as application callback
  * [mix test] Add `--formatter` option to `mix test`
  * [mix xref] Provide "did you mean?" suggestions for `mix xref`

### 2. Bug fixes

#### Elixir

  * [Float] Avoid multiple roundings in `Float.ceil/2`, `Float.floor/2` and `Float.round/2`
  * [Kernel] Don't crash in `macro_exported?/3` when dealing with Erlang modules
  * [Kernel] Ensure locals calls are rewritten when calling a local function or macro from inside a module
  * [Kernel] Annotate the context for variables as zero-arity funs in quotes
  * [Kernel.SpecialForms] Ensure comprehensions with guards and filters keep proper ordering,
  * [Kernel.SpecialForms] Produce meaningful warning when with's else clauses have no effect
  * [Macro] Wrap fn calls in parens in `Macro.to_string/2`
  * [Macro] Do not print aliases as keys inside keyword lists in `Macro.to_string/2`
  * [OptionParser] Support options in `OptionParser.to_argv/2` to ensure `:count` switches are correctly encoded
  * [Stream] Ensure `Stream.take/2` does not consume next element on `:suspend`
  * [String] Fix infinite recursion in `String.replace_leading/3` and `String.replace_trailing/3` when given an empty string
  * [Task] Fix `Task.shutdown/1,2` infinite block when task has no monitor
  * [Task] Ensure task cannot link after parents unlinks

#### ExUnit

  * [ExUnit] Fix a race condition in `assert_receive` where we would assert a message was not received but show it in the list of messages when the message is delivered right after the timeout value

### IEx

  * [IEx.Helpers] Purge consolidated protocols before and after `recompile/0`

### Mix

  * [Mix.Dep] Use `gmake` on FreeBSD instead of `make` when compiling make dependencies
  * [Mix.Project] Only copy files from source when they're newer than destination (for Windows machines)
  * [Mix.Task] Ensure non-recursive tasks inside umbrella are reenabled

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Enum] `Enum.partition/2` has been deprecated in favor of `Enum.split_with/2`
  * [System] Deprecate plural time units in favor of singular ones to align with future Erlang releases

#### ExUnit

  * [ExUnit] Using GenEvent to implement ExUnit formatters is deprecated. Please use the new `GenServer` based formatters instead

### 4. Deprecations

#### Elixir

  * [Behaviour] The `Behaviour` module is deprecated. Callbacks may now be defined directly via the `@callback` attribute
  * [Enum] Deprecate `Enum.uniq/2` in favor of `Enum.uniq_by/2`
  * [Float] `Float.to_char_list/2` and `Float.to_string/2` are deprecated (use the :erlang functions if such conversions are desired)
  * [Kernel] Deprecate support for making private functions overridable. Overridable functions must always be public as they must be contracts
  * [Kernel] Warn if variable is used as a function call
  * [OptionParser] Deprecate aliases with multiple letters, such as `-abc`
  * [Set] Deprecate the `Set` module
  * [Stream] Deprecate `Stream.uniq/2` in favor of `Stream.uniq_by/2`

#### IEx

  * [IEx.Helpers] `import_file/2` is deprecated in favor of `import_file_if_available/1`

#### Mix

  * [Mix.Utils] `underscore/1` and `camelize/1` are deprecated

## v1.3

The CHANGELOG for v1.3 releases can be found [in the v1.3 branch](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md).
