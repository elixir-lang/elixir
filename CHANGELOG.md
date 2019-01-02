# Changelog for Elixir v1.8

Elixir v1.8 comes with many improvements at the infrastructure level, improving compilation time, speeding up common patterns, and adding features around introspection of the system.

## Custom struct inspections

Elixir now provides a derivable implementation of the `Inspect` protocol. In a nutshell, this means it is really easy to filter data from your data structures whenever they are inspected. For example, imagine you have a user struct with security and privacy sensitive information:

```elixir
defmodule User do
  defstruct [:id, :name, :age, :email, :encrypted_password]
end
```

By default, if you inspect a user via `inspect(user)`, it will include all fields. This can cause fields such as `:email` and `:encrypted_password` to appear in logs, error reports, etc. You could always define a custom implementation of the `Inspect` protocol for such cases but Elixir v1.8 makes it simpler by allowing you to derive the `Inspect` protocol:

```elixir
defmodule User do
  @derive {Inspect, only: [:id, :name, :age]}
  defstruct [:id, :name, :age, :email, :encrypted_password]
end
```

Now all user structs will be printed with all remaining fields collapsed:

    #User<id: 1, name: "Jane", age: 33, ...>

You can also pass `@derive {Inspect, except: [...]}` in case you want to keep all fields by default and exclude only some.

## Time zone database support

In Elixir v1.3, Elixir added four types, known as Calendar types, to work with dates and times: `Time`, `Date`, `NaiveDateTime` (without time zone) and `DateTime` (with time zone). Over the last releases we have added many enhancements to the Calendar types but the `DateTime` module always evolved at a slower pace since Elixir did not provide support for a time zone database.

Elixir v1.8 now defines a `Calendar.TimeZoneDatabase` behaviour, allowing developers to bring in their own time zone databases. By defining an explicit contract for time zone behaviours, Elixir can now extend the `DateTime` API, adding functions such as `DateTime.shift_zone/3`. By default, Elixir ships with a time zone database called `Calendar.UTCOnlyTimeZoneDatabase` that only handles UTC.

Other Calendar related improvements include the addition of `Date.day_of_year/1`, `Date.quarter_of_year/1`, `Date.year_of_era/1`, and `Date.day_of_era/1`.

## Faster compilation and other performance improvements

Due to improvements to the compiler made over the last year, Elixir v1.8 should compile code about 5% faster on average. This is yet another release where we have been able to reduce compilation times and provide a more joyful development experience to everyone.

The compiler also emits more efficient code for range checks in guards (such as `x in y..z`), for charlists with interpolation (such as `'foo #{bar} baz'`), and when working with records via the `Record` module.

Finally, EEx templates got their own share of optimizations, emitting more compact code that runs faster.

## Improved instrumentation and ownership with `$callers`

The `Task` module is one of the most common ways to spawn light-weight processes to perform work concurrently. Whenever you spawn a new process, Elixir annotates the parent of that process through the `$ancestors` key. This information can be used by instrumentation tools to track the relationship between events occurring within multiple processes. However, many times, tracking only the `$ancestors` is not enough.

For example, we recommend developers to always start tasks under a supervisor. This provides more visibility and allows us to control how those tasks are terminated when a node shuts down. In your code, this can be done by invoking something like: `Task.Supervisor.start_child(MySupervisor, task_specification)`. This means that, although your code is the one who invokes the task, the actual parent of the task would be the supervisor, as the supervisor is the one spawning it. We would list the supervisor as one of the `$ancestors` for the task, but the relationship between your code and the task is lost.

In Elixir v1.8, we now track the relationship between your code and the task via the `$callers` key in the process dictionary, which aligns well with the existing `$ancestors` key. Therefore, assuming the `Task.Supervisor` call above, we have:

    [your code] -- calls --> [supervisor] ---- spawns --> [task]

which means we store the following relationships:

    [your code]              [supervisor] <-- ancestor -- [task]
         ^                                                  |
         |--------------------- caller ---------------------|

When a task is spawned directly from your code, without a supervisor, then the process running your code will be listed under both `$ancestors` and `$callers`.

This small feature is very powerful. It allows instrumentation and monitoring tools to better track and relate the events happening in your system. This feature can also be used by tools like the "Ecto Sandbox". The "Ecto Sandbox" allows developers to run tests concurrently against the database, by using transactions and an ownership mechanism where each process explicitly gets a connection assigned to it. Without `$callers`, every time you spawned a task that queries the database, the task would not know its caller, and therefore it would be unable to know which connection was assigned to it. This often meant features that relies on tasks could not be tested concurrently. With `$callers`, figuring out this relationship is trivial and you have more tests using the full power of your machine.

## v1.8.0-rc.1 (2018-01-03)

### 1. Bug fixes

#### Elixir

  * [Kernel] Only validate the argument of `record/1` and `record/2` types in typespecs (regression)

## v1.8.0-rc.0 (2018-12-24)

### 1. Enhancements

#### EEx

  * [EEx] Optimize the default template engine to compile and execute more efficiently

#### Elixir

  * [Calendar] Add `Calendar.TimeZoneDatabase` and a `Calendar.UTCOnlyTimeZoneDatabase` implementation
  * [Calendar] Add callbacks `day_of_year/3`, `quarter_of_year/3`, `year_of_era/1`, and `day_of_era/3`
  * [Code.Formatter] Preserve user's choice of new line after most operators
  * [Date] Add `Date.day_of_year/1`, `Date.quarter_of_year/1`, `Date.year_of_era/1`, and `Date.day_of_era/1`
  * [DateTime] Add `DateTime.from_naive/3`, `DateTime.now/1`, and `DateTime.shift_zone/3`
  * [File] Allow `:raw` option in `File.exists?/2`, `File.regular?/2`, and `File.dir?/2`
  * [File] Allow POSIX time as an integer in `File.touch/2` and `File.touch!/2`
  * [Inspect] Allow `Inspect` protocol to be derivable with the `:only`/`:except` options
  * [Kernel] Do not propagate counters to variables in quote inside another quote
  * [Kernel] Warn on ambiguous use of `::` and `|` in typespecs
  * [Kernel] Add `:delegate_to` `@doc` metadata tag when using `defdelegate`
  * [Kernel] Improve compile-time building of ranges via the `..` operator
  * [Kernel] Compile charlist interpolation more efficiently
  * [Kernel.SpecialForms] Add `:reduce` option to `for` comprehensions
  * [List] Add `List.myers_difference/3` and `List.improper?/1`
  * [Macro] Add `Macro.struct!/2` for proper struct resolution during compile time
  * [Map] Optimize and merge nested maps `put` and `merge` operations
  * [Range] Add `Range.disjoint?/2`
  * [Record] Reduce memory allocation when updating multiple fields in a record
  * [Registry] Allow associating a value on `:via` tuple
  * [String] Add `String.bag_distance/2`
  * [Task] Add `$callers` tracking to `Task` - this makes it easier to find which process spawned a task and use it for tracking ownership and monitoring

#### ExUnit

  * [ExUnit] Add `ExUnit.after_suite/1` callback
  * [ExUnit.Assertions] Show last N messages (instead of first N) from mailbox on `assert_receive` fail

#### IEx

  * [IEx.Helpers] Add `port/1` and `port/2`
  * [IEx.Server] Expose `IEx.Server.run/1` for custom IEx sessions with the ability to broker pry sessions

#### Mix

  * [Mix] Add `Mix.target/0` and `Mix.target/1` to control dependency management per target
  * [Mix.Project] Add `:depth` and `:parents` options to `deps_paths/1`
  * [mix archive.install] Add a timeout when installing archives
  * [mix compile] Include optional dependencies in `:extra_applications`
  * [mix escript.install] Add a timeout when installing escripts
  * [mix format] Warn when the same file may be formatted by multiple `.formatter.exs`
  * [mix test] Allow setting the maximum number of failures via `--max-failures`
  * [mix test] Print a message instead of raising on unmatched tests inside umbrella projects

### 2. Bug fixes

#### Elixir

  * [Calendar] Allow printing dates with more than 9999 years
  * [Exception] Exclude deprecated functions in "did you mean?" hints
  * [Float] Handle subnormal floats in `Float.ratio/1`
  * [Kernel] Remove `Guard test tuple_size(...) can never succeed` Dialyzer warning on `try`
  * [Kernel] Expand operands in `size*unit` bitstring modifier instead of expecting `size` and `unit` to be literal integers
  * [Kernel] Do not deadlock on circular struct dependencies in typespecs
  * [Kernel] Raise proper error message when passing flags to the Erlang compiler that Elixir cannot handle
  * [Kernel] Do not leak variables in `cond` clauses with a single matching at compile-time clause
  * [NaiveDateTime] Do not accept leap seconds in builder and parsing functions
  * [String] Fix ZWJ handling in Unicode grapheme clusters

#### IEx

  * [IEx.Helpers] Use typespec info (instead of docs chunk) and properly format callbacks in `b/1`

#### Logger

  * [Logger] Allow Logger backends to be dynamically removed when an application is shutting down

#### Mix

  * [mix compile] Ensure changes in deps propagate to all umbrella children - this fix a long standing issue where updating a dependency would not recompile all projects accordingly, requiring a complete removal of `_build`
  * [mix compile] Avoid time drift when checking and updating compiler manifest files
  * [mix compile.app] Respect the `:only` option between umbrella siblings
  * [mix compile.protocols] Reconsolidate protocols if local dependencies are stale
  * [mix deps] Properly mark dependencies with different `:system_env` as diverged
  * [mix new] Use `--module` value when setting up filenames

### 3. Soft-deprecations (no warnings emitted)

None.

### 4. Hard-deprecations

#### Elixir

  * [Enum] Passing a non-empty list to `Enum.into/2` was inconsistent with maps and is deprecated in favor of `Kernel.++/2` or `Keyword.merge/2`
  * [Inspect.Algebra] `surround/3` is deprecated in favor of `Inspect.Algebra.concat/2` and `Inspect.Algebra.nest/2`
  * [Inspect.Algebra] `surround_many/6` is deprecated in favor of `container_doc/6`
  * [Kernel] Using `@since` will now emit a unused attribute warning. Use `@doc since: "1.7.2"` instead
  * [Kernel] Passing a non-empty list as `:into` in `for` comprehensions was inconsistent with maps and is deprecated in favor of `Kernel.++/2` or `Keyword.merge/2`
  * [Kernel.ParallelCompiler] `files/2` is deprecated in favor of `compile/2`
  * [Kernel.ParallelCompiler] `files_to_path/2` is deprecated in favor of `compile_to_path/2`
  * [Kernel.ParallelRequire] `files/2` is deprecated in favor of `Kernel.ParallelCompiler.require/2`
  * [System] `:seconds`, `:milliseconds`, etc. as time units is deprecated in favor of `:second`, `:millisecond`, etc.
  * [System] `System.cwd/0` and `System.cwd!/0` are deprecated in favor of `File.cwd/0` and `File.cwd!/0`

#### Mix

  * [mix compile.erlang] Returning `{:ok, contents}` or `:error` as the callback in `Mix.Compilers.Erlang.compile/6` is deprecated in favor of returning `{:ok, contents, warnings}` or `{:error, errors, warnings}`

## v1.7

The CHANGELOG for v1.7 releases can be found [in the v1.7 branch](https://github.com/elixir-lang/elixir/blob/v1.7/CHANGELOG.md).
