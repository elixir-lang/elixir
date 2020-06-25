# Changelog for Elixir v1.11

## Compilation-time improvements

Elixir v1.11 features many improvements to how the compiler tracks file dependencies, such that touching one file causes less files to be recompiled. In previous versions, Elixir tracked three types of dependencies:

  * compile-time dependencies - if A depends on B at compile-time, such as by using a macro, whenever B changes, A is recompiled
  * struct dependencies - if A depends on B's struct, whenever B's struct definition changed, A is recompiled
  * runtime dependencies - if A depends on B at runtime, A is never recompiled

However, because dependencies are transitive, if A depends on B at compile-time and B depends on C at runtime, A would depend on C at compile-time. Therefore, it is very important to reduce the amount of compile-time dependencies.

Elixir v1.11 improves this situation by replacing the struct dependencies by "exports dependencies". In other words, if A depends on B, whenever B public's interface changes is recompiled, A is recompiled. What is B's public interface? Its struct definition and all of its public functions and macros.

This change allows us to mark `import`s and `require`s as "exports dependencies" instead of "compile-time" dependencies. This simplifies the dependency graph considerably. For example, [in the Hex.pm project](https://github.com/hexpm/hexpm), changing the `user.ex` file in Elixir v1.10 would emit this:

    $ touch lib/hexpm/accounts/user.ex && mix compile
    Compiling 90 files (.ex)

In Elixir v1.11, we now get:

    $ touch lib/hexpm/accounts/user.ex && mix compile
    Compiling 16 files (.ex)

To make things even better, Elixir v1.11 also introduces a more granular file tracking to path dependencies, such as the ones found in umbrella projects. In previous versions, a module from a path dependency would always be treated as a compile-time dependency. Not anymore! Elixir v1.11 may tag them as an export dependency if appropriate.

To round up the list of improvements, the `--profile=time` option added in Elixir v1.10 now also includes the time to compile each individual file. For example, in the Plug project, one can now get:

    [profile] lib/plug/conn.ex compiled in 935ms
    [profile] lib/plug/ssl.ex compiled in 147ms (plus 744ms waiting)
    [profile] lib/plug/static.ex compiled in 238ms (plus 654ms waiting)
    [profile] lib/plug/csrf_protection.ex compiled in 237ms (plus 790ms waiting)
    [profile] lib/plug/debugger.ex compiled in 719ms (plus 947ms waiting)
    [profile] Finished compilation cycle of 60 modules in 1802ms
    [profile] Finished group pass check of 60 modules in 75ms

While implementing those features, we have also made the `--long-compilation-threshold` flag more precise. In previous versions, `--long-compilation-threshold` would consider both the time a file spent to compile and the time spent waiting on other files to emit warnings. In Elixir v1.11, we consider only the compilation time. This means less false positives and you can now effectively get all files that take longer than 2s to compile by passing `--long-compilation-threshold 2`.

## More compiler checks

TODO: Talk about application boundaries and map field checking.

## v1.11.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Track column information in EEx templates when enabled in the compiler
  * [EEx] Show column information in EEx error messages
  * [EEx] Support `:indentation` option when compiling EEx templates for proper column tracking

#### Elixir

  * [Calendar] Add `Calendar.strftime/3` for datetime formatting
  * [Calendar] Add linear integer representations to Calendar modules: `Date.from_gregorian_days/2`, `Date.to_gregorian_days/1`, `NaiveDateTime.from_gregorian_seconds/3`, `NaiveDateTime.to_gregorian_seconds/1`, `Time.from_seconds_after_midnight/1`, and `Time.to_seconds_after_midnight/1`
  * [Calendar] Add `new!` to Date/Time/NaiveDateTime/DateTime (`new` has also been added to `DateTime` for completeness)
  * [Code] Add `:column` to `Code.string_to_quoted*/2`
  * [Code] Add `Code.can_await_module_compilation?/0` to check if the parallel compiler is enabled and it can await for other modules to be compiled
  * [Enum] Allow a sorting function on `Enum.min_max_by/3,4`, including the new `compare/2` conventions
  * [Kernel] Add `is_struct/2` guard
  * [Kernel] Support `map.field` syntax in guards
  * [Kernel] Add `+++` and `---` with right associativity to the list of custom operators
  * [Kernel.ParallelCompiler] Report individual file compilation times when `profile: :time` is given
  * [Kernel.ParallelCompiler] Improve precision of `:long_compilation_threshold` so it takes only compilation times into account (and not waiting times)
  * [Task] Add `Task.await_many/2`

#### ExUnit

  * [ExUnit] Add support for coloring on Windows 10 consoles/shells
  * [ExUnit] Add `ExUnit.fetch_test_supervisor/0`
  * [ExUnit.Assertion] Allow receive timeouts to be computed at runtime
  * [ExUnit.Doctest] Allow users to add tags to doctests

#### IEx

  * [IEx] Add support for coloring on Windows 10 consoles/shells
  * [IEx.Helpers] Show docs from Erlang modules that have been compiled with the docs chunk

#### Logger

  * [Logger] Add `notice`, `critical`, `alert`, and `emergency` log levels
  * [Logger] Support structured logging by logging maps or keyword lists
  * [Logger] Allow level to be set per module with `Logger.put_module_level/2`

#### Mix

  * [mix] Introduce `MIX_XDG` as a simpler mechanism to opt-in to the XDG specification
  * [mix] Allow requirements for a Mix task to be listed via the `@requirements` module attribute
  * [mix archive.install] Support `--repo` option on hex packages
  * [mix compile] Support the `__mix_recompile__?/0` callback for custom behaviour on when Mix should recompile a given module
  * [mix compile.elixir] Mark modules for path dependencies as "Export dependencies" if they changed but their public interface is the same
  * [mix compile.elixir] Track application boundaries in the Elixir compiler. If you invoke code from Erlang or Elixir standard libraries and you don't depend on the proper applications, a warning will be emitted. A warning will also be emitted if you invoke code from an umbrella sibling that you don't depend on - effectively forbidding cyclic dependencies between apps
  * [mix deps] Sort the dependencies alphabetically before printing
  * [mix deps.unlock] Print which dependencies get unlocked when using the `--unused` flag
  * [mix escript.install] Support `--repo` option on hex packages
  * [mix new] Add `@impl` to application generated by `mix new --sup`
  * [mix release] Enable overriding `sys.config` location via `RELEASE_SYS_CONFIG` env var
  * [mix release] Boot a release under configuration in interactive mode and then swap to embedded mode (if running on Erlang/OTP 23+)
  * [mix test.coverage] Add `mix test.coverage` that aggregates coverage results from umbrellas and OS partitioning

### 2. Bug fixes

#### EEx

  * [EEx] Make trimming behaviour via the `:trim` option more consistent

#### Elixir

  * [Code] Do not send language keyword through the `:static_atoms_encoder` in `Code.string_to_quoted`
  * [Kernel] Validate values given to `:line` in quote to avoid emitting invalid ASTs
  * [Kernel] Report the correct line number when raising inside a macro
  * [Kernel.SpecialForms] Add `|/2` to the list of special forms to avoid inconsistent behaviour on overrides
  * [Keyword] Enforce keys to be atoms in `Keyword.keys/1`
  * [Version] Add defaults and enforce keys in `Version` struct

#### IEx

  * [IEx] Fix tokenizer emitting repeated warnings in the REPL

#### Mix

  * [mix release] Load `.app` from dependencies path when it is a project dependency

### 3. Soft-deprecations (no warnings emitted)

  * [Logger] `warn` log level is deprecated in favor of `warning`

### 4. Hard-deprecations

#### Elixir

  * [Application] Deprecate non-atom keys on `put_env`, `get_env`, `fetch_env`, and `delete_env`
  * [Supervisor] Deprecate `Supervisor.start_child/2` and `Supervisor.terminate_child/2` in favor of `DynamicSupervisor`
  * [Supervisor.Spec] Deprecate `Supervisor.Spec.worker/3` and `Supervisor.Spec.supervisor/3` in favor of the new typespecs

## v1.10

The CHANGELOG for v1.10 releases can be found [in the v1.10 branch](https://github.com/elixir-lang/elixir/blob/v1.10/CHANGELOG.md).
