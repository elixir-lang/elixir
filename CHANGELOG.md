# Changelog for Elixir v1.11

Over the last releases, the Elixir team has been focusing on the compiler, both in terms of catching more mistakes at compilation time and at making it faster. Elixir v1.11 has made excellent progress on both fronts. This release also includes many other goodies, such as tighter Erlang integration, support for more guard expressions, built-in datetime formatting, and other calendar enhancements.

## Tighter Erlang integration

Following on the steps of Elixir v1.10, we have further integrated with Erlang's new logger by adding four new log levels: `notice`, `critical`, `alert`, and `emergency`. Matching all log levels found in the Syslog standard. The Logger module now supports structured logging by passing maps and keyword lists to its various functions. It is also possible to specify the log level per module, via the `Logger.put_module_level/2` function. Log levels per application will be added in future releases.

IEx also has been improved to show the documentation for Erlang modules directly from your Elixir terminal. This works with Erlang/OTP 23+ and requires Erlang modules to have been compiled with documentation chunks.

## Compiler checks: application boundaries

Elixir v1.11 builds on top of the compilation tracers added in v1.10 to track application boundaries. From this release, Elixir will warn if you invoke a function from an existing module but this module does not belong to any of your listed dependencies.

These two conditions may seem contradictory. After all, if a module is available, it must have come from a dependency. However, this is not true in two scenarios:

  * Modules from Elixir and Erlang/OTP are always available - even if their applications are not explicitly listed as a dependency

  * In an umbrella project, because all child applications are compiled within the same VM, you may have a module from a sibling project available, even if you don't list it

When assembling a release, Elixir includes only the applications you explicitly depend on in the assembled artifact. This could lead to a situation where you used a module from Erlang/Elixir successfully during development and test but this module would not be available in production.

This new compiler check makes sure that all modules that you invoke are listed as part of your dependencies, emitting a warning like below otherwise:

    :ssl.connect/2 defined in application :ssl is used by the current
    application but the current application does not directly depend
    on :ssl. To fix this, you must do one of:

      1. If :ssl is part of Erlang/Elixir, you must include it under
         :extra_applications inside "def application" in your mix.exs

      2. If :ssl is a dependency, make sure it is listed under "def deps"
         in your mix.exs

      3. In case you don't want to add a requirement to :sll, you may
         optionally skip this warning by adding [xref: [exclude: :ssl]
         to your "def project" in mix.exs

This comes with extra benefits in umbrella projects, as it requires child applications to explicitly list their dependencies, completely rejecting cyclic dependencies between siblings.

## Compiler checks: data constructors

In Elixir v1.11, the compiler also tracks structs and maps fields across a function body. For example, imagine you wanted to write this code:

    def drive?(%User{age: age}), do: age >= 18

However, there is either a typo on the `:age` field or the `:age` field was not yet defined. In the example above, the compiler will fail stating that `:age` does not exist in the `User` struct. However, if you wrote this code:

    def drive?(%User{} = user), do: user.age >= 18

The compiler would not catch the missing field and an error would only be raised at runtime. With v1.11, Elixir will track the usage of all maps and struct fields within the same function, emitting warnings for cases like above:

    warning: undefined field `age` in expression:

        # example.exs:7
        user.age

    where "user" was given the type %User{} in:

        # example.exs:7
        %User{} = user

    Conflict found at
      example.exs:7: Check.drive?/1

The compiler also checks binary constructors. For example, consider you have to send a string over the wire with length-based encoding, where the string is prefixed by its length, up to 4MBs. Your initial attempt may be this:

    def run_length(string) when is_binary(string) do
      <<byte_size(string)::32, string>>
    end

However, the code above has a bug. Each segment given between `<<>>` must be an integer, unless specified otherwise. With Elixir v1.11, the compiler will let you know so:

    warning: incompatible types:

        binary() !~ integer()

    in expression:

        <<byte_size(string)::integer()-size(32), string>>

    where "string" was given the type integer() in:

        # foo.exs:4
        <<byte_size(string)::integer()-size(32), string>>

    where "string" was given the type binary() in:

        # foo.exs:3
        is_binary(string)

    Conflict found at
      foo.exs:4: Check.run_length/1

Which can be fixed by adding `::binary` to the second component:

    def run_length(string) when is_binary(string) do
      <<byte_size(string)::32, string::binary>>
    end

While some of those could be fixed automatically by the compiler, future versions will also perform those checks across functions and potentially across modules, where automatic fixes wouldn't be desired (nor possible).

## Compilation time improvements

Elixir v1.11 features many improvements to how the compiler tracks file dependencies, such that touching one file causes less files to be recompiled. In previous versions, Elixir tracked three types of dependencies:

  * compile time dependencies - if A depends on B at compile time, such as by using a macro, whenever B changes, A is recompiled
  * struct dependencies - if A depends on B's struct, whenever B's struct definition changed, A is recompiled
  * runtime dependencies - if A depends on B at runtime, A is never recompiled

However, because dependencies are transitive, if A depends on B at compile time and B depends on C at runtime, A would depend on C at compile time. Therefore, it is very important to reduce the amount of compile time dependencies.

Elixir v1.11 replaces the struct dependencies by "exports dependencies". In other words, if A depends on B, whenever B public's interface changes is recompiled, A is recompiled. B's public interface is made by its struct definition and all of its public functions and macros.

This change allows us to mark `import`s and `require`s as "exports dependencies" instead of "compile time" dependencies. This simplifies the dependency graph considerably. For example, [in the Hex.pm project](https://github.com/hexpm/hexpm), changing the `user.ex` file in Elixir v1.10 would emit this:

    $ touch lib/hexpm/accounts/user.ex && mix compile
    Compiling 90 files (.ex)

In Elixir v1.11, we now get:

    $ touch lib/hexpm/accounts/user.ex && mix compile
    Compiling 16 files (.ex)

To make things even better, Elixir v1.11 also introduces a more granular file tracking to path dependencies. In previous versions, a module from a path dependency would always be treated as a compile time dependency. Not anymore! Elixir v1.11 may tag them as an export instead of compile time if appropriate. Path dependencies are the building blocks of umbrella projects, so umbrella users should see dramatic improvements on latest Elixir.

To round up the list of compiler enhancements, the `--profile=time` option added in Elixir v1.10 now also includes the time to compile each individual file. For example, in the Plug project, one can now get:

    [profile] lib/plug/conn.ex compiled in 935ms
    [profile] lib/plug/ssl.ex compiled in 147ms (plus 744ms waiting)
    [profile] lib/plug/static.ex compiled in 238ms (plus 654ms waiting)
    [profile] lib/plug/csrf_protection.ex compiled in 237ms (plus 790ms waiting)
    [profile] lib/plug/debugger.ex compiled in 719ms (plus 947ms waiting)
    [profile] Finished compilation cycle of 60 modules in 1802ms
    [profile] Finished group pass check of 60 modules in 75ms

While implementing those features, we have also made the `--long-compilation-threshold` flag more precise. In previous versions, `--long-compilation-threshold` would consider both the time a file spent to compile and the time spent waiting on other files to emit warnings. In Elixir v1.11, we consider only the compilation time. This means less false positives and you can now effectively get all files that take longer than 2s to compile by passing `--long-compilation-threshold 2`.

## `mix xref graph` improvements

To bring visibility to the compiler tracking improvements described in the previous section, we have also added new features to `mix xref`. `mix xref` is a task that describes cross-references between files in your projects. The `mix xref graph` subsection focuses on the dependency graph.

First we have made the existing `--label` flag to consider transitive dependencies. Using `--sink FILE` and `--label compile` can be a powerful combo to find out which files will change whenever the given `FILE` changes. For example, in the Hex.pm project, we get:

    $ mix xref graph --sink lib/hexpm/accounts/user.ex --label compile
    lib/hexpm/billing/hexpm.ex
    └── lib/hexpm/billing/billing.ex (compile)
    lib/hexpm/billing/local.ex
    └── lib/hexpm/billing/billing.ex (compile)
    lib/hexpm/emails/bamboo.ex
    ├── lib/hexpm/accounts/email.ex (compile)
    └── lib/hexpm/accounts/user.ex (compile)
    lib/hexpm/emails/emails.ex
    └── lib/hexpm_web/views/email_view.ex (compile)
    lib/hexpm_web/controllers/api/docs_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/key_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/organization_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/organization_user_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/owner_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/package_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/release_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/repository_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/api/retirement_controller.ex
    └── lib/hexpm_web/controllers/auth_helpers.ex (compile)
    lib/hexpm_web/controllers/blog_controller.ex
    └── lib/hexpm_web/views/blog_view.ex (compile)
    lib/hexpm_web/endpoint.ex
    ├── lib/hexpm_web/plug_parser.ex (compile)
    └── lib/hexpm_web/session.ex (compile)

All the files at the root will recompile if `lib/hexpm/accounts/user.ex` changes. Their children describe the *why*. For example, the `repository_controller.ex` file will recompile if user changes because it has a compile time dependency on `auth_helpers.ex` (which depends on `user.ex` at runtime). This indirect compile time dependency is often the source of recompilations and Elixir v1.11 now makes it trivial to spot them. A developer interested in reducing compilation times would remove the compile time dependency on files such as `auth_helpers.ex`, which are frequent in the snippet above.

Another improvement to `mix xref graph` is the addition of `--format cycles`, which will print all cycles in your compilation dependency graph. A `--min-cycle-size` flag can be used if you want to discard short cycles.

## Other improvements

Elixir v1.11 adds the `is_struct/2` guard and also includes support for `map.field` syntax in guards.

The Calendar module now includes the `Calendar.strftime/3` function, which provides datetime formatting based on the `strftime` format. All Calendar types also got new conversion functions from and to gregorian timestamps, such as: `Date.from_gregorian_days/2` and `NaiveDateTime.to_gregorian_seconds/1`.

Mix also includes a new task, `mix test.coverage`, which generates aggregated coverage reports from in umbrella projects and partitioned test suites.

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
  * [ExUnit] Add `@tag :tmp_dir` support to ExUnit. The temporary directory is automatically created and pruned before each test
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
  * [mix release] Add `rel_templates_path` to configure the source of template files such as "env.sh.eex", "vm.args.eex" and "overlays"
  * [mix release] Allow some chunks to be kept in the `:strip_beams` config
  * [mix test.coverage] Add `mix test.coverage` that aggregates coverage results from umbrellas and OS partitioning
  * [mix xref] Make the `--label` option for `mix xref graph` transitive by default and add `--only-direct` for only direct dependencies
  * [mix xref] Add `--format cycles` support for `mix xref graph`

### 2. Bug fixes

#### EEx

  * [EEx] Make trimming behaviour via the `:trim` option more consistent

#### Elixir

  * [Code] Do not send language keyword through the `:static_atoms_encoder` in `Code.string_to_quoted`
  * [Kernel] Validate values given to `:line` in quote to avoid emitting invalid ASTs
  * [Kernel] Report the correct line number when raising inside a macro
  * [Kernel] Fix an issue where `elixirc` would not accept paths with backslash (`\`) separators on Windows
  * [Kernel.SpecialForms] Add `|/2` to the list of special forms to avoid inconsistent behaviour on overrides
  * [Keyword] Enforce keys to be atoms in `Keyword.keys/1`
  * [Version] Add defaults and enforce keys in `Version` struct

#### IEx

  * [IEx] Fix tokenizer emitting repeated warnings in the REPL

#### Mix

  * [mix release] Load `.app` from dependencies path when it is a project dependency
  * [mix release] Always include "rel/overlays" in the list of overlays directories if available

### 3. Soft-deprecations (no warnings emitted)

### Logger

  * [Logger] `warn` log level is deprecated in favor of `warning`

### Mix

  * [mix release] `runtime_config_path` is deprecated in favor of `releases_config_path`

### 4. Hard-deprecations

#### Elixir

  * [Application] Deprecate non-atom keys on `put_env`, `get_env`, `fetch_env`, and `delete_env`
  * [Supervisor] Deprecate `Supervisor.start_child/2` and `Supervisor.terminate_child/2` in favor of `DynamicSupervisor`
  * [Supervisor.Spec] Deprecate `Supervisor.Spec.worker/3` and `Supervisor.Spec.supervisor/3` in favor of the new typespecs

## v1.10

The CHANGELOG for v1.10 releases can be found [in the v1.10 branch](https://github.com/elixir-lang/elixir/blob/v1.10/CHANGELOG.md).
