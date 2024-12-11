# Changelog for Elixir v1.18

Elixir v1.18 is an impressive release with improvements across the two main efforts happening within the Elixir ecosystem right now: set-theoretic types and language servers. It also comes with built-in JSON support and adds new capabilities to its unit testing library. Here is a quick break down.

## Type system improvements

The most exciting change in Elixir v1.18 is type checking of function calls, alongside gradual inference of patterns and return types. To understand how this will impact your programs, consider the following code:

```elixir
defmodule User do
  defstruct [:age, :car_choice]

  def drive(%User{age: age, car_choice: car}, car_choices) when age >= 18 do
    if car in car_choices do
      {:ok, car}
    else
      {:error, :no_choice}
    end
  end

  def drive(%User{}, _car_choices) do
    {:error, :not_allowed}
  end
end
```

Elixir's type system will infer that the `drive/2` function expects a `%User{}` struct and returns either `{:ok, dynamic()}`, `{:error, :no_choice}`, or `{:error, :not_allowed}`.

Therefore, the following code should emit a violation, due to an invalid argument:

```elixir
User.drive({:ok, %User{}}, car_choices)
```

Here is the warning:

```
    warning: incompatible types given to User.drive/2:

        User.drive({:ok, %User{age: nil, car_choice: nil}}, car_choices)

    given types:

        {:ok, %User{age: nil, car_choice: nil}}, empty_list()

    but expected one of:

        dynamic(%User{age: term(), car_choice: term()}), dynamic()

    where "car_choices" was given the type:

        # type: empty_list()
        # from: lib/foo.ex:21:17
        car_choices = []

    typing violation found at:
    │
 22 │     User.drive({:ok, %User{}}, car_choices)
    │          ~
    │
    └─ lib/foo.ex:22:10: Example.run/0
```

> The mismatched arguments are shown in red, if your terminal supports ANSI coloring.

And the next snippet will warn because the `:error` clause will never match, as that's not a valid return type of the `User.drive/2` call:

```elixir
case User.drive(user, car_choices) do
  {:ok, car} -> car
  :error -> Logger.error("User cannot drive")
end
```

And here is the warning:

```
    warning: the following clause will never match:

        :error

    because it attempts to match on the result of:

        User.drive(user, car_choices)

    which has type:

        dynamic({:ok, term()} or {:error, :no_choice} or {:error, :not_allowed})

    typing violation found at:
    │
 26 │       :error -> Logger.error("User cannot drive")
    │       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    │
    └─ lib/foo.ex:26: Example.run/0
```

For more details on typing inference and the trade-offs made by the Elixir team, [see our official documentation](https://hexdocs.pm/elixir/1.18/gradual-set-theoretic-types.html#type-inference).

There are many other improvements to the type system, which we will go in detail within the official release. Meanwhile, here is a list summary of the overall improvements done to the type system:

* Type inference of patterns (typing inference of guards will be part of an upcoming release)

* Type checking of all language constructs, including local and remote calls, except `for`, `with`, and closures

* Type checking of all functions inlined by the compiler found in `Kernel`

* Type checking of all conversion functions inlined by the compiler

* [Support for tuples and lists as composite types](https://elixir-lang.org/blog/2024/08/28/typing-lists-and-tuples/) as well as type checking of their basic operations

* Detection of clauses and patterns that will never match from `case`, `cond`, and `=`

* Detection of unused clauses in private functions

## ExUnit improvements

ExUnit now supports parameterized tests to run the same test module multiple times under different parameters.

For example, Elixir ships a local, decentralized and scalable key-value process storage called `Registry`. The registry can be partitioned and its implementation differs depending if partitioning is enabled or not. Therefore, during tests, we want to ensure both modes are exercised. With Elixir v1.18, we can achieve this by writing:

```elixir
defmodule Registry.Test do
  use ExUnit.Case,
    async: true,
    parameterize: [
      %{partitions: 1},
      %{partitions: 8}
    ]

  # ... the actual tests ...
end
```

ExUnit parameterizes whole test modules. If your modules are configured to run concurrently, as above, so will the parameterized ones.

ExUnit also comes with the ability of specifying test groups. While ExUnit supports running tests concurrently, those tests must not have shared state between them. However, in large applications, it may be common for some tests to depend on some shared state, and other tests to depend on a completely separate state. For example, part of your tests may depend on Cassandra, while others depend on Redis. Prior to Elixir v1.18, these tests could not run concurrently, but in v1.18 they might as long as they are assigned to different groups. Tests modules within the same group do not run concurrently, but across groups, they might.

With features like async tests, suite partitioning, and now grouping, Elixir developers have plenty of flexibility to make the most use of their machine resources, both in development and in CI.

## `mix format --migrate`

The `mix format` command now supports an explicit `--migrate` flag, which will convert constructs that have been deprecated in Elixir to their latest version. Because this flag rewrites the AST, it is not guaranteed the migrated format will always be valid when used in combination with macros that also perform AST rewriting.

As of this release, the following migrations are executed:

  * Normalize parens in bitstring modifiers - it removes unnecessary parentheses in known bitstring modifiers, for example `<<foo::binary()>>` becomes `<<foo::binary>>`, or adds parentheses for custom modifiers, where `<<foo::custom_type>>` becomes `<<foo::custom_type()>>`.

  * Charlists as sigils - formats charlists as `~c` sigils, for example `'foo'` becomes `~c"foo"`.

  * `unless` as negated `if`s - rewrites `unless` expressions using `if` with a negated condition, for example `unless foo do` becomes `if !foo do`.

More migrations may be added in future releases.

## JSON support

This release includes official support for JSON encoding and decoding.

Both encoder and decoder fully conform to [RFC 8259](https://tools.ietf.org/html/rfc8259) and [ECMA 404](https://ecma-international.org/publications-and-standards/standards/ecma-404/) standards.

### Encoding

Encoding can be done via `JSON.encode!/1` and `JSON.encode_to_iodata!/1` functions. The default encoding rules are applied as follows:

| **Elixir**             | **JSON** |
|------------------------|----------|
| `integer() \| float()` | Number   |
| `true \| false `       | Boolean  |
| `nil`                  | Null     |
| `binary()`             | String   |
| `atom()`               | String   |
| `list()`               | Array    |
| `%{binary() => _}`     | Object   |
| `%{atom() => _}`       | Object   |
| `%{integer() => _}`    | Object   |

You may also implement the `JSON.Encoder` protocol for custom data structures. If you have a struct, you can derive the implementation of the `JSON.Encoder` by specifying which fields should be encoded to JSON:

```elixir
  @derive {JSON.Encoder, only: [...]}
  defstruct ...
```

### Decoding

Decoding can be done via `JSON.decode/2` and `JSON.decode!/2` functions. The default decoding rules are applied as follows:

| **JSON** | **Elixir**             |
|----------|------------------------|
| Number   | `integer() \| float()` |
| Boolean  | `true \| false`        |
| Null     | `nil`                  |
| String   | `binary()`             |
| Object   | `%{binary() => _}`     |

## Language server listeners

4 months ago, we welcomed [the Official Language Server team](https://elixir-lang.org/blog/2024/08/15/welcome-elixir-language-server-team/), with the goal of unifying the efforts behind code intelligence, tools, and editors in Elixir. Elixir v1.18 brings new features on this front by introducing locks and listeners to its compilation. Let's understand what it means.

At the moment, all language server implementations have their own compilation environment. This means that your project and dependencies during development are compiled once, for your own use, and then again for the language server. This duplicate effort could cause the language server experience to lag, when it could be relying on the already compiled artifacts of your project.

This release address by introducing a compiler lock, ensuring that only a single operating system process running Elixir compiles your project at a given moment, and by providing the ability for one operating system process to listen to the compilation results of others. In other words, different Elixir instances can now communicate over the same compilation build, instead of racing each other.

These enhancements do not only improve editor tooling, but they also directly benefit projects like IEx and Phoenix. For example, you can invoke `IEx.configure(auto_reload: true)` and IEx will automatically reload modules changed elsewhere, either by a separate terminal or your IDE.

## Potential incompatibilities

This release no longer supports WERL (a graphical user interface on Windows used by Erlang 25 and earlier). For a better user experience on Windows terminals, use Erlang/OTP 26+ (this is also the last Elixir release to support Erlang/OTP 25).

Furthermore, in order to support inference of patterns, Elixir will raise if it finds recursive variable definitions. This means patterns that never match, such as this one, will no longer compile:

    def foo(x = {:ok, y}, x = y)

However, recursion of root variables (where variables directly point to each other), will also fail to compile:

    def foo(x = y, y = z, z = x)

While the definition above could succeed (as long as all three arguments are equal), the cycle is not necessary and could be removed, as below:

    def foo(x = y, y = z, z)

You may also prefer to write using guards:

    def foo(x, y, z) when x == y and y == z

## v1.18.0-rc.0 (2024-12-10)

### 1. Enhancements

#### Elixir

  * [CLI] Add experimental PowerShell scripts for `elixir`, `elixirc`, and `mix` on Windows. Those provide a safer entry point for running Elixir from other platforms
  * [Calendar] Add `Duration.to_string/1`
  * [Code] Support several migration options in `Code.format_string!/2`
  * [Code] Add parenthesis around `--` and `---` in `Code.format_string!/2` to make precedence clearer
  * [Code] Include more metadata in `Code.string_to_quoted/2` when `token_metadata: true` to help compute ranges from the AST
  * [Code.Fragment] Have `:capture_arg` as its own entry in `Code.Fragment.surround_context/2`
  * [Config] Add `Config.read_config/1`
  * [Enumerable] Add `Enum.product_by/2` and `Enum.sum_by/2`
  * [Exception] Add `MissingApplicationsError` exception to denote missing applications
  * [Kernel] Update source code parsing to match [UTS #55](https://www.unicode.org/reports/tr55/) latest recommendations. In particular, mixed script is allowed in identifiers as long as they are separate by underscores (`_`), such as `http_сервер`. Previously allowed highly restrictive identifiers, which mixed Latin and other scripts, such as the japanese word for t-shirt, `Tシャツ`, now require the underscore as well
  * [Kernel] Warn on bidirectional confusability in identifiers
  * [Kernel] Verify the type of the binary generators
  * [Kernel] Track the type of tuples in patterns and inside `elem/2`
  * [Kernel] Perform validation of root AST nodes in `unquote` and `unquote_splicing` to catch bugs earlier
  * [Kernel] Add source, behaviour, and record information to Docs chunk metadata
  * [Kernel] Support deterministic builds in tandem with Erlang by setting `ERL_COMPILER_OPTIONS=deterministic`. Keep in mind deterministic builds strip source and other compile time information, which may be relevant for programs
  * [Kernel] Allow aliases and imports to be enabled conditionally in module body
  * [List] Add `List.ends_with?/2`
  * [Macro] Improve `dbg` handling of `if/2`, `with/1` and of code blocks
  * [Macro] Add `Macro.struct_info!/2` to return struct information mirroring `mod.__info__(:struct)`
  * [Registry] Add `Registry.lock/3` for local locking
  * [PartitionSupervisor] Add `PartitionSupervisor.resize!/2` to resize the number of partitions in a supervisor (up to the limit it was started with)
  * [Process] Handle arbitrarily high integer values in `Process.sleep/1`
  * [Protocol] Add `@undefined_impl_description` to customize error message when an implementation is undefined
  * [Protocol] Add `__deriving__/1` as optional macro callback to `Protocol`, no longer requiring empty implementations
  * [String] Inspect special whitespace and zero-width characters using their Unicode representation
  * [String] Update Unicode to 16.0

#### ExUnit

  * [ExUnit] Support parameterized tests on `ExUnit.Case`
  * [ExUnit] Support test groups: tests in the same group never run concurrently
  * [ExUnit.Case] Add `test_pid` as a tag

#### IEx

  * [IEx] Add `:dot_iex` support to `IEx.configure/1`
  * [IEx] Add report for normal/shutdown exits in IEx

#### Mix

  * [mix compile] Ensure only a single operating system process can compile at a given time
  * [mix deps.get] Ensure only a single operating system process can fetch deps at a given time
  * [mix format] Add `mix format --migrate` to migrate from deprecated functionality
  * [mix format] Add new options and metadata to improve formatting applying by editors and other environments
  * [mix test] Taint failure manifest if requiring or compiling tests fail
  * [Mix.Project] Add a `:listeners` configuration to listen to compilation events from the current and other operating system processes
  * [Mix.Task.Compiler] Add API for fetching all persisted compiler diagnostics
  * [Mix.Task.Compiler] Add API for fetching all compiler tasks

### 2. Bug fixes

#### Elixir

  * [Code] Fix delimiter metadata for single quoted atoms and remote calls in `Code.string_to_quoted/2`
  * [Code.Formatter] Fix formatter adding extra escapes to quoted remote calls
  * [Code.Fragment] Properly handle keyword keys as their own entry
  * [Inspect.Algebra] Ensure `next_break_fits` respects `line_length`
  * [Kernel] Validate AST on `unquote` and `unquote_splicing` to provide better error reports instead of failing too late inside the compiler
  * [Module] Include module attribute line and name when tracing its aliases
  * [Stream] Do not halt streams twice in `Stream.transform/5`
  * [URI] Fix a bug when a schemaless URI is given to `URI.merge/2`

#### ExUnit

  * [ExUnit.Assertions] Raise if guards are used in `assert/1` with `=`
  * [ExUnit.Assertions] Format inserted/deleted maps in list assertions

#### IEx

  * [IEx.Helpers] `IEx.Helpers.recompile/0` will reload modules changed by other operating system processes

#### Mix

  * [mix compile] Ensure warnings from external resources are emitted with `--all-warnings` when files do not change
  * [mix deps.compile] Fix escaping issues when invoking `rebar3` in some cases
  * [mix escript] Fix escript layout and support storing `priv` directories
  * [mix release] Make `.app` files deterministic in releases
  * [Mix.Shell] Fix `Mix.Shell` on Windows when outputting non UTF-8 characters

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Inspect.Algebra] `color/3` is deprecated in favor of `color_doc/3`
  * [Inspect.Algebra] `fold_doc/2` is deprecated in favor of `fold/2`
  * [Kernel] Deprecate `unless` in favor of `if`. Use `mix format --migrate` to automate the migration
  * [Macro] `Macro.struct!/2` is deprecated in favor of `Macro.struct_info!/2`
  * [Protocol] Defining `__deriving__/3` inside the `Any` implementation is deprecated, derive it inside the protocol definition itself

### 4. Hard deprecations

#### EEx

  * [EEx] `<%#` is deprecated in favor of `<%!--` or `<% #`
  * [EEx] `c:EEx.handle_text/2` is deprecated in favor of `c:EEx.handle_text/3`

#### Elixir

  * [Code] Setting `:warnings_as_errors` is deprecated via `Code.put_compiler_option/2`. This must not affect developers, as the `:warnings_as_errors` option is managed by Mix tasks, and not directly used via the `Code` module
  * [Enumerable] Deprecate returning a two-arity function in `Enumerable.slice/1`
  * [List] `List.zip/1` is deprecated in favor of `Enum.zip/1`
  * [Module] Deprecate `Module.eval_quoted/3` in favor of `Code.eval_quoted/3`
  * [Range] Deprecate inferring negative ranges on `Range.new/2`
  * [Tuple] `Tuple.append/2` is deprecated, use `Tuple.insert_at/3` instead

#### Mix

  * [mix cmd] Deprecate `mix cmd --app APP` in favor of `mix do --app APP`
  * [Mix.Tasks.Compile] Deprecate `compilers/0` in favor of `Mix.Task.Compiler.compilers/0`

## v1.17

The CHANGELOG for v1.17 releases can be found [in the v1.17 branch](https://github.com/elixir-lang/elixir/blob/v1.17/CHANGELOG.md).
