# Compatibility and Deprecations

## Compatibility between Elixir v1.x versions

Elixir v1.x releases are backwards compatible: well-defined behaviours and documented APIs in a given release will continue working on future releases.

Although we expect the vast majority of programs to remain compatible over time, it is impossible to guarantee that no future change will break any program. Under some unlikely circumstances, we may introduce changes that break existing code:

  * Security: a security issue in the implementation may arise whose resolution requires backwards incompatible changes. We reserve the right to address such security issues.

  * Bugs: if an API has undesired behaviour, a program that depends on the buggy behaviour may break if the bug is fixed. We reserve the right to fix such bugs.

  * Compiler front-end: improvements may be done to the compiler, introducing new warnings for ambiguous modes and providing more detailed error messages. Those can lead to compilation errors (when running with `--warning-as-errors`) or tooling failures when asserting on specific error messages (although one should avoid such). We reserve the right to do such improvements.

  * Imports: new functions may be added to the `Kernel` module, which is auto-imported. They may collide with local functions defined in your modules. Collisions can be resolved in a backwards compatible fashion using `import Kernel, except: [...]` with a list of all functions you don't want to be imported from `Kernel`. We reserve the right to do such additions.

In order to continue evolving the language without introducing breaking changes, Elixir will rely on deprecations to demote certain practices and promote new ones. Our deprecation policy is outlined in the ["Deprecations" section](#deprecations).

Elixir binaries are guaranteed to be backwards compatible within the same branch. Code compiled with v1.0 shall work with Elixir v1.0.1 runtime but code compiled with Elixir v1.0 should be recompiled to work from Elixir v1.1.0 onwards. The Mix build tool automatically tracks those changes.

The only exception to the rules above are experimental features, which will be explicitly marked as such, and do not provide any compatibility guarantee until they are stabilized.

## Compatibility between Elixir and Erlang/OTP

Erlang/OTP versioning is independent from the versioning of Elixir. Each version of Elixir supports a specific range of Erlang/OTP versions. The compatibility table is shown below.

Elixir version | Supported Erlang/OTP versions
:------------- | :----------------------------
1.0            | 17 - 17 (and OTP 18 from v1.0.5)
1.1            | 17 - 18
1.2            | 18 - 18 (and OTP 19 from v1.2.6)
1.3            | 18 - 19
1.4            | 18 - 19 (and OTP 20 from v1.4.5)
1.5            | 18 - 20
1.6            | 19 - 20

While Elixir often adds compatibility to new Erlang versions on released branches, such as support for OTP 20 in v1.4.5, those releases usually contain the minimum changes for Elixir to run without errors. Only the next minor release, in this case v1.5.0, does effectively leverage the new features provided by the latest Erlang release.

## Deprecations

### Policy

Elixir deprecations happen in 3 steps:

  1. The feature is soft-deprecated. It means both CHANGELOG and documentation must list the feature as deprecated but no warning is effectively emitted by running the code. There is no requirement to soft-deprecate a feature.

  2. The feature is effectively deprecated by emitting warnings on usage. In order to deprecate a feature, the proposed alternative MUST exist for AT LEAST two versions. For example, `Enum.uniq/2` was soft-deprecated in favor of `Enum.uniq_by/2` in Elixir v1.1. This means a deprecation warning may only be emitted by Elixir v1.3 or later.

  3. The feature is removed. This can only happen on major releases. This means deprecated features in Elixir v1.x shall only be removed by Elixir v2.x.

### Table of deprecations

Deprecated feature                               | Deprecated in | Replaced by (available since)
:----------------------------------------------- | :------------ | :----------------------------
`Enum.chunk/2/3/4`                               | [v1.7]        | `Enum.chunk_every/2/3/4` (v1.5)
`not left in right`                              | [v1.7]        | `left not in right` (v1.5)
`Registry.start_link/3`                          | [v1.7]        | `Registry.start_link/1` (v1.5)
`Stream.chunk/2/3/4`                             | [v1.7]        | `Stream.chunk_every/2/3/4` (v1.5)
`Enum.partition/2`                               | [v1.6]        | `Enum.split_with/2` (v1.4)
`Keyword.replace/3`                              | [v1.6]        | Use `Keyword.fetch/2` + `Keyword.put/3` (v1.0)
`Macro.unescape_tokens/1` and `Macro.unescape_tokens/2` | [v1.6] | Use `Enum.map/2` to traverse over the arguments (v1.0)
`Module.add_doc/6`                               | [v1.6]        | Use `@doc` instead
`Map.replace/3`                                  | [v1.6]        | Use `Map.fetch/2` + `Map.put/3` (v1.0)
`Range.range?/1`                                 | [v1.6]        | Pattern match on `_.._` instead (v1.0)
`Atom.to_char_list/1`                            | [v1.5]        | `Atom.to_charlist/1` (v1.3)
`Enum.filter_map/3`                              | [v1.5]        | `Enum.filter/2` + `Enum.map/2` or for comprehensions (v1.0)
`Float.to_char_list/1`                           | [v1.5]        | `Float.to_charlist/1` (v1.3)
`GenEvent` module                                | [v1.5]        | `Supervisor` and `GenServer` (v1.0);<br/>[`GenStage`](https://hex.pm/packages/gen_stage) (v1.3);<br/>[`:gen_event`](http://www.erlang.org/doc/man/gen_event.html) (OTP 17)
`Integer.to_char_list/1` and `Integer.to_char_list/2` | [v1.5]   | `Integer.to_charlist/1` and `Integer.to_charlist/2` (v1.3)
`Kernel.to_char_list/1`                          | [v1.5]        | `Kernel.to_charlist/1` (v1.3)
`List.Chars.to_char_list/1`                      | [v1.5]        | `List.Chars.to_charlist/1` (v1.3)
`Stream.filter_map/3`                            | [v1.5]        | `Stream.filter/2` + `Stream.map/2` (v1.0)
`String.ljust/3` and `String.rjust/3`            | [v1.5]        | `String.pad_leading/3` and `String.pad_trailing/3` with a binary padding (v1.3)
`String.strip/1` and `String.strip/2`            | [v1.5]        | `String.trim/1` and `String.trim/2` (v1.3)
`String.lstrip/1` and `String.rstrip/1`          | [v1.5]        | `String.trim_leading/1` and `String.trim_trailing/1` (v1.3)
`String.lstrip/2` and `String.rstrip/2`          | [v1.5]        | `String.trim_leading/2` and `String.trim_trailing/2` with a binary as second argument (v1.3)
`String.to_char_list/1`                          | [v1.5]        | `String.to_charlist/1` (v1.3)
`()` to mean `nil`                               | [v1.5]        | `nil` (v1.0)
`:as_char_lists` value in `t:Inspect.Opts.t/0` type | [v1.5]     | `:as_charlists` (v1.3)
`:char_lists` key in `t:Inspect.Opts.t/0` type   | [v1.5]        | `:charlists` (v1.3)
`char_list/0` type                               | [v1.5]        | `t:charlist/0` type (v1.3)
`@compile {:parse_transform, _}` in `Module`     | [v1.5]        | *None*
EEx: `<%=` in middle and end expressions         | [v1.5]        | Use `<%` (= is allowed only on start expressions) (v1.0)
`Access.key/1`                                   | [v1.4]        | `Access.key/2` (v1.3)
`Behaviour` module                               | [v1.4]        | `@callback` (v1.0)
`Enum.uniq/2`                                    | [v1.4]        | `Enum.uniq_by/2` (v1.2)
`Float.to_char_list/2`                           | [v1.4]        | `:erlang.float_to_list/2` (OTP 17)
`Float.to_string/2`                              | [v1.4]        | `:erlang.float_to_binary/2` (OTP 17)
`HashDict` module                                | [v1.4]        | `Map` (v1.2)
`HashSet` module                                 | [v1.4]        | `MapSet` (v1.1)
`Set` module                                     | [v1.4]        | `MapSet` (v1.1)
`Stream.uniq/2`                                  | [v1.4]        | `Stream.uniq_by/2` (v1.2)
`IEx.Helpers.import_file/2`                      | [v1.4]        | `IEx.Helpers.import_file_if_available/1` (v1.3)
`Mix.Utils.camelize/1`                           | [v1.4]        | `Macro.camelize/1` (v1.2)
`Mix.Utils.underscore/1`                         | [v1.4]        | `Macro.underscore/1` (v1.2)
Variable used as function call                   | [v1.4]        | Use parentheses (v1.0)
Anonymous functions with no expression after `->` | [v1.4]       | Use an expression or explicitly return `nil` (v1.0)
`Dict` module                                    | [v1.3]        | `Keyword` (v1.0) or `Map` (v1.2)
`Keyword.size/1`                                 | [v1.3]        | `Kernel.length/1` (v1.0)
`Map.size/1`                                     | [v1.3]        | `Kernel.map_size/1` (v1.0)
`Set` behaviour                                  | [v1.3]        | `MapSet` data structure (v1.1)
`String.valid_character?/1`                      | [v1.3]        | `String.valid?/1` (v1.0)
`Task.find/2`                                    | [v1.3]        | Use direct message matching (v1.0)
`:append_first` option in `Kernel.defdelegate/2` | [v1.3]        | Define the function explicitly (v1.0)
`/r` option in `Regex`                           | [v1.3]        | `/U` (v1.1)
`\x{X*}` inside strings/sigils/charlists         | [v1.3]        | `\uXXXX` or `\u{X*}` (v1.1)
Map or dictionary as second argument in `Enum.group_by/3` | [v1.3] | Use `Enum.reduce/3` (v1.0)
Non-map as second argument in `URI.decode_query/2` | [v1.3]      | Use a map (v1.0)
`Dict` behaviour                                 | [v1.2]        | `MapSet` data structure (v1.1)
`Access` protocol                                | [v1.1]        | `Access` behaviour (v1.1)
`as: true \| false` in `alias/2` and `require/2` | [v1.1]        | *None*
`?\xHEX`                                         | [v1.1]        | `0xHEX` (v1.0)

[v1.1]: https://github.com/elixir-lang/elixir/blob/v1.1/CHANGELOG.md#4-deprecations
[v1.2]: https://github.com/elixir-lang/elixir/blob/v1.2/CHANGELOG.md#changelog-for-elixir-v12
[v1.3]: https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md#4-deprecations
[v1.4]: https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md#4-deprecations
[v1.5]: https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md#4-deprecations
[v1.6]: https://github.com/elixir-lang/elixir/blob/v1.6/CHANGELOG.md#4-deprecations
[v1.7]: https://github.com/elixir-lang/elixir/blob/master/CHANGELOG.md#4-deprecations
