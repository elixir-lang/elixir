# Changelog for Elixir v1.1

## v1.1.0-dev

### 1 Enhancements

  * [Enum] Add `Enum.minmax/1` and `Enum.minmax_by/2`
  * [ExUnit] Add number of skipped tests to ExUnit output
  * [File] Add `File.lstat/1` and `File.lstat/1` that works like `File.stat/1` but is able to return symlink information (i.e. it does not traverse symlinks)
  * [Kernel] Raise when `var.Alias` syntax is used and it does not expand to an atom at compile time (previously it just warned)
  * [Mix] `mix help` task now supports `mix help --search PATTERN` for filtering task names
  * [StringIO] `StringIO.flush/1` was added to flush the output of a StringIO device
  * [URI] Default ports were added for "ws" and "wss" schemas

### 2. Bug fixes

  * [ExUnit] Skipped tests now correctly count towards the total of tests in the result returned by `ExUnit.run/0`
  * [Regex] Fix splitting of empty strings with regexes when trim is set to true. Now both `String.split/3` and `Regex.split/3` return an empty list when called with an empty string and trim is enabled

### 3. Soft deprecations (no warnings emitted)

  * [Regex] Ungreedy option `r` is deprecated in favor of `U`
