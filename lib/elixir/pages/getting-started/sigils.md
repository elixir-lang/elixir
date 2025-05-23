<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Sigils

Elixir provides double-quoted strings as well as a concept called charlists, which are defined using the `~c"hello world"` sigil syntax. In this chapter, we will learn more about sigils and how to define our own.

One of Elixir's goals is extensibility: developers should be able to extend the language to fit any particular domain. Sigils provide the foundation for extending the language with custom textual representations. Sigils start with the tilde (`~`) character which is followed by either a single lower-case letter or one or more upper-case letters, and then a delimiter. Optional modifiers are added after the final delimiter.

## Regular expressions

The most common sigil in Elixir is `~r`, which is used to create [regular expressions](https://en.wikipedia.org/wiki/Regular_Expressions):

```elixir
# A regular expression that matches strings which contain "foo" or "bar":
iex> regex = ~r/foo|bar/
~r/foo|bar/
iex> "foo" =~ regex
true
iex> "bat" =~ regex
false
```

Elixir provides Perl-compatible regular expressions (regexes), as implemented by the [PCRE](http://www.pcre.org/) library. Regexes also support modifiers. For example, the `i` modifier makes a regular expression case insensitive:

```elixir
iex> "HELLO" =~ ~r/hello/
false
iex> "HELLO" =~ ~r/hello/i
true
```

Check out the `Regex` module for more information on other modifiers and the supported operations with regular expressions.

So far, all examples have used `/` to delimit a regular expression. However, sigils support 8 different delimiters:

```elixir
~r/hello/
~r|hello|
~r"hello"
~r'hello'
~r(hello)
~r[hello]
~r{hello}
~r<hello>
```

The reason behind supporting different delimiters is to provide a way to write literals without escaped delimiters. For example, a regular expression with forward slashes like `~r(^https?://)` reads arguably better than `~r/^https?:\/\//`. Similarly, if the regular expression has forward slashes and capturing groups (that use `()`), you may then choose double quotes instead of parentheses.

## Strings, charlists, and word lists sigils

Besides regular expressions, Elixir ships with three other sigils.

### Strings

The `~s` sigil is used to generate strings, like double quotes are. The `~s` sigil is useful when a string contains double quotes:

```elixir
iex> ~s(this is a string with "double" quotes, not 'single' ones)
"this is a string with \"double\" quotes, not 'single' ones"
```

### Charlists

The `~c` sigil is the regular way to represent charlists.

```elixir
iex> [?c, ?a, ?t]
~c"cat"
iex> ~c(this is a char list containing "double quotes")
~c"this is a char list containing \"double quotes\""
```

### Word lists

The `~w` sigil is used to generate lists of words (*words* are just regular strings). Inside the `~w` sigil, words are separated by whitespace.

```elixir
iex> ~w(foo bar bat)
["foo", "bar", "bat"]
```

The `~w` sigil also accepts the `c`, `s` and `a` modifiers (for charlists, strings, and atoms, respectively), which specify the data type of the elements of the resulting list:

```elixir
iex> ~w(foo bar bat)a
[:foo, :bar, :bat]
```

## Interpolation and escaping in string sigils

Elixir supports some sigil variants to deal with escaping characters and interpolation. In particular, uppercase letters sigils do not perform interpolation nor escaping. For example, although both `~s` and `~S` will return strings, the former allows escape codes and interpolation while the latter does not:

```elixir
iex> ~s(String with escape codes \x26 #{"inter" <> "polation"})
"String with escape codes & interpolation"
iex> ~S(String without escape codes \x26 without #{interpolation})
"String without escape codes \\x26 without \#{interpolation}"
```

The following escape codes can be used in strings and charlists:

  * `\\` – single backslash
  * `\a` – bell/alert
  * `\b` – backspace
  * `\d` - delete
  * `\e` - escape
  * `\f` - form feed
  * `\n` – newline
  * `\r` – carriage return
  * `\s` – space
  * `\t` – tab
  * `\v` – vertical tab
  * `\0` - null byte
  * `\xDD` - represents a single byte in hexadecimal (such as `\x13`)
  * `\uDDDD` and `\u{D...}` - represents a Unicode codepoint in hexadecimal (such as `\u{1F600}`)

In addition to those, a double quote inside a double-quoted string needs to be escaped as `\"`, and, analogously, a single quote inside a single-quoted char list needs to be escaped as `\'`. Nevertheless, it is better style to change delimiters as seen above than to escape them.

Sigils also support heredocs, that is, three double-quotes or single-quotes as separators:

```elixir
iex> ~s"""
...> this is
...> a heredoc string
...> """
```

The most common use case for heredoc sigils is when writing documentation. For example, writing escape characters in the documentation would soon become error prone because of the need to double-escape some characters:

```elixir
@doc """
Converts double-quotes to single-quotes.

## Examples

    iex> convert("\\\"foo\\\"")
    "'foo'"

"""
def convert(...)
```

By using `~S`, this problem can be avoided altogether:

```elixir
@doc ~S"""
Converts double-quotes to single-quotes.

## Examples

    iex> convert("\"foo\"")
    "'foo'"

"""
def convert(...)
```

## Calendar sigils

Elixir offers several sigils to deal with various flavors of times and dates.

### Date

A [%Date{}](`Date`) struct contains the fields `year`, `month`, `day`, and `calendar`. You can create one using the `~D` sigil:

```elixir
iex> d = ~D[2019-10-31]
~D[2019-10-31]
iex> d.day
31
```

### Time

The [%Time{}](`Time`) struct contains the fields `hour`, `minute`, `second`, `microsecond`, and `calendar`. You can create one using the `~T` sigil:

```elixir
iex> t = ~T[23:00:07.0]
~T[23:00:07.0]
iex> t.second
7
```

### NaiveDateTime

The [%NaiveDateTime{}](`NaiveDateTime`) struct contains fields from both `Date` and `Time`. You can create one using the `~N` sigil:

```elixir
iex> ndt = ~N[2019-10-31 23:00:07]
~N[2019-10-31 23:00:07]
```

Why is it called naive? Because it does not contain timezone information. Therefore, the given datetime may not exist at all or it may exist twice in certain timezones - for example, when we move the clock back and forward for daylight saving time.

### UTC DateTime

A [%DateTime{}](`DateTime`) struct contains the same fields as a `NaiveDateTime` with the addition of fields to track timezones. The `~U` sigil allows developers to create a DateTime in the UTC timezone:

```elixir
iex> dt = ~U[2019-10-31 19:59:03Z]
~U[2019-10-31 19:59:03Z]
iex> %DateTime{minute: minute, time_zone: time_zone} = dt
~U[2019-10-31 19:59:03Z]
iex> minute
59
iex> time_zone
"Etc/UTC"
```

## Custom sigils

As hinted at the beginning of this chapter, sigils in Elixir are extensible. In fact, using the sigil `~r/foo/i` is equivalent to calling `sigil_r` with a binary and a char list as the argument:

```elixir
iex> sigil_r(<<"foo">>, [?i])
~r"foo"i
```

We can access the documentation for the `~r` sigil via `sigil_r`:

```elixir
iex> h sigil_r
...
```

We can also provide our own sigils by implementing functions that follow the `sigil_{character}` pattern. For example, let's implement the `~i` sigil that returns an integer (with the optional `n` modifier to make it negative):

```elixir
iex> defmodule MySigils do
...>   def sigil_i(string, []), do: String.to_integer(string)
...>   def sigil_i(string, [?n]), do: -String.to_integer(string)
...> end
iex> import MySigils
iex> ~i(13)
13
iex> ~i(42)n
-42
```

Custom sigils may be either a single lowercase character, or an uppercase character followed by more uppercase characters and digits.

Sigils can also be used to do compile-time work with the help of macros. For example, regular expressions in Elixir are compiled into an efficient representation during compilation of the source code, therefore skipping this step at runtime. If you're interested in the subject, you can learn more about macros and check out how sigils are implemented in the `Kernel` module (where the `sigil_*` functions are defined).
