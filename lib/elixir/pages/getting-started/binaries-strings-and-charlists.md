<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Binaries, strings, and charlists

In ["Basic types"](basic-types.md), we learned a bit about strings and we used the `is_binary/1` function for checks:

```elixir
iex> string = "hello"
"hello"
iex> is_binary(string)
true
```

In this chapter, we will gain clarity on what exactly binaries are and how they relate to strings. We will also learn about charlists, `~c"like this"`, which are often used for interoperability with Erlang.

Although strings are one of the most common data types in computer languages, they are subtly complex and are often misunderstood. To understand strings in Elixir, let's first discuss [Unicode](https://en.wikipedia.org/wiki/Unicode) and character encodings, specifically the [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoding.

## Unicode and Code Points

In order to facilitate meaningful communication between computers across multiple languages, a standard is required so that the ones and zeros on one machine mean the same thing when they are transmitted to another. The [Unicode Standard](https://unicode.org/standard/standard.html) acts as an official registry of virtually all the characters we know: this includes characters from classical and historical texts, emoji, and formatting and control characters as well.

Unicode organizes all of the characters in its repertoire into code charts, and each character is given a unique numerical index. This numerical index is known as a [Code Point](https://en.wikipedia.org/wiki/Code_point).

In Elixir you can use a `?` in front of a character literal to reveal its code point:

```elixir
iex> ?a
97
iex> ?ł
322
```

Note that most Unicode code charts will refer to a code point by its hexadecimal (hex) representation, e.g. `97` translates to `0061` in hex, and we can represent any Unicode character in an Elixir string by using the `\uXXXX` notation and the hex representation of its code point number:

```elixir
iex> "\u0061" == "a"
true
iex> 0x0061 = 97 = ?a
97
```

The hex representation will also help you look up information about a code point, e.g. [https://codepoints.net/U+0061](https://codepoints.net/U+0061) has a data sheet all about the lower case `a`, a.k.a. code point 97.

## UTF-8 and Encodings

Now that we understand what the Unicode standard is and what code points are, we can finally talk about encodings. Whereas the code point is **what** we store, an encoding deals with **how** we store it: encoding is an implementation. In other words, we need a mechanism to convert the code point numbers into bytes so they can be stored in memory, written to disk, etc.

Elixir uses UTF-8 to encode its strings, which means that code points are encoded as a series of 8-bit bytes. UTF-8 is a **variable width** character encoding that uses one to four bytes to store each code point. It is capable of encoding all valid Unicode code points. Let's see an example:

```elixir
iex> string = "héllo"
"héllo"
iex> String.length(string)
5
iex> byte_size(string)
6
```

Although the string above has 5 characters, it uses 6 bytes, as two bytes are used to represent the character `é`.

> Note: if you are running on Windows, there is a chance your terminal does not use UTF-8 by default. You can change the encoding of your current session by running `chcp 65001` before entering `iex` (`iex.bat`).

Besides defining characters, UTF-8 also provides a notion of graphemes. Graphemes may consist of multiple characters that are often perceived as one. For example, the [woman firefighter emoji](https://emojipedia.org/woman-firefighter/) is represented as the combination of three characters: the woman emoji (👩), a hidden zero-width joiner, and the fire engine emoji (🚒):

```elixir
iex> String.codepoints("👩‍🚒")
["👩", "‍", "🚒"]
iex> String.graphemes("👩‍🚒")
["👩‍🚒"]
```

However, Elixir is smart enough to know they are seen as a single character, and therefore the length is still one:

```elixir
iex> String.length("👩‍🚒")
1
```

> Note: if you can't see the emoji above in your terminal, you need to make sure your terminal supports emoji and that you are using a font that can render them.

Although these rules may sound complicated, UTF-8 encoded documents are everywhere. This page itself is encoded in UTF-8. The encoding information is given to your browser which then knows how to render all of the bytes, characters, and graphemes accordingly.

If you want to see the exact bytes that a string would be stored in a file, a common trick is to concatenate the null byte `<<0>>` to it:

```elixir
iex> "hełło" <> <<0>>
<<104, 101, 197, 130, 197, 130, 111, 0>>
```

Alternatively, you can view a string's binary representation by using `IO.inspect/2`:

```elixir
iex> IO.inspect("hełło", binaries: :as_binaries)
<<104, 101, 197, 130, 197, 130, 111>>
```

We are getting a little bit ahead of ourselves. Let's talk about bitstrings to learn about what exactly the `<<>>` constructor means.

## Bitstrings

Although we have covered code points and UTF-8 encoding, we still need to go a bit deeper into how exactly we store the encoded bytes, and this is where we introduce the **bitstring**. A bitstring is a fundamental data type in Elixir, denoted with the [`<<>>`](`<<>>/1`) syntax. **A bitstring is a contiguous sequence of bits in memory.**

By default, 8 bits (i.e. 1 byte) is used to store each number in a bitstring, but you can manually specify the number of bits via a `::n` modifier to denote the size in `n` bits, or you can use the more verbose declaration `::size(n)`:

```elixir
iex> <<42>> == <<42::8>>
true
iex> <<3::4>>
<<3::size(4)>>
```

For example, the decimal number `3` when represented with 4 bits in base 2 would be `0011`, which is equivalent to the values `0`, `0`, `1`, `1`, each stored using 1 bit:

```elixir
iex> <<0::1, 0::1, 1::1, 1::1>> == <<3::4>>
true
```

Any value that exceeds what can be stored by the number of bits provisioned is truncated:

```elixir
iex> <<1>> == <<257>>
true
```

Here, 257 in base 2 would be represented as `100000001`, but since we have reserved only 8 bits for its representation (by default), the left-most bit is ignored and the value becomes truncated to `00000001`, or simply `1` in decimal.

A complete reference for the bitstring constructor can be found in [`<<>>`](`<<>>/1`)'s documentation.

## Binaries

**A binary is a bitstring where the number of bits is divisible by 8.** That means that every binary is a bitstring, but not every bitstring is a binary. We can use the `is_bitstring/1` and `is_binary/1` functions to demonstrate this.

```elixir
iex> is_bitstring(<<3::4>>)
true
iex> is_binary(<<3::4>>)
false
iex> is_bitstring(<<0, 255, 42>>)
true
iex> is_binary(<<0, 255, 42>>)
true
iex> is_binary(<<42::16>>)
true
```

We can pattern match on binaries / bitstrings:

```elixir
iex> <<0, 1, x>> = <<0, 1, 2>>
<<0, 1, 2>>
iex> x
2
iex> <<0, 1, x>> = <<0, 1, 2, 3>>
** (MatchError) no match of right hand side value: <<0, 1, 2, 3>>
```

Note that unless you explicitly use `::` modifiers, each entry in the binary pattern is expected to match a single byte (exactly 8 bits). If we want to match on a binary of unknown size, we can use the `binary` modifier at the end of the pattern:

```elixir
iex> <<0, 1, x::binary>> = <<0, 1, 2, 3>>
<<0, 1, 2, 3>>
iex> x
<<2, 3>>
```

There are a couple other modifiers that can be useful when doing pattern matches on binaries. The `binary-size(n)` modifier will match `n` bytes in a binary:

```elixir
iex> <<head::binary-size(2), rest::binary>> = <<0, 1, 2, 3>>
<<0, 1, 2, 3>>
iex> head
<<0, 1>>
iex> rest
<<2, 3>>
```

**A string is a UTF-8 encoded binary**, where the code point for each character is encoded using 1 to 4 bytes. Thus every string is a binary, but due to the UTF-8 standard encoding rules, not every binary is a valid string.

```elixir
iex> is_binary("hello")
true
iex> is_binary(<<239, 191, 19>>)
true
iex> String.valid?(<<239, 191, 19>>)
false
```

The string concatenation operator [`<>`](`<>/2`) is actually a binary concatenation operator:

```elixir
iex> "a" <> "ha"
"aha"
iex> <<0, 1>> <> <<2, 3>>
<<0, 1, 2, 3>>
```

Given that strings are binaries, we can also pattern match on strings:

```elixir
iex> <<head, rest::binary>> = "banana"
"banana"
iex> head == ?b
true
iex> rest
"anana"
```

However, remember that binary pattern matching works on *bytes*, so matching on the string like "über" with multibyte characters won't match on the *character*, it will match on the *first byte of that character*:

```elixir
iex> "ü" <> <<0>>
<<195, 188, 0>>
iex> <<x, rest::binary>> = "über"
"über"
iex> x == ?ü
false
iex> rest
<<188, 98, 101, 114>>
```

Above, `x` matched on only the first byte of the multibyte `ü` character.

Therefore, when pattern matching on strings, it is important to use the `utf8` modifier:

```elixir
iex> <<x::utf8, rest::binary>> = "über"
"über"
iex> x == ?ü
true
iex> rest
"ber"
```

## Charlists

Our tour of our bitstrings, binaries, and strings is nearly complete, but we have one more data type to explain: the charlist.

**A charlist is a list of integers where all the integers are valid code points.** In practice, you will not come across them often, only in specific scenarios such as interfacing with older Erlang libraries that do not accept binaries as arguments.

```elixir
iex> ~c"hello"
~c"hello"
iex> [?h, ?e, ?l, ?l, ?o]
~c"hello"
```

The [`~c`](`Kernel.sigil_c/2`) sigil (we'll cover sigils later in the ["Sigils"](sigils.md) chapter) indicates the fact that we are dealing with a charlist and not a regular string.

Instead of containing bytes, a charlist contains integer code points. However, the list is only printed as a sigil if all code points are within the ASCII range:

```elixir
iex> ~c"hełło"
[104, 101, 322, 322, 111]
iex> is_list(~c"hełło")
true
```

This is done to ease interoperability with Erlang, even though it may lead to some surprising behavior. For example, if you are storing a list of integers that happen to range between 0 and 127, by default IEx will interpret this as a charlist and it will display the corresponding ASCII characters.

```elixir
iex> heartbeats_per_minute = [99, 97, 116]
~c"cat"
```

You can always force charlists to be printed in their list representation by calling the `inspect/2` function:

```elixir
iex> inspect(heartbeats_per_minute, charlists: :as_list)
"[99, 97, 116]"
```

Furthermore, you can convert a charlist to a string and back by using the `to_string/1` and `to_charlist/1`:

```elixir
iex> to_charlist("hełło")
[104, 101, 322, 322, 111]
iex> to_string(~c"hełło")
"hełło"
iex> to_string(:hello)
"hello"
iex> to_string(1)
"1"
```

The functions above are polymorphic, in other words, they accept many shapes: not only do they convert charlists to strings (and vice-versa), they can also convert integers, atoms, and so on.

String (binary) concatenation uses the [`<>`](`<>/2`) operator but charlists, being lists, use the list concatenation operator [`++`](`++/2`):

```elixir
iex> ~c"this " <> ~c"fails"
** (ArgumentError) expected binary argument in <> operator but got: ~c"this "
    (elixir) lib/kernel.ex:1821: Kernel.wrap_concatenation/3
    (elixir) lib/kernel.ex:1808: Kernel.extract_concatenations/2
    (elixir) expanding macro: Kernel.<>/2
    iex:1: (file)
iex> ~c"this " ++ ~c"works"
~c"this works"
iex> "he" ++ "llo"
** (ArgumentError) argument error
    :erlang.++("he", "llo")
iex> "he" <> "llo"
"hello"
```

With binaries, strings, and charlists out of the way, it is time to talk about key-value data structures.
