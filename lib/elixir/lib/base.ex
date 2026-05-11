# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Base do
  import Bitwise

  @moduledoc """
  This module provides data encoding and decoding functions
  according to [RFC 4648](https://tools.ietf.org/html/rfc4648).

  This document defines the commonly used base 16, base 32, and base
  64 encoding schemes.

  ## Base 16 alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | 0        |     4 | 4        |     8 | 8        |    12 | C        |
  |     1 | 1        |     5 | 5        |     9 | 9        |    13 | D        |
  |     2 | 2        |     6 | 6        |    10 | A        |    14 | E        |
  |     3 | 3        |     7 | 7        |    11 | B        |    15 | F        |

  ## Base 32 alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | A        |     9 | J        |    18 | S        |    27 | 3        |
  |     1 | B        |    10 | K        |    19 | T        |    28 | 4        |
  |     2 | C        |    11 | L        |    20 | U        |    29 | 5        |
  |     3 | D        |    12 | M        |    21 | V        |    30 | 6        |
  |     4 | E        |    13 | N        |    22 | W        |    31 | 7        |
  |     5 | F        |    14 | O        |    23 | X        |       |          |
  |     6 | G        |    15 | P        |    24 | Y        | (pad) | =        |
  |     7 | H        |    16 | Q        |    25 | Z        |       |          |
  |     8 | I        |    17 | R        |    26 | 2        |       |          |


  ## Base 32 (extended hex) alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | 0        |     9 | 9        |    18 | I        |    27 | R        |
  |     1 | 1        |    10 | A        |    19 | J        |    28 | S        |
  |     2 | 2        |    11 | B        |    20 | K        |    29 | T        |
  |     3 | 3        |    12 | C        |    21 | L        |    30 | U        |
  |     4 | 4        |    13 | D        |    22 | M        |    31 | V        |
  |     5 | 5        |    14 | E        |    23 | N        |       |          |
  |     6 | 6        |    15 | F        |    24 | O        | (pad) | =        |
  |     7 | 7        |    16 | G        |    25 | P        |       |          |
  |     8 | 8        |    17 | H        |    26 | Q        |       |          |

  ## Base 64 alphabet

  | Value |  Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:----------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | A         |    17 | R        |    34 | i        |    51 | z        |
  |     1 | B         |    18 | S        |    35 | j        |    52 | 0        |
  |     2 | C         |    19 | T        |    36 | k        |    53 | 1        |
  |     3 | D         |    20 | U        |    37 | l        |    54 | 2        |
  |     4 | E         |    21 | V        |    38 | m        |    55 | 3        |
  |     5 | F         |    22 | W        |    39 | n        |    56 | 4        |
  |     6 | G         |    23 | X        |    40 | o        |    57 | 5        |
  |     7 | H         |    24 | Y        |    41 | p        |    58 | 6        |
  |     8 | I         |    25 | Z        |    42 | q        |    59 | 7        |
  |     9 | J         |    26 | a        |    43 | r        |    60 | 8        |
  |    10 | K         |    27 | b        |    44 | s        |    61 | 9        |
  |    11 | L         |    28 | c        |    45 | t        |    62 | +        |
  |    12 | M         |    29 | d        |    46 | u        |    63 | /        |
  |    13 | N         |    30 | e        |    47 | v        |       |          |
  |    14 | O         |    31 | f        |    48 | w        | (pad) | =        |
  |    15 | P         |    32 | g        |    49 | x        |       |          |
  |    16 | Q         |    33 | h        |    50 | y        |       |          |

  ## Base 64 (URL and filename safe) alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | A        |    17 | R        |    34 | i        |    51 | z        |
  |     1 | B        |    18 | S        |    35 | j        |    52 | 0        |
  |     2 | C        |    19 | T        |    36 | k        |    53 | 1        |
  |     3 | D        |    20 | U        |    37 | l        |    54 | 2        |
  |     4 | E        |    21 | V        |    38 | m        |    55 | 3        |
  |     5 | F        |    22 | W        |    39 | n        |    56 | 4        |
  |     6 | G        |    23 | X        |    40 | o        |    57 | 5        |
  |     7 | H        |    24 | Y        |    41 | p        |    58 | 6        |
  |     8 | I        |    25 | Z        |    42 | q        |    59 | 7        |
  |     9 | J        |    26 | a        |    43 | r        |    60 | 8        |
  |    10 | K        |    27 | b        |    44 | s        |    61 | 9        |
  |    11 | L        |    28 | c        |    45 | t        |    62 | -        |
  |    12 | M        |    29 | d        |    46 | u        |    63 | _        |
  |    13 | N        |    30 | e        |    47 | v        |       |          |
  |    14 | O        |    31 | f        |    48 | w        | (pad) | =        |
  |    15 | P        |    32 | g        |    49 | x        |       |          |
  |    16 | Q        |    33 | h        |    50 | y        |       |          |

  """

  @type encode_case :: :upper | :lower
  @type decode_case :: :upper | :lower | :mixed

  b16_alphabet = ~c"0123456789ABCDEF"
  b64_alphabet = ~c"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  b64url_alphabet = ~c"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  b32_alphabet = ~c"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
  b32hex_alphabet = ~c"0123456789ABCDEFGHIJKLMNOPQRSTUV"

  to_lower_enc = &Enum.map(&1, fn c -> if c in ?A..?Z, do: c - ?A + ?a, else: c end)

  to_mixed_dec =
    &Enum.flat_map(&1, fn {encoding, value} = pair ->
      if encoding in ?A..?Z do
        [pair, {encoding - ?A + ?a, value}]
      else
        [pair]
      end
    end)

  to_lower_dec =
    &Enum.map(&1, fn {encoding, value} = pair ->
      if encoding in ?A..?Z do
        {encoding - ?A + ?a, value}
      else
        pair
      end
    end)

  to_encode_list = fn alphabet ->
    for e1 <- alphabet, e2 <- alphabet, do: bsl(e1, 8) + e2
  end

  to_decode_list = fn alphabet ->
    alphabet = Enum.sort(alphabet)
    map = Map.new(alphabet)
    {min, _} = List.first(alphabet)
    {max, _} = List.last(alphabet)
    {min, Enum.map(min..max, &map[&1])}
  end

  defp bad_character!(byte) do
    raise ArgumentError,
          "non-alphabet character found: #{inspect(<<byte>>, binaries: :as_strings)} (byte #{byte})"
  end

  defp maybe_pad(acc, false, _count), do: acc
  defp maybe_pad(acc, true, 6), do: acc <> "======"
  defp maybe_pad(acc, true, 4), do: acc <> "===="
  defp maybe_pad(acc, true, 3), do: acc <> "==="
  defp maybe_pad(acc, true, 2), do: acc <> "=="
  defp maybe_pad(acc, true, 1), do: acc <> "="

  defp remove_ignored(string, nil), do: string

  defp remove_ignored(string, :whitespace) do
    for <<char::8 <- string>>, char not in ~c"\s\t\r\n", into: <<>>, do: <<char::8>>
  end

  # SWAR (SIMD Within A Register) fast paths for valid16?/2 and valid32?/2
  # (non-hex). Each chunk of 8 bytes is validated in one guard: 7 bytes via
  # bitwise arithmetic on a single 56-bit integer, plus a per-byte range
  # check for the 8th byte. 56 bits is the largest width that fits in a BEAM
  # small int on 64-bit (fixnum range is 59-bit signed); at 64 bits every
  # `w + 0x80..` would allocate a bignum on the heap and the optimisation
  # would collapse. See https://github.com/erlang/otp/pull/10938 for the
  # corresponding pattern in OTP.
  @swar_mask80 0x80808080808080

  # Per-range SWAR constants, broadcast across 7 lanes. Naming convention:
  #   @swar_ge_X = 0x80 - X  → high bit of `(w + @swar_ge_X)` lane is set
  #                            iff that byte is ≥ X
  #   @swar_gt_X = 0x7F - X  → high bit of `(w + @swar_gt_X)` lane is set
  #                            iff that byte is > X
  # A byte is in range [lo, hi] iff
  #   `bxor(w + @swar_ge_lo, w + @swar_gt_hi)` has its high bit set.
  @swar_ge_0 0x50505050505050
  @swar_gt_9 0x46464646464646
  @swar_ge_2 0x4E4E4E4E4E4E4E
  @swar_gt_7 0x48484848484848
  @swar_ge_A 0x3F3F3F3F3F3F3F
  @swar_gt_F 0x39393939393939
  @swar_gt_V 0x29292929292929
  @swar_gt_Z 0x25252525252525
  @swar_ge_a 0x1F1F1F1F1F1F1F
  @swar_gt_f 0x19191919191919
  @swar_gt_v 0x09090909090909
  @swar_gt_z 0x05050505050505

  # For base64 standard, '/' (0x2F) sits exactly one below '0' (0x30), so we
  # extend the digit range to [0x2F, 0x39], which absorbs '/' into one range
  # check — saves one Mycroft singleton. Trick lifted from
  # https://lemire.me/blog/2025/04/13/detect-control-characters-quotes-and-backslashes-efficiently-using-swar/
  @swar_ge_slash 0x51515151515151

  # Mycroft zero-byte detection for base64 singletons (+, -, _).
  # Per lane: high bit set iff `bxor(w, K*ones) - 0x01..01` has its high bit
  # set, i.e. that byte's V value was 0 → original byte was K. Simplified
  # (no `bnot V` term) — for ASCII-gated `w`, borrow-propagation false
  # positives only occur for adjacent bytes that happen to equal `K xor 0x01`,
  # which is outside the base64 alphabet, so it never matters here.
  # Pattern follows https://github.com/elixir-lang/elixir/pull/15255.
  @swar_mask01 0x01010101010101
  @swar_plus_x7 0x2B2B2B2B2B2B2B
  @swar_dash_x7 0x2D2D2D2D2D2D2D
  @swar_under_x7 0x5F5F5F5F5F5F5F

  # Per-byte validity guards (used in both the SWAR clauses for the 8th byte
  # of each stride and in the body of the sub-8-byte tail clauses).
  defguardp valid_char16upper?(c) when c in ?0..?9 or c in ?A..?F
  defguardp valid_char16lower?(c) when c in ?0..?9 or c in ?a..?f
  defguardp valid_char16mixed?(c) when c in ?0..?9 or c in ?A..?F or c in ?a..?f

  defguardp valid_char32upper?(c) when c in ?A..?Z or c in ?2..?7
  defguardp valid_char32lower?(c) when c in ?a..?z or c in ?2..?7
  defguardp valid_char32mixed?(c) when c in ?A..?Z or c in ?a..?z or c in ?2..?7

  # Most common range first — letters dominate (22/32) over digits (10/32)
  # in hex base32, so letters go first in the OR short-circuit.
  defguardp valid_char32hexupper?(c) when c in ?A..?V or c in ?0..?9
  defguardp valid_char32hexlower?(c) when c in ?a..?v or c in ?0..?9
  defguardp valid_char32hexmixed?(c) when c in ?A..?V or c in ?a..?v or c in ?0..?9

  defguardp valid_char64base?(c)
            when c in ?A..?Z or c in ?a..?z or c in ?0..?9 or c == ?+ or c == ?/

  defguardp valid_char64url?(c)
            when c in ?A..?Z or c in ?a..?z or c in ?0..?9 or c == ?- or c == ?_

  # SWAR 7-byte word validity. Structure for each guard:
  #   1. ASCII gate `band(w, MASK80) == 0` — every byte < 0x80 so the
  #      additions below cannot carry across lanes.
  #   2. "Each byte is in range A OR range B (OR range C)" gate — OR per-
  #      range XOR masks (high bit set in lane iff byte in that range), AND
  #      with MASK80, demand all 7 high bits set.
  defguardp valid_word16upper?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bxor(w + @swar_ge_0, w + @swar_gt_9),
                       bxor(w + @swar_ge_A, w + @swar_gt_F)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word16lower?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bxor(w + @swar_ge_0, w + @swar_gt_9),
                       bxor(w + @swar_ge_a, w + @swar_gt_f)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word16mixed?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bor(
                         bxor(w + @swar_ge_0, w + @swar_gt_9),
                         bxor(w + @swar_ge_A, w + @swar_gt_F)
                       ),
                       bxor(w + @swar_ge_a, w + @swar_gt_f)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word32upper?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bxor(w + @swar_ge_A, w + @swar_gt_Z),
                       bxor(w + @swar_ge_2, w + @swar_gt_7)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word32lower?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bxor(w + @swar_ge_a, w + @swar_gt_z),
                       bxor(w + @swar_ge_2, w + @swar_gt_7)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word32mixed?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bor(
                         bxor(w + @swar_ge_A, w + @swar_gt_Z),
                         bxor(w + @swar_ge_a, w + @swar_gt_z)
                       ),
                       bxor(w + @swar_ge_2, w + @swar_gt_7)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word32hexupper?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bxor(w + @swar_ge_0, w + @swar_gt_9),
                       bxor(w + @swar_ge_A, w + @swar_gt_V)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word32hexlower?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bxor(w + @swar_ge_0, w + @swar_gt_9),
                       bxor(w + @swar_ge_a, w + @swar_gt_v)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word32hexmixed?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bor(
                         bxor(w + @swar_ge_0, w + @swar_gt_9),
                         bxor(w + @swar_ge_A, w + @swar_gt_V)
                       ),
                       bxor(w + @swar_ge_a, w + @swar_gt_v)
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  # base64 SWAR word validity: 3 ranges (A-Z, a-z, 0-9) OR'd with singletons.
  # For base, the digit range is extended to [0x2F, 0x39] to absorb '/' as
  # part of one range (Lemire merge), leaving only '+' as a Mycroft singleton.
  # For url, the singletons '-' and '_' are detected via two Mycroft terms.
  defguardp valid_word64base?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bor(
                         bor(
                           bxor(w + @swar_ge_A, w + @swar_gt_Z),
                           bxor(w + @swar_ge_a, w + @swar_gt_z)
                         ),
                         bxor(w + @swar_ge_slash, w + @swar_gt_9)
                       ),
                       bxor(w, @swar_plus_x7) - @swar_mask01
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  defguardp valid_word64url?(w)
            when band(w, @swar_mask80) == 0 and
                   band(
                     bor(
                       bor(
                         bor(
                           bxor(w + @swar_ge_A, w + @swar_gt_Z),
                           bxor(w + @swar_ge_a, w + @swar_gt_z)
                         ),
                         bxor(w + @swar_ge_0, w + @swar_gt_9)
                       ),
                       bor(
                         bxor(w, @swar_dash_x7) - @swar_mask01,
                         bxor(w, @swar_under_x7) - @swar_mask01
                       )
                     ),
                     @swar_mask80
                   ) == @swar_mask80

  @doc """
  Encodes a binary string into a base 16 encoded string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to use when encoding

  The values for `:case` can be:

    * `:upper` - uses upper case characters (default)
    * `:lower` - uses lower case characters

  ## Examples

      iex> Base.encode16("foobar")
      "666F6F626172"

      iex> Base.encode16("foobar", case: :lower)
      "666f6f626172"

  """
  @spec encode16(binary, case: encode_case) :: binary
  def encode16(data, opts \\ []) when is_binary(data) do
    case Keyword.get(opts, :case, :upper) do
      :upper -> encode16upper(data, "")
      :lower -> encode16lower(data, "")
    end
  end

  for {base, alphabet} <- [upper: b16_alphabet, lower: to_lower_enc.(b16_alphabet)] do
    name = :"encode16#{base}"
    encoded = to_encode_list.(alphabet)

    @compile {:inline, [{name, 1}]}
    defp unquote(name)(byte) do
      elem({unquote_splicing(encoded)}, byte)
    end

    defp unquote(name)(<<c1, c2, c3, c4, c5, c6, c7, c8, rest::binary>>, acc) do
      unquote(name)(
        rest,
        <<
          acc::binary,
          unquote(name)(c1)::16,
          unquote(name)(c2)::16,
          unquote(name)(c3)::16,
          unquote(name)(c4)::16,
          unquote(name)(c5)::16,
          unquote(name)(c6)::16,
          unquote(name)(c7)::16,
          unquote(name)(c8)::16
        >>
      )
    end

    defp unquote(name)(<<c1, c2, c3, c4, rest::binary>>, acc) do
      unquote(name)(
        rest,
        <<
          acc::binary,
          unquote(name)(c1)::16,
          unquote(name)(c2)::16,
          unquote(name)(c3)::16,
          unquote(name)(c4)::16
        >>
      )
    end

    defp unquote(name)(<<c1, c2, rest::binary>>, acc) do
      unquote(name)(rest, <<acc::binary, unquote(name)(c1)::16, unquote(name)(c2)::16>>)
    end

    defp unquote(name)(<<c1, rest::binary>>, acc) do
      unquote(name)(rest, <<acc::binary, unquote(name)(c1)::16>>)
    end

    defp unquote(name)(<<>>, acc) do
      acc
    end
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding

  The values for `:case` can be:

    * `:upper` - only allows upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  ## Examples

      iex> Base.decode16("666F6F626172")
      {:ok, "foobar"}

      iex> Base.decode16("666f6f626172", case: :lower)
      {:ok, "foobar"}

      iex> Base.decode16("666f6F626172", case: :mixed)
      {:ok, "foobar"}

  """
  @spec decode16(binary, case: decode_case) :: {:ok, binary} | :error
  def decode16(string, opts \\ []) do
    {:ok, decode16!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding

  The values for `:case` can be:

    * `:upper` - only allows upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode16!("666F6F626172")
      "foobar"

      iex> Base.decode16!("666f6f626172", case: :lower)
      "foobar"

      iex> Base.decode16!("666f6F626172", case: :mixed)
      "foobar"

  """
  @spec decode16!(binary, case: decode_case) :: binary
  def decode16!(string, opts \\ [])

  def decode16!(string, opts) when is_binary(string) and rem(byte_size(string), 2) == 0 do
    case Keyword.get(opts, :case, :upper) do
      :upper -> decode16upper!(string, "")
      :lower -> decode16lower!(string, "")
      :mixed -> decode16mixed!(string, "")
    end
  end

  def decode16!(string, _opts) when is_binary(string) do
    raise ArgumentError,
          "string given to decode has wrong length. An even number of bytes was expected, got: #{byte_size(string)}. " <>
            "Double check your string for unwanted characters or pad it accordingly"
  end

  @doc """
  Checks if a string is a valid base 16 encoded string.

  > #### When to use this {: .tip}
  >
  > Use this function when you just need to *validate* that a string is
  > valid base 16 data, without actually producing a decoded output string.
  > This function is both more performant and memory efficient than using
  > `decode16/2`, checking that the result is `{:ok, ...}`, and then
  > discarding the decoded binary.

  ## Options

  Accepts the same options as `decode16/2`.

  ## Examples

      iex> Base.valid16?("666F6F626172")
      true

      iex> Base.valid16?("666f6f626172", case: :lower)
      true

      iex> Base.valid16?("666f6F626172", case: :mixed)
      true

      iex> Base.valid16?("ff", case: :upper)
      false

  """
  @doc since: "1.19.0"
  @spec valid16?(binary, case: decode_case) :: boolean
  def valid16?(string, opts \\ [])

  def valid16?(string, opts) when is_binary(string) and rem(byte_size(string), 2) == 0 do
    case Keyword.get(opts, :case, :upper) do
      :upper -> validate16upper?(string)
      :lower -> validate16lower?(string)
      :mixed -> validate16mixed?(string)
    end
  end

  def valid16?(string, _opts) when is_binary(string) do
    false
  end

  upper = Enum.with_index(b16_alphabet)

  for {base, alphabet} <- [upper: upper, lower: to_lower_dec.(upper), mixed: to_mixed_dec.(upper)] do
    decode_name = :"decode16#{base}!"
    validate_name = :"validate16#{base}?"
    valid_char_name = :"valid_char16#{base}?"
    valid_word_name = :"valid_word16#{base}?"

    {min, decoded} = to_decode_list.(alphabet)

    # SWAR fast path: 7 bytes per stride, validated entirely via
    # `valid_word16<base>?` in the body. The `and` short-circuits when SWAR
    # fails on any byte. Tail bytes (1-6 leftover) recurse through the
    # single-byte clause below.
    defp unquote(validate_name)(<<w::56, rest::binary>>),
      do: unquote(valid_word_name)(w) and unquote(validate_name)(rest)

    defp unquote(validate_name)(<<>>), do: true

    defp unquote(validate_name)(<<char, rest::binary>>),
      do: unquote(valid_char_name)(char) and unquote(validate_name)(rest)

    defp unquote(decode_name)(char) do
      index = char - unquote(min)

      cond do
        index not in 0..unquote(length(decoded) - 1) -> bad_character!(char)
        new_char = elem({unquote_splicing(decoded)}, index) -> new_char
        true -> bad_character!(char)
      end
    end

    defp unquote(decode_name)(<<c1, c2, c3, c4, c5, c6, c7, c8, rest::binary>>, acc) do
      unquote(decode_name)(
        rest,
        <<
          acc::binary,
          unquote(decode_name)(c1)::4,
          unquote(decode_name)(c2)::4,
          unquote(decode_name)(c3)::4,
          unquote(decode_name)(c4)::4,
          unquote(decode_name)(c5)::4,
          unquote(decode_name)(c6)::4,
          unquote(decode_name)(c7)::4,
          unquote(decode_name)(c8)::4
        >>
      )
    end

    defp unquote(decode_name)(<<c1, c2, c3, c4, rest::binary>>, acc) do
      unquote(decode_name)(
        rest,
        <<
          acc::binary,
          unquote(decode_name)(c1)::4,
          unquote(decode_name)(c2)::4,
          unquote(decode_name)(c3)::4,
          unquote(decode_name)(c4)::4
        >>
      )
    end

    defp unquote(decode_name)(<<c1::8, c2::8, rest::binary>>, acc) do
      unquote(decode_name)(
        rest,
        <<acc::binary, unquote(decode_name)(c1)::4, unquote(decode_name)(c2)::4>>
      )
    end

    defp unquote(decode_name)(<<>>, acc) do
      acc
    end
  end

  @doc """
  Encodes a binary string into a base 64 encoded string.

  Accepts `padding: false` option which will omit padding from
  the output string.

  ## Examples

      iex> Base.encode64("foobar")
      "Zm9vYmFy"

      iex> Base.encode64("foob")
      "Zm9vYg=="

      iex> Base.encode64("foob", padding: false)
      "Zm9vYg"

  """
  @spec encode64(binary, padding: boolean) :: binary
  def encode64(data, opts \\ []) when is_binary(data) do
    pad? = Keyword.get(opts, :padding, true)
    encode64base(data, "", pad?)
  end

  @doc """
  Encodes a binary string into a base 64 encoded string with URL and filename
  safe alphabet.

  Accepts `padding: false` option which will omit padding from
  the output string.

  ## Examples

      iex> Base.url_encode64(<<255, 127, 254, 252>>)
      "_3_-_A=="

      iex> Base.url_encode64(<<255, 127, 254, 252>>, padding: false)
      "_3_-_A"

  """
  @spec url_encode64(binary, padding: boolean) :: binary
  def url_encode64(data, opts \\ []) when is_binary(data) do
    pad? = Keyword.get(opts, :padding, true)
    encode64url(data, "", pad?)
  end

  for {base, alphabet} <- [base: b64_alphabet, url: b64url_alphabet] do
    name = :"encode64#{base}"
    encoded = to_encode_list.(alphabet)

    @compile {:inline, [{name, 1}]}
    defp unquote(name)(byte) do
      elem({unquote_splicing(encoded)}, byte)
    end

    defp unquote(name)(<<c1::12, c2::12, c3::12, c4::12, rest::binary>>, acc, pad?) do
      unquote(name)(
        rest,
        <<
          acc::binary,
          unquote(name)(c1)::16,
          unquote(name)(c2)::16,
          unquote(name)(c3)::16,
          unquote(name)(c4)::16
        >>,
        pad?
      )
    end

    defp unquote(name)(<<c1::12, c2::12, c3::12, c4::4>>, acc, pad?) do
      <<
        acc::binary,
        unquote(name)(c1)::16,
        unquote(name)(c2)::16,
        unquote(name)(c3)::16,
        c4 |> bsl(2) |> unquote(name)() |> band(0x00FF)::8
      >>
      |> maybe_pad(pad?, 1)
    end

    defp unquote(name)(<<c1::12, c2::12, c3::8>>, acc, pad?) do
      <<
        acc::binary,
        unquote(name)(c1)::16,
        unquote(name)(c2)::16,
        c3 |> bsl(4) |> unquote(name)()::16
      >>
      |> maybe_pad(pad?, 2)
    end

    defp unquote(name)(<<c1::12, c2::12>>, acc, _pad?) do
      <<
        acc::binary,
        unquote(name)(c1)::16,
        unquote(name)(c2)::16
      >>
    end

    defp unquote(name)(<<c1::12, c2::4>>, acc, pad?) do
      <<
        acc::binary,
        unquote(name)(c1)::16,
        c2 |> bsl(2) |> unquote(name)() |> band(0x00FF)::8
      >>
      |> maybe_pad(pad?, 1)
    end

    defp unquote(name)(<<c1::8>>, acc, pad?) do
      <<
        acc::binary,
        c1 |> bsl(4) |> unquote(name)()::16
      >>
      |> maybe_pad(pad?, 2)
    end

    defp unquote(name)(<<>>, acc, _pad?) do
      acc
    end
  end

  @doc """
  Decodes a base 64 encoded string into a binary string.

  Accepts `ignore: :whitespace` option which will ignore all the
  whitespace characters in the input string.

  Accepts `padding: false` option which will ignore padding from
  the input string.

  ## Examples

      iex> Base.decode64("Zm9vYmFy")
      {:ok, "foobar"}

      iex> Base.decode64("Zm9vYmFy\\n", ignore: :whitespace)
      {:ok, "foobar"}

      iex> Base.decode64("Zm9vYg==")
      {:ok, "foob"}

      iex> Base.decode64("Zm9vYg", padding: false)
      {:ok, "foob"}

  """
  @spec decode64(binary, ignore: :whitespace, padding: boolean) :: {:ok, binary} | :error
  def decode64(string, opts \\ []) when is_binary(string) do
    {:ok, decode64!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 64 encoded string into a binary string.

  Accepts `ignore: :whitespace` option which will ignore all the
  whitespace characters in the input string.

  Accepts `padding: false` option which will ignore padding from
  the input string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode64!("Zm9vYmFy")
      "foobar"

      iex> Base.decode64!("Zm9vYmFy\\n", ignore: :whitespace)
      "foobar"

      iex> Base.decode64!("Zm9vYg==")
      "foob"

      iex> Base.decode64!("Zm9vYg", padding: false)
      "foob"

  """
  @spec decode64!(binary, ignore: :whitespace, padding: boolean) :: binary
  def decode64!(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)
    string |> remove_ignored(opts[:ignore]) |> decode64base!(pad?)
  end

  @doc """
  Validates a base 64 encoded string.

  > #### When to use this {: .tip}
  >
  > Use this function when you just need to *validate* that a string is
  > valid base 64 data, without actually producing a decoded output string.
  > This function is both more performant and memory efficient than using
  > `decode64/2`, checking that the result is `{:ok, ...}`, and then
  > discarding the decoded binary.

  ## Options

  Accepts the same options as `decode64/2`.

  ## Examples

      iex> Base.valid64?("Zm9vYmFy")
      true

      iex> Base.valid64?("Zm9vYmFy\\n", ignore: :whitespace)
      true

      iex> Base.valid64?("Zm9vYg==")
      true

  """
  @doc since: "1.19.0"
  @spec valid64?(binary, ignore: :whitespace, padding: boolean) :: boolean
  def valid64?(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)
    string |> remove_ignored(opts[:ignore]) |> validate64base?(pad?)
  end

  @doc """
  Decodes a base 64 encoded string with URL and filename safe alphabet
  into a binary string.

  Accepts `ignore: :whitespace` option which will ignore all the
  whitespace characters in the input string.

  Accepts `padding: false` option which will ignore padding from
  the input string.

  ## Examples

      iex> Base.url_decode64("_3_-_A==")
      {:ok, <<255, 127, 254, 252>>}

      iex> Base.url_decode64("_3_-_A==\\n", ignore: :whitespace)
      {:ok, <<255, 127, 254, 252>>}

      iex> Base.url_decode64("_3_-_A", padding: false)
      {:ok, <<255, 127, 254, 252>>}

  """
  @spec url_decode64(binary, ignore: :whitespace, padding: boolean) :: {:ok, binary} | :error
  def url_decode64(string, opts \\ []) when is_binary(string) do
    {:ok, url_decode64!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 64 encoded string with URL and filename safe alphabet
  into a binary string.

  Accepts `ignore: :whitespace` option which will ignore all the
  whitespace characters in the input string.

  Accepts `padding: false` option which will ignore padding from
  the input string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.url_decode64!("_3_-_A==")
      <<255, 127, 254, 252>>

      iex> Base.url_decode64!("_3_-_A==\\n", ignore: :whitespace)
      <<255, 127, 254, 252>>

      iex> Base.url_decode64!("_3_-_A", padding: false)
      <<255, 127, 254, 252>>

  """
  @spec url_decode64!(binary, ignore: :whitespace, padding: boolean) :: binary
  def url_decode64!(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)
    string |> remove_ignored(opts[:ignore]) |> decode64url!(pad?)
  end

  @doc """
  Validates a base 64 encoded string with URL and filename safe alphabet.

  > #### When to use this {: .tip}
  >
  > Use this function when you just need to *validate* that a string is
  > valid (URL-safe) base 64 data, without actually producing a decoded
  > output string. This function is both more performant and memory efficient
  > than using `url_decode64/2`, checking that the result is `{:ok, ...}`,
  > and then discarding the decoded binary.

  ## Options

  Accepts the same options as `url_decode64/2`.

  ## Examples

      iex> Base.url_valid64?("_3_-_A==")
      true

      iex> Base.url_valid64?("_3_-_A==\\n", ignore: :whitespace)
      true

      iex> Base.url_valid64?("_3_-_A", padding: false)
      true

  """
  @doc since: "1.19.0"
  @spec url_valid64?(binary, ignore: :whitespace, padding: boolean) :: boolean
  def url_valid64?(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)
    string |> remove_ignored(opts[:ignore]) |> validate64url?(pad?)
  end

  for {base, alphabet} <- [base: b64_alphabet, url: b64url_alphabet] do
    decode_name = :"decode64#{base}!"

    validate_name = :"validate64#{base}?"
    validate_main_name = :"validate_main64#{validate_name}?"
    valid_char_name = :"valid_char64#{base}?"
    valid_word_name = :"valid_word64#{base}?"
    {min, decoded} = alphabet |> Enum.with_index() |> to_decode_list.()

    # SWAR fast path: 7 bytes per stride, validated via `valid_word64<base>?`
    # in the body. Tail leftover (1-6 bytes after a 7-byte stride hits an
    # 8-byte-multiple `main`) recurses through the single-byte clause.
    defp unquote(validate_main_name)(<<w::56, rest::binary>>),
      do: unquote(valid_word_name)(w) and unquote(validate_main_name)(rest)

    defp unquote(validate_main_name)(<<>>), do: true

    defp unquote(validate_main_name)(<<char, rest::binary>>),
      do: unquote(valid_char_name)(char) and unquote(validate_main_name)(rest)

    defp unquote(validate_name)(<<>>, _pad?), do: true

    defp unquote(validate_name)(string, pad?) do
      segs = div(byte_size(string) + 7, 8) - 1
      <<main::size(^segs)-binary-unit(64), rest::binary>> = string
      main_valid? = unquote(validate_main_name)(main)

      case rest do
        _ when not main_valid? ->
          false

        <<c1::8, c2::8, ?=, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2)

        <<c1::8, c2::8, c3::8, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3)

        <<c1::8, c2::8, c3::8, c4::8>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, ?=, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6) and
            unquote(valid_char_name)(c7)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6) and
            unquote(valid_char_name)(c7) and
            unquote(valid_char_name)(c8)

        <<c1::8, c2::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2)

        <<c1::8, c2::8, c3::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6) and
            unquote(valid_char_name)(c7)

        _ ->
          false
      end
    end

    defp unquote(decode_name)(char) do
      index = char - unquote(min)

      cond do
        index not in 0..unquote(length(decoded) - 1) -> bad_character!(char)
        new_char = elem({unquote_splicing(decoded)}, index) -> new_char
        true -> bad_character!(char)
      end
    end

    defp unquote(decode_name)(<<>>, _pad?), do: <<>>

    defp unquote(decode_name)(string, pad?) do
      segs = div(byte_size(string) + 7, 8) - 1
      <<main::size(^segs)-binary-unit(64), rest::binary>> = string

      main =
        for <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8 <- main>>, into: <<>> do
          <<
            unquote(decode_name)(c1)::6,
            unquote(decode_name)(c2)::6,
            unquote(decode_name)(c3)::6,
            unquote(decode_name)(c4)::6,
            unquote(decode_name)(c5)::6,
            unquote(decode_name)(c6)::6,
            unquote(decode_name)(c7)::6,
            unquote(decode_name)(c8)::6
          >>
        end

      case rest do
        <<c1::8, c2::8, ?=, ?=>> ->
          <<main::bits, unquote(decode_name)(c1)::6, bsr(unquote(decode_name)(c2), 4)::2>>

        <<c1::8, c2::8, c3::8, ?=>> ->
          <<main::bits, unquote(decode_name)(c1)::6, unquote(decode_name)(c2)::6,
            bsr(unquote(decode_name)(c3), 2)::4>>

        <<c1::8, c2::8, c3::8, c4::8>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::6,
            unquote(decode_name)(c2)::6,
            unquote(decode_name)(c3)::6,
            unquote(decode_name)(c4)::6
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, ?=, ?=>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::6,
            unquote(decode_name)(c2)::6,
            unquote(decode_name)(c3)::6,
            unquote(decode_name)(c4)::6,
            unquote(decode_name)(c5)::6,
            bsr(unquote(decode_name)(c6), 4)::2
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, ?=>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::6,
            unquote(decode_name)(c2)::6,
            unquote(decode_name)(c3)::6,
            unquote(decode_name)(c4)::6,
            unquote(decode_name)(c5)::6,
            unquote(decode_name)(c6)::6,
            bsr(unquote(decode_name)(c7), 2)::4
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::6,
            unquote(decode_name)(c2)::6,
            unquote(decode_name)(c3)::6,
            unquote(decode_name)(c4)::6,
            unquote(decode_name)(c5)::6,
            unquote(decode_name)(c6)::6,
            unquote(decode_name)(c7)::6,
            unquote(decode_name)(c8)::6
          >>

        <<c1::8, c2::8>> when not pad? ->
          <<main::bits, unquote(decode_name)(c1)::6, bsr(unquote(decode_name)(c2), 4)::2>>

        <<c1::8, c2::8, c3::8>> when not pad? ->
          <<main::bits, unquote(decode_name)(c1)::6, unquote(decode_name)(c2)::6,
            bsr(unquote(decode_name)(c3), 2)::4>>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8>> when not pad? ->
          <<
            main::bits,
            unquote(decode_name)(c1)::6,
            unquote(decode_name)(c2)::6,
            unquote(decode_name)(c3)::6,
            unquote(decode_name)(c4)::6,
            unquote(decode_name)(c5)::6,
            bsr(unquote(decode_name)(c6), 4)::2
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8>> when not pad? ->
          <<
            main::bits,
            unquote(decode_name)(c1)::6,
            unquote(decode_name)(c2)::6,
            unquote(decode_name)(c3)::6,
            unquote(decode_name)(c4)::6,
            unquote(decode_name)(c5)::6,
            unquote(decode_name)(c6)::6,
            bsr(unquote(decode_name)(c7), 2)::4
          >>

        _ ->
          raise ArgumentError, "incorrect padding"
      end
    end
  end

  @doc """
  Encodes a binary string into a base 32 encoded string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to use when encoding
    * `:padding` - specifies whether to apply padding

  The values for `:case` can be:

    * `:upper` - uses upper case characters (default)
    * `:lower` - uses lower case characters

  The values for `:padding` can be:

    * `true` - pad the output string to the nearest multiple of 8 (default)
    * `false` - omit padding from the output string

  ## Examples

      iex> Base.encode32("foobar")
      "MZXW6YTBOI======"

      iex> Base.encode32("foobar", case: :lower)
      "mzxw6ytboi======"

      iex> Base.encode32("foobar", padding: false)
      "MZXW6YTBOI"

  """
  @spec encode32(binary, case: encode_case, padding: boolean) :: binary
  def encode32(data, opts \\ []) when is_binary(data) do
    pad? = Keyword.get(opts, :padding, true)

    case Keyword.get(opts, :case, :upper) do
      :upper -> encode32upper(data, "", pad?)
      :lower -> encode32lower(data, "", pad?)
    end
  end

  @doc """
  Encodes a binary string into a base 32 encoded string with an
  extended hexadecimal alphabet.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to use when encoding
    * `:padding` - specifies whether to apply padding

  The values for `:case` can be:

    * `:upper` - uses upper case characters (default)
    * `:lower` - uses lower case characters

  The values for `:padding` can be:

    * `true` - pad the output string to the nearest multiple of 8 (default)
    * `false` - omit padding from the output string

  ## Examples

      iex> Base.hex_encode32("foobar")
      "CPNMUOJ1E8======"

      iex> Base.hex_encode32("foobar", case: :lower)
      "cpnmuoj1e8======"

      iex> Base.hex_encode32("foobar", padding: false)
      "CPNMUOJ1E8"

  """
  @spec hex_encode32(binary, case: encode_case, padding: boolean) :: binary
  def hex_encode32(data, opts \\ []) when is_binary(data) do
    pad? = Keyword.get(opts, :padding, true)

    case Keyword.get(opts, :case, :upper) do
      :upper -> encode32hexupper(data, "", pad?)
      :lower -> encode32hexlower(data, "", pad?)
    end
  end

  for {base, alphabet} <- [
        upper: b32_alphabet,
        lower: to_lower_enc.(b32_alphabet),
        hexupper: b32hex_alphabet,
        hexlower: to_lower_enc.(b32hex_alphabet)
      ] do
    name = :"encode32#{base}"
    encoded = to_encode_list.(alphabet)

    @compile {:inline, [{name, 1}]}
    defp unquote(name)(byte) do
      elem({unquote_splicing(encoded)}, byte)
    end

    defp unquote(name)(<<c1::10, c2::10, c3::10, c4::10, rest::binary>>, acc, pad?) do
      unquote(name)(
        rest,
        <<
          acc::binary,
          unquote(name)(c1)::16,
          unquote(name)(c2)::16,
          unquote(name)(c3)::16,
          unquote(name)(c4)::16
        >>,
        pad?
      )
    end

    defp unquote(name)(<<c1::10, c2::10, c3::10, c4::2>>, acc, pad?) do
      <<
        acc::binary,
        unquote(name)(c1)::16,
        unquote(name)(c2)::16,
        unquote(name)(c3)::16,
        c4 |> bsl(3) |> unquote(name)() |> band(0x00FF)::8
      >>
      |> maybe_pad(pad?, 1)
    end

    defp unquote(name)(<<c1::10, c2::10, c3::4>>, acc, pad?) do
      <<
        acc::binary,
        unquote(name)(c1)::16,
        unquote(name)(c2)::16,
        c3 |> bsl(1) |> unquote(name)() |> band(0x00FF)::8
      >>
      |> maybe_pad(pad?, 3)
    end

    defp unquote(name)(<<c1::10, c2::6>>, acc, pad?) do
      <<
        acc::binary,
        unquote(name)(c1)::16,
        c2 |> bsl(4) |> unquote(name)()::16
      >>
      |> maybe_pad(pad?, 4)
    end

    defp unquote(name)(<<c1::8>>, acc, pad?) do
      <<acc::binary, c1 |> bsl(2) |> unquote(name)()::16>>
      |> maybe_pad(pad?, 6)
    end

    defp unquote(name)(<<>>, acc, _pad?) do
      acc
    end
  end

  @doc """
  Decodes a base 32 encoded string into a binary string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding
    * `:padding` - specifies whether to require padding

  The values for `:case` can be:

    * `:upper` - only allows  upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  The values for `:padding` can be:

    * `true` - requires the input string to be padded to the nearest multiple of 8 (default)
    * `false` - ignores padding from the input string

  ## Examples

      iex> Base.decode32("MZXW6YTBOI======")
      {:ok, "foobar"}

      iex> Base.decode32("mzxw6ytboi======", case: :lower)
      {:ok, "foobar"}

      iex> Base.decode32("mzXW6ytBOi======", case: :mixed)
      {:ok, "foobar"}

      iex> Base.decode32("MZXW6YTBOI", padding: false)
      {:ok, "foobar"}

  """
  @spec decode32(binary, case: decode_case, padding: boolean) :: {:ok, binary} | :error
  def decode32(string, opts \\ []) do
    {:ok, decode32!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 32 encoded string into a binary string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding
    * `:padding` - specifies whether to require padding

  The values for `:case` can be:

    * `:upper` - only allows upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  The values for `:padding` can be:

    * `true` - requires the input string to be padded to the nearest multiple of 8 (default)
    * `false` - ignores padding from the input string

  ## Examples

      iex> Base.decode32!("MZXW6YTBOI======")
      "foobar"

      iex> Base.decode32!("mzxw6ytboi======", case: :lower)
      "foobar"

      iex> Base.decode32!("mzXW6ytBOi======", case: :mixed)
      "foobar"

      iex> Base.decode32!("MZXW6YTBOI", padding: false)
      "foobar"

  """
  @spec decode32!(binary, case: decode_case, padding: boolean) :: binary
  def decode32!(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)

    case Keyword.get(opts, :case, :upper) do
      :upper -> decode32upper!(string, pad?)
      :lower -> decode32lower!(string, pad?)
      :mixed -> decode32mixed!(string, pad?)
    end
  end

  @doc """
  Checks if a base 32 encoded string is valid.

  > #### When to use this {: .tip}
  >
  > Use this function when you just need to *validate* that a string is
  > valid base 32 data, without actually producing a decoded output string.
  > This function is both more performant and memory efficient than using
  > `decode32/2`, checking that the result is `{:ok, ...}`, and then
  > discarding the decoded binary.

  ## Options

  Accepts the same options as `decode32/2`.

  ## Examples

      iex> Base.valid32?("MZXW6YTBOI======")
      true

      iex> Base.valid32?("mzxw6ytboi======", case: :lower)
      true

      iex> Base.valid32?("zzz")
      false

  """
  @doc since: "1.19.0"
  @spec valid32?(binary, case: decode_case, padding: boolean) :: boolean()
  def valid32?(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)

    case Keyword.get(opts, :case, :upper) do
      :upper -> validate32upper?(string, pad?)
      :lower -> validate32lower?(string, pad?)
      :mixed -> validate32mixed?(string, pad?)
    end
  end

  @doc """
  Decodes a base 32 encoded string with extended hexadecimal alphabet
  into a binary string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding
    * `:padding` - specifies whether to require padding

  The values for `:case` can be:

    * `:upper` - only allows upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  The values for `:padding` can be:

    * `true` - requires the input string to be padded to the nearest multiple of 8 (default)
    * `false` - ignores padding from the input string

  ## Examples

      iex> Base.hex_decode32("CPNMUOJ1E8======")
      {:ok, "foobar"}

      iex> Base.hex_decode32("cpnmuoj1e8======", case: :lower)
      {:ok, "foobar"}

      iex> Base.hex_decode32("cpnMuOJ1E8======", case: :mixed)
      {:ok, "foobar"}

      iex> Base.hex_decode32("CPNMUOJ1E8", padding: false)
      {:ok, "foobar"}

  """
  @spec hex_decode32(binary, case: decode_case, padding: boolean) :: {:ok, binary} | :error
  def hex_decode32(string, opts \\ []) do
    {:ok, hex_decode32!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 32 encoded string with extended hexadecimal alphabet
  into a binary string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding
    * `:padding` - specifies whether to require padding

  The values for `:case` can be:

    * `:upper` - only allows upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  The values for `:padding` can be:

    * `true` - requires the input string to be padded to the nearest multiple of 8 (default)
    * `false` - ignores padding from the input string

  ## Examples

      iex> Base.hex_decode32!("CPNMUOJ1E8======")
      "foobar"

      iex> Base.hex_decode32!("cpnmuoj1e8======", case: :lower)
      "foobar"

      iex> Base.hex_decode32!("cpnMuOJ1E8======", case: :mixed)
      "foobar"

      iex> Base.hex_decode32!("CPNMUOJ1E8", padding: false)
      "foobar"

  """
  @spec hex_decode32!(binary, case: decode_case, padding: boolean) :: binary
  def hex_decode32!(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)

    case Keyword.get(opts, :case, :upper) do
      :upper -> decode32hexupper!(string, pad?)
      :lower -> decode32hexlower!(string, pad?)
      :mixed -> decode32hexmixed!(string, pad?)
    end
  end

  @doc """
  Checks if a base 32 encoded string with extended hexadecimal alphabet is valid.

  > #### When to use this {: .tip}
  >
  > Use this function when you just need to *validate* that a string is
  > valid (extended hexadecimal) base 32 data, without actually producing
  > a decoded output string. This function is both more performant and
  > memory efficient than using `hex_decode32/2`, checking that the result
  > is `{:ok, ...}`, and then discarding the decoded binary.

  ## Options

  Accepts the same options as `hex_decode32/2`.

  ## Examples

      iex> Base.hex_valid32?("CPNMUOJ1E8======")
      true

      iex> Base.hex_valid32?("cpnmuoj1e8======", case: :lower)
      true

      iex> Base.hex_valid32?("zzz", padding: false)
      false

  """
  @doc since: "1.19.0"
  @spec hex_valid32?(binary, case: decode_case, padding: boolean) :: boolean
  def hex_valid32?(string, opts \\ []) when is_binary(string) do
    pad? = Keyword.get(opts, :padding, true)

    case Keyword.get(opts, :case, :upper) do
      :upper -> validate32hexupper?(string, pad?)
      :lower -> validate32hexlower?(string, pad?)
      :mixed -> validate32hexmixed?(string, pad?)
    end
  end

  upper = Enum.with_index(b32_alphabet)
  hexupper = Enum.with_index(b32hex_alphabet)

  for {base, alphabet} <- [
        upper: upper,
        lower: to_lower_dec.(upper),
        mixed: to_mixed_dec.(upper),
        hexupper: hexupper,
        hexlower: to_lower_dec.(hexupper),
        hexmixed: to_mixed_dec.(hexupper)
      ] do
    decode_name = :"decode32#{base}!"
    validate_name = :"validate32#{base}?"
    validate_main_name = :"validate_main32#{validate_name}?"
    valid_char_name = :"valid_char32#{base}?"
    {min, decoded} = to_decode_list.(alphabet)

    # SWAR fast path: 7 bytes per stride, validated via `valid_word32<base>?`
    # in the body. Tail leftover (1-6 bytes after a 7-byte stride hits an
    # 8-byte-multiple `main`) recurses through the single-byte clause.
    valid_word_name = :"valid_word32#{base}?"

    defp unquote(validate_main_name)(<<w::56, rest::binary>>),
      do: unquote(valid_word_name)(w) and unquote(validate_main_name)(rest)

    defp unquote(validate_main_name)(<<>>), do: true

    defp unquote(validate_main_name)(<<char, rest::binary>>),
      do: unquote(valid_char_name)(char) and unquote(validate_main_name)(rest)

    defp unquote(validate_name)(<<>>, _pad?), do: true

    defp unquote(validate_name)(string, pad?) do
      segs = div(byte_size(string) + 7, 8) - 1
      <<main::size(^segs)-binary-unit(64), rest::binary>> = string
      main_valid? = unquote(validate_main_name)(main)

      case rest do
        _ when not main_valid? ->
          false

        <<c1::8, c2::8, ?=, ?=, ?=, ?=, ?=, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2)

        <<c1::8, c2::8, c3::8, c4::8, ?=, ?=, ?=, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, ?=, ?=, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, ?=>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6) and
            unquote(valid_char_name)(c7)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8>> ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6) and
            unquote(valid_char_name)(c7) and
            unquote(valid_char_name)(c8)

        <<c1::8, c2::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2)

        <<c1::8, c2::8, c3::8, c4::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4)

        <<c1::8, c2::8, c3::8, c4::8, c5::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5)

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8>> when not pad? ->
          unquote(valid_char_name)(c1) and
            unquote(valid_char_name)(c2) and
            unquote(valid_char_name)(c3) and
            unquote(valid_char_name)(c4) and
            unquote(valid_char_name)(c5) and
            unquote(valid_char_name)(c6) and
            unquote(valid_char_name)(c7)

        _ ->
          false
      end
    end

    defp unquote(decode_name)(char) do
      index = char - unquote(min)

      cond do
        index not in 0..unquote(length(decoded) - 1) -> bad_character!(char)
        new_char = elem({unquote_splicing(decoded)}, index) -> new_char
        true -> bad_character!(char)
      end
    end

    defp unquote(decode_name)(<<>>, _), do: <<>>

    defp unquote(decode_name)(string, pad?) do
      segs = div(byte_size(string) + 7, 8) - 1
      <<main::size(^segs)-binary-unit(64), rest::binary>> = string

      main =
        for <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8 <- main>>, into: <<>> do
          <<
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            unquote(decode_name)(c4)::5,
            unquote(decode_name)(c5)::5,
            unquote(decode_name)(c6)::5,
            unquote(decode_name)(c7)::5,
            unquote(decode_name)(c8)::5
          >>
        end

      case rest do
        <<c1::8, c2::8, ?=, ?=, ?=, ?=, ?=, ?=>> ->
          <<main::bits, unquote(decode_name)(c1)::5, bsr(unquote(decode_name)(c2), 2)::3>>

        <<c1::8, c2::8, c3::8, c4::8, ?=, ?=, ?=, ?=>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            bsr(unquote(decode_name)(c4), 4)::1
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, ?=, ?=, ?=>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            unquote(decode_name)(c4)::5,
            bsr(unquote(decode_name)(c5), 1)::4
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, ?=>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            unquote(decode_name)(c4)::5,
            unquote(decode_name)(c5)::5,
            unquote(decode_name)(c6)::5,
            bsr(unquote(decode_name)(c7), 3)::2
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8>> ->
          <<
            main::bits,
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            unquote(decode_name)(c4)::5,
            unquote(decode_name)(c5)::5,
            unquote(decode_name)(c6)::5,
            unquote(decode_name)(c7)::5,
            unquote(decode_name)(c8)::5
          >>

        <<c1::8, c2::8>> when not pad? ->
          <<main::bits, unquote(decode_name)(c1)::5, bsr(unquote(decode_name)(c2), 2)::3>>

        <<c1::8, c2::8, c3::8, c4::8>> when not pad? ->
          <<
            main::bits,
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            bsr(unquote(decode_name)(c4), 4)::1
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8>> when not pad? ->
          <<
            main::bits,
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            unquote(decode_name)(c4)::5,
            bsr(unquote(decode_name)(c5), 1)::4
          >>

        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8>> when not pad? ->
          <<
            main::bits,
            unquote(decode_name)(c1)::5,
            unquote(decode_name)(c2)::5,
            unquote(decode_name)(c3)::5,
            unquote(decode_name)(c4)::5,
            unquote(decode_name)(c5)::5,
            unquote(decode_name)(c6)::5,
            bsr(unquote(decode_name)(c7), 3)::2
          >>

        _ ->
          raise ArgumentError, "incorrect padding"
      end
    end
  end
end
