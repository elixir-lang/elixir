defmodule Base do
  import Bitwise

  @moduledoc """
  This module provides data encoding and decoding functions
  according to [RFC 4648](http://tools.ietf.org/html/rfc4648).

  This document defines the commonly used base 16, base 32, and base
  64 encoding schemes.

  ## Base 16 alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         0|      4|         4|      8|         8|     12|         C|
      |      1|         1|      5|         5|      9|         9|     13|         D|
      |      2|         2|      6|         6|     10|         A|     14|         E|
      |      3|         3|      7|         7|     11|         B|     15|         F|

  ## Base 32 alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         A|      9|         J|     18|         S|     27|         3|
      |      1|         B|     10|         K|     19|         T|     28|         4|
      |      2|         C|     11|         L|     20|         U|     29|         5|
      |      3|         D|     12|         M|     21|         V|     30|         6|
      |      4|         E|     13|         N|     22|         W|     31|         7|
      |      5|         F|     14|         O|     23|         X|       |          |
      |      6|         G|     15|         P|     24|         Y|  (pad)|         =|
      |      7|         H|     16|         Q|     25|         Z|       |          |
      |      8|         I|     17|         R|     26|         2|       |          |


  ## Base 32 (extended hex) alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         0|      9|         9|     18|         I|     27|         R|
      |      1|         1|     10|         A|     19|         J|     28|         S|
      |      2|         2|     11|         B|     20|         K|     29|         T|
      |      3|         3|     12|         C|     21|         L|     30|         U|
      |      4|         4|     13|         D|     22|         M|     31|         V|
      |      5|         5|     14|         E|     23|         N|       |          |
      |      6|         6|     15|         F|     24|         O|  (pad)|         =|
      |      7|         7|     16|         G|     25|         P|       |          |
      |      8|         8|     17|         H|     26|         Q|       |          |

  ## Base 64 alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         A|     17|         R|     34|         i|     51|         z|
      |      1|         B|     18|         S|     35|         j|     52|         0|
      |      2|         C|     19|         T|     36|         k|     53|         1|
      |      3|         D|     20|         U|     37|         l|     54|         2|
      |      4|         E|     21|         V|     38|         m|     55|         3|
      |      5|         F|     22|         W|     39|         n|     56|         4|
      |      6|         G|     23|         X|     40|         o|     57|         5|
      |      7|         H|     24|         Y|     41|         p|     58|         6|
      |      8|         I|     25|         Z|     42|         q|     59|         7|
      |      9|         J|     26|         a|     43|         r|     60|         8|
      |     10|         K|     27|         b|     44|         s|     61|         9|
      |     11|         L|     28|         c|     45|         t|     62|         +|
      |     12|         M|     29|         d|     46|         u|     63|         /|
      |     13|         N|     30|         e|     47|         v|       |          |
      |     14|         O|     31|         f|     48|         w|  (pad)|         =|
      |     15|         P|     32|         g|     49|         x|       |          |
      |     16|         Q|     33|         h|     50|         y|       |          |

  ## Base 64 (URL and filename safe) alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         A|     17|         R|     34|         i|     51|         z|
      |      1|         B|     18|         S|     35|         j|     52|         0|
      |      2|         C|     19|         T|     36|         k|     53|         1|
      |      3|         D|     20|         U|     37|         l|     54|         2|
      |      4|         E|     21|         V|     38|         m|     55|         3|
      |      5|         F|     22|         W|     39|         n|     56|         4|
      |      6|         G|     23|         X|     40|         o|     57|         5|
      |      7|         H|     24|         Y|     41|         p|     58|         6|
      |      8|         I|     25|         Z|     42|         q|     59|         7|
      |      9|         J|     26|         a|     43|         r|     60|         8|
      |     10|         K|     27|         b|     44|         s|     61|         9|
      |     11|         L|     28|         c|     45|         t|     62|         -|
      |     12|         M|     29|         d|     46|         u|     63|         _|
      |     13|         N|     30|         e|     47|         v|       |          |
      |     14|         O|     31|         f|     48|         w|  (pad)|         =|
      |     15|         P|     32|         g|     49|         x|       |          |
      |     16|         Q|     33|         h|     50|         y|       |          |

  """

  b16_alphabet    = Enum.with_index '0123456789ABCDEF'
  b64_alphabet    = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  b64url_alphabet = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
  b32_alphabet    = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'
  b32hex_alphabet = Enum.with_index '0123456789ABCDEFGHIJKLMNOPQRSTUV'

  Enum.each [{:enc16,    :dec16,    b16_alphabet},
             {:enc32,    :dec32,    b32_alphabet},
             {:enc64,    :dec64,    b64_alphabet},
             {:enc64url, :dec64url, b64url_alphabet},
             {:enc32hex, :dec32hex, b32hex_alphabet}], fn({enc, dec, alphabet}) ->
    for {encoding, value} <- alphabet do
      defp unquote(enc)(unquote(value)), do: unquote(encoding)
      defp unquote(dec)(unquote(encoding)), do: unquote(value)
    end
    defp unquote(dec)(c) do
      raise ArgumentError, "non-alphabet digit found: #{inspect <<c>>, binaries: :as_strings} (byte #{c})"
    end
  end

  @compile {:inline, from_upper: 1, from_lower: 1, from_mixed: 1,
                     to_lower: 1, to_upper: 1, enc16: 1, dec16: 1,
                     enc32: 1, dec32: 1, enc32hex: 1, dec32hex: 1,
                     enc64: 1, dec64: 1, enc64url: 1, dec64url: 1}

  defp to_lower(char) when char in ?A..?Z,
    do: char + (?a - ?A)
  defp to_lower(char),
    do: char

  defp to_upper(char), do: char

  defp from_upper(char), do: char

  defp from_lower(char) when char in ?a..?z,
    do: char - (?a - ?A)
  defp from_lower(char) when not char in ?A..?Z,
    do: char
  defp from_lower(char),
    do: raise(ArgumentError, "non-alphabet digit found: \"#{<<char>>}\" (byte #{char})")

  defp from_mixed(char) when char in ?a..?z,
    do: char - (?a - ?A)
  defp from_mixed(char),
    do: char

  @doc """
  Encodes a binary string into a base 16 encoded string.

  Accepts an atom `:upper` (default) for encoding to upper case characters or
  `:lower` for lower case characters.

  ## Examples

      iex> Base.encode16("foobar")
      "666F6F626172"

      iex> Base.encode16("foobar", case: :lower)
      "666f6f626172"

  """
  @spec encode16(binary) :: binary
  @spec encode16(binary, Keyword.t) :: binary
  def encode16(data, opts \\ []) when is_binary(data) do
    case = Keyword.get(opts, :case, :upper)
    do_encode16(case, data)
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  Accepts an atom `:upper` (default) for decoding from upper case characters or
  `:lower` for lower case characters. `:mixed` can be given for mixed case
  characters.

  ## Examples

      iex> Base.decode16("666F6F626172")
      {:ok, "foobar"}

      iex> Base.decode16("666f6f626172", case: :lower)
      {:ok, "foobar"}

      iex> Base.decode16("666f6F626172", case: :mixed)
      {:ok, "foobar"}

  """
  @spec decode16(binary) :: {:ok, binary} | :error
  @spec decode16(binary, Keyword.t) :: {:ok, binary} | :error
  def decode16(string, opts \\ []) do
    {:ok, decode16!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  Accepts an atom `:upper` (default) for decoding from upper case characters or
  `:lower` for lower case characters. `:mixed` can be given for mixed case
  characters.

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
  @spec decode16!(binary) :: binary
  @spec decode16!(binary, Keyword.t) :: binary
  def decode16!(string, opts \\ [])

  def decode16!(string, opts) when is_binary(string) and rem(byte_size(string), 2) == 0 do
    case = Keyword.get(opts, :case, :upper)
    do_decode16(case, string)
  end

  def decode16!(string, _opts) when is_binary(string) do
    raise ArgumentError, "odd-length string"
  end

  @doc """
  Encodes a binary string into a base 64 encoded string.

  ## Examples

      iex> Base.encode64("foobar")
      "Zm9vYmFy"

  """
  @spec encode64(binary) :: binary
  def encode64(data) when is_binary(data) do
    do_encode64(data)
  end

  @doc """
  Decodes a base 64 encoded string into a binary string.

  ## Examples

      iex> Base.decode64("Zm9vYmFy")
      {:ok, "foobar"}

  """
  @spec decode64(binary) :: {:ok, binary} | :error
  def decode64(string) when is_binary(string) do
    {:ok, decode64!(string)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 64 encoded string into a binary string.

  The following alphabet is used both for encoding and decoding:

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode64!("Zm9vYmFy")
      "foobar"

  """
  @spec decode64!(binary) :: binary
  def decode64!(string) when is_binary(string) and rem(byte_size(string), 4) == 0 do
    do_decode64(string)
  end

  def decode64!(string) when is_binary(string) do
    raise ArgumentError, "incorrect padding"
  end

  @doc """
  Encodes a binary string into a base 64 encoded string with URL and filename
  safe alphabet.

  ## Examples

      iex> Base.url_encode64(<<255, 127, 254, 252>>)
      "_3_-_A=="

  """
  @spec url_encode64(binary) :: binary
  def url_encode64(data) when is_binary(data) do
    do_encode64url(data)
  end

  @doc """
  Decodes a base 64 encoded string with URL and filename safe alphabet
  into a binary string.

  ## Examples

      iex> Base.url_decode64("_3_-_A==")
      {:ok, <<255, 127, 254, 252>>}

  """
  @spec url_decode64(binary) :: {:ok, binary} | :error
  def url_decode64(string) when is_binary(string) do
    {:ok, url_decode64!(string)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 64 encoded string with URL and filename safe alphabet
  into a binary string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.url_decode64!("_3_-_A==")
      <<255, 127, 254, 252>>

  """
  @spec url_decode64!(binary) :: binary
  def url_decode64!(string) when is_binary(string) and rem(byte_size(string), 4) == 0 do
    do_decode64url(string)
  end

  def url_decode64!(string) when is_binary(string) do
    raise ArgumentError, "incorrect padding"
  end

  @doc """
  Encodes a binary string into a base 32 encoded string.

  Accepts an atom `:upper` (default) for encoding to upper case characters or
  `:lower` for lower case characters.

  ## Examples

      iex> Base.encode32("foobar")
      "MZXW6YTBOI======"

      iex> Base.encode32("foobar", case: :lower)
      "mzxw6ytboi======"

  """
  @spec encode32(binary) :: binary
  @spec encode32(binary, Keyword.t) :: binary
  def encode32(data, opts \\ []) when is_binary(data) do
    case = Keyword.get(opts, :case, :upper)
    do_encode32(case, data)
  end

  @doc """
  Decodes a base 32 encoded string into a binary string.

  Accepts an atom `:upper` (default) for decoding from upper case characters or
  `:lower` for lower case characters. `:mixed` can be given for mixed case
  characters.

  ## Examples

      iex> Base.decode32("MZXW6YTBOI======")
      {:ok, "foobar"}

      iex> Base.decode32("mzxw6ytboi======", case: :lower)
      {:ok, "foobar"}

      iex> Base.decode32("mzXW6ytBOi======", case: :mixed)
      {:ok, "foobar"}

  """
  @spec decode32(binary) :: {:ok, binary} | :error
  @spec decode32(binary, Keyword.t) :: {:ok, binary} | :error
  def decode32(string, opts \\ []) do
    {:ok, decode32!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 32 encoded string into a binary string.

  Accepts an atom `:upper` (default) for decoding from upper case characters or
  `:lower` for lower case characters. `:mixed` can be given for mixed case
  characters.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode32!("MZXW6YTBOI======")
      "foobar"

      iex> Base.decode32!("mzxw6ytboi======", case: :lower)
      "foobar"

      iex> Base.decode32!("mzXW6ytBOi======", case: :mixed)
      "foobar"

  """
  @spec decode32!(binary) :: binary
  @spec decode32!(binary, Keyword.t) :: binary
  def decode32!(string, opts \\ [])

  def decode32!(string, opts) when is_binary(string) and rem(byte_size(string), 8) == 0 do
    case = Keyword.get(opts, :case, :upper)
    do_decode32(case, string)
  end

  def decode32!(string, _opts) when is_binary(string) do
    raise ArgumentError, "incorrect padding"
  end

  @doc """
  Encodes a binary string into a base 32 encoded string with an
  extended hexadecimal alphabet.

  Accepts an atom `:upper` (default) for encoding to upper case characters or
  `:lower` for lower case characters.

  ## Examples

      iex> Base.hex_encode32("foobar")
      "CPNMUOJ1E8======"

      iex> Base.hex_encode32("foobar", case: :lower)
      "cpnmuoj1e8======"

  """
  @spec hex_encode32(binary) :: binary
  @spec hex_encode32(binary, Keyword.t) :: binary
  def hex_encode32(data, opts \\ []) when is_binary(data) do
    case = Keyword.get(opts, :case, :upper)
    do_hex_encode32(case, data)
  end

  @doc """
  Decodes a base 32 encoded string with extended hexadecimal alphabet
  into a binary string.

  Accepts an atom `:upper` (default) for decoding from upper case characters or
  `:lower` for lower case characters. `:mixed` can be given for mixed case
  characters.

  ## Examples

      iex> Base.hex_decode32("CPNMUOJ1E8======")
      {:ok, "foobar"}

      iex> Base.hex_decode32("cpnmuoj1e8======", case: :lower)
      {:ok, "foobar"}

      iex> Base.hex_decode32("cpnMuOJ1E8======", case: :mixed)
      {:ok, "foobar"}

  """
  @spec hex_decode32(binary) :: {:ok, binary} | :error
  @spec hex_decode32(binary, Keyword.t) :: {:ok, binary} | :error
  def hex_decode32(string, opts \\ []) do
    {:ok, hex_decode32!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 32 encoded string with extended hexadecimal alphabet
  into a binary string.

  Accepts an atom `:upper` (default) for decoding from upper case characters or
  `:lower` for lower case characters. `:mixed` can be given for mixed case
  characters.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.hex_decode32!("CPNMUOJ1E8======")
      "foobar"

      iex> Base.hex_decode32!("cpnmuoj1e8======", case: :lower)
      "foobar"

      iex> Base.hex_decode32!("cpnMuOJ1E8======", case: :mixed)
      "foobar"

  """
  @spec hex_decode32!(binary) :: binary
  @spec hex_decode32!(binary, Keyword.t) :: binary
  def hex_decode32!(string, opts \\ [])

  def hex_decode32!(string, opts) when is_binary(string) and rem(byte_size(string), 8) == 0 do
    case = Keyword.get(opts, :case, :upper)
    do_hex_decode32(case, string)
  end

  def hex_decode32!(string, _opts) when is_binary(string) do
    raise ArgumentError, "incorrect padding"
  end

  defp do_encode16(_, <<>>), do: <<>>
  defp do_encode16(:upper, data) do
    for <<c::4 <- data>>, into: <<>>, do: <<enc16(c)::8>>
  end
  defp do_encode16(:lower, data) do
    for <<c::4 <- data>>, into: <<>>, do: <<to_lower(enc16(c))::8>>
  end

  defp do_decode16(_, <<>>), do: <<>>
  defp do_decode16(:upper, string) when rem(byte_size(string), 2) == 0 do
    for <<c1::8, c2::8 <- string>>, into: <<>> do
      <<dec16(c1)::4, dec16(c2)::4>>
    end
  end
  defp do_decode16(:lower, string) when rem(byte_size(string), 2) == 0 do
    for <<c1::8, c2::8 <- string>>, into: <<>> do
      <<dec16(from_lower(c1))::4, dec16(from_lower(c2))::4>>
    end
  end
  defp do_decode16(:mixed, string) when rem(byte_size(string), 2) == 0 do
    for <<c1::8, c2::8 <- string>>, into: <<>> do
      <<dec16(from_mixed(c1))::4, dec16(from_mixed(c2))::4>>
    end
  end

  defp do_encode64(<<>>), do: <<>>
  defp do_encode64(data) do
    split =  3 * div(byte_size(data), 3)
    <<main::size(split)-binary, rest::binary>> = data
    main = for <<c::6 <- main>>, into: <<>>, do: <<enc64(c)::8>>
    case rest do
      <<c1::6, c2::6, c3::4>> ->
        <<main::binary, enc64(c1)::8, enc64(c2)::8, enc64(bsl(c3, 2))::8, ?=>>
      <<c1::6, c2::2>> ->
        <<main::binary, enc64(c1)::8, enc64(bsl(c2, 4))::8, ?=, ?=>>
      <<>> ->
        main
    end
  end

  defp do_decode64(<<>>), do: <<>>
  defp do_decode64(string) do
    split = byte_size(string) - 4
    <<main::size(split)-binary, rest::binary>> = string
    main = for <<c::8 <- main>>, into: <<>>, do: <<dec64(c)::6>>
    case rest do
      <<c1::8, c2::8, ?=, ?=>> ->
        <<main::binary, dec64(c1)::6, bsr(dec64(c2), 4)::2>>
      <<c1::8, c2::8, c3::8, ?=>> ->
        <<main::binary, dec64(c1)::6, dec64(c2)::6, bsr(dec64(c3), 2)::4>>
      <<c1::8, c2::8, c3::8, c4::8>> ->
        <<main::binary, dec64(c1)::6, dec64(c2)::6, dec64(c3)::6, dec64(c4)::6>>
      <<>> ->
        main
    end
  end

  defp do_encode64url(<<>>), do: <<>>
  defp do_encode64url(data) do
    split =  3 * div(byte_size(data), 3)
    <<main::size(split)-binary, rest::binary>> = data
    main = for <<c::6 <- main>>, into: <<>>, do: <<enc64url(c)::8>>
    case rest do
      <<c1::6, c2::6, c3::4>> ->
        <<main::binary, enc64url(c1)::8, enc64url(c2)::8, enc64url(bsl(c3, 2))::8, ?=>>
      <<c1::6, c2::2>> ->
        <<main::binary, enc64url(c1)::8, enc64url(bsl(c2, 4))::8, ?=, ?=>>
      <<>> ->
        main
    end
  end

  defp do_decode64url(<<>>), do: <<>>
  defp do_decode64url(string) do
    split = byte_size(string) - 4
    <<main::size(split)-binary, rest::binary>> = string
    main = for <<c::8 <- main>>, into: <<>>, do: <<dec64url(c)::6>>
    case rest do
      <<c1::8, c2::8, ?=, ?=>> ->
        <<main::binary, dec64url(c1)::6, bsr(dec64url(c2), 4)::2>>
      <<c1::8, c2::8, c3::8, ?=>> ->
        <<main::binary, dec64url(c1)::6, dec64url(c2)::6, bsr(dec64url(c3), 2)::4>>
      <<c1::8, c2::8, c3::8, c4::8>> ->
        <<main::binary, dec64url(c1)::6, dec64url(c2)::6, dec64url(c3)::6, dec64url(c4)::6>>
      <<>> ->
        main
    end
  end



  defp do_encode32(_, <<>>), do: <<>>

  for {case, fun} <- [upper: :to_upper, lower: :to_lower] do
    defp do_encode32(unquote(case), data) do
      split =  5 * div(byte_size(data), 5)
      <<main::size(split)-binary, rest::binary>> = data
      main = for <<c::5 <- main>>, into: <<>>, do: <<unquote(fun)(enc32(c))::8>>
      case rest do
        <<c1::5, c2::5, c3::5, c4::5, c5::5, c6::5, c7::2>> ->
          <<main::binary,
            unquote(fun)(enc32(c1))::8, unquote(fun)(enc32(c2))::8,
            unquote(fun)(enc32(c3))::8, unquote(fun)(enc32(c4))::8,
            unquote(fun)(enc32(c5))::8, unquote(fun)(enc32(c6))::8,
            unquote(fun)(enc32(bsl(c7, 3)))::8, ?=>>
        <<c1::5, c2::5, c3::5, c4::5, c5::4>> ->
          <<main::binary,
            unquote(fun)(enc32(c1))::8, unquote(fun)(enc32(c2))::8,
            unquote(fun)(enc32(c3))::8, unquote(fun)(enc32(c4))::8,
            unquote(fun)(enc32(bsl(c5, 1)))::8, ?=,  ?=, ?=>>
        <<c1::5, c2::5, c3::5, c4::1>> ->
          <<main::binary,
            unquote(fun)(enc32(c1))::8, unquote(fun)(enc32(c2))::8,
            unquote(fun)(enc32(c3))::8, unquote(fun)(enc32(bsl(c4, 4)))::8,
            ?=, ?=,  ?=, ?=>>
        <<c1::5, c2::3>> ->
          <<main::binary,
            unquote(fun)(enc32(c1))::8, unquote(fun)(enc32(bsl(c2, 2)))::8, ?=, ?=,
            ?=, ?=, ?=, ?=>>
        <<>> ->
          main
      end
    end
  end

  defp do_decode32(_, <<>>), do: <<>>

  for {case, fun} <- [upper: :from_upper, lower: :from_lower, mixed: :from_mixed] do
    defp do_decode32(unquote(case), string) do
      split = byte_size(string) - 8
      <<main::size(split)-binary, rest::binary>> = string
      main = for <<c::8 <- main>>, into: <<>>, do: <<dec32(unquote(fun)(c))::5>>
      case rest do
        <<c1::8, c2::8, ?=, ?=, ?=, ?=, ?=, ?=>> ->
          <<main::binary, dec32(unquote(fun)(c1))::5,
            bsr(dec32(unquote(fun)(c2)), 2)::3>>
        <<c1::8, c2::8, c3::8, c4::8, ?=, ?=, ?=, ?=>> ->
          <<main::binary,
            dec32(unquote(fun)(c1))::5, dec32(unquote(fun)(c2))::5,
            dec32(unquote(fun)(c3))::5, bsr(dec32(unquote(fun)(c4)), 4)::1>>
        <<c1::8, c2::8, c3::8, c4::8, c5::8, ?=, ?=, ?=>> ->
          <<main::binary,
            dec32(unquote(fun)(c1))::5, dec32(unquote(fun)(c2))::5,
            dec32(unquote(fun)(c3))::5, dec32(unquote(fun)(c4))::5,
            bsr(dec32(unquote(fun)(c5)), 1)::4>>
        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, ?=>> ->
          <<main::binary,
            dec32(unquote(fun)(c1))::5, dec32(unquote(fun)(c2))::5,
            dec32(unquote(fun)(c3))::5, dec32(unquote(fun)(c4))::5,
            dec32(unquote(fun)(c5))::5, dec32(unquote(fun)(c6))::5,
            bsr(dec32(unquote(fun)(c7)), 3)::2>>
        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8>> ->
          <<main::binary,
            dec32(unquote(fun)(c1))::5, dec32(unquote(fun)(c2))::5,
            dec32(unquote(fun)(c3))::5, dec32(unquote(fun)(c4))::5,
            dec32(unquote(fun)(c5))::5, dec32(unquote(fun)(c6))::5,
            dec32(unquote(fun)(c7))::5, dec32(unquote(fun)(c8))::5>>
        <<>> ->
          main
      end
    end
  end

  defp do_hex_encode32(_, <<>>), do: <<>>

  for {case, fun} <- [upper: :to_upper, lower: :to_lower] do
    defp do_hex_encode32(unquote(case), data) do
      split =  5 * div(byte_size(data), 5)
      <<main::size(split)-binary, rest::binary>> = data
      main = for <<c::5 <- main>>, into: <<>>, do: <<unquote(fun)(enc32hex(c))::8>>
      case rest do
        <<c1::5, c2::5, c3::5, c4::5, c5::5, c6::5, c7::2>> ->
          <<main::binary,
            unquote(fun)(enc32hex(c1))::8, unquote(fun)(enc32hex(c2))::8,
            unquote(fun)(enc32hex(c3))::8, unquote(fun)(enc32hex(c4))::8,
            unquote(fun)(enc32hex(c5))::8, unquote(fun)(enc32hex(c6))::8,
            unquote(fun)(enc32hex(bsl(c7, 3)))::8, ?=>>
        <<c1::5, c2::5, c3::5, c4::5, c5::4>> ->
          <<main::binary,
            unquote(fun)(enc32hex(c1))::8, unquote(fun)(enc32hex(c2))::8,
            unquote(fun)(enc32hex(c3))::8, unquote(fun)(enc32hex(c4))::8,
            unquote(fun)(enc32hex(bsl(c5, 1)))::8, ?=,  ?=, ?=>>
        <<c1::5, c2::5, c3::5, c4::1>> ->
          <<main::binary,
            unquote(fun)(enc32hex(c1))::8, unquote(fun)(enc32hex(c2))::8,
            unquote(fun)(enc32hex(c3))::8, unquote(fun)(enc32hex(bsl(c4, 4)))::8,
            ?=, ?=,  ?=, ?=>>
        <<c1::5, c2::3>> ->
          <<main::binary,
            unquote(fun)(enc32hex(c1))::8, unquote(fun)(enc32hex(bsl(c2, 2)))::8, ?=, ?=,
            ?=, ?=, ?=, ?=>>
        <<>> ->
          main
      end
    end
  end

  defp do_hex_decode32(_, <<>>), do: <<>>

  for {case, fun} <- [upper: :from_upper, lower: :from_lower, mixed: :from_mixed] do
    defp do_hex_decode32(unquote(case), string) do
      split = byte_size(string) - 8
      <<main::size(split)-binary, rest::binary>> = string
      main = for <<c::8 <- main>>, into: <<>>, do: <<dec32hex(unquote(fun)(c))::5>>
      case rest do
        <<c1::8, c2::8, ?=, ?=, ?=, ?=, ?=, ?=>> ->
          <<main::binary, dec32hex(unquote(fun)(c1))::5,
            bsr(dec32hex(unquote(fun)(c2)), 2)::3>>
        <<c1::8, c2::8, c3::8, c4::8, ?=, ?=, ?=, ?=>> ->
          <<main::binary,
            dec32hex(unquote(fun)(c1))::5, dec32hex(unquote(fun)(c2))::5,
            dec32hex(unquote(fun)(c3))::5, bsr(dec32hex(unquote(fun)(c4)), 4)::1>>
        <<c1::8, c2::8, c3::8, c4::8, c5::8, ?=, ?=, ?=>> ->
          <<main::binary,
            dec32hex(unquote(fun)(c1))::5, dec32hex(unquote(fun)(c2))::5,
            dec32hex(unquote(fun)(c3))::5, dec32hex(unquote(fun)(c4))::5,
            bsr(dec32hex(unquote(fun)(c5)), 1)::4>>
        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, ?=>> ->
          <<main::binary,
            dec32hex(unquote(fun)(c1))::5, dec32hex(unquote(fun)(c2))::5,
            dec32hex(unquote(fun)(c3))::5, dec32hex(unquote(fun)(c4))::5,
            dec32hex(unquote(fun)(c5))::5, dec32hex(unquote(fun)(c6))::5,
            bsr(dec32hex(unquote(fun)(c7)), 3)::2>>
        <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8>> ->
          <<main::binary,
            dec32hex(unquote(fun)(c1))::5, dec32hex(unquote(fun)(c2))::5,
            dec32hex(unquote(fun)(c3))::5, dec32hex(unquote(fun)(c4))::5,
            dec32hex(unquote(fun)(c5))::5, dec32hex(unquote(fun)(c6))::5,
            dec32hex(unquote(fun)(c7))::5, dec32hex(unquote(fun)(c8))::5>>
        <<>> ->
          main
      end
    end
  end
end
