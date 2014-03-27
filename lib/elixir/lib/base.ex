defmodule Base do
  import Bitwise

  @moduledoc """
  This module provides data encoding and decoding functions
  according to [RFC 4648](http://tools.ietf.org/html/rfc4648).

  This document defines the commonly used base 64, base 32, and base
   16 encoding schemes.
  """

  b16_alphabet    = Enum.with_index '0123456789ABCDEF'
  b64_alphabet    = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  b64url_alphabet = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
  b32_alphabet    = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'
  b32hex_alphabet = Enum.with_index '0123456789ABCDEFGHIJKLMNOPQRSTUV'

  Enum.each [ { :enc16,    :dec16,    b16_alphabet },
              { :enc64,    :dec64,    b64_alphabet },
              { :enc32,    :dec32,    b32_alphabet },
              { :enc64url, :dec64url, b64url_alphabet },
              { :enc32hex, :dec32hex, b32hex_alphabet } ], fn({enc, dec, alphabet}) ->
    for {encoding, value} <- alphabet do
      defp unquote(enc)(unquote(value)), do: unquote(encoding)
      defp unquote(dec)(unquote(encoding)), do: unquote(value)
    end
    defp unquote(dec)(c) do
      raise ArgumentError, message: "non-alphabet digit found: #{<<c>>}"
    end
  end

  @doc """
  Encodes a binary string into a base 16 encoded string.

  ## Examples

      iex> Base.encode16("foobar")
      "666F6F626172"

  """
  @spec encode16(binary) :: binary
  def encode16(data) when is_binary(data) do
    do_encode16(data, &enc16/1)
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  The following alphabet is used both for encoding and decoding:

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
  |      0|         0|      4|         4|      8|         8|     12|         C|
  |      1|         1|      5|         5|      9|         9|     13|         D|
  |      2|         2|      6|         6|     10|         A|     14|         E|
  |      3|         3|      7|         7|     11|         B|     15|         F|

  ## Examples

      iex> Base.decode16("666F6F626172")
      {:ok, "foobar"}

  """
  @spec decode16(binary) :: { :ok, binary } | :error
  def decode16(string) when is_binary(string) do
    { :ok, decode16!(string) }
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode16!("666F6F626172")
      "foobar"

  """
  @spec decode16!(binary) :: binary
  def decode16!(string) when is_binary(string) do
    do_decode16(string, &dec16/1)
  end

  @doc """
  Encodes a binary string into a base 64 encoded string.

  ## Examples

      iex> Base.encode64("foobar")
      "Zm9vYmFy"

  """
  @spec encode64(binary) :: binary
  def encode64(data) when is_binary(data) do
    do_encode64(data, &enc64/1)
  end

  @doc """
  Decodes a base 64 encoded string into a binary string.

  The following alphabet is used both for encoding and decoding:

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

  ## Examples

      iex> Base.decode64("Zm9vYmFy")
      {:ok, "foobar"}

  """
  @spec decode64(binary) :: { :ok, binary } | :error
  def decode64(string) when is_binary(string) do
    { :ok, do_decode64(string, &dec64/1) }
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
  def decode64!(string) when is_binary(string) do
    do_decode64(string, &dec64/1)
  end

  @doc """
  Encodes a binary string into a base 64 encoded string with URL and filename
  safe alphabet.

  ## Examples

      iex> Base.url_encode64(<<255,127,254,252>>)
      "_3_-_A=="

  """
  @spec url_encode64(binary) :: binary
  def url_encode64(data) when is_binary(data) do
    do_encode64(data, &enc64url/1)
  end

  @doc """
  Decodes a base 64 encoded string with URL and filename safe alphabet
  into a binary string.

  The following alphabet is used both for encoding and decoding:

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

  ## Examples

      iex> Base.url_decode64("_3_-_A==")
      {:ok, <<255,127,254,252>>}

  """
  @spec url_decode64(binary) :: { :ok, binary } | :error
  def url_decode64(string) when is_binary(string) do
    { :ok, do_decode64(string, &dec64url/1) }
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
      <<255,127,254,252>>

  """
  @spec url_decode64!(binary) :: binary
  def url_decode64!(string) when is_binary(string) do
    do_decode64(string, &dec64url/1)
  end

  @doc """
  Encodes a binary string into a base 32 encoded string.

  ## Examples

      iex> Base.encode32("foobar")
      "MZXW6YTBOI======"

  """
  @spec encode32(binary) :: binary
  def encode32(data) when is_binary(data) do
    do_encode32(data, &enc32/1)
  end

  @doc """
  Decodes a base 32 encoded string into a binary string.

  The following alphabet is used both for encoding and decoding:

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

  ## Examples

      iex> Base.decode32("MZXW6YTBOI======")
      {:ok, "foobar"}

  """
  @spec decode32(binary) :: { :ok, binary } | :error
  def decode32(string) do
    { :ok, do_decode32(string, &dec32/1) }
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 32 encoded string into a binary string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode32!("MZXW6YTBOI======")
      "foobar"

  """
  @spec decode32!(binary) :: binary
  def decode32!(string) do
    do_decode32(string, &dec32/1)
  end

  @doc """
  Encodes a binary string into a base 32 encoded string with an
  extended hexadecimal alphabet.

  ## Examples

      iex> Base.hex_encode32("foobar")
      "CPNMUOJ1E8======"

  """
  @spec hex_encode32(binary) :: binary
  def hex_encode32(data) when is_binary(data) do
    do_encode32(data, &enc32hex/1)
  end

  @doc """
  Decodes a base 32 encoded string with extended hexadecimal alphabet
  into a binary string.

  The following alphabet is used both for encoding and decoding:

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

  ## Examples

      iex> Base.hex_decode32("CPNMUOJ1E8======")
      {:ok, "foobar"}

  """
  @spec hex_decode32(binary) :: { :ok, binary } | :error
  def hex_decode32(string) when is_binary(string) do
    { :ok, do_decode32(string, &dec32hex/1) }
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 32 encoded string with extended hexadecimal alphabet
  into a binary string.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.hex_decode32!("CPNMUOJ1E8======")
      "foobar"

  """
  @spec hex_decode32!(binary) :: binary
  def hex_decode32!(string) when is_binary(string) do
    do_decode32(string, &dec32hex/1)
  end

  defp do_encode16(<<>>, _), do: <<>>
  defp do_encode16(data, enc) do
    for <<c::4 <- data>>, into: <<>>, do: <<enc.(c)::8>>
  end

  defp do_decode16(<<>>, _), do: <<>>
  defp do_decode16(string, dec) when rem(byte_size(string), 2) == 0 do
    for <<c1::8, c2::8 <- string>>, into: <<>>  do
      <<dec.(c1)::4, dec.(c2)::4>>
    end
  end
  defp do_decode16(_, _) do
    raise ArgumentError, message: "odd-length string"
  end

  defp do_encode64(<<>>, _), do: <<>>
  defp do_encode64(data, enc) do
    split =  3 * div(byte_size(data), 3)
    <<main::[size(split), binary], rest::binary>> = data
    main = for <<c::6 <- main>>, into: <<>>, do: <<enc.(c)::8>>
    case rest do
      <<c1::6, c2::6, c3::4>> ->
        <<main::binary, enc.(c1)::8, enc.(c2)::8, enc.(bsl(c3, 2))::8, ?=>>
      <<c1::6, c2::2>> ->
        <<main::binary, enc.(c1)::8, enc.(bsl(c2, 4))::8, ?=, ?=>>
      <<>> ->
        main
    end
  end

  defp do_decode64(<<>>, _), do: <<>>
  defp do_decode64(string, dec) when rem(byte_size(string), 4) == 0 do
    split = byte_size(string) - 4
    <<main::[size(split), binary], rest::binary>> = string
    main = for <<c::8 <- main>>, into: <<>>, do: <<dec.(c)::6>>
    case rest do
      <<c1::8, c2::8, ?=, ?=>> ->
        <<main::binary, dec.(c1)::6, bsr(dec.(c2), 4)::2>>
      <<c1::8, c2::8, c3::8, ?=>> ->
        <<main::binary, dec.(c1)::6, dec.(c2)::6, bsr(dec.(c3), 2)::4>>
      <<c1::8, c2::8, c3::8, c4::8>> ->
        <<main::binary, dec.(c1)::6, dec.(c2)::6, dec.(c3)::6, dec.(c4)::6>>
      <<>> ->
        main
    end
  end
  defp do_decode64(_, _) do
    raise ArgumentError, message: "incorrect padding"
  end

  defp do_encode32(<<>>, _), do: <<>>
  defp do_encode32(data, enc) do
    split =  5 * div(byte_size(data), 5)
    <<main::[size(split), binary], rest::binary>> = data
    main = for <<c::5 <- main>>, into: <<>>, do: <<enc.(c)::8>>
    case rest do
      <<c1::5, c2::5, c3::5, c4::5, c5::5, c6::5, c7::2>> ->
        <<main::binary,
          enc.(c1)::8, enc.(c2)::8, enc.(c3)::8, enc.(c4)::8,
          enc.(c5)::8, enc.(c6)::8, enc.(bsl(c7, 3))::8, ?=>>
      <<c1::5, c2::5, c3::5, c4::5, c5::4>> ->
        <<main::binary,
          enc.(c1)::8, enc.(c2)::8, enc.(c3)::8, enc.(c4)::8,
          enc.(bsl(c5, 1))::8, ?=,  ?=, ?=>>
      <<c1::5, c2::5, c3::5, c4::1>> ->
        <<main::binary,
          enc.(c1)::8, enc.(c2)::8,  enc.(c3)::8, enc.(bsl(c4, 4))::8,
          ?=, ?=,  ?=, ?=>>
      <<c1::5, c2::3>> ->
        <<main::binary,
          enc.(c1)::8, enc.(bsl(c2, 2))::8, ?=, ?=,
          ?=, ?=, ?=, ?=>>
      <<>> ->
        main
    end
  end

  defp do_decode32(<<>>, _), do: <<>>
  defp do_decode32(string, dec) when rem(byte_size(string), 8) == 0 do
    split = byte_size(string) - 8
    <<main::[size(split), binary], rest::binary>> = string
    main = for <<c::8 <- main>>, into: <<>>, do: <<dec.(c)::5>>
    case rest do
      <<c1::8, c2::8, ?=, ?=, ?=, ?=, ?=, ?=>> ->
        <<main::binary, dec.(c1)::5, bsr(dec.(c2), 2)::3>>
      <<c1::8, c2::8, c3::8, c4::8, ?=, ?=, ?=, ?=>> ->
        <<main::binary,
          dec.(c1)::5, dec.(c2)::5, dec.(c3)::5, bsr(dec.(c4), 4)::1>>
      <<c1::8, c2::8, c3::8, c4::8, c5::8, ?=, ?=, ?=>> ->
        <<main::binary,
          dec.(c1)::5, dec.(c2)::5, dec.(c3)::5, dec.(c4)::5,
          bsr(dec.(c5), 1)::4>>
      <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, ?=>> ->
        <<main::binary,
          dec.(c1)::5, dec.(c2)::5, dec.(c3)::5, dec.(c4)::5,
          dec.(c5)::5, dec.(c6)::5,  bsr(dec.(c7), 3)::2>>
      <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8, c8::8>> ->
        <<main::binary,
          dec.(c1)::5, dec.(c2)::5, dec.(c3)::5, dec.(c4)::5,
          dec.(c5)::5, dec.(c6)::5, dec.(c7)::5, dec.(c8)::5>>
      <<>> ->
        main
    end
  end
  defp do_decode32(_, _) do
    raise ArgumentError, message: "incorrect padding"
  end

end
