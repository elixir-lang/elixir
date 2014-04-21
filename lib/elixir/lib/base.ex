defmodule Base do
  import Bitwise

  @moduledoc """
  This module provides data encoding and decoding functions
  according to [RFC 4648](http://tools.ietf.org/html/rfc4648).

  This document defines the commonly used base 64, base 32, and base
   16 encoding schemes.
  """

  b16_upper     = '0123456789ABCDEF'
  b16_lower     = '0123456789abcdef'
  b64           = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  b64_url       = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
  b32_upper     = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'
  b32_lower     = 'abcdefghijklmnopqrstuvwxyz234567'
  b32_hex_upper = '0123456789ABCDEFGHIJKLMNOPQRSTUV'
  b32_hex_lower = '0123456789abcdefghijklmnopqrstuv'

  Enum.each [ { :enc16_upper,     :dec16_upper,     b16_upper },
              { :enc16_lower,     :dec16_lower,     b16_lower },
              { :enc64,           :dec64,           b64 },
              { :enc32_upper,     :dec32_upper,     b32_upper },
              { :enc32_lower,     :dec32_lower,     b32_lower },
              { :enc64_url,       :dec64_url,       b64_url },
              { :enc32_hex_upper, :dec32_hex_upper, b32_hex_upper },
              { :enc32_hex_lower, :dec32_hex_lower, b32_hex_lower }
            ], fn({enc, dec, alphabet}) ->
    for {encoding, value} <- Enum.with_index(alphabet) do
      defp unquote(enc)(unquote(value)), do: unquote(encoding)
      defp unquote(dec)(unquote(encoding)), do: unquote(value)
    end
    defp unquote(dec)(c) do
      raise ArgumentError, message: "non-alphabet digit found: #{<<c>>}"
    end
  end

  @doc """
  Encodes a binary string into a base 16 encoded string.

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  ## Examples

      iex> Base.encode16("foobar")
      "666F6F626172"

      iex> Base.encode16("Hello World", lowercase: true)
      "48656c6c6f20576f726c64"

  """
  @spec encode16(binary, Keyword.t) :: binary
  def encode16(data, opts \\ []) when is_binary(data) do
    if opts[:lowercase] do
      do_encode16(data, &enc16_lower/1)
    else
      do_encode16(data, &enc16_upper/1)
    end
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

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  ## Examples

      iex> Base.decode16("666F6F626172")
      {:ok, "foobar"}

      iex> Base.decode16("48656c6c6f20576f726c64", lowercase: true)
      {:ok, "Hello World"}

  """
  @spec decode16(binary, Keyword.t) :: { :ok, binary } | :error
  def decode16(string, opts \\ []) when is_binary(string) do
    { :ok, decode16!(string, opts) }
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode16!("666F6F626172")
      "foobar"

      iex> Base.decode16!("48656c6c6f20576f726c64", lowercase: true)
      "Hello World"

  """
  @spec decode16!(binary, Keyword.t) :: binary
  def decode16!(string, opts \\ []) when is_binary(string) do
    if opts[:lowercase] do
      do_decode16(string, &dec16_lower/1)
    else
      do_decode16(string, &dec16_upper/1)
    end
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
    { :ok, decode64!(string) }
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 64 encoded string into a binary string.

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
    do_encode64(data, &enc64_url/1)
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
    { :ok, url_decode64!(string) }
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
    do_decode64(string, &dec64_url/1)
  end

  @doc """
  Encodes a binary string into a base 32 encoded string.

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  ## Examples

      iex> Base.encode32("foobar")
      "MZXW6YTBOI======"

      iex> Base.encode32("foobar", lowercase: true)
      "mzxw6ytboi======"

  """
  @spec encode32(binary, Keyword.t) :: binary
  def encode32(data, opts \\ []) when is_binary(data) do
    if opts[:lowercase] do
      do_encode32(data, &enc32_lower/1)
    else
      do_encode32(data, &enc32_upper/1)
    end
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

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  ## Examples

      iex> Base.decode32("MZXW6YTBOI======")
      {:ok, "foobar"}

      iex> Base.decode32("mzxw6ytboi======", lowercase: true)
      {:ok, "foobar"}

  """
  @spec decode32(binary, Keyword.t) :: { :ok, binary } | :error
  def decode32(string, opts \\ []) do
    { :ok, decode32!(string, opts) }
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 32 encoded string into a binary string.

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode32!("MZXW6YTBOI======")
      "foobar"

      iex> Base.decode32!("mzxw6ytboi======", lowercase: true)
      "foobar"

  """
  @spec decode32!(binary, Keyword.t) :: binary
  def decode32!(string, opts \\ []) do
    if opts[:lowercase] do
      do_decode32(string, &dec32_lower/1)
    else
      do_decode32(string, &dec32_upper/1)
    end
  end

  @doc """
  Encodes a binary string into a base 32 encoded string with an
  extended hexadecimal alphabet.

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  ## Examples

      iex> Base.hex_encode32("foobar")
      "CPNMUOJ1E8======"

      iex> Base.hex_encode32("foobar", lowercase: true)
      "cpnmuoj1e8======"

  """
  @spec hex_encode32(binary, Keyword.t) :: binary
  def hex_encode32(data, opts \\ []) when is_binary(data) do
    if opts[:lowercase] do
      do_encode32(data, &enc32_hex_lower/1)
    else
      do_encode32(data, &enc32_hex_upper/1)
    end
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

  By default an uppercase alphabet is used. An alternate lowercase alphabet
  can be selected by setting the `lowercase` option to `true`.

  ## Examples

      iex> Base.hex_decode32("CPNMUOJ1E8======")
      {:ok, "foobar"}

      iex> Base.hex_decode32("cpnmuoj1e8======", lowercase: true)
      {:ok, "foobar"}

  """
  @spec hex_decode32(binary, Keyword.t) :: { :ok, binary } | :error
  def hex_decode32(string, opts \\ []) when is_binary(string) do
    { :ok, hex_decode32!(string, opts) }
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

      iex> Base.hex_decode32!("cpnmuoj1e8======", lowercase: true)
      "foobar"

  """
  @spec hex_decode32!(binary, Keyword.t) :: binary
  def hex_decode32!(string, opts \\ []) when is_binary(string) do
    if opts[:lowercase] do
      do_decode32(string, &dec32_hex_lower/1)
    else
      do_decode32(string, &dec32_hex_upper/1)
    end
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
    split =  6 * div(bit_size(data), 6)
    <<main::[size(split), bitstring], rest::bitstring>> = data
    main = for <<c::6 <- main>>, into: <<>>, do: <<enc.(c)::8>>
    case rest do
      <<c::4>> ->
        <<main::bitstring, enc.(bsl(c, 2))::8, ?=>>
      <<c::2>> ->
        <<main::bitstring, enc.(bsl(c, 4))::8, ?=, ?=>>
      <<>> ->
        main
    end
  end

  defp do_decode64(<<>>, _), do: <<>>
  defp do_decode64(string, dec) when rem(byte_size(string), 4) == 0 do
    split = byte_size(string) - 3
    <<main::[size(split), binary], rest::binary>> = string
    main = for <<c::8 <- main>>, into: <<>>, do: <<dec.(c)::6>>
    case rest do
      <<c::8, ?=, ?=>> ->
        <<main::bitstring, bsr(dec.(c), 4)::2>>
      <<c1::8, c2::8, ?=>> ->
        <<main::bitstring, dec.(c1)::6, bsr(dec.(c2), 2)::4>>
      <<c1::8, c2::8, c3::8>> ->
        <<main::bitstring, dec.(c1)::6, dec.(c2)::6, dec.(c3)::6>>
      <<>> ->
        main
    end
  end
  defp do_decode64(_, _) do
    raise ArgumentError, message: "incorrect padding"
  end

  defp do_encode32(<<>>, _), do: <<>>
  defp do_encode32(data, enc) do
    split =  5 * div(bit_size(data), 5)
    <<main::[size(split), bitstring], rest::bitstring>> = data
    main = for <<c::5 <- main>>, into: <<>>, do: <<enc.(c)::8>>
    case rest do
      <<c::2>> ->
        <<main::bitstring, enc.(bsl(c, 3))::8, ?=>>
      <<c::4>> ->
        <<main::bitstring, enc.(bsl(c, 1))::8, ?=,  ?=, ?=>>
      <<c::1>> ->
        <<main::bitstring, enc.(bsl(c, 4))::8,  ?=, ?=,  ?=, ?=>>
      <<c::3>> ->
        <<main::bitstring, enc.(bsl(c, 2))::8, ?=, ?=,  ?=, ?=, ?=, ?=>>
      <<>> ->
        main
    end
  end

  defp do_decode32(<<>>, _), do: <<>>
  defp do_decode32(string, dec) when rem(byte_size(string), 8) == 0 do
    split = byte_size(string) - 7
    <<main::[size(split), binary], rest::binary>> = string
    main = for <<c::8 <- main>>, into: <<>>, do: <<dec.(c)::5>>
    case rest do
      <<c::8, ?=, ?=, ?=, ?=, ?=, ?=>> ->
        <<main::bitstring, bsr(dec.(c), 2)::3>>
      <<c1::8, c2::8, c3::8, ?=, ?=, ?=, ?=>> ->
        <<main::bitstring, dec.(c1)::5, dec.(c2)::5, bsr(dec.(c3), 4)::1>>
      <<c1::8, c2::8, c3::8, c4::8, ?=, ?=, ?=>> ->
        <<main::bitstring, dec.(c1)::5, dec.(c2)::5, dec.(c3)::5, bsr(dec.(c4), 1)::4>>
      <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, ?=>> ->
        <<main::bitstring, dec.(c1)::5, dec.(c2)::5, dec.(c3)::5, dec.(c4)::5, dec.(c5)::5, bsr(dec.(c6), 3)::2>>
      <<c1::8, c2::8, c3::8, c4::8, c5::8, c6::8, c7::8>> ->
        <<main::bitstring, dec.(c1)::5, dec.(c2)::5, dec.(c3)::5, dec.(c4)::5, dec.(c5)::5, dec.(c6)::5, dec.(c7)::5>>
      <<>> ->
        main
    end
  end
  defp do_decode32(_, _) do
    raise ArgumentError, message: "incorrect padding"
  end

end
