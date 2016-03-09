defmodule URI do
  @moduledoc """
  Utilities for working with and creating URIs.
  """

  defstruct scheme: nil, path: nil, query: nil,
            fragment: nil, authority: nil,
            userinfo: nil, host: nil, port: nil

  @type t :: %__MODULE__{}

  import Bitwise

  @doc """
  Returns the default port for a given scheme.

  If the scheme is unknown to URI, returns `nil`.
  Any scheme may be registered via `default_port/2`.

  ## Examples

      iex> URI.default_port("ftp")
      21

      iex> URI.default_port("ponzi")
      nil

  """
  def default_port(scheme) when is_binary(scheme) do
    :elixir_config.get({:uri, scheme})
  end

  @doc """
  Registers a scheme with a default port.

  It is recommended for this function to be invoked in your
  application start callback in case you want to register
  new URIs.
  """
  def default_port(scheme, port) when is_binary(scheme) and port > 0 do
    :elixir_config.put({:uri, scheme}, port)
  end

  @doc """
  Encodes an enumerable into a query string.

  Takes an enumerable (containing a sequence of two-element tuples)
  and returns a string in the form of `key1=value1&key2=value2...` where
  keys and values are URL encoded as per `encode_www_form/1`.

  Keys and values can be any term that implements the `String.Chars`
  protocol, except lists which are explicitly forbidden.

  ## Examples

      iex> hd = %{"foo" => 1, "bar" => 2}
      iex> URI.encode_query(hd)
      "bar=2&foo=1"

      iex> query = %{"key" => "value with spaces"}
      iex> URI.encode_query(query)
      "key=value+with+spaces"

  """
  def encode_query(l), do: Enum.map_join(l, "&", &pair/1)

  @doc """
  Decodes a query string into a map.

  Given a query string of the form of `key1=value1&key2=value2...`, produces a
  map with one entry for each key-value pair. Each key and value will be a
  binary. Keys and values will be percent-unescaped.

  Use `query_decoder/1` if you want to iterate over each value manually.

  ## Examples

      iex> URI.decode_query("foo=1&bar=2")
      %{"bar" => "2", "foo" => "1"}

  """
  def decode_query(q, map \\ %{})

  def decode_query(q, %{__struct__: _} = dict) when is_binary(q) do
    IO.write :stderr, "warning: URI.decode_query/2 is deprecated, please use URI.decode_query/1\n" <>
                      Exception.format_stacktrace
    decode_query_dict(q, dict)
  end

  def decode_query(q, map) when is_binary(q) and is_map(map) do
    decode_query_map(q, map)
  end

  def decode_query(q, dict) when is_binary(q) do
    IO.write :stderr, "warning: URI.decode_query/2 is deprecated, please use URI.decode_query/1\n" <>
                      Exception.format_stacktrace
    decode_query_dict(q, dict)
  end

  defp decode_query_map(q, map) do
    case do_decode_query(q) do
      nil         -> map
      {{k, v}, q} -> decode_query_map(q, Map.put(map, k, v))
    end
  end

  defp decode_query_dict(q, dict) do
    case do_decode_query(q) do
      nil         -> dict
      {{k, v}, q} -> decode_query_dict(q, Dict.put(dict, k, v))
    end
  end

  @doc """
  Returns an iterator function over the query string that decodes
  the query string in steps.

  ## Examples

      iex> URI.query_decoder("foo=1&bar=2") |> Enum.map(&(&1))
      [{"foo", "1"}, {"bar", "2"}]

  """
  def query_decoder(q) when is_binary(q) do
    Stream.unfold(q, &do_decode_query/1)
  end

  defp do_decode_query("") do
    nil
  end

  defp do_decode_query(q) do
    {first, next} =
      case :binary.split(q, "&") do
        [first, rest] -> {first, rest}
        [first]       -> {first, ""}
      end

    current =
      case :binary.split(first, "=") do
        [key, value] ->
          {decode_www_form(key), decode_www_form(value)}
        [key] ->
          {decode_www_form(key), nil}
      end

    {current, next}
  end

  defp pair({k, _}) when is_list(k) do
    raise ArgumentError, "encode_query/1 keys cannot be lists, got: #{inspect k}"
  end

  defp pair({_, v}) when is_list(v) do
    raise ArgumentError, "encode_query/1 values cannot be lists, got: #{inspect v}"
  end

  defp pair({k, v}) do
    encode_www_form(Kernel.to_string(k)) <>
    "=" <> encode_www_form(Kernel.to_string(v))
  end

  @doc """
  Checks if the character is a "reserved" character in a URI.

  Reserved characters are specified in [RFC3986, section 2.2](http://tools.ietf.org/html/rfc3986#section-2.2).
  """
  def char_reserved?(c) do
    c in ':/?#[]@!$&\'()*+,;='
  end

  @doc """
  Checks if the character is a "unreserved" character in a URI.

  Unreserved characters are specified in [RFC3986, section 2.3](http://tools.ietf.org/html/rfc3986#section-2.3).
  """
  def char_unreserved?(c) do
    c in ?0..?9 or
    c in ?a..?z or
    c in ?A..?Z or
    c in '~_-.'
  end

  @doc """
  Checks if the character is allowed unescaped in a URI.

  This is the default used by `URI.encode/2` where both
  reserved and unreserved characters are kept unescaped.
  """
  def char_unescaped?(c) do
    char_reserved?(c) or char_unreserved?(c)
  end

  @doc """
  Percent-escapes a URI.
  Accepts `predicate` function as an argument to specify if char can be left as is.

  ## Example

      iex> URI.encode("ftp://s-ite.tld/?value=put it+й")
      "ftp://s-ite.tld/?value=put%20it+%D0%B9"

  """
  def encode(str, predicate \\ &char_unescaped?/1) when is_binary(str) do
    for <<c <- str>>, into: "", do: percent(c, predicate)
  end

  @doc """
  Encodes a string as "x-www-form-urlencoded".

  ## Example

      iex> URI.encode_www_form("put: it+й")
      "put%3A+it%2B%D0%B9"

  """
  def encode_www_form(str) when is_binary(str) do
    for <<c <- str>>, into: "" do
      case percent(c, &char_unreserved?/1) do
        "%20" -> "+"
        pct   -> pct
      end
    end
  end

  defp percent(c, predicate) do
    if predicate.(c) do
      <<c>>
    else
      "%" <> hex(bsr(c, 4)) <> hex(band(c, 15))
    end
  end

  defp hex(n) when n <= 9, do: <<n + ?0>>
  defp hex(n), do: <<n + ?A - 10>>

  @doc """
  Percent-unescapes a URI.

  ## Examples

      iex> URI.decode("http%3A%2F%2Felixir-lang.org")
      "http://elixir-lang.org"

  """
  def decode(uri) do
    unpercent(uri, "", false)
  catch
    :malformed_uri ->
      raise ArgumentError, "malformed URI #{inspect uri}"
  end

  @doc """
  Decodes a string as "x-www-form-urlencoded".

  ## Examples

      iex> URI.decode_www_form("%3Call+in%2F")
      "<all in/"

  """
  def decode_www_form(str) do
    unpercent(str, "", true)
  catch
    :malformed_uri ->
      raise ArgumentError, "malformed URI #{inspect str}"
  end

  defp unpercent(<<?+, tail::binary>>, acc, spaces = true) do
    unpercent(tail, <<acc::binary, ?\s>>, spaces)
  end

  defp unpercent(<<?%, hex_1, hex_2, tail::binary>>, acc, spaces) do
    unpercent(tail, <<acc::binary, bsl(hex_to_dec(hex_1), 4) + hex_to_dec(hex_2)>>, spaces)
  end
  defp unpercent(<<?%, _::binary>>, _acc, _spaces), do: throw(:malformed_uri)

  defp unpercent(<<head, tail::binary>>, acc, spaces) do
    unpercent(tail, <<acc::binary, head>>, spaces)
  end
  defp unpercent(<<>>, acc, _spaces), do: acc

  defp hex_to_dec(n) when n in ?A..?F, do: n - ?A + 10
  defp hex_to_dec(n) when n in ?a..?f, do: n - ?a + 10
  defp hex_to_dec(n) when n in ?0..?9, do: n - ?0
  defp hex_to_dec(_n), do: throw(:malformed_uri)

  @doc """
  Parses a well-formed URI reference into its components.

  Note this function expects a well-formed URI and does not perform
  any validation. See the examples section below of how `URI.parse/1`
  can be used to parse a wide range of relative URIs.

  This function uses the parsing regular expression as defined
  in the [Appendix B of RFC3986](http://tools.ietf.org/html/rfc3986#appendix-B).

  When a URI is given without a port, the values registered via
  `URI.default_port/1` and `URI.default_port/2` are used.

  ## Examples

      iex> URI.parse("http://elixir-lang.org/")
      %URI{scheme: "http", path: "/", query: nil, fragment: nil,
           authority: "elixir-lang.org", userinfo: nil,
           host: "elixir-lang.org", port: 80}

      iex> URI.parse("//elixir-lang.org/")
      %URI{authority: "elixir-lang.org", fragment: nil, host: "elixir-lang.org",
           path: "/", port: nil, query: nil, scheme: nil, userinfo: nil}

      iex> URI.parse("/foo/bar")
      %URI{authority: nil, fragment: nil, host: nil, path: "/foo/bar",
           port: nil, query: nil, scheme: nil, userinfo: nil}

      iex> URI.parse("foo/bar")
      %URI{authority: nil, fragment: nil, host: nil, path: "foo/bar",
           port: nil, query: nil, scheme: nil, userinfo: nil}

  """
  def parse(%URI{} = uri), do: uri

  def parse(s) when is_binary(s) do
    # From http://tools.ietf.org/html/rfc3986#appendix-B
    regex = ~r/^(([a-z][a-z0-9\+\-\.]*):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/i
    parts = nillify(Regex.run(regex, s))

    destructure [_, _, scheme, _, authority, path, _, query, _, fragment], parts
    {userinfo, host, port} = split_authority(authority)

    authority = authority &&
      IO.iodata_to_binary([
        if(userinfo, do: userinfo <> "@", else: ""),
        host || "",
        if(port, do: ":" <> Integer.to_string(port), else: "")
      ])

    scheme = normalize_scheme(scheme)
    port   = port || (scheme && default_port(scheme))

    %URI{
      scheme: scheme, path: path, query: query,
      fragment: fragment, authority: authority,
      userinfo: userinfo, host: host, port: port
    }
  end

  # Split an authority into its userinfo, host and port parts.
  defp split_authority(s) do
    s = s || ""
    components = Regex.run ~r/(^(.*)@)?(\[[a-zA-Z0-9:.]*\]|[^:]*)(:(\d*))?/, s

    destructure [_, _, userinfo, host, _, port], nillify(components)
    port = if port, do: String.to_integer(port)
    host = if host, do: host |> String.lstrip(?[) |> String.rstrip(?])

    {userinfo, host, port}
  end

  defp normalize_scheme(nil),     do: nil
  defp normalize_scheme(scheme),  do: String.downcase(scheme)

  # Regex.run returns empty strings sometimes. We want
  # to replace those with nil for consistency.
  defp nillify(l) do
    for s <- l do
      if byte_size(s) > 0, do: s
    end
  end

  @doc """
  Converts the URI to string.

      iex> URI.to_string(URI.parse("http://google.com"))
      "http://google.com"
  """
  defdelegate to_string(uri), to: String.Chars.URI
end

defimpl String.Chars, for: URI do
  def to_string(%{scheme: scheme, port: port, path: path,
                  query: query, fragment: fragment} = uri) do
    uri =
      case scheme && URI.default_port(scheme) do
        ^port -> %{uri | port: nil}
        _     -> uri
      end

    # Based on http://tools.ietf.org/html/rfc3986#section-5.3
    authority = extract_authority(uri)

    ""
    |> if_value(scheme,    & &1 <> scheme <> ":")
    |> if_value(authority, & &1 <> "//" <> authority)
    |> if_value(path,      & &1 <> path)
    |> if_value(query,     & &1 <> "?" <> query)
    |> if_value(fragment,  & &1 <> "#" <> fragment)
  end

  defp extract_authority(%{host: nil, authority: authority}) do
    authority
  end
  defp extract_authority(%{host: host, userinfo: userinfo, port: port}) do
    host
    |> if_value(userinfo, & userinfo <> "@" <> &1)
    |> if_value(port, & &1 <> ":" <> Integer.to_string(port))
  end

  defp if_value(result, nil, _fun), do: result
  defp if_value(result, _value, fun), do: fun.(result)
end
