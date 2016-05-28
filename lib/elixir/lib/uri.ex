defmodule URI do
  @moduledoc """
  Utilities for working with URIs.

  This module provides functions for working with URIs (for example, parsing
  URIs or encoding query strings). For reference, most of the functions in this
  module refer to [RFC 3986](https://tools.ietf.org/html/rfc3986).
  """

  defstruct scheme: nil, path: nil, query: nil,
            fragment: nil, authority: nil,
            userinfo: nil, host: nil, port: nil

  @type t :: %__MODULE__{
    scheme: nil | binary,
    path: nil | binary,
    query: nil | binary,
    fragment: nil | binary,
    authority: nil | binary,
    userinfo: nil | binary,
    host: nil | binary,
    port: nil | :inet.port_number,
  }

  import Bitwise

  @doc """
  Returns the default port for a given scheme.

  If the scheme is unknown to the `URI` module, this function returns
  `nil`. The default port for any scheme can be configured globally
  via `default_port/2`.

  ## Examples

      iex> URI.default_port("ftp")
      21

      iex> URI.default_port("ponzi")
      nil

  """
  @spec default_port(binary) :: nil | pos_integer
  def default_port(scheme) when is_binary(scheme) do
    :elixir_config.get({:uri, scheme})
  end

  @doc """
  Registers the default port `port` for the given `scheme`.

  After this function is called, `port` will be returned by
  `default_port/1` for the given scheme `scheme`. Note that this function
  changes the default port for the given `scheme` *globally*, meaning for
  every application.

  It is recommended for this function to be invoked in your
  application's start callback in case you want to register
  new URIs.
  """
  @spec default_port(binary, pos_integer) :: :ok
  def default_port(scheme, port) when is_binary(scheme) and port > 0 do
    :elixir_config.put({:uri, scheme}, port)
  end

  @doc """
  Encodes an enumerable into a query string.

  Takes an enumerable that enumerates as a list of two-element
  tuples (e.g., a map or a keyword list) and returns a string
  in the form of `key1=value1&key2=value2...` where keys and
  values are URL encoded as per `encode_www_form/1`.

  Keys and values can be any term that implements the `String.Chars`
  protocol, except lists which are explicitly forbidden.

  ## Examples

      iex> hd = %{"foo" => 1, "bar" => 2}
      iex> URI.encode_query(hd)
      "bar=2&foo=1"

      iex> query = %{"key" => "value with spaces"}
      iex> URI.encode_query(query)
      "key=value+with+spaces"

      iex> URI.encode_query %{key: [:a, :list]}
      ** (ArgumentError) encode_query/1 values cannot be lists, got: [:a, :list]

  """
  @spec encode_query(term) :: binary
  def encode_query(l), do: Enum.map_join(l, "&", &pair/1)

  @doc """
  Decodes a query string into a map.

  Given a query string of the form of `key1=value1&key2=value2...`, this
  function inserts each key-value pair in the query string as one entry in the
  given `map`. Keys and values in the resulting map will be binaries. Keys and
  values will be percent-unescaped.

  Use `query_decoder/1` if you want to iterate over each value manually.

  ## Examples

      iex> URI.decode_query("foo=1&bar=2")
      %{"bar" => "2", "foo" => "1"}

      iex> URI.decode_query("percent=oh+yes%21", %{"starting" => "map"})
      %{"percent" => "oh yes!", "starting" => "map"}

  """
  @spec decode_query(binary, map) :: map
  def decode_query(q, map \\ %{})

  def decode_query(q, %{__struct__: _} = dict) when is_binary(q) do
    IO.warn "URI.decode_query/2 is deprecated, please use URI.decode_query/1"
    decode_query_dict(q, dict)
  end

  def decode_query(q, map) when is_binary(q) and is_map(map) do
    decode_query_map(q, map)
  end

  def decode_query(q, dict) when is_binary(q) do
    IO.warn "URI.decode_query/2 is deprecated, please use URI.decode_query/1"
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
  Returns a stream of two-element tuples representing key-value pairs in the
  given `query`.

  Key and value in each tuple will be binaries and will be percent-unescaped.

  ## Examples

      iex> URI.query_decoder("foo=1&bar=2") |> Enum.to_list()
      [{"foo", "1"}, {"bar", "2"}]

  """
  @spec query_decoder(binary) :: Enumerable.t
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

  Reserved characters are specified in
  [RFC 3986, section 2.2](http://tools.ietf.org/html/rfc3986#section-2.2).

  ## Examples

      iex> URI.char_reserved?(?+)
      true

  """
  @spec char_reserved?(term) :: boolean
  def char_reserved?(c) do
    c in ':/?#[]@!$&\'()*+,;='
  end

  @doc """
  Checks if the character is a "unreserved" character in a URI.

  Unreserved characters are specified in
  [RFC 3986, section 2.3](http://tools.ietf.org/html/rfc3986#section-2.3).

  ## Examples

      iex> URI.char_unreserved?(?_)
      true

  """
  @spec char_unreserved?(term) :: boolean
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

  ## Examples

      iex> URI.char_unescaped?(?{)
      false

  """
  @spec char_unescaped?(term) :: boolean
  def char_unescaped?(c) do
    char_reserved?(c) or char_unreserved?(c)
  end

  @doc """
  Percent-escapes the given string.

  This function accepts a `predicate` function as an optional argument; if
  passed, this function will be called with each character (byte) in `str` as
  its argument and should return `true` if that character should not be escaped
  and left as is.

  ## Example

      iex> URI.encode("ftp://s-ite.tld/?value=put it+й")
      "ftp://s-ite.tld/?value=put%20it+%D0%B9"

      iex> URI.encode("a string", &(&1 != ?i))
      "a str%69ng"

  """
  @spec encode(binary, (byte -> boolean)) :: binary
  def encode(str, predicate \\ &char_unescaped?/1) when is_binary(str) do
    for <<c <- str>>, into: "", do: percent(c, predicate)
  end

  @doc """
  Encodes a string as "x-www-form-urlencoded".

  ## Example

      iex> URI.encode_www_form("put: it+й")
      "put%3A+it%2B%D0%B9"

  """
  @spec encode_www_form(binary) :: binary
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
  @spec decode(binary) :: binary
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
  @spec decode_www_form(binary) :: binary
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
  any validation. See the "Examples" section below for examples of how
  `URI.parse/1` can be used to parse a wide range of URIs.

  This function uses the parsing regular expression as defined
  in [RFC 3986, Appendix B](http://tools.ietf.org/html/rfc3986#appendix-B).

  When a URI is given without a port, the value returned by
  `URI.default_port/1` for the URI's scheme is used for the `:port` field.

  If a `%URI{}` struct is given to this function, this function returns it
  unmodified.

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
  @spec parse(t | binary) :: t
  def parse(uri)

  def parse(%URI{} = uri), do: uri

  def parse(s) when is_binary(s) do
    # From http://tools.ietf.org/html/rfc3986#appendix-B
    regex = ~r/^(([a-z][a-z0-9\+\-\.]*):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/i
    parts = nillify(Regex.run(regex, s))

    destructure [_, _, scheme, _, authority, path, _, query, _, fragment], parts
    {userinfo, host, port} = split_authority(authority)

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
    host = if host, do: host |> String.trim_leading("[") |> String.trim_trailing("]")
    port = if port, do: String.to_integer(port)

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
  Returns the string representation of the given `URI` struct.

      iex> URI.to_string(URI.parse("http://google.com"))
      "http://google.com"

      iex> URI.to_string(%URI{scheme: "foo", host: "bar.baz"})
      "foo://bar.baz"

  """
  @spec to_string(t) :: binary
  defdelegate to_string(uri), to: String.Chars.URI

  @doc ~S"""
  Merges two URIs.

  This function merges two URIs as per
  [RFC 3986, section 5.2](http://tools.ietf.org/html/rfc3986#section-5.2).

  ## Examples

      iex> URI.merge(URI.parse("http://google.com"), "/query") |> to_string
      "http://google.com/query"

      iex> URI.merge("http://example.com", "http://google.com") |> to_string
      "http://google.com"

  """
  @spec merge(t | binary, t | binary) :: t
  def merge(uri, rel)

  def merge(%URI{authority: nil}, _rel) do
    raise ArgumentError, "you must merge onto an absolute URI"
  end
  def merge(_base, %URI{scheme: rel_scheme} = rel) when rel_scheme != nil do
    rel
  end
  def merge(%URI{} = base, %URI{path: rel_path} = rel) when rel_path in ["", nil] do
    %{base | query: rel.query || base.query, fragment: rel.fragment}
  end
  def merge(%URI{} = base, %URI{} = rel) do
    new_path = merge_paths(base.path, rel.path)
    %{base | path: new_path, query: rel.query, fragment: rel.fragment}
  end
  def merge(base, rel) do
    merge(parse(base), parse(rel))
  end

  defp merge_paths(nil, rel_path),
    do: merge_paths("/", rel_path)
  defp merge_paths(_, "/" <> _ = rel_path),
    do: rel_path
  defp merge_paths(base_path, rel_path) do
    [_ | base_segments] = path_to_segments(base_path)
    path_to_segments(rel_path)
    |> Kernel.++(base_segments)
    |> remove_dot_segments([])
    |> Enum.join("/")
  end

  defp remove_dot_segments([], [head, ".." | acc]),
    do: remove_dot_segments([], [head | acc])
  defp remove_dot_segments([], acc),
    do: acc
  defp remove_dot_segments(["." | tail], acc),
    do: remove_dot_segments(tail, acc)
  defp remove_dot_segments([head | tail], ["..", ".." | _] = acc),
    do: remove_dot_segments(tail, [head | acc])
  defp remove_dot_segments(segments, [_, ".." | acc]),
    do: remove_dot_segments(segments, acc)
  defp remove_dot_segments([head | tail], acc),
    do: remove_dot_segments(tail, [head | acc])

  def path_to_segments(path) do
    [head | tail] = String.split(path, "/")
    reverse_and_discard_empty(tail, [head])
  end

  defp reverse_and_discard_empty([], acc),
    do: acc
  defp reverse_and_discard_empty([head], acc),
    do: [head | acc]
  defp reverse_and_discard_empty(["" | tail], acc),
    do: reverse_and_discard_empty(tail, acc)
  defp reverse_and_discard_empty([head | tail], acc),
    do: reverse_and_discard_empty(tail, [head | acc])
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

    if(scheme, do: scheme <> ":", else: "") <>
    if(authority, do: "//" <> authority, else: "") <>
    if(path, do: path, else: "") <>
    if(query, do: "?" <> query, else: "") <>
    if(fragment, do: "#" <> fragment, else: "")
  end

  defp extract_authority(%{host: nil, authority: authority}) do
    authority
  end
  defp extract_authority(%{host: host, userinfo: userinfo, port: port}) do
    # According to the grammar at
    # https://tools.ietf.org/html/rfc3986#appendix-A, a "host" can have a colon
    # in it only if it's an IPv6 or "IPvFuture" address), so if there's a colon
    # in the host we can safely surround it with [].
    if(userinfo, do: userinfo <> "@", else: "") <>
    if(String.contains?(host, ":"), do: "[" <> host <> "]", else: host) <>
    if(port, do: ":" <> Integer.to_string(port), else: "")
  end
end
