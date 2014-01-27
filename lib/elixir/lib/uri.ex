defmodule URI do
  @moduledoc """
  Utilities for working with and creating URIs.
  """

  defrecord Info, [scheme: nil, path: nil, query: nil,
                   fragment: nil, authority: nil,
                   userinfo: nil, host: nil, port: nil]

  import Bitwise

  @ports [
    { "ftp", 21 },
    { "http", 80 },
    { "https", 443 },
    { "ldap", 389 },
    { "sftp", 22 },
    { "tftp", 69 },
  ]

  Enum.each @ports, fn { scheme, port } ->
    def normalize_scheme(unquote(scheme)), do: unquote(scheme)
    def default_port(unquote(scheme)),     do: unquote(port)
  end

  @doc """
  Normalizes the scheme according to the spec by downcasing it.
  """
  def normalize_scheme(nil),     do: nil
  def normalize_scheme(scheme),  do: String.downcase(scheme)

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
    { :ok, dict } = :application.get_env(:elixir, :uri)
    Dict.get(dict, scheme)
  end

  @doc """
  Registers a scheme with a default port.
  """
  def default_port(scheme, port) when is_binary(scheme) and port > 0 do
    { :ok, dict } = :application.get_env(:elixir, :uri)
    :application.set_env(:elixir, :uri, Dict.put(dict, scheme, port))
  end

  @doc """
  Encodes an enumerable into a query string.

  Takes an enumerable (containing a sequence of two-item tuples)
  and returns a string of the form "key1=value1&key2=value2..." where
  keys and values are URL encoded as per `encode/1`. Keys and values can
  be any term that implements the `String.Chars` protocol (i.e. can be converted
  to a binary).

  ## Examples

      iex> hd = HashDict.new([{"foo", 1}, {"bar", "2"}])
      iex> URI.encode_query(hd)
      "bar=2&foo=1"

  """
  def encode_query(l), do: Enum.map_join(l, "&", &pair/1)

  @doc """
  Decodes a query string into a `HashDict`.

  Given a query string of the form "key1=value1&key2=value2...", produces a
  `HashDict` with one entry for each key-value pair. Each key and value will be a
  binary. Keys and values will be percent-unescaped.

  Use `query_decoder/1` if you want to iterate over each value manually.

  ## Examples

      iex> URI.decode_query("foo=1&bar=2") |> Dict.to_list
      [{"bar", "2"}, {"foo", "1"}]

      iex> hd = HashDict.new()
      iex> URI.decode_query("foo=1&bar=2", hd) |> HashDict.keys
      ["bar", "foo"]
      iex> URI.decode_query("foo=1&bar=2", hd) |> HashDict.values
      ["2", "1"]

  """
  def decode_query(q, dict \\ HashDict.new) when is_binary(q) do
    Enum.reduce query_decoder(q), dict, fn({ k, v }, acc) -> Dict.put(acc, k, v) end
  end

  @doc """
  Returns an iterator function over the query string that decodes
  the query string in steps.

  ## Examples

      iex> URI.query_decoder("foo=1&bar=2") |> Enum.map &(&1)
      [{"foo", "1"}, {"bar", "2"}]

  """
  def query_decoder(q) when is_binary(q) do
    Stream.unfold(q, &do_decoder/1)
  end

  defp do_decoder("") do
    nil
  end

  defp do_decoder(q) do
    { first, next } =
      case :binary.split(q, "&") do
        [first, rest] -> { first, rest }
        [first]       -> { first, "" }
      end

    current =
      case :binary.split(first, "=") do
        [ key, value ] -> { decode(key), decode(value) }
        [ key ]        -> { decode(key), nil }
      end

    { current, next }
  end

  defp pair({k, v}) do
    encode(to_string(k)) <> "=" <> encode(to_string(v))
  end

  @doc """
  Percent-escape a URI.

  ## Example

      iex> URI.encode("http://elixir-lang.com/getting_started/2.html")
      "http%3A%2F%2Felixir-lang.com%2Fgetting_started%2F2.html"

  """
  def encode(s), do: bc(<<c>> inbits s, do: <<percent(c) :: binary>>)

  defp percent(32), do: <<?+>>
  defp percent(?-), do: <<?->>
  defp percent(?_), do: <<?_>>
  defp percent(?.), do: <<?.>>

  defp percent(c)
      when c >= ?0 and c <= ?9
      when c >= ?a and c <= ?z
      when c >= ?A and c <= ?Z do
    <<c>>
  end

  defp percent(c), do: "%" <> hex(bsr(c, 4)) <> hex(band(c, 15))

  defp hex(n) when n <= 9, do: <<n + ?0>>
  defp hex(n), do: <<n + ?A - 10>>

  @doc """
  Percent-unescape a URI.

  ## Examples

      iex> URI.decode("http%3A%2F%2Felixir-lang.com")
      "http://elixir-lang.com"

  """
  def decode(<<?%, hex1, hex2, tail :: binary >>) do
    << bsl(hex2dec(hex1), 4) + hex2dec(hex2) >> <> decode(tail)
  end

  def decode(<<head, tail :: binary >>) do
    <<check_plus(head)>> <> decode(tail)
  end

  def decode(<<>>), do: <<>>

  defp hex2dec(n) when n in ?A..?F, do: n - ?A + 10
  defp hex2dec(n) when n in ?a..?f, do: n - ?a + 10
  defp hex2dec(n) when n in ?0..?9, do: n - ?0

  defp check_plus(?+), do: 32
  defp check_plus(c),  do: c

  @doc """
  Parses a URI into components.

  URIs have portions that are handled specially for the particular
  scheme of the URI. For example, http and https have different
  default ports. Such values can be accessed and registered via
  `URI.default_port/1` and `URI.default_port/2`.

  ## Examples

      iex> URI.parse("http://elixir-lang.org/")
      URI.Info[scheme: "http", path: "/", query: nil, fragment: nil,
               authority: "elixir-lang.org", userinfo: nil,
               host: "elixir-lang.org", port: 80]

  """
  def parse(s) when is_binary(s) do
    # From http://tools.ietf.org/html/rfc3986#appendix-B
    regex = %r/^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/
    parts = nillify(Regex.run(regex, s))

    destructure [_, _, scheme, _, authority, path, _, query, _, fragment], parts
    { userinfo, host, port } = split_authority(authority)

    if authority do
      authority = ""

      if userinfo, do: authority = authority <> userinfo <> "@"
      if host, do: authority = authority <> host
      if port, do: authority = authority <> ":" <> integer_to_binary(port)
    end

    scheme = normalize_scheme(scheme)

    if nil?(port) and not nil?(scheme) do
      port = default_port(scheme)
    end

    URI.Info[
      scheme: scheme, path: path, query: query,
      fragment: fragment, authority: authority,
      userinfo: userinfo, host: host, port: port
    ]
  end

  # Split an authority into its userinfo, host and port parts.
  defp split_authority(s) do
    s = s || ""
    components = Regex.run %r/(^(.*)@)?(\[[a-zA-Z0-9:.]*\]|[^:]*)(:(\d*))?/, s

    destructure [_, _, userinfo, host, _, port], nillify(components)
    port = if port, do: binary_to_integer(port)
    host = if host, do: host |> String.lstrip(?[) |> String.rstrip(?])

    { userinfo, host, port }
  end

  # Regex.run returns empty strings sometimes. We want
  # to replace those with nil for consistency.
  defp nillify(l) do
    lc s inlist l do
      if size(s) > 0, do: s, else: nil
    end
  end
end

defimpl String.Chars, for: URI.Info do
  def to_string(URI.Info[] = uri) do
    scheme = uri.scheme

    if scheme && (port = URI.default_port(scheme)) do
      if uri.port == port, do: uri = uri.port(nil)
    end

    result = ""

    if uri.scheme,   do: result = result <> uri.scheme <> "://"
    if uri.userinfo, do: result = result <> uri.userinfo <> "@"
    if uri.host,     do: result = result <> uri.host
    if uri.port,     do: result = result <> ":" <> integer_to_binary(uri.port)
    if uri.path,     do: result = result <> uri.path
    if uri.query,    do: result = result <> "?" <> uri.query
    if uri.fragment, do: result = result <> "#" <> uri.fragment

    result
  end
end
