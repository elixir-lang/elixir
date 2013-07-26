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
  Takes an enumerable (containing a sequence of two-item tuples)
  and returns a string of the form "k=v&k2=v2..." where keys and values are
  URL encoded as per `encode/1`. Keys and values can be any term
  that implements the `Binary.Chars` protocol (i.e. can be converted
  to a binary).
  """
  def encode_query(l), do: Enum.map_join(l, "&", pair(&1))

  @doc """
  Given a query string of the form "key1=value1&key=value2...", produces an
  orddict with one entry for each key-value pair. Each key and value will be a
  binary. It also does percent-unescaping of both keys and values.

  Use `query_decoder/1` if you want to iterate over each value manually.
  """
  def decode_query(q, dict // HashDict.new) when is_binary(q) do
    Enum.reduce query_decoder(q), dict, fn({ k, v }, acc) -> Dict.put(acc, k, v) end
  end

  @doc """
  Returns an iterator function over the query string that decodes
  the query string in steps.
  """
  def query_decoder(q) when is_binary(q) do
    fn(acc, fun) ->
      do_decoder(q, acc, fun)
    end
  end

  defp do_decoder("", acc, _fun) do
    acc
  end

  defp do_decoder(q, acc, fun) do
    next =
      case :binary.split(q, "&") do
        [first, rest] -> rest
        [first]       -> ""
      end

    current =
      case :binary.split(first, "=") do
        [ key, value ] -> { decode(key), decode(value) }
        [ key ]        -> { decode(key), nil }
      end

    do_decoder(next, fun.(current, acc), fun)
  end

  defp pair({k, v}) do
    encode(to_binary(k)) <> "=" <> encode(to_binary(v))
  end

  @doc """
  Percent (URL) encodes a URI.
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
  Unpercent (URL) decodes a URI.
  """
  def decode(<<?%, hex1, hex2, tail :: binary >>) do
    << bsl(hex2dec(hex1), 4) + hex2dec(hex2) >> <> decode(tail)
  end

  def decode(<<head, tail :: binary >>) do
    <<check_plus(head)>> <> decode(tail)
  end

  def decode(<<>>), do: <<>>

  defp hex2dec(n) when n in ?A..?F, do: n - ?A + 10
  defp hex2dec(n) when n in ?0..?9, do: n - ?0

  defp check_plus(?+), do: 32
  defp check_plus(c),  do: c

  @doc """
  Parses a URI into components.

  URIs have portions that are handled specially for the
  particular scheme of the URI. For example, http and https
  have different default ports. Sometimes the parsing
  of portions themselves are different. This parser
  is extensible via behavior modules. If you have a
  module named `URI.MYSCHEME` with a function called
  `parse` that takes a single argument, the generically
  parsed URI, that function will be called when this
  parse function is passed a URI of that scheme. This
  allows you to build on top of what the URI library
  currently offers. You also need to define `default_port`
  which takes no arguments and returns the default port
  for that particular scheme. Take a look at `URI.HTTPS` for an
  example of one of these extension modules.
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
    components = Regex.run %r/(^(.*)@)?([^:]*)(:(\d*))?/, s
    destructure [_, _, userinfo, host, _, port], nillify(components)
    port = if port, do: binary_to_integer(port)
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

defimpl Binary.Chars, for: URI.Info do
  def to_binary(URI.Info[] = uri) do
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
