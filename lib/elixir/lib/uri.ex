defmodule URI do
  @on_load :preload_parsers

  defrecord Info, [scheme: nil, path: nil, query: nil,
                   fragment: nil, authority: nil,
                   userinfo: nil, host: nil, port: nil,
                   specifics: nil]

  import Bitwise

  @moduledoc """
  Utilities for working with and creating URIs.
  """

  @doc """
  Takes an enumerable (containing a sequence of two-item tuples)
  and returns a string of k=v&k2=v2... where keys and values are
  URL encoded as per encode. Keys and values can be any term
  that implements the Binary.Chars protocol (i.e. can be converted
  to binary).
  """
  def encode_query(l), do: Enum.map_join(l, "&", pair(&1))

  @doc """
  Given a query string of the form "key1=value1&key=value2...", produces an
  orddict with one entry for each key-value pair. Each key and value will be a
  binary. It also does percent-unescaping of both keys and values.

  Use decoder/1 if you want to customize or iterate each value manually.
  """
  def decode_query(q, dict // OrdDict.new) do
    Enum.reduce query_decoder(q), dict, fn({ k, v }, acc) -> Dict.put(acc, k, v) end
  end

  @doc """
  Returns an iterator function over the query string that decodes
  the query string in steps.
  """
  def query_decoder(q) do
    fn -> { do_decoder(&1), do_decoder(to_binary(q)) } end
  end

  defp do_decoder("") do
    :stop
  end

  defp do_decoder(q) do
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

    { current, next }
  end

  defp pair({k, v}) do
    encode(to_binary(k)) <> "=" <> encode(to_binary(v))
  end

  @doc """
  Percent (URL) encodes a URI.
  """
  def encode(s), do: bc <<c>> inbits s, do: <<percent(c) :: binary>>

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
  module named URI.MYSCHEME with a function called
  'parse' that takes a single argument, the generically
  parsed URI, that function will be called when this
  parse function is passed a URI of that scheme. This
  allows you to build on top of what the URI library
  currently offers. You also need to define default_port
  which takes 0 arguments and returns the default port
  for that particular scheme. Take a look at URI.HTTPS for an
  example of one of these extension modules.
  """
  def parse(s) do
    # From http://tools.ietf.org/html/rfc3986#appendix-B
    regex = %r/^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/
    parts = nillify(Regex.run(regex, to_binary(s)))

    destructure [_, _, scheme, _, authority, path, _, query, _, fragment], parts
    { userinfo, host, port } = split_authority(authority)

    info = URI.Info[
      scheme: scheme, path: path, query: query,
      fragment: fragment, authority: authority,
      userinfo: userinfo, host: host, port: port
    ]

    scheme_specific(scheme, info)
  end

  defp scheme_specific(scheme, info) do
    if scheme do
      module =
        try do
          Module.safe_concat(URI, :string.to_upper(binary_to_list(scheme)))
        rescue
          ArgumentError -> nil
        end

      if module && match?({:module,^module}, Code.ensure_loaded(module)) do
        module.parse(default_port(info, module))
      else
        info
      end
    else
      info
    end
  end

  defp default_port(info, module) do
    if info.port, do: info, else: info.port(module.default_port)
  end

  # Split an authority into its userinfo, host and port parts.
  defp split_authority(s) do
    s = s || ""
    components = Regex.run %r/(^(.*)@)?([^:]*)(:(\d*))?/, s
    destructure [_, _, userinfo, host, _, port], nillify(components)
    port = if port, do: list_to_integer(binary_to_list(port))
    { userinfo, host, port }
  end

  # Regex.run returns empty strings sometimes. We want
  # to replace those with nil for consistency.
  defp nillify(l) do
    lc s inlist l do
      if size(s) > 0 do
        s
      else
        nil
      end
    end
  end

  # Reference parsers so the parse/1 doesn't fail
  # on safe_concat.
  defp preload_parsers do
    parsers = [URI.FTP, URI.HTTP, URI.HTTPS, URI.LDAP, URI.SFTP, URI.TFTP]
    Enum.each parsers, Code.ensure_loaded(&1)
    :ok
  end
end
