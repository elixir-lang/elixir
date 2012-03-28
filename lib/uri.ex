defmodule URI do
  @moduledoc """
  Utilities for working with and creating URIs.
  """

  @doc """
  Takes an orddict and returns a string of
  k=v&k2=v2... where keys and values are URL
  encoded as per url_encode. Keys and values
  can be a mixture of atoms, lists, and binaries.
  """
  def encode_query(l), do: Enum.join(Enum.map(l, pair(&1)), "&")

  defp pair({k, v}) do
    url_encode(normalize(k)) <> "=" <> url_encode(normalize(v))
  end

  defp normalize(x) when is_list(x), do: :unicode.characters_to_binary(x)
  defp normalize(x) when is_atom(x), do: :erlang.atom_to_binary(x, :utf8)
  defp normalize(x) when is_binary(x), do: x
  defp normalize(_), do: raise "Not a list, binary, nor atom."

  @doc """
  Percent (URL) encodes a URI.
  """
  def url_encode(s) when is_list(s) do
    binary_to_list url_encode(:unicode.characters_to_binary(s))
  end

  def url_encode(s), do: bc <<c>> in s, do: <<percent(c)|binary>>

  defp percent(32), do: <<?+>>
  defp percent(?-), do: <<?->>
  defp percent(?_), do: <<?_>>
  defp percent(?.), do: <<?.>>

  defp percent(c) when
    c >= ?0 and c <= ?9 when
    c >= ?a and c <= ?z when
    c >= ?A and c <= ?Z do
    <<c>>
  end

  defp percent(c), do: escape_byte(c)

  defp escape_byte(c), do: "%" <> hex(c)

  defp hex(n) when n <= 9, do: <<n + ?0>>
  defp hex(n) when n > 15 do
    hex(:erlang.bsr(n, 4)) <> hex(:erlang.band(n, 15))
  end
  defp hex(n), do: <<n + ?A - 10>>

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
  def parse(s) when is_list(s), do: parse(list_to_binary(s))
  def parse(s) do
    # From http://tools.ietf.org/html/rfc3986#appendix-B
    regex = %r/^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/
    destructure([_, _, scheme, _, authority, path, _, query, _, fragment],
                nillify(Regex.run(regex, s)))
    scheme_specific(Orddict.merge([scheme: scheme, path: path, query: query,
                                  fragment: fragment, authority: authority],
                                  split_authority(authority)))
  end

  defp scheme_specific(parsed_uri) do
    scheme = Orddict.get(parsed_uri, :scheme)
    if scheme do
      # TODO: A better way of looking up modules.
      module = Module.concat(URI, :string.to_upper(binary_to_list(scheme)))
      case :code.ensure_loaded(module) do
      match: {:error, _}
        parsed_uri
      match: {:module, _}
        module.parse(default_port(parsed_uri, module.default_port))
      end
    else:
      parsed_uri
    end
  end

  defp default_port(parsed_uri, port) do
    if Orddict.get(parsed_uri, :port) do
      parsed_uri
    else:
      Orddict.put(parsed_uri, :port, port)
    end
  end

  # Split an authority into its userinfo, host and port parts.
  defp split_authority(s) do
    s = s || ""
    components = Regex.run %r/(^(.*)@)?([^:]*)(:(\d*))?/, s
    destructure([_, _, userinfo, host, _, port], nillify(components))
    port = if port, do: list_to_integer(binary_to_list(port))
    [userinfo: userinfo, host: host, port: port]
  end

  # find/2 returns empty strings sometimes. We want
  # to replace those with nil for consistency and ease
  # of use.
  defp nillify(l) do
    lc s in l do
      if :erlang.size(s) > 0 do
        s
      else:
        nil
      end
    end
  end
end
