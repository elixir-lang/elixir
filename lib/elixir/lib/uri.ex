defmodule URI do
  @moduledoc """
  Utilities for working with URIs.

  This module provides functions for working with URIs (for example, parsing
  URIs or encoding query strings). The functions in this module are implemented
  according to [RFC 3986](https://tools.ietf.org/html/rfc3986).
  """

  defstruct scheme: nil,
            path: nil,
            query: nil,
            fragment: nil,
            authority: nil,
            userinfo: nil,
            host: nil,
            port: nil

  @type t :: %__MODULE__{
          scheme: nil | binary,
          path: nil | binary,
          query: nil | binary,
          fragment: nil | binary,
          authority: nil | binary,
          userinfo: nil | binary,
          host: nil | binary,
          port: nil | :inet.port_number()
        }

  import Bitwise

  @reserved_characters ':/?#[]@!$&\'()*+,;='
  @formatted_reserved_characters Enum.map_join(@reserved_characters, ", ", &<<?`, &1, ?`>>)

  @doc """
  Returns the default port for a given `scheme`.

  If the scheme is unknown to the `URI` module, this function returns
  `nil`. The default port for any scheme can be configured globally
  via `default_port/2`.

  ## Examples

      iex> URI.default_port("ftp")
      21

      iex> URI.default_port("ponzi")
      nil

  """
  @spec default_port(binary) :: nil | non_neg_integer
  def default_port(scheme) when is_binary(scheme) do
    :elixir_config.get({:uri, scheme}, nil)
  end

  @doc """
  Registers the default `port` for the given `scheme`.

  After this function is called, `port` will be returned by
  `default_port/1` for the given scheme `scheme`. Note that this function
  changes the default port for the given `scheme` *globally*, meaning for
  every application.

  It is recommended for this function to be invoked in your
  application's start callback in case you want to register
  new URIs.
  """
  @spec default_port(binary, non_neg_integer) :: :ok
  def default_port(scheme, port) when is_binary(scheme) and is_integer(port) and port >= 0 do
    :elixir_config.put({:uri, scheme}, port)
  end

  @doc """
  Encodes `enumerable` into a query string using `encoding`.

  Takes an enumerable that enumerates as a list of two-element
  tuples (for instance, a map or a keyword list) and returns a string
  in the form of `key1=value1&key2=value2...`.

  Keys and values can be any term that implements the `String.Chars`
  protocol with the exception of lists, which are explicitly forbidden.

  You can specify one of the following `encoding` strategies:

    * `:www_form` - (default, since v1.12.0) keys and values are URL encoded as
      per `encode_www_form/1`. This is the format typically used by browsers on
      query strings and form data. It encodes " " as "+".

    * `:rfc_3986` - (since v1.12.0) the same as `:www_form` except it encodes
      " " as "%20" according [RFC 3986](https://tools.ietf.org/html/rfc3986).
      This is the best option if you are encoding in a non-browser situation,
      since encoding spaces as "+" can be ambiguous to URI parsers. This can
      inadvertently lead to spaces being interpreted as literal plus signs.

  Encoding defaults to `:www_form` for backward compatibility.

  ## Examples

      iex> query = %{"foo" => 1, "bar" => 2}
      iex> URI.encode_query(query)
      "bar=2&foo=1"

      iex> query = %{"key" => "value with spaces"}
      iex> URI.encode_query(query)
      "key=value+with+spaces"

      iex> query = %{"key" => "value with spaces"}
      iex> URI.encode_query(query, :rfc_3986)
      "key=value%20with%20spaces"

      iex> URI.encode_query(%{key: [:a, :list]})
      ** (ArgumentError) encode_query/2 values cannot be lists, got: [:a, :list]

  """
  @spec encode_query(Enum.t(), :rfc_3986 | :www_form) :: binary
  def encode_query(enumerable, encoding \\ :www_form) do
    Enum.map_join(enumerable, "&", &encode_kv_pair(&1, encoding))
  end

  defp encode_kv_pair({key, _}, _encoding) when is_list(key) do
    raise ArgumentError, "encode_query/2 keys cannot be lists, got: #{inspect(key)}"
  end

  defp encode_kv_pair({_, value}, _encoding) when is_list(value) do
    raise ArgumentError, "encode_query/2 values cannot be lists, got: #{inspect(value)}"
  end

  defp encode_kv_pair({key, value}, :rfc_3986) do
    encode(Kernel.to_string(key), &char_unreserved?/1) <>
      "=" <> encode(Kernel.to_string(value), &char_unreserved?/1)
  end

  defp encode_kv_pair({key, value}, :www_form) do
    encode_www_form(Kernel.to_string(key)) <> "=" <> encode_www_form(Kernel.to_string(value))
  end

  @doc """
  Decodes `query` into a map.

  Given a query string in the form of `key1=value1&key2=value2...`, this
  function inserts each key-value pair in the query string as one entry in the
  given `map`. Keys and values in the resulting map will be binaries. Keys and
  values will be percent-unescaped.

  You can specify one of the following `encoding` options:

    * `:www_form` - (default, since v1.12.0) keys and values are decoded as per
      `decode_www_form/1`. This is the format typically used by browsers on
      query strings and form data. It decodes "+" as " ".

    * `:rfc_3986` - (since v1.12.0) keys and values are decoded as per
      `decode/1`. The result is the same as `:www_form` except for leaving "+"
      as is in line with [RFC 3986](https://tools.ietf.org/html/rfc3986).

  Encoding defaults to `:www_form` for backward compatibility.

  Use `query_decoder/1` if you want to iterate over each value manually.

  ## Examples

      iex> URI.decode_query("foo=1&bar=2")
      %{"bar" => "2", "foo" => "1"}

      iex> URI.decode_query("percent=oh+yes%21", %{"starting" => "map"})
      %{"percent" => "oh yes!", "starting" => "map"}

      iex> URI.decode_query("percent=oh+yes%21", %{}, :rfc_3986)
      %{"percent" => "oh+yes!"}

  """
  @spec decode_query(binary, %{optional(binary) => binary}, :rfc_3986 | :www_form) :: %{
          optional(binary) => binary
        }
  def decode_query(query, map \\ %{}, encoding \\ :www_form)

  def decode_query(query, %_{} = dict, encoding) when is_binary(query) do
    IO.warn(
      "URI.decode_query/3 expects the second argument to be a map, other usage is deprecated"
    )

    decode_query_into_dict(query, dict, encoding)
  end

  def decode_query(query, map, encoding) when is_binary(query) and is_map(map) do
    decode_query_into_map(query, map, encoding)
  end

  def decode_query(query, dict, encoding) when is_binary(query) do
    IO.warn(
      "URI.decode_query/3 expects the second argument to be a map, other usage is deprecated"
    )

    decode_query_into_dict(query, dict, encoding)
  end

  defp decode_query_into_map(query, map, encoding) do
    case decode_next_query_pair(query, encoding) do
      nil ->
        map

      {{key, value}, rest} ->
        decode_query_into_map(rest, Map.put(map, key, value), encoding)
    end
  end

  defp decode_query_into_dict(query, dict, encoding) do
    case decode_next_query_pair(query, encoding) do
      nil ->
        dict

      {{key, value}, rest} ->
        # Avoid warnings about Dict being deprecated
        dict_module = Dict
        decode_query_into_dict(rest, dict_module.put(dict, key, value), encoding)
    end
  end

  @doc """
  Returns a stream of two-element tuples representing key-value pairs in the
  given `query`.

  Key and value in each tuple will be binaries and will be percent-unescaped.

  You can specify one of the following `encoding` options:

    * `:www_form` - (default, since v1.12.0) keys and values are decoded as per
      `decode_www_form/1`. This is the format typically used by browsers on
      query strings and form data. It decodes "+" as " ".

    * `:rfc_3986` - (since v1.12.0) keys and values are decoded as per
      `decode/1`. The result is the same as `:www_form` except for leaving "+"
      as is in line with [RFC 3986](https://tools.ietf.org/html/rfc3986).

  Encoding defaults to `:www_form` for backward compatibility.

  ## Examples

      iex> URI.query_decoder("foo=1&bar=2") |> Enum.to_list()
      [{"foo", "1"}, {"bar", "2"}]

      iex> URI.query_decoder("food=bread%26butter&drinks=tap%20water+please") |> Enum.to_list()
      [{"food", "bread&butter"}, {"drinks", "tap water please"}]

      iex> URI.query_decoder("food=bread%26butter&drinks=tap%20water+please", :rfc_3986) |> Enum.to_list()
      [{"food", "bread&butter"}, {"drinks", "tap water+please"}]

  """
  @spec query_decoder(binary, :rfc_3986 | :www_form) :: Enumerable.t()
  def query_decoder(query, encoding \\ :www_form) when is_binary(query) do
    Stream.unfold(query, &decode_next_query_pair(&1, encoding))
  end

  defp decode_next_query_pair("", _encoding) do
    nil
  end

  defp decode_next_query_pair(query, encoding) do
    {undecoded_next_pair, rest} =
      case :binary.split(query, "&") do
        [next_pair, rest] -> {next_pair, rest}
        [next_pair] -> {next_pair, ""}
      end

    next_pair =
      case :binary.split(undecoded_next_pair, "=") do
        [key, value] ->
          {decode_with_encoding(key, encoding), decode_with_encoding(value, encoding)}

        [key] ->
          {decode_with_encoding(key, encoding), ""}
      end

    {next_pair, rest}
  end

  defp decode_with_encoding(string, :www_form) do
    decode_www_form(string)
  end

  defp decode_with_encoding(string, :rfc_3986) do
    decode(string)
  end

  @doc ~s"""
  Checks if `character` is a reserved one in a URI.

  As specified in [RFC 3986, section 2.2](https://tools.ietf.org/html/rfc3986#section-2.2),
  the following characters are reserved: #{@formatted_reserved_characters}

  ## Examples

      iex> URI.char_reserved?(?+)
      true

  """
  @spec char_reserved?(byte) :: boolean
  def char_reserved?(character) do
    character in @reserved_characters
  end

  @doc """
  Checks if `character` is an unreserved one in a URI.

  As specified in [RFC 3986, section 2.3](https://tools.ietf.org/html/rfc3986#section-2.3),
  the following characters are unreserved:

    * Alphanumeric characters: `A-Z`, `a-z`, `0-9`
    * `~`, `_`, `-`, `.`

  ## Examples

      iex> URI.char_unreserved?(?_)
      true

  """
  @spec char_unreserved?(byte) :: boolean
  def char_unreserved?(character) do
    character in ?0..?9 or character in ?a..?z or character in ?A..?Z or character in '~_-.'
  end

  @doc """
  Checks if `character` is allowed unescaped in a URI.

  This is the default used by `URI.encode/2` where both
  [reserved](`char_reserved?/1`) and [unreserved characters](`char_unreserved?/1`)
  are kept unescaped.

  ## Examples

      iex> URI.char_unescaped?(?{)
      false

  """
  @spec char_unescaped?(byte) :: boolean
  def char_unescaped?(character) do
    char_reserved?(character) or char_unreserved?(character)
  end

  @doc """
  Percent-escapes all characters that require escaping in `string`.

  This means reserved characters, such as `:` and `/`, and the
  so-called unreserved characters, which have the same meaning both
  escaped and unescaped, won't be escaped by default.

  See `encode_www_form/1` if you are interested in escaping reserved
  characters too.

  This function also accepts a `predicate` function as an optional
  argument. If passed, this function will be called with each byte
  in `string` as its argument and should return a truthy value (anything other
  than `false` or `nil`) if the given byte should be left as is, or return a
  falsy value (`false` or `nil`) if the character should be escaped. Defaults
  to `URI.char_unescaped?/1`.

  ## Examples

      iex> URI.encode("ftp://s-ite.tld/?value=put it+й")
      "ftp://s-ite.tld/?value=put%20it+%D0%B9"

      iex> URI.encode("a string", &(&1 != ?i))
      "a str%69ng"

  """
  @spec encode(binary, (byte -> as_boolean(term))) :: binary
  def encode(string, predicate \\ &char_unescaped?/1)
      when is_binary(string) and is_function(predicate, 1) do
    for <<byte <- string>>, into: "", do: percent(byte, predicate)
  end

  @doc """
  Encodes `string` as "x-www-form-urlencoded".

  Note "x-www-form-urlencoded" is not specified as part of
  RFC 3986. However, it is a commonly used format to encode
  query strings and form data by browsers.

  ## Example

      iex> URI.encode_www_form("put: it+й")
      "put%3A+it%2B%D0%B9"

  """
  @spec encode_www_form(binary) :: binary
  def encode_www_form(string) when is_binary(string) do
    for <<byte <- string>>, into: "" do
      case percent(byte, &char_unreserved?/1) do
        "%20" -> "+"
        percent -> percent
      end
    end
  end

  defp percent(char, predicate) do
    if predicate.(char) do
      <<char>>
    else
      <<"%", hex(bsr(char, 4)), hex(band(char, 15))>>
    end
  end

  defp hex(n) when n <= 9, do: n + ?0
  defp hex(n), do: n + ?A - 10

  @doc """
  Percent-unescapes a URI.

  ## Examples

      iex> URI.decode("https%3A%2F%2Felixir-lang.org")
      "https://elixir-lang.org"

  """
  @spec decode(binary) :: binary
  def decode(uri) do
    unpercent(uri, "", false)
  catch
    :malformed_uri ->
      raise ArgumentError, "malformed URI #{inspect(uri)}"
  end

  @doc """
  Decodes `string` as "x-www-form-urlencoded".

  Note "x-www-form-urlencoded" is not specified as part of
  RFC 3986. However, it is a commonly used format to encode
  query strings and form data by browsers.

  ## Examples

      iex> URI.decode_www_form("%3Call+in%2F")
      "<all in/"

  """
  @spec decode_www_form(binary) :: binary
  def decode_www_form(string) when is_binary(string) do
    unpercent(string, "", true)
  catch
    :malformed_uri ->
      raise ArgumentError, "malformed URI #{inspect(string)}"
  end

  defp unpercent(<<?+, tail::binary>>, acc, spaces = true) do
    unpercent(tail, <<acc::binary, ?\s>>, spaces)
  end

  defp unpercent(<<?%, hex1, hex2, tail::binary>>, acc, spaces) do
    unpercent(tail, <<acc::binary, bsl(hex_to_dec(hex1), 4) + hex_to_dec(hex2)>>, spaces)
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
  in [RFC 3986, Appendix B](https://tools.ietf.org/html/rfc3986#appendix-B).

  When a URI is given without a port, the value returned by
  `URI.default_port/1` for the URI's scheme is used for the `:port` field.

  If a `%URI{}` struct is given to this function, this function returns it
  unmodified.

  ## Examples

      iex> URI.parse("https://elixir-lang.org/")
      %URI{
        authority: "elixir-lang.org",
        fragment: nil,
        host: "elixir-lang.org",
        path: "/",
        port: 443,
        query: nil,
        scheme: "https",
        userinfo: nil
      }

      iex> URI.parse("//elixir-lang.org/")
      %URI{
        authority: "elixir-lang.org",
        fragment: nil,
        host: "elixir-lang.org",
        path: "/",
        port: nil,
        query: nil,
        scheme: nil,
        userinfo: nil
      }

      iex> URI.parse("/foo/bar")
      %URI{
        authority: nil,
        fragment: nil,
        host: nil,
        path: "/foo/bar",
        port: nil,
        query: nil,
        scheme: nil,
        userinfo: nil
      }

      iex> URI.parse("foo/bar")
      %URI{
        authority: nil,
        fragment: nil,
        host: nil,
        path: "foo/bar",
        port: nil,
        query: nil,
        scheme: nil,
        userinfo: nil
      }

  """
  @spec parse(t | binary) :: t
  def parse(%URI{} = uri), do: uri

  def parse(string) when is_binary(string) do
    # From https://tools.ietf.org/html/rfc3986#appendix-B
    # Parts:    12                        3  4          5       6  7        8 9
    regex = ~r{^(([a-z][a-z0-9\+\-\.]*):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?}i

    parts = Regex.run(regex, string)

    destructure [
                  _full,
                  # 1
                  _scheme_with_colon,
                  # 2
                  scheme,
                  # 3
                  authority_with_slashes,
                  # 4
                  _authority,
                  # 5
                  path,
                  # 6
                  query_with_question_mark,
                  # 7
                  _query,
                  # 8
                  _fragment_with_hash,
                  # 9
                  fragment
                ],
                parts

    scheme = nillify(scheme)
    path = nillify(path)
    query = nillify_query(query_with_question_mark)
    {authority, userinfo, host, port} = split_authority(authority_with_slashes)

    scheme = scheme && String.downcase(scheme)
    port = port || (scheme && default_port(scheme))

    %URI{
      scheme: scheme,
      path: path,
      query: query,
      fragment: fragment,
      authority: authority,
      userinfo: userinfo,
      host: host,
      port: port
    }
  end

  defp nillify_query("?" <> query), do: query
  defp nillify_query(_other), do: nil

  # Split an authority into its userinfo, host and port parts.
  defp split_authority("") do
    {nil, nil, nil, nil}
  end

  defp split_authority("//") do
    {"", nil, "", nil}
  end

  defp split_authority("//" <> authority) do
    regex = ~r/(^(.*)@)?(\[[a-zA-Z0-9:.]*\]|[^:]*)(:(\d*))?/
    components = Regex.run(regex, authority)

    destructure [_, _, userinfo, host, _, port], components
    userinfo = nillify(userinfo)
    host = if nillify(host), do: host |> String.trim_leading("[") |> String.trim_trailing("]")
    port = if nillify(port), do: String.to_integer(port)

    {authority, userinfo, host, port}
  end

  # Regex.run returns empty strings sometimes. We want
  # to replace those with nil for consistency.
  defp nillify(""), do: nil
  defp nillify(other), do: other

  @doc """
  Returns the string representation of the given [URI struct](`t:t/0`).

  ## Examples

      iex> uri = URI.parse("http://google.com")
      iex> URI.to_string(uri)
      "http://google.com"

      iex> uri = URI.parse("foo://bar.baz")
      iex> URI.to_string(uri)
      "foo://bar.baz"

  Note that when creating this string representation, the `:authority` value will be
  used if the `:host` is `nil`. Otherwise, the `:userinfo`, `:host`, and `:port` will
  be used.

      iex> URI.to_string(%URI{authority: "foo@example.com:80"})
      "//foo@example.com:80"

      iex> URI.to_string(%URI{userinfo: "bar", host: "example.org", port: 81})
      "//bar@example.org:81"

      iex> URI.to_string(%URI{
      ...>   authority: "foo@example.com:80",
      ...>   userinfo: "bar",
      ...>   host: "example.org",
      ...>   port: 81
      ...> })
      "//bar@example.org:81"

  """
  @spec to_string(t) :: binary
  defdelegate to_string(uri), to: String.Chars.URI

  @doc ~S"""
  Merges two URIs.

  This function merges two URIs as per
  [RFC 3986, section 5.2](https://tools.ietf.org/html/rfc3986#section-5.2).

  ## Examples

      iex> URI.merge(URI.parse("http://google.com"), "/query") |> to_string()
      "http://google.com/query"

      iex> URI.merge("http://example.com", "http://google.com") |> to_string()
      "http://google.com"

  """
  @spec merge(t | binary, t | binary) :: t
  def merge(uri, rel)

  def merge(%URI{authority: nil}, _rel) do
    raise ArgumentError, "you must merge onto an absolute URI"
  end

  def merge(_base, %URI{scheme: rel_scheme} = rel) when rel_scheme != nil do
    %{rel | path: remove_dot_segments_from_path(rel.path)}
  end

  def merge(base, %URI{authority: authority} = rel) when authority != nil do
    %{rel | scheme: base.scheme, path: remove_dot_segments_from_path(rel.path)}
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

  defp merge_paths(nil, rel_path), do: merge_paths("/", rel_path)
  defp merge_paths(_, "/" <> _ = rel_path), do: remove_dot_segments_from_path(rel_path)

  defp merge_paths(base_path, rel_path) do
    [_ | base_segments] = path_to_segments(base_path)

    path_to_segments(rel_path)
    |> Kernel.++(base_segments)
    |> remove_dot_segments([])
    |> Enum.join("/")
  end

  defp remove_dot_segments_from_path(nil) do
    nil
  end

  defp remove_dot_segments_from_path(path) do
    path
    |> path_to_segments()
    |> remove_dot_segments([])
    |> Enum.join("/")
  end

  defp remove_dot_segments([], [head, ".." | acc]), do: remove_dot_segments([], [head | acc])
  defp remove_dot_segments([], acc), do: acc
  defp remove_dot_segments(["." | tail], acc), do: remove_dot_segments(tail, acc)

  defp remove_dot_segments([head | tail], ["..", ".." | _] = acc),
    do: remove_dot_segments(tail, [head | acc])

  defp remove_dot_segments(segments, [_, ".." | acc]), do: remove_dot_segments(segments, acc)
  defp remove_dot_segments([head | tail], acc), do: remove_dot_segments(tail, [head | acc])

  defp path_to_segments(path) do
    [head | tail] = String.split(path, "/")
    reverse_and_discard_empty(tail, [head])
  end

  defp reverse_and_discard_empty([], acc), do: acc
  defp reverse_and_discard_empty([head], acc), do: [head | acc]
  defp reverse_and_discard_empty(["" | tail], acc), do: reverse_and_discard_empty(tail, acc)

  defp reverse_and_discard_empty([head | tail], acc),
    do: reverse_and_discard_empty(tail, [head | acc])
end

defimpl String.Chars, for: URI do
  def to_string(%{host: host, authority: authority, path: path} = uri)
      when (host != nil or authority != nil) and is_binary(path) and
             path != "" and binary_part(path, 0, 1) != "/" do
    raise ArgumentError,
          ":path in URI must be nil or an absolute path if :host or :authority are given, " <>
            "got: #{inspect(uri)}"
  end

  def to_string(%{scheme: scheme, port: port, path: path, query: query, fragment: fragment} = uri) do
    uri =
      case scheme && URI.default_port(scheme) do
        ^port -> %{uri | port: nil}
        _ -> uri
      end

    # Based on https://tools.ietf.org/html/rfc3986#section-5.3
    authority = extract_authority(uri)

    IO.iodata_to_binary([
      if(scheme, do: [scheme, ?:], else: []),
      if(authority, do: ["//" | authority], else: []),
      if(path, do: path, else: []),
      if(query, do: ["?" | query], else: []),
      if(fragment, do: ["#" | fragment], else: [])
    ])
  end

  defp extract_authority(%{host: nil, authority: authority}) do
    authority
  end

  defp extract_authority(%{host: host, userinfo: userinfo, port: port}) do
    # According to the grammar at
    # https://tools.ietf.org/html/rfc3986#appendix-A, a "host" can have a colon
    # in it only if it's an IPv6 or "IPvFuture" address, so if there's a colon
    # in the host we can safely surround it with [].
    [
      if(userinfo, do: [userinfo | "@"], else: []),
      if(String.contains?(host, ":"), do: ["[", host | "]"], else: host),
      if(port, do: [":" | Integer.to_string(port)], else: [])
    ]
  end
end
