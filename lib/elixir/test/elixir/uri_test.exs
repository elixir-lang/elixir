Code.require_file "test_helper.exs", __DIR__

defmodule URITest do
  use ExUnit.Case, async: true

  test :encode_with_binary do
    raw = <<13, 10, 38, 60, 62, 34, 32, 227, 130, 134, 227, 130, 147, 227, 130, 134, 227, 130, 147>>
    expected = "%0D%0A%26%3C%3E%22+%E3%82%86%E3%82%93%E3%82%86%E3%82%93"
    assert URI.encode(raw) == expected
  end

  test :encode_query do
    assert URI.encode_query([{:foo, :bar}, {:baz, :quux}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{"foo", "bar"}, {"baz", "quux"}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{'foo', 'bar'}, {'baz', 'quux'}]) == "foo=bar&baz=quux"
  end

  test :encode_query_mixed do
    assert URI.encode_query([{"foo", :bar}]) == "foo=bar"
    assert URI.encode_query([{"foo", 'bar'}]) == "foo=bar"
    assert URI.encode_query([{:foo, "bar"}]) == "foo=bar"
    assert URI.encode_query([{:foo, 'bar'}]) == "foo=bar"
  end

  test :decode_query do
    assert HashDict.equal?(URI.decode_query("q=search%20query&cookie=ab%26cd&block%20buster="),
      HashDict.new [{"block buster", ""}, {"cookie", "ab&cd"}, {"q", "search query"}])
    assert HashDict.equal?(URI.decode_query(""), HashDict.new)
    assert HashDict.equal?(URI.decode_query("something=weird%3Dhappening"), HashDict.new [{"something", "weird=happening"}])

    assert URI.decode_query("", []) == []

    assert HashDict.equal?(URI.decode_query("garbage"), HashDict.new [{"garbage", nil}])
    assert HashDict.equal?(URI.decode_query("=value"), HashDict.new [{"", "value"}])
    assert HashDict.equal?(URI.decode_query("something=weird=happening"), HashDict.new [{"something", "weird=happening"}])
  end

  test :decoder do
    decoder  = URI.query_decoder("q=search%20query&cookie=ab%26cd&block%20buster=")
    expected = [{"q", "search query"}, {"cookie", "ab&cd"}, {"block buster", ""}]
    assert Enum.map(decoder, fn(x) -> x end) == expected
  end

  test :decode do
    data_to_be_decoded = "%26%3C%3E%22+%E3%82%86%E3%82%93%E3%82%86%E3%82%93"
    assert URI.decode(data_to_be_decoded) == "&<>\" ゆんゆん"
  end

  test :parse_http do
    assert URI.Info[scheme: "http", host: "foo.com", path: "/path/to/something",
                    query: "foo=bar&bar=foo", fragment: "fragment", port: 80,
                    authority: "foo.com", userinfo: nil] ==
                URI.parse("http://foo.com/path/to/something?foo=bar&bar=foo#fragment")
  end

  test :parse_https do
    assert URI.Info[scheme: "https", host: "foo.com", authority: "foo.com",
                    query: nil, fragment: nil, port: 443, path: nil, userinfo: nil] ==
                 URI.parse("https://foo.com")
  end

  test :parse_file do
    assert URI.Info[scheme: "file", host: nil, path: "/foo/bar/baz", userinfo: nil,
                    query: nil, fragment: nil, port: nil, authority: nil] ==
                 URI.parse("file:///foo/bar/baz")
  end

  test :parse_ftp do
    assert URI.Info[scheme: "ftp", host: "private.ftp-servers.example.com",
                    userinfo: "user001:secretpassword", authority: "user001:secretpassword@private.ftp-servers.example.com",
                    path: "/mydirectory/myfile.txt", query: nil, fragment: nil,
                    port: 21] ==
                 URI.parse("ftp://user001:secretpassword@private.ftp-servers.example.com/mydirectory/myfile.txt")
  end

  test :parse_sftp do
    assert URI.Info[scheme: "sftp", host: "private.ftp-servers.example.com",
                    userinfo: "user001:secretpassword", authority: "user001:secretpassword@private.ftp-servers.example.com",
                    path: "/mydirectory/myfile.txt", query: nil, fragment: nil,
                    port: 22] ==
                 URI.parse("sftp://user001:secretpassword@private.ftp-servers.example.com/mydirectory/myfile.txt")
  end

  test :parse_tftp do
    assert URI.Info[scheme: "tftp", host: "private.ftp-servers.example.com",
                    userinfo: "user001:secretpassword", authority: "user001:secretpassword@private.ftp-servers.example.com",
                    path: "/mydirectory/myfile.txt", query: nil, fragment: nil, port: 69] ==
                 URI.parse("tftp://user001:secretpassword@private.ftp-servers.example.com/mydirectory/myfile.txt")
  end


  test :parse_ldap do
    assert URI.Info[scheme: "ldap", host: nil, authority: nil, userinfo: nil,
                    path: "/dc=example,dc=com", query: "?sub?(givenName=John)",
                    fragment: nil, port: 389] ==
                 URI.parse("ldap:///dc=example,dc=com??sub?(givenName=John)")
    assert URI.Info[scheme: "ldap", host: "ldap.example.com", authority: "ldap.example.com",
                    userinfo: nil, path: "/cn=John%20Doe,dc=example,dc=com", fragment: nil,
                    port: 389, query: nil] ==
                 URI.parse("ldap://ldap.example.com/cn=John%20Doe,dc=example,dc=com")
  end

  test :parse_splits_authority do
    assert URI.Info[scheme: "http", host: "foo.com", path: nil,
                    query: nil, fragment: nil, port: 4444,
                    authority: "foo:bar@foo.com:4444",
                    userinfo: "foo:bar"] ==
                 URI.parse("http://foo:bar@foo.com:4444")
    assert URI.Info[scheme: "https", host: "foo.com", path: nil,
                    query: nil, fragment: nil, port: 443,
                    authority: "foo:bar@foo.com", userinfo: "foo:bar"] ==
                 URI.parse("https://foo:bar@foo.com")
    assert URI.Info[scheme: "http", host: "foo.com", path: nil,
                    query: nil, fragment: nil, port: 4444,
                    authority: "foo.com:4444",
                    userinfo: nil] ==
                 URI.parse("http://foo.com:4444")
  end

  test :parse_bad_uris do
    assert URI.parse("https:??@?F?@#>F//23/")
    assert URI.parse("")
    assert URI.parse(":https")
    assert URI.parse("https")
  end

  test :downcase_properly do
    assert URI.parse("hTtP://google.com").scheme == "http"
    assert URI.parse("http://GoOgLe.CoM").host == "google.com"
    assert URI.parse("http://LOL:wut@GoOgLe.CoM").authority == "LOL:wut@google.com"
  end

  test :to_binary do
    assert to_binary(URI.parse("http://google.com")) == "http://google.com"
    assert to_binary(URI.parse("http://google.com:443")) == "http://google.com:443"
    assert to_binary(URI.parse("https://google.com:443")) == "https://google.com"
    assert to_binary(URI.parse("http://lol:wut@google.com")) == "http://lol:wut@google.com"
    assert to_binary(URI.parse("http://google.com/elixir")) == "http://google.com/elixir"
    assert to_binary(URI.parse("http://google.com?q=lol")) == "http://google.com?q=lol"
    assert to_binary(URI.parse("http://google.com?q=lol#omg")) == "http://google.com?q=lol#omg"
  end
end
