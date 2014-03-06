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
    assert URI.encode_query([{"foo", :bar}]) == "foo=bar"

    assert_raise ArgumentError, fn ->
      URI.encode_query([{"foo", 'bar'}])
    end
  end

  test :decode_query do
    assert URI.decode_query("", []) == []
    assert URI.decode_query("", %{}) == %{}

    assert URI.decode_query("q=search%20query&cookie=ab%26cd&block%20buster=") ==
           %{"block buster" => "", "cookie" => "ab&cd", "q" => "search query"}

    assert URI.decode_query("something=weird%3Dhappening") ==
           %{"something" => "weird=happening"}

    assert URI.decode_query("garbage") ==
           %{"garbage" => nil}
    assert URI.decode_query("=value") ==
           %{"" => "value"}
    assert URI.decode_query("something=weird=happening") ==
           %{"something" => "weird=happening"}
  end

  test :decoder do
    decoder  = URI.query_decoder("q=search%20query&cookie=ab%26cd&block%20buster=")
    expected = [{"q", "search query"}, {"cookie", "ab&cd"}, {"block buster", ""}]
    assert Enum.map(decoder, fn(x) -> x end) == expected
  end

  test :decode do
    data_to_be_decoded = "%26%3C%3E%22+%E3%82%86%E3%82%93%E3%82%86%E3%82%93"
    assert URI.decode(data_to_be_decoded) == "&<>\" ゆんゆん"

    assert_raise ArgumentError, ~r/malformed URI/, fn ->
      assert URI.decode("% invalid")
    end
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

  test :default_port do
    assert URI.default_port("http") == 80
    assert URI.default_port("unknown") == nil

    URI.default_port("unknown", 13)
    assert URI.default_port("unknown") == 13
  end

  test :parse_bad_uris do
    assert URI.parse("https:??@?F?@#>F//23/")
    assert URI.parse("")
    assert URI.parse(":https")
    assert URI.parse("https")
  end

  test :ipv6_addresses do
    addrs = [
      "::",                                      # undefined
      "::1",                                     # loopback
      "1080::8:800:200C:417A",                   # unicast
      "FF01::101",                               # multicast
      "2607:f3f0:2:0:216:3cff:fef0:174a",        # abbreviated
      "2607:f3F0:2:0:216:3cFf:Fef0:174A",        # mixed hex case
      "2051:0db8:2d5a:3521:8313:ffad:1242:8e2e", # complete
      "::00:192.168.10.184"                      # embedded IPv4
    ]

    Enum.each addrs, fn(addr) ->
      simple_uri = URI.parse("http://[#{addr}]/")
      assert simple_uri.host == addr

      userinfo_uri = URI.parse("http://user:pass@[#{addr}]/")
      assert userinfo_uri.host == addr
      assert userinfo_uri.userinfo == "user:pass"

      port_uri = URI.parse("http://[#{addr}]:2222/")
      assert port_uri.host == addr
      assert port_uri.port == 2222

      userinfo_port_uri = URI.parse("http://user:pass@[#{addr}]:2222/")
      assert userinfo_port_uri.host == addr
      assert userinfo_port_uri.userinfo == "user:pass"
      assert userinfo_port_uri.port == 2222
    end
  end

  test :downcase_scheme do
    assert URI.parse("hTtP://google.com").scheme == "http"
  end

  test :to_string do
    assert to_string(URI.parse("http://google.com")) == "http://google.com"
    assert to_string(URI.parse("http://google.com:443")) == "http://google.com:443"
    assert to_string(URI.parse("https://google.com:443")) == "https://google.com"
    assert to_string(URI.parse("http://lol:wut@google.com")) == "http://lol:wut@google.com"
    assert to_string(URI.parse("http://google.com/elixir")) == "http://google.com/elixir"
    assert to_string(URI.parse("http://google.com?q=lol")) == "http://google.com?q=lol"
    assert to_string(URI.parse("http://google.com?q=lol#omg")) == "http://google.com?q=lol#omg"
  end

  test :escape do
    assert URI.decode("%2f%41%4a%55") == "/AJU"
    assert URI.decode("%2F%41%4A%55") == "/AJU"
  end
end
