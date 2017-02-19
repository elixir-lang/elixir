Code.require_file "test_helper.exs", __DIR__

defmodule URITest do
  use ExUnit.Case, async: true

  doctest URI

  test "encode" do
    assert URI.encode("4_test.is-s~") == "4_test.is-s~"
    assert URI.encode("\r\n&<%>\" ゆ", &URI.char_unreserved?/1) ==
           "%0D%0A%26%3C%25%3E%22%20%E3%82%86"
  end

  test "encode WWW form" do
    assert URI.encode_www_form("4test ~1.x") == "4test+~1.x"
    assert URI.encode_www_form("poll:146%") == "poll%3A146%25"
    assert URI.encode_www_form("/\n+/ゆ") == "%2F%0A%2B%2F%E3%82%86"
  end

  test "encode query" do
    assert URI.encode_query([{:foo, :bar}, {:baz, :quux}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{"foo", "bar"}, {"baz", "quux"}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{"foo z", :bar}]) == "foo+z=bar"

    assert_raise ArgumentError, fn ->
      URI.encode_query([{"foo", 'bar'}])
    end

    assert_raise ArgumentError, fn ->
      URI.encode_query([{'foo', "bar"}])
    end
  end

  test "decode query" do
    assert URI.decode_query("", %{}) == %{}

    assert URI.decode_query("safe=off", %{"cookie" => "foo"}) ==
           %{"safe" => "off", "cookie" => "foo"}

    assert URI.decode_query("q=search%20query&cookie=ab%26cd&block+buster=") ==
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

  test "decoder" do
    decoder  = URI.query_decoder("q=search%20query&cookie=ab%26cd&block%20buster=")
    expected = [{"q", "search query"}, {"cookie", "ab&cd"}, {"block buster", ""}]
    assert Enum.map(decoder, &(&1)) == expected
  end

  test "decode" do
    assert URI.decode("%0D%0A%26%3C%25%3E%22%20%E3%82%86") == "\r\n&<%>\" ゆ"
    assert URI.decode("%2f%41%4a%55") == "/AJU"
    assert URI.decode("4_t+st.is-s~") == "4_t+st.is-s~"

    assert_raise ArgumentError, ~R/malformed URI/, fn ->
      URI.decode("% invalid")
    end
    assert_raise ArgumentError, ~R/malformed URI/, fn ->
      URI.decode("invalid%")
    end
  end

  test "decode WWW form" do
    assert URI.decode_www_form("%3Eval+ue%2B") == ">val ue+"
    assert URI.decode_www_form("%E3%82%86+") == "ゆ "

    assert_raise ArgumentError, fn ->
      URI.decode_www_form("%ZZ")
    end
  end

  test "parse URI" do
    assert URI.parse(uri = %URI{scheme: "http", host: "foo.com"}) == uri
  end

  test "parse HTTP" do
    assert %URI{scheme: "http", host: "foo.com", path: "/path/to/something",
                query: "foo=bar&bar=foo", fragment: "fragment", port: 80,
                authority: "foo.com", userinfo: nil} ==
           URI.parse("http://foo.com/path/to/something?foo=bar&bar=foo#fragment")
  end

  test "parse HTTPS" do
    assert %URI{scheme: "https", host: "foo.com", authority: "foo.com",
                query: nil, fragment: nil, port: 443, path: nil, userinfo: nil} ==
           URI.parse("https://foo.com")
  end

  test "parse file" do
    assert %URI{scheme: "file", host: nil, path: "/foo/bar/baz", userinfo: nil,
                query: nil, fragment: nil, port: nil, authority: nil} ==
           URI.parse("file:///foo/bar/baz")
  end

  test "parse FTP" do
    assert %URI{scheme: "ftp", host: "private.ftp-server.example.com",
                userinfo: "user001:password", authority: "user001:password@private.ftp-server.example.com",
                path: "/my_directory/my_file.txt", query: nil, fragment: nil,
                port: 21} ==
           URI.parse("ftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt")
  end

  test "parse SFTP" do
    assert %URI{scheme: "sftp", host: "private.ftp-server.example.com",
                userinfo: "user001:password", authority: "user001:password@private.ftp-server.example.com",
                path: "/my_directory/my_file.txt", query: nil, fragment: nil, port: 22} ==
           URI.parse("sftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt")
  end

  test "parse TFTP" do
    assert %URI{scheme: "tftp", host: "private.ftp-server.example.com",
                userinfo: "user001:password", authority: "user001:password@private.ftp-server.example.com",
                path: "/my_directory/my_file.txt", query: nil, fragment: nil, port: 69} ==
           URI.parse("tftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt")
  end


  test "parse LDAP" do
    assert %URI{scheme: "ldap", host: nil, authority: nil, userinfo: nil,
                path: "/dc=example,dc=com", query: "?sub?(givenName=John)",
                fragment: nil, port: 389} ==
           URI.parse("ldap:///dc=example,dc=com??sub?(givenName=John)")
    assert %URI{scheme: "ldap", host: "ldap.example.com", authority: "ldap.example.com",
                userinfo: nil, path: "/cn=John%20Doe,dc=example,dc=com", fragment: nil,
                port: 389, query: nil} ==
           URI.parse("ldap://ldap.example.com/cn=John%20Doe,dc=example,dc=com")
  end

  test "parse splits authority" do
    assert %URI{scheme: "http", host: "foo.com", path: nil,
                query: nil, fragment: nil, port: 4444,
                authority: "foo:bar@foo.com:4444",
                userinfo: "foo:bar"} ==
           URI.parse("http://foo:bar@foo.com:4444")
    assert %URI{scheme: "https", host: "foo.com", path: nil,
                query: nil, fragment: nil, port: 443,
                authority: "foo:bar@foo.com", userinfo: "foo:bar"} ==
           URI.parse("https://foo:bar@foo.com")
    assert %URI{scheme: "http", host: "foo.com", path: nil,
                query: nil, fragment: nil, port: 4444,
                authority: "foo.com:4444", userinfo: nil} ==
           URI.parse("http://foo.com:4444")
  end

  test "default port" do
    assert URI.default_port("http") == 80
    try do
      URI.default_port("http", 8000)
      assert URI.default_port("http") == 8000
    after
      URI.default_port("http", 80)
    end

    assert URI.default_port("unknown") == nil
    URI.default_port("unknown", 13)
    assert URI.default_port("unknown") == 13
  end

  test "parse bad URIs" do
    assert URI.parse("")
    assert URI.parse("https:??@?F?@#>F//23/")

    assert URI.parse(":https").path == ":https"
    assert URI.parse("https").path == "https"
    assert URI.parse("ht\0tps://foo.com").path == "ht\0tps://foo.com"
  end

  test "IPv6 addresses" do
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
      assert simple_uri.authority == "[#{addr}]"
      assert simple_uri.host == addr

      userinfo_uri = URI.parse("http://user:pass@[#{addr}]/")
      assert userinfo_uri.authority == "user:pass@[#{addr}]"
      assert userinfo_uri.host == addr
      assert userinfo_uri.userinfo == "user:pass"

      port_uri = URI.parse("http://[#{addr}]:2222/")
      assert port_uri.authority == "[#{addr}]:2222"
      assert port_uri.host == addr
      assert port_uri.port == 2222

      userinfo_port_uri = URI.parse("http://user:pass@[#{addr}]:2222/")
      assert userinfo_port_uri.authority == "user:pass@[#{addr}]:2222"
      assert userinfo_port_uri.host == addr
      assert userinfo_port_uri.userinfo == "user:pass"
      assert userinfo_port_uri.port == 2222
    end
  end

  test "downcase scheme" do
    assert URI.parse("hTtP://google.com").scheme == "http"
  end

  test "to string" do
    assert to_string(URI.parse("http://google.com")) == "http://google.com"
    assert to_string(URI.parse("http://google.com:443")) == "http://google.com:443"
    assert to_string(URI.parse("https://google.com:443")) == "https://google.com"
    assert to_string(URI.parse("http://lol:wut@google.com")) == "http://lol:wut@google.com"
    assert to_string(URI.parse("http://google.com/elixir")) == "http://google.com/elixir"
    assert to_string(URI.parse("http://google.com?q=lol")) == "http://google.com?q=lol"
    assert to_string(URI.parse("http://google.com?q=lol#omg")) == "http://google.com?q=lol#omg"
    assert to_string(URI.parse("//google.com/elixir")) == "//google.com/elixir"
    assert to_string(URI.parse("//google.com:8080/elixir")) == "//google.com:8080/elixir"
    assert to_string(URI.parse("//user:password@google.com/")) == "//user:password@google.com/"
    assert to_string(URI.parse("http://[2001:db8::]:8080")) == "http://[2001:db8::]:8080"
    assert to_string(URI.parse("http://[2001:db8::]")) == "http://[2001:db8::]"

    assert URI.to_string(URI.parse("http://google.com")) == "http://google.com"
    assert URI.to_string(URI.parse("//user:password@google.com/")) == "//user:password@google.com/"
  end

  test "merge/2" do
    assert_raise ArgumentError, "you must merge onto an absolute URI", fn ->
      URI.merge("/relative", "")
    end

    assert URI.merge("http://google.com/foo", "http://example.com/baz") |> to_string == "http://example.com/baz"
    assert URI.merge("http://google.com/foo", "//example.com/baz") |> to_string == "http://example.com/baz"

    assert URI.merge("http://example.com", URI.parse("/foo")) |> to_string == "http://example.com/foo"

    base = URI.parse("http://example.com/foo/bar")
    assert URI.merge(base, "") |> to_string == "http://example.com/foo/bar"
    assert URI.merge(base, "#fragment") |> to_string == "http://example.com/foo/bar#fragment"
    assert URI.merge(base, "?query") |> to_string == "http://example.com/foo/bar?query"
    assert URI.merge(base, %URI{path: ""}) |> to_string == "http://example.com/foo/bar"
    assert URI.merge(base, %URI{path: "", fragment: "fragment"}) |> to_string == "http://example.com/foo/bar#fragment"

    base = URI.parse("http://example.com")
    assert URI.merge(base, "/foo") |> to_string == "http://example.com/foo"
    assert URI.merge(base, "foo") |> to_string == "http://example.com/foo"

    base = URI.parse("http://example.com/foo/bar")
    assert URI.merge(base, "/baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, "baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, "../baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, ".././baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, "./baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, "bar/./baz") |> to_string == "http://example.com/foo/bar/baz"

    base = URI.parse("http://example.com/foo/bar/")
    assert URI.merge(base, "/baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, "baz") |> to_string == "http://example.com/foo/bar/baz"
    assert URI.merge(base, "../baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, ".././baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, "./baz") |> to_string == "http://example.com/foo/bar/baz"
    assert URI.merge(base, "bar/./baz") |> to_string == "http://example.com/foo/bar/bar/baz"

    base = URI.parse("http://example.com/foo/bar/baz")
    assert URI.merge(base, "../../foobar") |> to_string == "http://example.com/foobar"
    assert URI.merge(base, "../../../foobar") |> to_string == "http://example.com/foobar"
    assert URI.merge(base, "../../../../../../foobar") |> to_string == "http://example.com/foobar"

    base = URI.parse("http://example.com/foo/../bar")
    assert URI.merge(base, "baz") |> to_string == "http://example.com/baz"

    base = URI.parse("http://example.com/foo/./bar")
    assert URI.merge(base, "baz") |> to_string == "http://example.com/foo/baz"

    base = URI.parse("http://example.com/foo?query1")
    assert URI.merge(base, "?query2") |> to_string == "http://example.com/foo?query2"
    assert URI.merge(base, "") |> to_string == "http://example.com/foo?query1"

    base = URI.parse("http://example.com/foo#fragment1")
    assert URI.merge(base, "#fragment2") |> to_string == "http://example.com/foo#fragment2"
    assert URI.merge(base, "") |> to_string == "http://example.com/foo"
  end
end
