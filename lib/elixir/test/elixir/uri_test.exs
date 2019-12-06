Code.require_file("test_helper.exs", __DIR__)

defmodule URITest do
  use ExUnit.Case, async: true

  doctest URI

  test "encode/1,2" do
    assert URI.encode("4_test.is-s~") == "4_test.is-s~"

    assert URI.encode("\r\n&<%>\" ゆ", &URI.char_unreserved?/1) ==
             "%0D%0A%26%3C%25%3E%22%20%E3%82%86"
  end

  test "encode_www_form/1" do
    assert URI.encode_www_form("4test ~1.x") == "4test+~1.x"
    assert URI.encode_www_form("poll:146%") == "poll%3A146%25"
    assert URI.encode_www_form("/\n+/ゆ") == "%2F%0A%2B%2F%E3%82%86"
  end

  test "encode_query/1" do
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

  test "decode_query/1,2" do
    assert URI.decode_query("", %{}) == %{}

    assert URI.decode_query("safe=off", %{"cookie" => "foo"}) ==
             %{"safe" => "off", "cookie" => "foo"}

    assert URI.decode_query("q=search%20query&cookie=ab%26cd&block+buster=") ==
             %{"block buster" => "", "cookie" => "ab&cd", "q" => "search query"}

    assert URI.decode_query("something=weird%3Dhappening") == %{"something" => "weird=happening"}

    assert URI.decode_query("garbage") == %{"garbage" => nil}
    assert URI.decode_query("=value") == %{"" => "value"}
    assert URI.decode_query("something=weird=happening") == %{"something" => "weird=happening"}
  end

  test "query_decoder/1" do
    decoder = URI.query_decoder("q=search%20query&cookie=ab%26cd&block%20buster=")
    expected = [{"q", "search query"}, {"cookie", "ab&cd"}, {"block buster", ""}]
    assert Enum.map(decoder, & &1) == expected
  end

  test "decode/1" do
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

  test "decode_www_form/1" do
    assert URI.decode_www_form("%3Eval+ue%2B") == ">val ue+"
    assert URI.decode_www_form("%E3%82%86+") == "ゆ "

    assert_raise ArgumentError, fn ->
      URI.decode_www_form("%ZZ")
    end
  end

  describe "parse/1" do
    test "returns the given URI if a %URI{} struct is given" do
      assert URI.parse(uri = %URI{scheme: "http", host: "foo.com"}) == uri
    end

    test "works with HTTP scheme" do
      expected_uri = %URI{
        scheme: "http",
        host: "foo.com",
        path: "/path/to/something",
        query: "foo=bar&bar=foo",
        fragment: "fragment",
        port: 80,
        authority: "foo.com",
        userinfo: nil
      }

      assert URI.parse("http://foo.com/path/to/something?foo=bar&bar=foo#fragment") ==
               expected_uri
    end

    test "works with HTTPS scheme" do
      expected_uri = %URI{
        scheme: "https",
        host: "foo.com",
        authority: "foo.com",
        query: nil,
        fragment: nil,
        port: 443,
        path: nil,
        userinfo: nil
      }

      assert URI.parse("https://foo.com") == expected_uri
    end

    test "works with \"file\" scheme" do
      expected_uri = %URI{
        scheme: "file",
        host: "",
        path: "/foo/bar/baz",
        userinfo: nil,
        query: nil,
        fragment: nil,
        port: nil,
        authority: ""
      }

      assert URI.parse("file:///foo/bar/baz") == expected_uri
    end

    test "works with FTP scheme" do
      expected_uri = %URI{
        scheme: "ftp",
        host: "private.ftp-server.example.com",
        userinfo: "user001:password",
        authority: "user001:password@private.ftp-server.example.com",
        path: "/my_directory/my_file.txt",
        query: nil,
        fragment: nil,
        port: 21
      }

      ftp = "ftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
      assert URI.parse(ftp) == expected_uri
    end

    test "works with SFTP scheme" do
      expected_uri = %URI{
        scheme: "sftp",
        host: "private.ftp-server.example.com",
        userinfo: "user001:password",
        authority: "user001:password@private.ftp-server.example.com",
        path: "/my_directory/my_file.txt",
        query: nil,
        fragment: nil,
        port: 22
      }

      sftp = "sftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
      assert URI.parse(sftp) == expected_uri
    end

    test "works with TFTP scheme" do
      expected_uri = %URI{
        scheme: "tftp",
        host: "private.ftp-server.example.com",
        userinfo: "user001:password",
        authority: "user001:password@private.ftp-server.example.com",
        path: "/my_directory/my_file.txt",
        query: nil,
        fragment: nil,
        port: 69
      }

      tftp = "tftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
      assert URI.parse(tftp) == expected_uri
    end

    test "works with LDAP scheme" do
      expected_uri = %URI{
        scheme: "ldap",
        host: "",
        authority: "",
        userinfo: nil,
        path: "/dc=example,dc=com",
        query: "?sub?(givenName=John)",
        fragment: nil,
        port: 389
      }

      assert URI.parse("ldap:///dc=example,dc=com??sub?(givenName=John)") == expected_uri

      expected_uri = %URI{
        scheme: "ldap",
        host: "ldap.example.com",
        authority: "ldap.example.com",
        userinfo: nil,
        path: "/cn=John%20Doe,dc=foo,dc=com",
        fragment: nil,
        port: 389,
        query: nil
      }

      assert URI.parse("ldap://ldap.example.com/cn=John%20Doe,dc=foo,dc=com") == expected_uri
    end

    test "splits authority" do
      expected_uri = %URI{
        scheme: "http",
        host: "foo.com",
        path: nil,
        query: nil,
        fragment: nil,
        port: 4444,
        authority: "foo:bar@foo.com:4444",
        userinfo: "foo:bar"
      }

      assert URI.parse("http://foo:bar@foo.com:4444") == expected_uri

      expected_uri = %URI{
        scheme: "https",
        host: "foo.com",
        path: nil,
        query: nil,
        fragment: nil,
        port: 443,
        authority: "foo:bar@foo.com",
        userinfo: "foo:bar"
      }

      assert URI.parse("https://foo:bar@foo.com") == expected_uri

      expected_uri = %URI{
        scheme: "http",
        host: "foo.com",
        path: nil,
        query: nil,
        fragment: nil,
        port: 4444,
        authority: "foo.com:4444",
        userinfo: nil
      }

      assert URI.parse("http://foo.com:4444") == expected_uri
    end

    test "can parse bad URIs" do
      assert URI.parse("")
      assert URI.parse("https:??@?F?@#>F//23/")

      assert URI.parse(":https").path == ":https"
      assert URI.parse("https").path == "https"
      assert URI.parse("ht\0tps://foo.com").path == "ht\0tps://foo.com"
    end

    test "can parse IPv6 addresses" do
      addresses = [
        # undefined
        "::",
        # loopback
        "::1",
        # unicast
        "1080::8:800:200C:417A",
        # multicast
        "FF01::101",
        # abbreviated
        "2607:f3f0:2:0:216:3cff:fef0:174a",
        # mixed hex case
        "2607:f3F0:2:0:216:3cFf:Fef0:174A",
        # complete
        "2051:0db8:2d5a:3521:8313:ffad:1242:8e2e",
        # embedded IPv4
        "::00:192.168.10.184"
      ]

      Enum.each(addresses, fn addr ->
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
      end)
    end

    test "downcases the scheme" do
      assert URI.parse("hTtP://google.com").scheme == "http"
    end

    test "preserves empty fragments" do
      assert URI.parse("http://example.com#").fragment == ""
      assert URI.parse("http://example.com/#").fragment == ""
      assert URI.parse("http://example.com/test#").fragment == ""
    end

    test "preserves an empty query" do
      assert URI.parse("http://foo.com/?").query == ""
    end
  end

  test "default_port/1,2" do
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

  test "to_string/1 and Kernel.to_string/1" do
    assert to_string(URI.parse("http://google.com")) == "http://google.com"
    assert to_string(URI.parse("http://google.com:443")) == "http://google.com:443"
    assert to_string(URI.parse("https://google.com:443")) == "https://google.com"
    assert to_string(URI.parse("file:/path")) == "file:/path"
    assert to_string(URI.parse("file:///path")) == "file:///path"
    assert to_string(URI.parse("file://///path")) == "file://///path"
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
    assert URI.to_string(URI.parse("gid:hello/123")) == "gid:hello/123"

    assert URI.to_string(URI.parse("//user:password@google.com/")) ==
             "//user:password@google.com/"

    assert_raise ArgumentError,
                 ~r":path in URI must be nil or an absolute path if :host or :authority are given",
                 fn -> %URI{authority: "foo.com", path: "hello/123"} |> URI.to_string() end

    assert_raise ArgumentError,
                 ~r":path in URI must be nil or an absolute path if :host or :authority are given",
                 fn -> %URI{host: "foo.com", path: "hello/123"} |> URI.to_string() end
  end

  test "merge/2" do
    assert_raise ArgumentError, "you must merge onto an absolute URI", fn ->
      URI.merge("/relative", "")
    end

    assert URI.merge("http://google.com/foo", "http://example.com/baz")
           |> to_string == "http://example.com/baz"

    assert URI.merge("http://google.com/foo", "http://example.com/.././bar/../../baz")
           |> to_string == "http://example.com/baz"

    assert URI.merge("http://google.com/foo", "//example.com/baz")
           |> to_string == "http://example.com/baz"

    assert URI.merge("http://google.com/foo", "//example.com/.././bar/../../../baz")
           |> to_string == "http://example.com/baz"

    assert URI.merge("http://example.com", URI.parse("/foo"))
           |> to_string == "http://example.com/foo"

    assert URI.merge("http://example.com", URI.parse("/.././bar/../../../baz"))
           |> to_string == "http://example.com/baz"

    base = URI.parse("http://example.com/foo/bar")
    assert URI.merge(base, "") |> to_string == "http://example.com/foo/bar"
    assert URI.merge(base, "#fragment") |> to_string == "http://example.com/foo/bar#fragment"
    assert URI.merge(base, "?query") |> to_string == "http://example.com/foo/bar?query"
    assert URI.merge(base, %URI{path: ""}) |> to_string == "http://example.com/foo/bar"

    assert URI.merge(base, %URI{path: "", fragment: "fragment"})
           |> to_string == "http://example.com/foo/bar#fragment"

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
