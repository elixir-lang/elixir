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

  test "encode_query/1,2" do
    assert URI.encode_query([{:foo, :bar}, {:baz, :quux}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{"foo", "bar"}, {"baz", "quux"}]) == "foo=bar&baz=quux"

    assert URI.encode_query([{"foo z", :bar}]) == "foo+z=bar"
    assert URI.encode_query([{"foo z", :bar}], :rfc3986) == "foo%20z=bar"
    assert URI.encode_query([{"foo z", :bar}], :www_form) == "foo+z=bar"

    assert URI.encode_query([{"foo[]", "+=/?&# Ñ"}]) ==
             "foo%5B%5D=%2B%3D%2F%3F%26%23+%C3%91"

    assert URI.encode_query([{"foo[]", "+=/?&# Ñ"}], :rfc3986) ==
             "foo%5B%5D=%2B%3D%2F%3F%26%23%20%C3%91"

    assert URI.encode_query([{"foo[]", "+=/?&# Ñ"}], :www_form) ==
             "foo%5B%5D=%2B%3D%2F%3F%26%23+%C3%91"

    assert_raise ArgumentError, fn ->
      URI.encode_query([{"foo", ~c"bar"}])
    end

    assert_raise ArgumentError, fn ->
      URI.encode_query([{~c"foo", "bar"}])
    end
  end

  test "decode_query/1,2,3" do
    assert URI.decode_query("", %{}) == %{}

    assert URI.decode_query("safe=off", %{"cookie" => "foo"}) ==
             %{"safe" => "off", "cookie" => "foo"}

    assert URI.decode_query("q=search%20query&cookie=ab%26cd&block+buster=") ==
             %{"block buster" => "", "cookie" => "ab&cd", "q" => "search query"}

    assert URI.decode_query("q=search%20query&cookie=ab%26cd&block+buster=", %{}, :rfc3986) ==
             %{"block+buster" => "", "cookie" => "ab&cd", "q" => "search query"}

    assert URI.decode_query("something=weird%3Dhappening") == %{"something" => "weird=happening"}

    assert URI.decode_query("=") == %{"" => ""}
    assert URI.decode_query("key") == %{"key" => ""}
    assert URI.decode_query("key=") == %{"key" => ""}
    assert URI.decode_query("=value") == %{"" => "value"}
    assert URI.decode_query("something=weird=happening") == %{"something" => "weird=happening"}
  end

  test "query_decoder/1,2" do
    decoder = URI.query_decoder("q=search%20query&cookie=ab%26cd&block+buster=")
    expected = [{"q", "search query"}, {"cookie", "ab&cd"}, {"block buster", ""}]
    assert Enum.map(decoder, & &1) == expected

    decoder = URI.query_decoder("q=search%20query&cookie=ab%26cd&block+buster=", :rfc3986)
    expected = [{"q", "search query"}, {"cookie", "ab&cd"}, {"block+buster", ""}]
    assert Enum.map(decoder, & &1) == expected
  end

  test "decode/1" do
    assert URI.decode("%0D%0A%26%3C%25%3E%22%20%E3%82%86") == "\r\n&<%>\" ゆ"
    assert URI.decode("%2f%41%4a%55") == "/AJU"
    assert URI.decode("4_t+st.is-s~") == "4_t+st.is-s~"
    assert URI.decode("% invalid") == "% invalid"
    assert URI.decode("invalid %") == "invalid %"
    assert URI.decode("%%") == "%%"
  end

  test "decode_www_form/1" do
    assert URI.decode_www_form("%3Eval+ue%2B") == ">val ue+"
    assert URI.decode_www_form("%E3%82%86+") == "ゆ "
    assert URI.decode_www_form("% invalid") == "% invalid"
    assert URI.decode_www_form("invalid %") == "invalid %"
    assert URI.decode_www_form("%%") == "%%"
  end

  describe "new/1" do
    test "empty" do
      assert URI.new("") == {:ok, %URI{}}
    end

    test "errors on bad URIs" do
      assert URI.new("/>") == {:error, ">"}
      assert URI.new(":https") == {:error, ":"}
      assert URI.new("ht\0tps://foo.com") == {:error, "\0"}
    end
  end

  describe "new!/1" do
    test "returns the given URI if a %URI{} struct is given" do
      assert URI.new!(uri = %URI{scheme: "http", host: "foo.com"}) == uri
    end

    test "works with HTTP scheme" do
      expected_uri = %URI{
        scheme: "http",
        host: "foo.com",
        path: "/path/to/something",
        query: "foo=bar&bar=foo",
        fragment: "fragment",
        port: 80,
        userinfo: nil
      }

      assert URI.new!("http://foo.com/path/to/something?foo=bar&bar=foo#fragment") ==
               expected_uri
    end

    test "works with HTTPS scheme" do
      expected_uri = %URI{
        scheme: "https",
        host: "foo.com",
        query: nil,
        fragment: nil,
        port: 443,
        path: nil,
        userinfo: nil
      }

      assert URI.new!("https://foo.com") == expected_uri
    end

    test "works with file scheme" do
      expected_uri = %URI{
        scheme: "file",
        host: "",
        path: "/foo/bar/baz",
        userinfo: nil,
        query: nil,
        fragment: nil,
        port: nil
      }

      assert URI.new!("file:///foo/bar/baz") == expected_uri
    end

    test "works with FTP scheme" do
      expected_uri = %URI{
        scheme: "ftp",
        host: "private.ftp-server.example.com",
        userinfo: "user001:password",
        path: "/my_directory/my_file.txt",
        query: nil,
        fragment: nil,
        port: 21
      }

      ftp = "ftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
      assert URI.new!(ftp) == expected_uri
    end

    test "works with SFTP scheme" do
      expected_uri = %URI{
        scheme: "sftp",
        host: "private.ftp-server.example.com",
        userinfo: "user001:password",
        path: "/my_directory/my_file.txt",
        query: nil,
        fragment: nil,
        port: 22
      }

      sftp = "sftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
      assert URI.new!(sftp) == expected_uri
    end

    test "works with TFTP scheme" do
      expected_uri = %URI{
        scheme: "tftp",
        host: "private.ftp-server.example.com",
        userinfo: "user001:password",
        path: "/my_directory/my_file.txt",
        query: nil,
        fragment: nil,
        port: 69
      }

      tftp = "tftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
      assert URI.new!(tftp) == expected_uri
    end

    test "works with LDAP scheme" do
      expected_uri = %URI{
        scheme: "ldap",
        host: "",
        userinfo: nil,
        path: "/dc=example,dc=com",
        query: "?sub?(givenName=John)",
        fragment: nil,
        port: 389
      }

      assert URI.new!("ldap:///dc=example,dc=com??sub?(givenName=John)") == expected_uri

      expected_uri = %URI{
        scheme: "ldap",
        host: "ldap.example.com",
        userinfo: nil,
        path: "/cn=John%20Doe,dc=foo,dc=com",
        fragment: nil,
        port: 389,
        query: nil
      }

      assert URI.new!("ldap://ldap.example.com/cn=John%20Doe,dc=foo,dc=com") == expected_uri
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
        # link-local
        "fe80::",
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
        simple_uri = URI.new!("http://[#{addr}]/")
        assert simple_uri.host == addr

        userinfo_uri = URI.new!("http://user:pass@[#{addr}]/")
        assert userinfo_uri.host == addr
        assert userinfo_uri.userinfo == "user:pass"

        port_uri = URI.new!("http://[#{addr}]:2222/")
        assert port_uri.host == addr
        assert port_uri.port == 2222

        userinfo_port_uri = URI.new!("http://user:pass@[#{addr}]:2222/")
        assert userinfo_port_uri.host == addr
        assert userinfo_port_uri.userinfo == "user:pass"
        assert userinfo_port_uri.port == 2222
      end)
    end

    test "downcases the scheme" do
      assert URI.new!("hTtP://google.com").scheme == "http"
    end

    test "preserves empty fragments" do
      assert URI.new!("http://example.com#").fragment == ""
      assert URI.new!("http://example.com/#").fragment == ""
      assert URI.new!("http://example.com/test#").fragment == ""
    end

    test "preserves an empty query" do
      assert URI.new!("http://foo.com/?").query == ""
    end

    test "without scheme, undefined port after host translates to nil" do
      assert URI.new!("//https://www.example.com") ==
               %URI{
                 scheme: nil,
                 userinfo: nil,
                 host: "https",
                 port: nil,
                 path: "//www.example.com",
                 query: nil,
                 fragment: nil
               }
    end

    test "with scheme, undefined port after host translates to nil" do
      assert URI.new!("myscheme://myhost:/path/info") ==
               %URI{
                 scheme: "myscheme",
                 userinfo: nil,
                 host: "myhost",
                 port: nil,
                 path: "/path/info",
                 query: nil,
                 fragment: nil
               }
    end
  end

  test "http://http://http://@http://http://?http://#http://" do
    assert URI.parse("http://http://http://@http://http://?http://#http://") ==
             %URI{
               scheme: "http",
               authority: "http:",
               userinfo: nil,
               host: "http",
               port: 80,
               path: "//http://@http://http://",
               query: "http://",
               fragment: "http://"
             }

    assert URI.new!("http://http://http://@http://http://?http://#http://") ==
             %URI{
               scheme: "http",
               userinfo: nil,
               host: "http",
               port: 80,
               path: "//http://@http://http://",
               query: "http://",
               fragment: "http://"
             }
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
    assert to_string(URI.new!("http://google.com")) == "http://google.com"
    assert to_string(URI.new!("http://google.com:443")) == "http://google.com:443"
    assert to_string(URI.new!("https://google.com:443")) == "https://google.com"
    assert to_string(URI.new!("file:/path")) == "file:/path"
    assert to_string(URI.new!("file:///path")) == "file:///path"
    assert to_string(URI.new!("file://///path")) == "file://///path"
    assert to_string(URI.new!("http://lol:wut@google.com")) == "http://lol:wut@google.com"
    assert to_string(URI.new!("http://google.com/elixir")) == "http://google.com/elixir"
    assert to_string(URI.new!("http://google.com?q=lol")) == "http://google.com?q=lol"
    assert to_string(URI.new!("http://google.com?q=lol#omg")) == "http://google.com?q=lol#omg"
    assert to_string(URI.new!("//google.com/elixir")) == "//google.com/elixir"
    assert to_string(URI.new!("//google.com:8080/elixir")) == "//google.com:8080/elixir"
    assert to_string(URI.new!("//user:password@google.com/")) == "//user:password@google.com/"
    assert to_string(URI.new!("http://[2001:db8::]:8080")) == "http://[2001:db8::]:8080"
    assert to_string(URI.new!("http://[2001:db8::]")) == "http://[2001:db8::]"

    assert URI.to_string(URI.new!("http://google.com")) == "http://google.com"
    assert URI.to_string(URI.new!("gid:hello/123")) == "gid:hello/123"

    assert URI.to_string(URI.new!("//user:password@google.com/")) ==
             "//user:password@google.com/"

    assert_raise ArgumentError,
                 ~r":path in URI must be empty or an absolute path if URL has a :host",
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

    assert URI.merge("http://example.com", URI.new!("/foo"))
           |> to_string == "http://example.com/foo"

    assert URI.merge("http://example.com", URI.new!("/.././bar/../../../baz"))
           |> to_string == "http://example.com/baz"

    base = URI.new!("http://example.com/foo/bar")
    assert URI.merge(base, "") |> to_string == "http://example.com/foo/bar"
    assert URI.merge(base, "#fragment") |> to_string == "http://example.com/foo/bar#fragment"
    assert URI.merge(base, "?query") |> to_string == "http://example.com/foo/bar?query"
    assert URI.merge(base, %URI{}) |> to_string == "http://example.com/foo/bar"

    assert URI.merge(base, %URI{fragment: "fragment"})
           |> to_string == "http://example.com/foo/bar#fragment"

    base = URI.new!("http://example.com")
    assert URI.merge(base, "/foo") |> to_string == "http://example.com/foo"
    assert URI.merge(base, "foo") |> to_string == "http://example.com/foo"

    base = URI.new!("http://example.com/foo/bar")
    assert URI.merge(base, "/baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, "baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, "../baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, ".././baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, "./baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, "bar/./baz") |> to_string == "http://example.com/foo/bar/baz"

    base = URI.new!("http://example.com/foo/bar/")
    assert URI.merge(base, "/baz") |> to_string == "http://example.com/baz"
    assert URI.merge(base, "baz") |> to_string == "http://example.com/foo/bar/baz"
    assert URI.merge(base, "../baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, ".././baz") |> to_string == "http://example.com/foo/baz"
    assert URI.merge(base, "./baz") |> to_string == "http://example.com/foo/bar/baz"
    assert URI.merge(base, "bar/./baz") |> to_string == "http://example.com/foo/bar/bar/baz"

    base = URI.new!("http://example.com/foo/bar/baz")
    assert URI.merge(base, "../../foobar") |> to_string == "http://example.com/foobar"
    assert URI.merge(base, "../../../foobar") |> to_string == "http://example.com/foobar"
    assert URI.merge(base, "../../../../../../foobar") |> to_string == "http://example.com/foobar"

    base = URI.new!("http://example.com/foo/../bar")
    assert URI.merge(base, "baz") |> to_string == "http://example.com/baz"

    base = URI.new!("http://example.com/foo/./bar")
    assert URI.merge(base, "baz") |> to_string == "http://example.com/foo/baz"

    base = URI.new!("http://example.com/foo?query1")
    assert URI.merge(base, "?query2") |> to_string == "http://example.com/foo?query2"
    assert URI.merge(base, "") |> to_string == "http://example.com/foo?query1"

    base = URI.new!("http://example.com/foo#fragment1")
    assert URI.merge(base, "#fragment2") |> to_string == "http://example.com/foo#fragment2"
    assert URI.merge(base, "") |> to_string == "http://example.com/foo"

    page_url = "https://example.com/guide/"
    image_url = "https://images.example.com/t/1600x/https://images.example.com/foo.jpg"

    assert URI.merge(URI.new!(page_url), URI.new!(image_url)) |> to_string ==
             "https://images.example.com/t/1600x/https://images.example.com/foo.jpg"
  end

  test "merge/2 (with RFC examples)" do
    # These are examples from:
    #
    # https://www.rfc-editor.org/rfc/rfc3986#section-5.4.1
    # https://www.rfc-editor.org/rfc/rfc3986#section-5.4.2
    #
    # They are taken verbatim from the above document for easy comparison

    base = "http://a/b/c/d;p?q"

    rel_and_result = %{
      "g:h" => "g:h",
      "g" => "http://a/b/c/g",
      "./g" => "http://a/b/c/g",
      "g/" => "http://a/b/c/g/",
      "/g" => "http://a/g",
      "//g" => "http://g",
      "?y" => "http://a/b/c/d;p?y",
      "g?y" => "http://a/b/c/g?y",
      "#s" => "http://a/b/c/d;p?q#s",
      "g#s" => "http://a/b/c/g#s",
      "g?y#s" => "http://a/b/c/g?y#s",
      ";x" => "http://a/b/c/;x",
      "g;x" => "http://a/b/c/g;x",
      "g;x?y#s" => "http://a/b/c/g;x?y#s",
      "" => "http://a/b/c/d;p?q",
      "." => "http://a/b/c/",
      "./" => "http://a/b/c/",
      ".." => "http://a/b/",
      "../" => "http://a/b/",
      "../g" => "http://a/b/g",
      "../.." => "http://a/",
      "../../" => "http://a/",
      "../../g" => "http://a/g",
      "../../../g" => "http://a/g",
      "../../../../g" => "http://a/g",
      "/./g" => "http://a/g",
      "/../g" => "http://a/g",
      "g." => "http://a/b/c/g.",
      ".g" => "http://a/b/c/.g",
      "g.." => "http://a/b/c/g..",
      "..g" => "http://a/b/c/..g",
      "./../g" => "http://a/b/g",
      "./g/." => "http://a/b/c/g/",
      "g/./h" => "http://a/b/c/g/h",
      "g/../h" => "http://a/b/c/h",
      "g;x=1/./y" => "http://a/b/c/g;x=1/y",
      "g;x=1/../y" => "http://a/b/c/y",
      "g?y/./x" => "http://a/b/c/g?y/./x",
      "g?y/../x" => "http://a/b/c/g?y/../x",
      "g#s/./x" => "http://a/b/c/g#s/./x",
      "g#s/../x" => "http://a/b/c/g#s/../x",
      "http:g" => "http:g"
    }

    for {rel, result} <- rel_and_result do
      assert URI.merge(base, rel) |> URI.to_string() == result
    end
  end

  test "append_query/2" do
    assert URI.append_query(URI.parse("http://example.com/?x=1"), "x=2").query == "x=1&x=2"
    assert URI.append_query(URI.parse("http://example.com/?x=1&"), "x=2").query == "x=1&x=2"
  end

  describe "append_path/2" do
    test "with valid paths" do
      examples = [
        {"http://example.com", "/", "http://example.com/"},
        {"http://example.com/", "/foo", "http://example.com/foo"},
        {"http://example.com/foo", "/bar", "http://example.com/foo/bar"},
        {"http://example.com/foo", "/bar/", "http://example.com/foo/bar/"},
        {"http://example.com/foo", "/bar/baz", "http://example.com/foo/bar/baz"},
        {"http://example.com/foo?var=1", "/bar/", "http://example.com/foo/bar/?var=1"},
        {"https://example.com/page/", "/urn:example:page",
         "https://example.com/page/urn:example:page"}
      ]

      for {base_url, path, expected_result} <- examples do
        result =
          base_url
          |> URI.parse()
          |> URI.append_path(path)
          |> URI.to_string()

        assert result == expected_result, """
        Path did not append as expected

          base_url: #{inspect(base_url)}
          path: #{inspect(path)}

          result:          #{inspect(result)}
          expected_result: #{inspect(expected_result)}
        """
      end
    end

    test "errors on invalid paths" do
      base_uri = URI.parse("http://example.com")

      assert_raise ArgumentError,
                   ~S|path must start with "/", got: "foo"|,
                   fn ->
                     URI.append_path(base_uri, "foo")
                   end

      assert_raise ArgumentError,
                   ~S|path cannot start with "//", got: "//foo"|,
                   fn ->
                     URI.append_path(base_uri, "//foo")
                   end
    end
  end

  ## Deprecate API

  describe "authority" do
    test "to_string" do
      assert URI.to_string(%URI{authority: "foo@example.com:80"}) ==
               "//foo@example.com:80"

      assert URI.to_string(%URI{userinfo: "bar", host: "example.org", port: 81}) ==
               "//bar@example.org:81"

      assert URI.to_string(%URI{
               authority: "foo@example.com:80",
               userinfo: "bar",
               host: "example.org",
               port: 81
             }) ==
               "//bar@example.org:81"
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

    test "works with WebSocket scheme" do
      expected_uri = %URI{
        authority: "ws.example.com",
        fragment: "content",
        host: "ws.example.com",
        path: "/path/to",
        port: 80,
        query: "here",
        scheme: "ws",
        userinfo: nil
      }

      assert URI.parse("ws://ws.example.com/path/to?here#content") == expected_uri
    end

    test "works with WebSocket Secure scheme" do
      expected_uri = %URI{
        authority: "ws.example.com",
        fragment: "content",
        host: "ws.example.com",
        path: "/path/to",
        port: 443,
        query: "here",
        scheme: "wss",
        userinfo: nil
      }

      assert URI.parse("wss://ws.example.com/path/to?here#content") == expected_uri
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
        # link-local
        "fe80::",
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

    test "merges empty path" do
      base = URI.parse("http://example.com")
      assert URI.merge(base, "/foo") |> to_string == "http://example.com/foo"
      assert URI.merge(base, "foo") |> to_string == "http://example.com/foo"
    end
  end
end
