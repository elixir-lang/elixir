defmodule URITest do
  use ExUnit.Case

  test :url_encode do
    expected = "%26%3C%3E%22+%E3%82%86%E3%82%93%E3%82%86%E3%82%93"
    list_expected = binary_to_list expected
    assert URI.url_encode(<<38,60,62,34,32,227,130,134,227,
                            130,147,227,130,134,227,130,147>>) == expected
    assert URI.url_encode([38,60,62,34,32,12422,12435,12422,12435]) == list_expected
  end

  test :encode_query do
    assert URI.encode_query([{:foo, :bar}, {:baz, :quux}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{"foo", "bar"}, {"baz", "quux"}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{'foo', 'bar'}, {'baz', 'quux'}]) == "foo=bar&baz=quux"
  end

  test :encode_query_mixed do
    assert URI.encode_query([{"foo", :bar}]) == "foo=bar"
    assert URI.encode_query([{'foo', :bar}]) == "foo=bar"
    assert URI.encode_query([{"foo", 'bar'}]) == "foo=bar"
    assert URI.encode_query([{'foo', "bar"}]) == "foo=bar"
    assert URI.encode_query([{:foo, "bar"}]) == "foo=bar"
    assert URI.encode_query([{:foo, 'bar'}]) == "foo=bar"
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

  test :parse_list do
    assert URI.Info[scheme: "http", host: "foo.com", authority: "foo.com",
                    query: nil, fragment: nil, port: 80, path: nil, userinfo: nil] ==
                 URI.parse('http://foo.com')
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
end
