defmodule URITest do
  use ExUnit.Case

  test :url_encode do
    expected = "%26%3C%3E%22+%E3%82%86%E3%82%93%E3%82%86%E3%82%93"
    list_expected = binary_to_list expected
    assert_equal URI.url_encode(<<38,60,62,34,32,227,130,134,227,
                                  130,147,227,130,134,227,130,147>>), expected
    assert_equal URI.url_encode([38,60,62,34,32,12422,12435,12422,12435]), list_expected
  end

  test :encode_query do
    assert_equal URI.encode_query([{:foo, :bar}, {:baz, :quux}]), "foo=bar&baz=quux"
    assert_equal URI.encode_query([{"foo", "bar"}, {"baz", "quux"}]), "foo=bar&baz=quux"
    assert_equal URI.encode_query([{'foo', 'bar'}, {'baz', 'quux'}]), "foo=bar&baz=quux"
  end

  test :encode_query_mixed do
    assert_equal URI.encode_query([{"foo", :bar}]), "foo=bar"
    assert_equal URI.encode_query([{'foo', :bar}]), "foo=bar"
    assert_equal URI.encode_query([{"foo", 'bar'}]), "foo=bar"
    assert_equal URI.encode_query([{'foo', "bar"}]), "foo=bar"
    assert_equal URI.encode_query([{:foo, "bar"}]), "foo=bar"
    assert_equal URI.encode_query([{:foo, 'bar'}]), "foo=bar"
  end

  test :parse_http do
    assert_equal URI.parse("http://foo.com/path/to/something?foo=bar&bar=foo#fragment"),
                 [scheme: "http", host: "foo.com", path: "/path/to/something",
                  query: "foo=bar&bar=foo", fragment: "fragment", port: 80,
                  authority: "foo.com", userinfo: nil]
  end

  test :parse_https do
    assert_equal URI.parse("https://foo.com"),
                 [scheme: "https", host: "foo.com", authority: "foo.com",
                  query: nil, fragment: nil, port: 443, path: nil, userinfo: nil]
  end

  test :parse_file do
    assert_equal URI.parse("file:///foo/bar/baz"),
                 [scheme: "file", host: nil, path: "/foo/bar/baz", userinfo: nil,
                  query: nil, fragment: nil, port: nil, authority: nil]
  end

  test :parse_ftp do
    assert_equal URI.parse("ftp://user001:secretpassword@private.ftp-servers.example.com/mydirectory/myfile.txt"),
                [scheme: "ftp", host: "private.ftp-servers.example.com",
                 userinfo: "user001:secretpassword", authority: "user001:secretpassword@private.ftp-servers.example.com",
                 path: "/mydirectory/myfile.txt", query: nil, fragment: nil,
                 port: 21]
  end

  test :parse_sftp do
    assert_equal URI.parse("sftp://user001:secretpassword@private.ftp-servers.example.com/mydirectory/myfile.txt"),
                [scheme: "sftp", host: "private.ftp-servers.example.com",
                 userinfo: "user001:secretpassword", authority: "user001:secretpassword@private.ftp-servers.example.com",
                 path: "/mydirectory/myfile.txt", query: nil, fragment: nil,
                 port: 22]
  end

  test :parse_tftp do
    assert_equal URI.parse("tftp://user001:secretpassword@private.ftp-servers.example.com/mydirectory/myfile.txt"),
                [scheme: "tftp", host: "private.ftp-servers.example.com",
                 userinfo: "user001:secretpassword", authority: "user001:secretpassword@private.ftp-servers.example.com",
                 path: "/mydirectory/myfile.txt", query: nil, fragment: nil,
                 port: 69]
  end


  test :parse_ldap do
    assert_equal URI.parse("ldap:///dc=example,dc=com??sub?(givenName=John)"),
                 [scheme: "ldap", host: nil, authority: nil, userinfo: nil,
                  path: "/dc=example,dc=com", query: "?sub?(givenName=John)",
                  fragment: nil, port: 389]
    assert_equal URI.parse("ldap://ldap.example.com/cn=John%20Doe,dc=example,dc=com"),
                 [scheme: "ldap", host: "ldap.example.com", authority: "ldap.example.com",
                  userinfo: nil, path: "/cn=John%20Doe,dc=example,dc=com", fragment: nil,
                  port: 389, query: nil]
  end

  test :parse_list do
    assert_equal URI.parse('http://foo.com'),
                 [scheme: "http", host: "foo.com", authority: "foo.com",
                  query: nil, fragment: nil, port: 80, path: nil, userinfo: nil]
  end


  test :parse_splits_authority do
    assert_equal URI.parse("http://foo:bar@foo.com:4444"),
                [scheme: "http", host: "foo.com", path: nil,
                 query: nil, fragment: nil, port: 4444,
                 authority: "foo:bar@foo.com:4444",
                 userinfo: "foo:bar"]
    assert_equal URI.parse("https://foo:bar@foo.com"),
                 [scheme: "https", host: "foo.com", path: nil,
                  query: nil, fragment: nil, port: 443,
                  authority: "foo:bar@foo.com", userinfo: "foo:bar"]
    assert_equal URI.parse("http://foo.com:4444"),
                 [scheme: "http", host: "foo.com", path: nil,
                  query: nil, fragment: nil, port: 4444,
                  authority: "foo.com:4444",
                  userinfo: nil]


  end

  test :parse_bad_uris do
    assert URI.parse("https:??@?F?@#>F//23/")
    assert URI.parse("")
    assert URI.parse(":https")
    assert URI.parse("https")
  end
end
