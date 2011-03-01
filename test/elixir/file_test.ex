object FileTest
  proto ExUnit::Case

  def expand_path_test
    "/foo/bar" = File.expand_path("/foo/bar")
    "/foo/bar" = File.expand_path("/foo/bar/")
    "/foo/bar" = File.expand_path("/foo/bar/.")
    "/foo/bar" = File.expand_path("/foo/bar/../bar")

    "/foo/bar" = File.expand_path("bar", "/foo")
    "/foo/bar" = File.expand_path("bar/", "/foo")
    "/foo/bar" = File.expand_path("bar/.", "/foo")
    "/foo/bar" = File.expand_path("bar/../bar", "/foo")

    full = File.expand_path("foo/bar")
    full = File.expand_path("bar/../bar", "foo")
  end
end

