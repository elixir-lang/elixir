Code.require_file "../test_helper", __FILE__

defmodule FileTest do
  use ExUnit::Case

  test "expand_path" do
    assert_equal "/foo/bar", File.expand_path("/foo/bar")
    assert_equal "/foo/bar", File.expand_path("/foo/bar/")
    assert_equal "/foo/bar", File.expand_path("/foo/bar/.")
    assert_equal "/foo/bar", File.expand_path("/foo/bar/../bar")

    assert_equal "/foo/bar", File.expand_path("bar", "/foo")
    assert_equal "/foo/bar", File.expand_path("bar/", "/foo")
    assert_equal "/foo/bar", File.expand_path("bar/.", "/foo")
    assert_equal "/foo/bar", File.expand_path("bar/../bar", "/foo")
    assert_equal "/bar", File.expand_path("../bar/../bar", "/foo/../foo/../foo")

    full  = File.expand_path("foo/bar")
    assert_equal full, File.expand_path("bar/../bar", "foo")
  end

  test "regular" do
    assert File.regular?(__FILE__)
    refute File.regular?("#{__FILE__}.unknown")
  end
end
