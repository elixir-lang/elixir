Code.require_file "../test_helper", __FILE__

defmodule FileTest do
  use ExUnit::Case

  def expand_path_test do
    "/foo/bar" = File.expand_path("/foo/bar")
    "/foo/bar" = File.expand_path("/foo/bar/")
    "/foo/bar" = File.expand_path("/foo/bar/.")
    "/foo/bar" = File.expand_path("/foo/bar/../bar")

    "/foo/bar" = File.expand_path("bar", "/foo")
    "/foo/bar" = File.expand_path("bar/", "/foo")
    "/foo/bar" = File.expand_path("bar/.", "/foo")
    "/foo/bar" = File.expand_path("bar/../bar", "/foo")
    "/bar" = File.expand_path("../bar/../bar", "/foo/../foo/../foo")

    full  = File.expand_path("foo/bar")
    ^full = File.expand_path("bar/../bar", "foo")
  end

  # def join_test
  #   "foo/bar" = File.join("foo", "bar")
  #   "foo/bar/baz" = File.join(["foo", "bar", "baz"])
  # end
  #
  # def split_test
  #   ["foo"] = File.split("foo")
  #   ["foo", "bar"] = File.split("foo/bar")
  #   ["foo", "bar", "baz"] = File.split("foo/bar/baz")
  # end

  def regular_test do
    true  = File.regular?(__FILE__)
    false = File.regular?("#{__FILE__}.unknown")
  end
end