Code.require_file "../test_helper", __FILE__

defmodule FileTest do
  use ExUnit.Case

  test :expand_path do
    assert_equal "/foo/bar", File.expand_path("/foo/bar")
    assert_equal "/foo/bar", File.expand_path("/foo/bar/")
    assert_equal "/foo/bar", File.expand_path("/foo/bar/.")
    assert_equal "/foo/bar", File.expand_path("/foo/bar/../bar")

    assert_equal "/foo/bar", File.expand_path("bar", "/foo")
    assert_equal "/foo/bar", File.expand_path("bar/", "/foo")
    assert_equal "/foo/bar", File.expand_path("bar/.", "/foo")
    assert_equal "/foo/bar", File.expand_path("bar/../bar", "/foo")
    assert_equal "/bar", File.expand_path("../bar/../bar", "/foo/../foo/../foo")

    full = File.expand_path("foo/bar")
    assert_equal full, File.expand_path("bar/../bar", "foo")
  end

  test :regular do
    assert File.regular?(__FILE__)
    refute File.regular?("#{__FILE__}.unknown")
  end

  test :basename do
    assert_equal "foo", File.basename("foo")
    assert_equal "bar", File.basename("/foo/bar")
    assert_equal "", File.basename("/")

    assert_equal "bar", File.basename("~/foo/bar.ex", ".ex")
    assert_equal "bar.exs", File.basename("~/foo/bar.exs", ".ex")
    assert_equal "bar.old", File.basename("~/for/bar.old.ex", ".ex")
  end

  test :join do
    assert_equal "", File.join([""])
    assert_equal "foo", File.join(["foo"])
    assert_equal "/foo/bar", File.join(["/", "foo", "bar"])
    assert_equal "~/foo/bar", File.join(["~", "foo", "bar"])
  end

  test :split do
    assert_equal ["/"], File.split("")
    assert_equal ["foo"], File.split("foo")
    assert_equal ["/", "foo", "bar"], File.split("/foo/bar")
  end

  test :read do
    assert_match { :ok, "FOO\n" }, File.read(File.expand_path('../../fixtures/foo.txt', __FILE__))

    assert_match { :error, :enoent }, File.read(File.expand_path('../../fixtures/missing.txt', __FILE__))
  end

  test :read! do
    assert_equal "FOO\n", File.read!(File.expand_path('../../fixtures/foo.txt', __FILE__))

    expected_message = "could not read file fixtures/missing.txt: no such file or directory"

    assert_raises File.Exception, expected_message, fn ->
      File.read!("fixtures/missing.txt")
    end
  end
end
