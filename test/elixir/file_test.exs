Code.require_file "../test_helper", __FILE__

defmodule FileTest do
  use ExUnit.Case

  test :expand_path_with_binary do
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

  test :expand_path_with_list do
    assert_equal '/foo/bar', File.expand_path('/foo/bar')
    assert_equal '/foo/bar', File.expand_path('/foo/bar/')
    assert_equal '/foo/bar', File.expand_path('/foo/bar/.')
    assert_equal '/foo/bar', File.expand_path('/foo/bar/../bar')
  end

  test :regular do
    assert File.regular?(__FILE__)
    assert File.regular?(binary_to_list(__FILE__))
    refute File.regular?("#{__FILE__}.unknown")
  end

  test :basename_with_binary do
    assert_equal "foo", File.basename("foo")
    assert_equal "bar", File.basename("/foo/bar")
    assert_equal "", File.basename("/")

    assert_equal "bar", File.basename("~/foo/bar.ex", ".ex")
    assert_equal "bar.exs", File.basename("~/foo/bar.exs", ".ex")
    assert_equal "bar.old", File.basename("~/for/bar.old.ex", ".ex")
  end

  test :basename_with_list do
    assert_equal 'foo', File.basename('foo')
    assert_equal 'bar', File.basename('/foo/bar')
    assert_equal '', File.basename('/')

    assert_equal 'bar', File.basename('~/foo/bar.ex', '.ex')
    assert_equal 'bar.exs', File.basename('~/foo/bar.exs', '.ex')
    assert_equal 'bar.old', File.basename('~/for/bar.old.ex', '.ex')
  end

  test :join_with_binary do
    assert_equal "", File.join([""])
    assert_equal "foo", File.join(["foo"])
    assert_equal "/foo/bar", File.join(["/", "foo", "bar"])
    assert_equal "~/foo/bar", File.join(["~", "foo", "bar"])
  end

  test :join_with_list do
    assert_equal '', File.join([''])
    assert_equal 'foo', File.join(['foo'])
    assert_equal '/foo/bar', File.join(['/', 'foo', 'bar'])
    assert_equal '~/foo/bar', File.join(['~', 'foo', 'bar'])
  end

  test :split_with_binary do
    assert_equal ["/"], File.split("")
    assert_equal ["foo"], File.split("foo")
    assert_equal ["/", "foo", "bar"], File.split("/foo/bar")
  end

  test :split_with_list do
    assert_equal '', File.split('')
    assert_equal ['foo'], File.split('foo')
    assert_equal ['/', 'foo', 'bar'], File.split('/foo/bar')
  end

  test :read_with_binary do
    assert_match { :ok, "FOO\n" }, File.read(File.expand_path("../../fixtures/foo.txt", __FILE__))
    assert_match { :error, :enoent }, File.read(File.expand_path("../../fixtures/missing.txt", __FILE__))
  end

  test :read_with_list do
    assert_match { :ok, "FOO\n" }, File.read(File.expand_path('../../fixtures/foo.txt', __FILE__))
    assert_match { :error, :enoent }, File.read(File.expand_path('../../fixtures/missing.txt', __FILE__))
  end

  test :read! do
    assert_equal "FOO\n", File.read!(File.expand_path("../../fixtures/foo.txt", __FILE__))
    expected_message = "could not read file fixtures/missing.txt: no such file or directory"

    assert_raises File.Exception, expected_message, fn ->
      File.read!("fixtures/missing.txt")
    end
  end

  test :read_info do
    {:ok, info} = File.read_info(__FILE__)
    assert info.mtime
  end

  test :read_info! do
    assert File.read_info!(__FILE__).mtime
  end

  test :read_info_with_invalid_file do
    assert_match { :error, _ }, File.read_info("./invalid_file")
  end

  test :read_info_with_invalid_file! do
    assert_raises File.Exception, fn ->
      File.read_info!("./invalid_file")
    end
  end
end
