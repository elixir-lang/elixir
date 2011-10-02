Code.require_file "../test_helper", __FILE__

module FileTest
  module Helpers

    def teardown(_)
      File.wildcard(File.join(temp_path, "file-test-*.txt")).each do (file)
        File.unlink file
      end
    end

    def create_fixture_file(num := 0, content := "")
      path = file_fixture_path(num)
      Erlang.file.write_file path, content.to_char_list
      path
    end

    def temp_path
      File.expand_path("../../tmp", __FILE__)
    end

    def file_fixture_path(num := 0)
      File.join temp_path, "file-test-#{num}.txt"
    end

  end
  mixin ExUnit::Case
  mixin FileTest::Helpers

  def expand_path_test
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
    ~full = File.expand_path("bar/../bar", "foo")
  end

  def join_test
    "foo/bar" = File.join("foo", "bar")
    "foo/bar/baz" = File.join(["foo", "bar", "baz"])
  end

  def split_test
    ["foo"] = File.split("foo")
    ["foo", "bar"] = File.split("foo/bar")
    ["foo", "bar", "baz"] = File.split("foo/bar/baz")
  end

  def read_info_test
    info1 = File.read_info __FILE__
    'regular = info1['type]
    true = info1.regular?
    true = info1.read?

    info2 = File.read_info File.expand_path("..", __FILE__)
    'directory = info2['type]
    true = info2.directory?

    self.assert_error 'enoent, -> File.read_info __FILE__ + ".unknown"
  end

  def read_test
    self.assert_included "object FileTest", File.read __FILE__
    self.assert_error 'enoent, -> File.read __FILE__ + ".unknown"
  end

  def regular_test
    true  = File.regular?(__FILE__)
    false = File.regular?(__FILE__ + ".unknown")
  end

  def wildcard_test
    files = File.wildcard File.expand_path("../../../lib/**/*", __FILE__)
    assert_included File.expand_path("../../../lib/code", __FILE__), files
    assert_included File.expand_path("../../../lib/code.ex", __FILE__), files
  end

  def wildcard_include_dot_test
    dotted = File.expand_path("../fixtures/.dotted.exs", __FILE__)

    files = File.wildcard File.expand_path("../fixtures/*", __FILE__)
    assert_not_included dotted, files

    files = File.wildcard File.expand_path("../fixtures/*", __FILE__), true
    assert_included dotted, files

    files = File.wildcard File.expand_path("../fixtures/**/*", __FILE__)
    assert_not_included dotted, files

    files = File.wildcard File.expand_path("../fixtures/**/*", __FILE__), true
    assert_included dotted, files
  end

  def touch_test
    'ok = File.touch File.expand_path("../../tmp/file.txt", __FILE__)
    assert_error 'eisdir, -> File.touch File.expand_path("../../tmp", __FILE__)
  end

  def delete_test
    path = file_fixture_path
    'ok = File.touch path
    'ok = File.delete path

    'ok = File.touch path
    'ok = File.unlink path

    assert_error 'enoent, -> File.delete file_fixture_path(888)
    assert_error 'enoent, -> File.unlink file_fixture_path(888)
  end

  def open_read_test
    path = create_fixture_file 0, "Hi!"
    file = File.open path, do (f)
      "Hi" = f.read(2).to_bin
    end
    '"File::Behavior" = file.__module_name__
    assert_error 'terminated, -> file.write("Bye!")
  end

  def open_write_test
    file = File.open file_fixture_path, ['write], -> (f) f.write("Hi!")
    3    = file.info.size
    assert_error 'terminated, -> file.write("Bye!")
  end

  module BehaviorTest
    mixin ExUnit::Case
    mixin FileTest::Helpers

    def bind_for_read_test
      path = create_fixture_file 0, "Hi!"
      file = File.new(path)
      '"File::Behavior" = file.__module_name__
      path = file.path
      assert_error 'enoent, -> File.new(file_fixture_path(888))
    end

    def bind_for_write_test
      '"File::Behavior" = File.new(file_fixture_path(888), ['write]).__module_name__
      assert_error 'eisdir, -> File.new File.expand_path("../../tmp", __FILE__), ['write]
    end

    def info_test
      path = create_fixture_file 0, "Hi!"
      3 = File.new(path).info.size
    end

    def write_close_test
      path = file_fixture_path
      file = File.new(path, ['write])
      'ok  = file.write "Hi!"
      'ok  = file.close
      3    = File.read_info(path).size
      assert_error 'terminated, -> file.write("Bye!")
    end

    def pos_test
      path = create_fixture_file 0, "Hi!"
      file = File.new(path)
      1 = file.pos(1)
      1 = file.pos({'bof, 1})
      2 = file.pos({'cur, 1})
      1 = file.pos({'eof, -2})
      assert_error 'einval, -> file.pos({'cur, -10})
    end

    def rewind_test
      path = create_fixture_file 0, "Hi!"
      file = File.new(path)
      2 = file.pos(2)
      0 = file.rewind
    end

    def read_test
      path = create_fixture_file 0, "Hi!"
      file = File.new(path)
      1 = file.pos(1)
      "i!" = file.read(2).to_bin
      'eof = file.read(1)
      assert_error 'ebadf, -> File.new(file_fixture_path, ['write]).read(5)
    end

    def inspect_test
      file = File.new(create_fixture_file)
      expected = "<\#File::Behavior #{file.path}>"
      ~expected = file.inspect
    end
  end
end

