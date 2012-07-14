Code.require_file "../test_helper", __FILE__

defmodule FileTest do
  import PathHelpers

  use ExUnit.Case, sync: true

  defmodule Cp do
    use ExUnit.Case, sync: true

    test :cp_with_src_file_and_dest_file do
      src  = fixture_path("foo.txt")
      dest = tmp_path("sample.txt")

      File.touch(dest)

      try do
        assert File.exists?(dest)
        assert File.cp(src, dest) == :ok
        assert File.exists?(dest)
      after
        File.rm(dest)
      end
    end

    test :cp_with_src_file_and_dest_dir do
      src   = fixture_path("foo.txt")
      dest  = tmp_path("tmp")
      final = File.join(dest, "foo.txt")

      File.mkdir(dest)

      try do
        refute File.exists?(final)
        assert File.cp(src, dest) == :ok
        assert File.exists?(final)
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_with_src_file_and_dest_unknown do
      src   = fixture_path("foo.txt")
      dest  = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.cp(src, dest) == :ok
        assert File.exists?(dest)
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_with_src_dir do
      src   = fixture_path("cp_r")
      dest  = tmp_path("tmp.file")
      assert File.cp(src, dest) == { :error, :eisdir }
    end

    test :cp_with_conflict do
      src   = fixture_path("foo.txt")
      dest  = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)
        assert File.cp(src, dest) == :ok
        assert File.read!(dest) == "FOO\n"
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_with_conflict_with_function do
      src   = fixture_path("foo.txt")
      dest  = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)
        assert File.cp(src, dest, fn(src_file, dest_file) ->
          assert src_file  == src
          assert dest_file == dest
          false
        end) == :ok
        assert File.read!(dest) == "hello"
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_with_src_file_and_dest_file! do
      src  = fixture_path("foo.txt")
      dest = tmp_path("sample.txt")

      File.touch(dest)

      try do
        assert File.exists?(dest)
        assert File.cp!(src, dest) == :ok
        assert File.exists?(dest)
      after
        File.rm(dest)
      end
    end

    test :cp_with_src_dir! do
      src   = fixture_path("cp_r")
      dest  = tmp_path("tmp.file")
      assert_raise File.CopyError, "could not copy recursively from #{src} to #{dest}: " <>
          "illegal operation on a directory", fn ->
        File.cp!(src, dest)
      end
    end

    #####

    test :cp_r_with_src_file_and_dest_file do
      src  = fixture_path("foo.txt")
      dest = tmp_path("sample.txt")

      File.touch(dest)

      try do
        assert File.exists?(dest)
        assert File.cp_r(src, dest) == { :ok, [dest] }
        assert File.exists?(dest)
      after
        File.rm(dest)
      end
    end

    test :cp_r_with_src_file_and_dest_dir do
      src   = fixture_path("foo.txt")
      dest  = tmp_path("tmp")
      final = File.join(dest, "foo.txt")

      File.mkdir(dest)

      try do
        refute File.exists?(final)
        assert File.cp_r(src, dest) == { :ok, [final] }
        assert File.exists?(final)
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_file_and_dest_unknown do
      src   = fixture_path("foo.txt")
      dest  = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.cp_r(src, dest) == { :ok, [dest] }
        assert File.exists?(dest)
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_dir_and_dest_dir do
      src  = fixture_path("cp_r")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        refute File.exists?(tmp_path("tmp/cp_r/a/1.txt"))
        refute File.exists?(tmp_path("tmp/cp_r/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/cp_r/b/3.txt"))

        { :ok, files } = File.cp_r(src, dest)
        assert length(files) == 7
        assert tmp_path("tmp/cp_r/a") in files
        assert tmp_path("tmp/cp_r/a/1.txt") in files

        assert File.exists?(tmp_path("tmp/cp_r/a/1.txt"))
        assert File.exists?(tmp_path("tmp/cp_r/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/cp_r/b/3.txt"))
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_dir_dot_and_dest_dir do
      src  = fixture_path("cp_r/.")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        refute File.exists?(tmp_path("tmp/a/1.txt"))
        refute File.exists?(tmp_path("tmp/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/b/3.txt"))

        { :ok, files } = File.cp_r(src, dest)
        assert length(files) == 7

        assert File.exists?(tmp_path("tmp/a/1.txt"))
        assert File.exists?(tmp_path("tmp/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/b/3.txt"))
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_dir_and_dest_file do
      src  = fixture_path("cp_r")
      dest = tmp_path("tmp.file")

      try do
        File.touch!(dest)
        assert File.cp_r(src, dest) == { :error, :enotdir }
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_dir_and_dest_unknown do
      src  = fixture_path("cp_r")
      dest = tmp_path("tmp")

      try do
        refute File.exists?(tmp_path("tmp/cp_r/a/1.txt"))
        refute File.exists?(tmp_path("tmp/cp_r/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/cp_r/b/3.txt"))

        { :ok, files } = File.cp_r(src, dest)
        assert length(files) == 7

        assert File.exists?(tmp_path("tmp/cp_r/a/1.txt"))
        assert File.exists?(tmp_path("tmp/cp_r/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/cp_r/b/3.txt"))
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_unknown do
      src  = fixture_path("unknown")
      dest = tmp_path("tmp")
      assert File.cp_r(src, dest) == { :error, :enoent }
    end

    test :cp_r_with_dir_and_file_conflict do
      src  = fixture_path("cp_r/.")
      dest = tmp_path("tmp")

      try do
        File.mkdir(dest)
        File.write!(File.join(dest, "a"), "hello")
        assert File.cp_r(src, dest) == { :error, :enotdir }
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_dir_and_dest_dir_using_lists do
      src  = fixture_path("cp_r/.") /> to_char_list
      dest = tmp_path("tmp") /> to_char_list

      File.mkdir(dest)

      try do
        refute File.exists?(tmp_path("tmp/a/1.txt"))
        refute File.exists?(tmp_path("tmp/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/b/3.txt"))

        { :ok, files } = File.cp_r(src, dest)
        assert length(files) == 7
        assert is_list(hd(files))

        assert File.exists?(tmp_path("tmp/a/1.txt"))
        assert File.exists?(tmp_path("tmp/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/b/3.txt"))
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_with_file_conflict do
      src  = fixture_path("cp_r/.")
      dest = tmp_path("tmp")

      File.mkdir_p tmp_path("tmp/a")
      File.write! tmp_path("tmp/a/1.txt"), "hello"

      try do
        assert File.exists?(tmp_path("tmp/a/1.txt"))
        File.cp_r(src, dest)
        assert File.read!(tmp_path("tmp/a/1.txt")) == ""
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_with_file_conflict_callback do
      src  = fixture_path("cp_r/.")
      dest = tmp_path("tmp")

      File.mkdir_p tmp_path("tmp/a")
      File.write! tmp_path("tmp/a/1.txt"), "hello"

      try do
        assert File.exists?(tmp_path("tmp/a/1.txt"))
        File.cp_r(src, dest, fn(src_file, dest_file) ->
          assert src_file == fixture_path("cp_r/a/1.txt")
          assert dest_file == tmp_path("tmp/a/1.txt")
          false
        end)
        assert File.read!(tmp_path("tmp/a/1.txt")) == "hello"
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r! do
      src  = fixture_path("cp_r/.")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        refute File.exists?(tmp_path("tmp/a/1.txt"))
        refute File.exists?(tmp_path("tmp/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/b/3.txt"))

        assert length(File.cp_r!(src, dest)) == 7

        assert File.exists?(tmp_path("tmp/a/1.txt"))
        assert File.exists?(tmp_path("tmp/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/b/3.txt"))
      after
        System.cmd "rm -rf #{dest}"
      end
    end

    test :cp_r_with_src_unknown! do
      src  = fixture_path("unknown")
      dest = tmp_path("tmp")
      assert_raise File.CopyError, "could not copy recursively from #{src} to #{dest}: no such file or directory", fn ->
        File.cp_r!(src, dest)
      end
    end

  end

  defmodule Paths do
    use ExUnit.Case

    test :expand_path_with_binary do
      assert File.expand_path("/foo/bar") == "/foo/bar"
      assert File.expand_path("/foo/bar/") == "/foo/bar"
      assert File.expand_path("/foo/bar/.") == "/foo/bar"
      assert File.expand_path("/foo/bar/../bar") == "/foo/bar"

      assert File.expand_path("bar", "/foo") == "/foo/bar"
      assert File.expand_path("bar/", "/foo") == "/foo/bar"
      assert File.expand_path("bar/.", "/foo") == "/foo/bar"
      assert File.expand_path("bar/../bar", "/foo") == "/foo/bar"
      assert File.expand_path("../bar/../bar", "/foo/../foo/../foo") == "/bar"

      full = File.expand_path("foo/bar")
      assert File.expand_path("bar/../bar", "foo") == full
    end

    test :expand_path_with_list do
      assert File.expand_path('/foo/bar') == '/foo/bar'
      assert File.expand_path('/foo/bar/') == '/foo/bar'
      assert File.expand_path('/foo/bar/.') == '/foo/bar'
      assert File.expand_path('/foo/bar/../bar') == '/foo/bar'
    end

    test :rootname_with_binary do
      assert File.rootname("~/foo/bar.ex", ".ex") == "~/foo/bar"
      assert File.rootname("~/foo/bar.exs", ".ex") == "~/foo/bar.exs"
      assert File.rootname("~/foo/bar.old.ex", ".ex") == "~/foo/bar.old"
    end

    test :rootname_with_list do
      assert File.rootname('~/foo/bar.ex', '.ex') == '~/foo/bar'
      assert File.rootname('~/foo/bar.exs', '.ex') == '~/foo/bar.exs'
      assert File.rootname('~/foo/bar.old.ex', '.ex') == '~/foo/bar.old'
    end

    test :extname_with_binary do
      assert File.extname("foo.erl") == ".erl"
      assert File.extname("~/foo/bar") == ""
    end

    test :extname_with_list do
      assert File.extname('foo.erl') == '.erl'
      assert File.extname('~/foo/bar') == ''
    end

    test :dirname_with_binary do
      assert File.dirname("/foo/bar.ex") == "/foo"
      assert File.dirname("~/foo/bar.ex") == "~/foo"
      assert File.dirname("/foo/bar/baz/") == "/foo/bar/baz"
    end

    test :dirname_with_list do
      assert File.dirname('/foo/bar.ex') == '/foo'
      assert File.dirname('~/foo/bar.ex') == '~/foo'
      assert File.dirname('/foo/bar/baz/') == '/foo/bar/baz'
    end

    test :basename_with_binary do
      assert File.basename("foo") == "foo"
      assert File.basename("/foo/bar") == "bar"
      assert File.basename("/") == ""

      assert File.basename("~/foo/bar.ex", ".ex") == "bar"
      assert File.basename("~/foo/bar.exs", ".ex") == "bar.exs"
      assert File.basename("~/for/bar.old.ex", ".ex") == "bar.old"
    end

    test :basename_with_list do
      assert File.basename('foo') == 'foo'
      assert File.basename('/foo/bar') == 'bar'
      assert File.basename('/') == ''

      assert File.basename('~/foo/bar.ex', '.ex') == 'bar'
      assert File.basename('~/foo/bar.exs', '.ex') == 'bar.exs'
      assert File.basename('~/for/bar.old.ex', '.ex') == 'bar.old'
    end

    test :join_with_binary do
      assert File.join([""]) == ""
      assert File.join(["foo"]) == "foo"
      assert File.join(["/", "foo", "bar"]) == "/foo/bar"
      assert File.join(["~", "foo", "bar"]) == "~/foo/bar"
    end

    test :join_with_list do
      assert File.join(['']) == ''
      assert File.join(['foo']) == 'foo'
      assert File.join(['/', 'foo', 'bar']) == '/foo/bar'
      assert File.join(['~', 'foo', 'bar']) == '~/foo/bar'
    end

    test :join_two_with_binary do
      assert File.join("/foo", "bar") == "/foo/bar"
      assert File.join("~", "foo") == "~/foo"
    end

    test :join_two_with_list do
      assert File.join('/foo', 'bar') == '/foo/bar'
      assert File.join('~', 'foo') == '~/foo'
    end

    test :split_with_binary do
      assert File.split("") == ["/"]
      assert File.split("foo") == ["foo"]
      assert File.split("/foo/bar") == ["/", "foo", "bar"]
    end

    test :split_with_list do
      assert File.split('') == ''
      assert File.split('foo') == ['foo']
      assert File.split('/foo/bar') == ['/', 'foo', 'bar']
    end
  end

  defmodule Queries do
    use ExUnit.Case, sync: true

    test :regular do
      assert File.regular?(__FILE__)
      assert File.regular?(binary_to_list(__FILE__))
      refute File.regular?("#{__FILE__}.unknown")
    end

    test :exists do
      assert File.exists?(__FILE__)
      assert File.exists?(fixture_path)
      assert File.exists?(fixture_path("foo.txt"))

      refute File.exists?(fixture_path("missing.txt"))
      refute File.exists?("_missing.txt")
    end
  end

  defmodule OpenReadWrite do
    use ExUnit.Case, sync: true

    test :read_with_binary do
      assert { :ok, "FOO\n" } = File.read(fixture_path("foo.txt"))
      assert { :error, :enoent } = File.read(fixture_path("missing.txt"))
    end

    test :read_with_list do
      assert { :ok, "FOO\n" } = File.read(File.expand_path('../fixtures/foo.txt', __FILE__))
      assert { :error, :enoent } = File.read(File.expand_path('../fixtures/missing.txt', __FILE__))
    end

    test :read_with_utf8 do
      assert { :ok, "Русский\n日\n" } = File.read(File.expand_path('../fixtures/utf8.txt', __FILE__))
    end

    test :read! do
      assert File.read!(fixture_path("foo.txt")) == "FOO\n"
      expected_message = "could not read file fixtures/missing.txt: no such file or directory"

      assert_raise File.Error, expected_message, fn ->
        File.read!("fixtures/missing.txt")
      end
    end

    test :write_ascii_content do
      fixture = tmp_path("tmp_test.txt")
      try do
        refute File.exists?(fixture)
        assert File.write(fixture, 'test text') == :ok
        assert File.read(fixture) == { :ok, "test text" }
      after
        File.rm(fixture)
      end
    end

    test :write_utf8 do
      fixture = tmp_path("tmp_test.txt")
      try do
        refute File.exists?(fixture)
        assert File.write(fixture, "Русский\n日\n") == :ok
        assert { :ok, "Русский\n日\n" } == File.read(fixture)
      after
        File.rm(fixture)
      end
    end

    test :write_with_options do
      fixture = tmp_path("tmp_test.txt")
      try do
        refute File.exists?(fixture)
        assert File.write(fixture, "Русский\n日\n") == :ok
        assert File.write(fixture, "test text", [:append]) == :ok
        assert { :ok, "Русский\n日\ntest text" } == File.read(fixture)
      after
        File.rm(fixture)
      end
    end

    test :open_file_without_modes do
      { :ok, file } = File.open(fixture_path("foo.txt"))
      assert IO.gets(file, "") == "FOO\n"
      assert File.close(file) == :ok
    end

    test :open_file_with_charlist do
      { :ok, file } = File.open(fixture_path("foo.txt"), [:charlist])
      assert IO.gets(file, "") == 'FOO\n'
      assert File.close(file) == :ok
    end

    test :open_utf8_by_default do
      { :ok, file } = File.open(fixture_path("utf8.txt"))
      assert IO.gets(file, "") == "Русский\n"
      assert File.close(file) == :ok
    end

    test :open_readonly_by_default do
      { :ok, file } = File.open(fixture_path("foo.txt"))
      assert_raise ArgumentError, fn -> IO.write(file, "foo") end
      assert File.close(file) == :ok
    end

    test :open_with_write_permission do
      fixture = tmp_path("tmp_text.txt")
      try do
        { :ok, file } = File.open(fixture, [:write])
        assert IO.write(file, "foo") == :ok
        assert File.close(file) == :ok
        assert File.read(fixture) == { :ok, "foo" }
      after
        File.rm(fixture)
      end
    end

    test :open_utf8_and_charlist do
      { :ok, file } = File.open(fixture_path("utf8.txt"), [:charlist])
      assert IO.gets(file, "") == [1056,1091,1089,1089,1082,1080,1081,10]
      assert File.close(file) == :ok
    end

    test :open_respects_encoding do
      { :ok, file } = File.open(fixture_path("utf8.txt"), [{:encoding, :latin1}])
      assert IO.gets(file, "") == <<195,144,194,160,195,145,194,131,195,145,194,129,195,145,194,129,195,144,194,186,195,144,194,184,195,144,194,185,10>>
      assert File.close(file) == :ok
    end

    test :open_a_missing_file do
      assert File.open('missing.txt') == {:error, :enoent}
    end

    test :open_a_file_with_function do
      file = fixture_path("foo.txt")
      assert File.open(file, IO.readline(&1)) == { :ok, "FOO\n" }
    end

    test :open_a_missing_file! do
      message = "could not open missing.txt: no such file or directory"
      assert_raise File.Error, message, fn ->
        File.open!('missing.txt')
      end
    end

    test :open_a_file_with_function! do
      file = fixture_path("foo.txt")
      assert File.open!(file, IO.readline(&1)) == "FOO\n"
    end
  end

  defmodule Mkdir do
    use ExUnit.Case, sync: true

    test :mkdir_with_binary do
      fixture = tmp_path("tmp_test")
      try do
        refute File.exists?(fixture)
        assert File.mkdir(fixture) == :ok
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{fixture}")
      end
    end

    test :mkdir_with_list do
      fixture = tmp_path("tmp_test") /> to_char_list
      try do
        refute File.exists?(fixture)
        assert File.mkdir(fixture) == :ok
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{fixture}")
      end
    end

    test :mkdir_with_invalid_path do
      fixture = fixture_path("foo.txt")
      invalid = File.join fixture, "test"
      assert File.exists?(fixture)
      assert File.mkdir(invalid) == { :error, :enotdir }
      refute File.exists?(invalid)
    end

    test :mkdir! do
      fixture = tmp_path("tmp_test")
      try do
        refute File.exists?(fixture)
        assert File.mkdir!(fixture) == :ok
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{fixture}")
      end
    end

    test :mkdir_with_invalid_path! do
      fixture = fixture_path("foo.txt")
      invalid = File.join fixture, "test"
      assert File.exists?(fixture)
      assert_raise File.Error, "could not make directory #{invalid}: not a directory", fn ->
        File.mkdir!(invalid)
      end
    end

    test :mkdir_p_with_one_directory do
      fixture = tmp_path("tmp_test")
      try do
        refute File.exists?(fixture)
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{fixture}")
      end
    end

    test :mkdir_p_with_nested_directory_and_binary do
      base    = tmp_path("tmp_test")
      fixture = File.join(base, "test")
      refute File.exists?(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{base}")
      end
    end

    test :mkdir_p_with_nested_directory_and_list do
      base    = tmp_path("tmp_test") /> to_char_list
      fixture = File.join(base, "test")
      refute File.exists?(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{base}")
      end
    end

    test :mkdir_p_with_nested_directory_and_existing_parent do
      base    = tmp_path("tmp_test")
      fixture = File.join(base, "test")

      File.mkdir(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{base}")
      end
    end

    test :mkdir_p_with_invalid_path do
      assert File.exists?(fixture_path("foo.txt"))
      invalid = File.join fixture_path("foo.txt"), "test/foo"
      assert File.mkdir(invalid) == { :error, :enotdir }
      refute File.exists?(invalid)
    end

    test :mkdir_p! do
      fixture = tmp_path("tmp_test")
      try do
        refute File.exists?(fixture)
        assert File.mkdir_p!(fixture) == :ok
        assert File.exists?(fixture)
      after
        System.cmd("rm -rf #{fixture}")
      end
    end

    test :mkdir_p_with_invalid_path! do
      fixture = fixture_path("foo.txt")
      invalid = File.join fixture, "test"
      assert File.exists?(fixture)
      assert_raise File.Error, "could not make directory (with -p) #{invalid}: not a directory", fn ->
        File.mkdir_p!(invalid)
      end
    end
  end

  defmodule Rm do
    use ExUnit.Case, sync: true

    test :rm_file do
      fixture = tmp_path("tmp_test.txt")
      File.write(fixture, "test")
      assert File.exists?(fixture)
      assert File.rm(fixture) == :ok
      refute File.exists?(fixture)
    end

    test :rm_file_with_dir do
      assert File.rm(fixture_path) == { :error, :eperm }
    end

    test :rm_nonexistent_file do
      assert File.rm('missing.txt') == { :error, :enoent }
    end

    test :rm! do
      fixture = tmp_path("tmp_test.txt")
      File.write(fixture, "test")
      assert File.exists?(fixture)
      assert File.rm!(fixture) == :ok
      refute File.exists?(fixture)
    end

    test :rm_with_invalid_file! do
      assert_raise File.Error, "could not remove file missing.file: no such file or directory", fn ->
        File.rm!("missing.file")
      end
    end

    test :rmdir do
      fixture = tmp_path("tmp_test")
      File.mkdir_p(fixture)
      assert File.dir?(fixture)
      assert File.rmdir(fixture) == :ok
      refute File.exists?(fixture)
    end

    test :rmdir_with_file do
      assert File.rmdir(fixture_path("foo.txt")) == { :error, :enotdir }
    end

    test :rmdir! do
      fixture = tmp_path("tmp_test")
      File.mkdir_p(fixture)
      assert File.dir?(fixture)
      assert File.rmdir!(fixture) == :ok
      refute File.exists?(fixture)
    end

    test :rmdir_with_file! do
      fixture = fixture_path("foo.txt")
      assert_raise File.Error, "could not remove directory #{fixture}: not a directory", fn ->
        File.rmdir!(fixture)
      end
    end

    test :rm_rf do
      fixture = tmp_path("tmp")
      File.mkdir(fixture)
      File.cp_r!(fixture_path("cp_r/."), fixture)

      assert File.exists?(tmp_path("tmp/a/1.txt"))
      assert File.exists?(tmp_path("tmp/a/a/2.txt"))
      assert File.exists?(tmp_path("tmp/b/3.txt"))

      { :ok, files } = File.rm_rf(fixture)
      assert length(files) == 7
      assert fixture in files
      assert tmp_path("tmp/a/1.txt") in files

      refute File.exists?(tmp_path("tmp/a/1.txt"))
      refute File.exists?(tmp_path("tmp/a/a/2.txt"))
      refute File.exists?(tmp_path("tmp/b/3.txt"))
      refute File.exists?(fixture)
    end

    test :rm_rf_with_char_list do
      fixture = tmp_path("tmp") /> to_char_list
      File.mkdir(fixture)
      File.cp_r!(fixture_path("cp_r/."), fixture)

      assert File.exists?(tmp_path("tmp/a/1.txt"))
      assert File.exists?(tmp_path("tmp/a/a/2.txt"))
      assert File.exists?(tmp_path("tmp/b/3.txt"))

      { :ok, files } = File.rm_rf(fixture)
      assert length(files) == 7
      assert fixture in files
      assert (tmp_path("tmp/a/1.txt") /> to_char_list) in files

      refute File.exists?(tmp_path("tmp/a/1.txt"))
      refute File.exists?(tmp_path("tmp/a/a/2.txt"))
      refute File.exists?(tmp_path("tmp/b/3.txt"))
      refute File.exists?(fixture)
    end

    test :rm_rf_with_file do
      fixture = tmp_path("tmp")
      File.write(fixture, "hello")
      assert File.rm_rf(fixture) == { :ok, [fixture] }
    end

    test :rm_rf_with_unknown do
      fixture = tmp_path("tmp.unknown")
      assert File.rm_rf(fixture) == { :ok, [] }
    end

    test :rm_rf_with_invalid do
      fixture = fixture_path "foo.txt/path"
      assert File.rm_rf(fixture) == { :error, :enotdir }
    end

    test :rm_rf! do
      fixture = tmp_path("tmp")
      File.mkdir(fixture)
      File.cp_r!(fixture_path("cp_r/."), fixture)

      assert File.exists?(tmp_path("tmp/a/1.txt"))
      assert File.exists?(tmp_path("tmp/a/a/2.txt"))
      assert File.exists?(tmp_path("tmp/b/3.txt"))

      files = File.rm_rf!(fixture)
      assert length(files) == 7
      assert fixture in files
      assert tmp_path("tmp/a/1.txt") in files

      refute File.exists?(tmp_path("tmp/a/1.txt"))
      refute File.exists?(tmp_path("tmp/a/a/2.txt"))
      refute File.exists?(tmp_path("tmp/b/3.txt"))
      refute File.exists?(fixture)
    end

    test :rm_rf_with_invalid! do
      fixture = fixture_path "foo.txt/path"
      assert_raise File.Error, "could not remove files and directories recursively from #{fixture}: not a directory", fn ->
        File.rm_rf!(fixture)
      end
    end
  end

  test :stat do
    {:ok, info} = File.stat(__FILE__)
    assert info.mtime
  end

  test :stat! do
    assert File.stat!(__FILE__).mtime
  end

  test :stat_with_invalid_file do
    assert { :error, _ } = File.stat("./invalid_file")
  end

  test :stat_with_invalid_file! do
    assert_raise File.Error, fn ->
      File.stat!("./invalid_file")
    end
  end

  test :copy do
    src  = fixture_path("foo.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy(src, dest) == { :ok, 4 }
      assert { :ok, "FOO\n" } == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test :copy_with_bytes_count do
    src  = fixture_path("foo.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy(src, dest, 2) == { :ok, 2 }
      assert { :ok, "FO" } == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test :copy_with_invalid_file do
    src  = fixture_path("invalid.txt")
    dest = tmp_path("tmp_test.txt")
    assert File.copy(src, dest, 2) == { :error, :enoent }
  end

  test :copy! do
    src  = fixture_path("foo.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy!(src, dest) == 4
      assert { :ok, "FOO\n" } == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test :copy_with_bytes_count! do
    src  = fixture_path("foo.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy!(src, dest, 2) == 2
      assert { :ok, "FO" } == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test :copy_with_invalid_file! do
    src  = fixture_path("invalid.txt")
    dest = tmp_path("tmp_test.txt")
    assert_raise File.CopyError, "could not copy from #{src} to #{dest}: no such file or directory", fn ->
      File.copy!(src, dest, 2)
    end
  end

  test :cwd_and_cd do
    { :ok, current } = File.cwd
    try do
      assert File.cd(fixture_path) == :ok
      assert File.exists?("foo.txt")
    after
      File.cd!(current)
    end
  end

  test :invalid_cd do
    assert File.cd(fixture_path("foo.txt")) == { :error, :enotdir }
  end

  test :invalid_cd! do
    message = "could not set current working directory to #{fixture_path("foo.txt")}: not a directory"
    assert_raise File.Error, message, fn ->
      File.cd!(fixture_path("foo.txt"))
    end
  end

  test :cd_with_function do
    assert File.cd!(fixture_path, fn ->
      assert File.exists?("foo.txt")
      :cd_result
    end) == :cd_result
  end

  test :touch_with_no_file do
    fixture = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(fixture)
      assert File.touch(fixture) == :ok
      assert { :ok, "" } == File.read(fixture)
    after
      File.rm(fixture)
    end
  end

  test :touch_with_file do
    fixture = tmp_path("tmp_test.txt")

    try do
      File.touch!(fixture)
      stat = File.stat!(fixture).mtime(last_year)
      File.write_stat!(fixture, stat)

      assert File.touch(fixture) == :ok
      assert stat.mtime < File.stat!(fixture).mtime
    after
      File.rm(fixture)
    end
  end

  test :touch_with_dir do
    assert File.touch(fixture_path) == :ok
  end

  test :touch_with_failure do
    fixture = fixture_path("foo.txt/bar")
    assert File.touch(fixture) == { :error, :enotdir }
  end

  test :touch_with_success! do
    assert File.touch!(fixture_path) == :ok
  end

  test :touch_with_failure! do
    fixture = fixture_path("foo.txt/bar")
    assert_raise File.Error, "could not touch #{fixture}: not a directory", fn ->
      File.touch!(fixture)
    end
  end

  defp last_year do
    last_year :calendar.local_time
  end

  defp last_year({ { year, month, day },time }) do
    { { year - 1, month, day }, time }
  end
end