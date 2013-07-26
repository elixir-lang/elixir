Code.require_file "test_helper.exs", __DIR__

defmodule Elixir.FileCase do
  use ExUnit.CaseTemplate
  import PathHelpers

  using do
    quote do
      import PathHelpers
    end
  end

  setup do
    File.mkdir_p!(tmp_path)
    :ok
  end

  teardown do
    File.rm_rf(tmp_path)
    :ok
  end
end

defmodule FileTest do
  use Elixir.FileCase
  import Regex, only: [escape: 1]

  defmodule Cp do
    use Elixir.FileCase

    test :cp_with_src_file_and_dest_file do
      src  = fixture_path("file.txt")
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
      src   = fixture_path("file.txt")
      dest  = tmp_path("tmp")
      final = Path.join(dest, "file.txt")

      File.mkdir(dest)

      try do
        refute File.exists?(final)
        assert File.cp(src, dest) == :ok
        assert File.exists?(final)
      after
        File.rm_rf dest
      end
    end

    test :cp_with_src_file_and_dest_unknown do
      src   = fixture_path("file.txt")
      dest  = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.cp(src, dest) == :ok
        assert File.exists?(dest)
      after
        File.rm_rf dest
      end
    end

    test :cp_with_src_dir do
      src   = fixture_path("cp_r")
      dest  = tmp_path("tmp.file")
      assert File.cp(src, dest) == { :error, :eisdir }
    end

    test :cp_with_conflict do
      src   = fixture_path("file.txt")
      dest  = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)
        assert File.cp(src, dest) == :ok
        assert File.read!(dest) == "FOO\n"
      after
        File.rm_rf dest
      end
    end

    test :cp_with_conflict_with_function do
      src   = fixture_path("file.txt")
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
        File.rm_rf dest
      end
    end

    test :cp_with_src_file_and_dest_file! do
      src  = fixture_path("file.txt")
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

    test :cp_r_with_src_file_and_dest_file do
      src  = fixture_path("file.txt")
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
      src   = fixture_path("file.txt")
      dest  = tmp_path("tmp")
      final = Path.join(dest, "file.txt")

      File.mkdir(dest)

      try do
        refute File.exists?(final)
        assert File.cp_r(src, dest) == { :ok, [final] }
        assert File.exists?(final)
      after
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_file_and_dest_unknown do
      src   = fixture_path("file.txt")
      dest  = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.cp_r(src, dest) == { :ok, [dest] }
        assert File.exists?(dest)
      after
        File.rm_rf dest
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
        File.rm_rf dest
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
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_dir_and_dest_file do
      src  = fixture_path("cp_r")
      dest = tmp_path("tmp.file")

      try do
        File.touch!(dest)
        assert (File.cp_r(src, dest) |> is_io_error?)
      after
        File.rm_rf dest
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
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_unknown do
      src  = fixture_path("unknown")
      dest = tmp_path("tmp")
      assert File.cp_r(src, dest) == { :error, :enoent, src }
    end

    test :cp_r_with_dir_and_file_conflict do
      src  = fixture_path("cp_r/.")
      dest = tmp_path("tmp")

      try do
        File.mkdir(dest)
        File.write!(Path.join(dest, "a"), "hello")
        assert (File.cp_r(src, dest)  |> is_io_error?)
      after
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_dir_and_dest_dir_using_lists do
      src  = fixture_path("cp_r/.") |> to_char_list
      dest = tmp_path("tmp") |> to_char_list

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
        File.rm_rf dest
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
        File.rm_rf dest
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
        File.rm_rf dest
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
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_unknown! do
      src  = fixture_path("unknown")
      dest = tmp_path("tmp")
      assert_raise File.CopyError, "could not copy recursively from #{src} to #{dest}. #{src}: no such file or directory", fn ->
        File.cp_r!(src, dest)
      end
    end

    test :cp_preserves_mode do
     File.mkdir_p!(tmp_path("tmp"))
     src  = fixture_path("cp_mode")
     dest = tmp_path("tmp/cp_mode")

     File.cp!(src, dest)
     File.Stat[mode: src_mode] = File.stat! src
     File.Stat[mode: dest_mode] = File.stat! dest
     assert src_mode == dest_mode

     # On overwrite
     File.cp! src, dest, fn(_, _) -> true end
     File.Stat[mode: src_mode] = File.stat! src
     File.Stat[mode: dest_mode] = File.stat! dest
     assert src_mode == dest_mode
    end

    defp is_io_error?(result) do
      elem(result, 1) in [:enotdir, :eio, :enoent, :eisdir]
    end
  end

  defmodule Queries do
    use ExUnit.Case

    test :regular do
      assert File.regular?(__FILE__)
      assert File.regular?(binary_to_list(__FILE__))
      refute File.regular?("#{__FILE__}.unknown")
    end

    test :exists do
      assert File.exists?(__FILE__)
      assert File.exists?(fixture_path)
      assert File.exists?(fixture_path("file.txt"))

      refute File.exists?(fixture_path("missing.txt"))
      refute File.exists?("_missing.txt")
    end
  end

  test :ls do
    { :ok, value } = File.ls(fixture_path)
    assert "code_sample.exs" in value
    assert "file.txt" in value

    { :error, :enoent } = File.ls(fixture_path("non-existent-subdirectory"))
  end

  test :ls! do
    value = File.ls!(fixture_path)
    assert "code_sample.exs" in value
    assert "file.txt" in value

    assert_raise File.Error, fn ->
      File.ls!(fixture_path("non-existent-subdirectory"))
    end
  end

  defmodule OpenReadWrite do
    use Elixir.FileCase

    test :read_with_binary do
      assert { :ok, "FOO\n" } = File.read(fixture_path("file.txt"))
      assert { :error, :enoent } = File.read(fixture_path("missing.txt"))
    end

    test :read_with_list do
      assert { :ok, "FOO\n" } = File.read(Path.expand('../fixtures/file.txt', __FILE__))
      assert { :error, :enoent } = File.read(Path.expand('../fixtures/missing.txt', __FILE__))
    end

    test :read_with_utf8 do
      assert { :ok, "Русский\n日\n" } = File.read(Path.expand('../fixtures/utf8.txt', __FILE__))
    end

    test :read! do
      assert File.read!(fixture_path("file.txt")) == "FOO\n"
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
      { :ok, file } = File.open(fixture_path("file.txt"))
      assert IO.gets(file, "") == "FOO\n"
      assert File.close(file) == :ok
    end

    test :open_file_with_charlist do
      { :ok, file } = File.open(fixture_path("file.txt"), [:charlist])
      assert IO.gets(file, "") == 'FOO\n'
      assert File.close(file) == :ok
    end

    test :open_utf8_by_default do
      { :ok, file } = File.open(fixture_path("utf8.txt"), [:utf8])
      assert IO.gets(file, "") == "Русский\n"
      assert File.close(file) == :ok
    end

    test :open_readonly_by_default do
      { :ok, file } = File.open(fixture_path("file.txt"))
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

    test :open_with_binwrite_permission do
      fixture = tmp_path("tmp_text.txt")
      try do
        { :ok, file } = File.open(fixture, [:write])
        assert IO.binwrite(file, "Русский") == :ok
        assert File.close(file) == :ok
        assert File.read(fixture) == { :ok, "Русский" }
      after
        File.rm(fixture)
      end
    end

    test :open_utf8_and_charlist do
      { :ok, file } = File.open(fixture_path("utf8.txt"), [:charlist, :utf8])
      assert IO.gets(file, "") == [1056, 1091, 1089, 1089, 1082, 1080, 1081, 10]
      assert File.close(file) == :ok
    end

    test :open_respects_encoding do
      { :ok, file } = File.open(fixture_path("utf8.txt"), [{:encoding, :latin1}])
      assert IO.gets(file, "") == <<195, 144, 194, 160, 195, 145, 194, 131, 195, 145, 194, 129, 195, 145, 194, 129, 195, 144, 194, 186, 195, 144, 194, 184, 195, 144, 194, 185, 10>>
      assert File.close(file) == :ok
    end

    test :open_a_missing_file do
      assert File.open('missing.txt') == {:error, :enoent}
    end

    test :open_a_file_with_function do
      file = fixture_path("file.txt")
      assert File.open(file, IO.read(&1, :line)) == { :ok, "FOO\n" }
    end

    test :open_a_missing_file! do
      message = "could not open missing.txt: no such file or directory"
      assert_raise File.Error, message, fn ->
        File.open!('missing.txt')
      end
    end

    test :open_a_file_with_function! do
      file = fixture_path("file.txt")
      assert File.open!(file, IO.read(&1, :line)) == "FOO\n"
    end
  end

  defmodule Mkdir do
    use Elixir.FileCase

    test :mkdir_with_binary do
      fixture = tmp_path("tmp_test")
      try do
        refute File.exists?(fixture)
        assert File.mkdir(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rmdir fixture
      end
    end

    test :mkdir_with_list do
      fixture = tmp_path("tmp_test") |> to_char_list
      try do
        refute File.exists?(fixture)
        assert File.mkdir(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rmdir fixture
      end
    end

    test :mkdir_with_invalid_path do
      fixture = fixture_path("file.txt")
      invalid = Path.join fixture, "test"
      assert File.exists?(fixture)
      assert (File.mkdir(invalid) |> is_io_error?)
      refute File.exists?(invalid)
    end

    test :mkdir! do
      fixture = tmp_path("tmp_test")
      try do
        refute File.exists?(fixture)
        assert File.mkdir!(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rmdir fixture
      end
    end

    test :mkdir_with_invalid_path! do
      fixture = fixture_path("file.txt")
      invalid = Path.join fixture, "test"
      assert File.exists?(fixture)
      assert_raise File.Error, %r"^could not make directory #{escape invalid}: (not a directory|no such file or directory)", fn ->
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
        File.rm_rf fixture
      end
    end

    test :mkdir_p_with_nested_directory_and_binary do
      base    = tmp_path("tmp_test")
      fixture = Path.join(base, "test")
      refute File.exists?(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        File.rm_rf base
      end
    end

    test :mkdir_p_with_nested_directory_and_list do
      base    = tmp_path("tmp_test") |> to_char_list
      fixture = Path.join(base, "test")
      refute File.exists?(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        File.rm_rf base
      end
    end

    test :mkdir_p_with_nested_directory_and_existing_parent do
      base    = tmp_path("tmp_test")
      fixture = Path.join(base, "test")

      File.mkdir(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        File.rm_rf base
      end
    end

    test :mkdir_p_with_invalid_path do
      assert File.exists?(fixture_path("file.txt"))
      invalid = Path.join fixture_path("file.txt"), "test/foo"
      assert (File.mkdir(invalid) |> is_io_error?)
      refute File.exists?(invalid)
    end

    test :mkdir_p! do
      fixture = tmp_path("tmp_test")
      try do
        refute File.exists?(fixture)
        assert File.mkdir_p!(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rm_rf fixture
      end
    end

    test :mkdir_p_with_invalid_path! do
      fixture = fixture_path("file.txt")
      invalid = Path.join fixture, "test"
      assert File.exists?(fixture)
      assert_raise File.Error, %r"^could not make directory \(with -p\) #{escape invalid}: (not a directory|no such file or directory)", fn ->
        File.mkdir_p!(invalid)
      end
    end

    defp is_io_error?(result) do
      {:error,errorcode} = result
      errorcode in [:enotdir, :eio, :enoent, :eisdir]
    end

  end

  defmodule Rm do
    use Elixir.FileCase

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
      assert (File.rmdir(fixture_path("file.txt")) |> is_io_error?)
    end

    test :rmdir! do
      fixture = tmp_path("tmp_test")
      File.mkdir_p(fixture)
      assert File.dir?(fixture)
      assert File.rmdir!(fixture) == :ok
      refute File.exists?(fixture)
    end

    test :rmdir_with_file! do
      fixture = fixture_path("file.txt")
      assert_raise File.Error, %r"^could not remove directory #{escape fixture}: (not a directory|I/O error)", fn ->
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

    test :rm_rf_with_symlink do
      from = tmp_path("tmp/from")
      to   = tmp_path("tmp/to")

      File.mkdir_p!(to)
      File.write!(Path.join(to, "hello"), "world")
      :file.make_symlink(to, from)

      if File.exists?(from) or not is_win? do
        assert File.exists?(from)

        { :ok, files } = File.rm_rf(from)
        assert length(files) == 1

        assert File.exists?(Path.join(to, "hello"))
        refute File.exists?(from)
      end
    after
      File.rm(tmp_path("tmp/from"))
    end

    test :rm_rf_with_char_list do
      fixture = tmp_path("tmp") |> to_char_list
      File.mkdir(fixture)
      File.cp_r!(fixture_path("cp_r/."), fixture)

      assert File.exists?(tmp_path("tmp/a/1.txt"))
      assert File.exists?(tmp_path("tmp/a/a/2.txt"))
      assert File.exists?(tmp_path("tmp/b/3.txt"))

      { :ok, files } = File.rm_rf(fixture)
      assert length(files) == 7
      assert fixture in files
      assert (tmp_path("tmp/a/1.txt") |> to_char_list) in files

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
      fixture = fixture_path "file.txt/path"
      assert File.rm_rf(fixture) == { :ok, [] }
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
      fixture = fixture_path "file.txt/path"
      assert File.rm_rf!(fixture) == []
    end

    defp is_io_error?(result) do
      elem(result, 1) in [:enotdir, :eio, :enoent, :eisdir]
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

  test :iterator do
    src  = File.open! fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      iterator = IO.stream(src)
      File.open dest, [:write], fn(target) ->
        Enum.each iterator, fn(line) ->
          IO.write target, Regex.replace(%r/"/, line, "'")
        end
      end
      assert File.read(dest) == { :ok, "FOO\n" }
    after
      File.rm(dest)
    end
  end

  test :iterator_with_path do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      iterator = File.stream!(src)
      File.open dest, [:write], fn(target) ->
        Enum.each iterator, fn(line) ->
          IO.write target, Regex.replace(%r/"/, line, "'")
        end
      end
      assert File.read(dest) == { :ok, "FOO\n" }
    after
      File.rm(dest)
    end
  end

  test :iterator! do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      iterator = File.stream!(src)
      File.open dest, [:write], fn(target) ->
        Enum.each iterator, fn(line) ->
          IO.write target, Regex.replace(%r/"/, line, "'")
        end
      end
      assert File.read(dest) == { :ok, "FOO\n" }
    after
      File.rm(dest)
    end
  end

  test :biniterator do
    src  = File.open! fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      iterator = IO.binstream(src)
      File.open dest, [:write], fn(target) ->
        Enum.each iterator, fn(line) ->
          IO.write target, Regex.replace(%r/"/, line, "'")
        end
      end
      assert File.read(dest) == { :ok, "FOO\n" }
    after
      File.rm(dest)
    end
  end

  test :biniterator_with_path do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      iterator = File.binstream!(src)
      File.open dest, [:write], fn(target) ->
        Enum.each iterator, fn(line) ->
          IO.write target, Regex.replace(%r/"/, line, "'")
        end
      end
      assert File.read(dest) == { :ok, "FOO\n" }
    after
      File.rm(dest)
    end
  end

  test :biniterator! do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      iterator = File.binstream!(src)
      File.open dest, [:write], fn(target) ->
        Enum.each iterator, fn(line) ->
          IO.write target, Regex.replace(%r/"/, line, "'")
        end
      end
      assert File.read(dest) == { :ok, "FOO\n" }
    after
      File.rm(dest)
    end
  end

  test :copy do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy(src, dest) == { :ok, 4 }
      assert File.read(dest) == { :ok, "FOO\n" }
    after
      File.rm(dest)
    end
  end

  test :copy_with_bytes_count do
    src  = fixture_path("file.txt")
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
    src  = fixture_path("file.txt")
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
    src  = fixture_path("file.txt")
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
      assert File.exists?("file.txt")
    after
      File.cd!(current)
    end
  end

  if :file.native_name_encoding == :utf8 do
    test :cwd_and_cd_with_utf8 do
      File.mkdir_p(tmp_path("héllò"))

      File.cd!(tmp_path("héllò"), fn ->
        assert Path.basename(File.cwd!) == "héllò"
      end)
    after
      File.rm_rf tmp_path("héllò")
    end
  end

  test :invalid_cd do
    assert(File.cd(fixture_path("file.txt")) |> is_io_error?)
  end

  test :invalid_cd! do
    message = %r"^could not set current working directory to #{escape fixture_path("file.txt")}: (not a directory|no such file or directory)"
    assert_raise File.Error, message, fn ->
      File.cd!(fixture_path("file.txt"))
    end
  end

  test :cd_with_function do
    assert File.cd!(fixture_path, fn ->
      assert File.exists?("file.txt")
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

  test :touch_with_timestamp do
    fixture = tmp_path("tmp_test.txt")

    try do
      assert File.touch!(fixture) == :ok
      stat = File.stat!(fixture)

      assert File.touch!(fixture, last_year) == :ok
      assert stat.mtime > File.stat!(fixture).mtime
    after
      File.rm(fixture)
    end
  end

  test :touch_with_dir do
    assert File.touch(fixture_path) == :ok
  end

  test :touch_with_failure do
    fixture = fixture_path("file.txt/bar")
    assert (File.touch(fixture) |> is_io_error?)
  end

  test :touch_with_success! do
    assert File.touch!(fixture_path) == :ok
  end

  test :touch_with_failure! do
    fixture = fixture_path("file.txt/bar")
    assert_raise File.Error, %r"could not touch #{escape fixture}: (not a directory|no such file or directory)", fn ->
      File.touch!(fixture)
    end
  end

  test :chmod_with_success do
    fixture = tmp_path("tmp_test.txt")

    File.touch(fixture)
    try do
      assert File.chmod(fixture, 0100666) == :ok
      stat = File.stat!(fixture)
      assert stat.mode == 0100666

      unless is_win? do
        assert File.chmod(fixture, 0100777) == :ok
        stat = File.stat!(fixture)
        assert stat.mode == 0100777
      end
    after
      File.rm(fixture)
    end
  end

  test :chmod_with_success! do
    fixture = tmp_path("tmp_test.txt")

    File.touch(fixture)
    try do
      assert File.chmod!(fixture, 0100666) == :ok
      stat = File.stat!(fixture)
      assert stat.mode == 0100666

      unless is_win? do
        assert File.chmod!(fixture, 0100777) == :ok
        stat = File.stat!(fixture)
        assert stat.mode == 0100777
      end
    after
      File.rm(fixture)
    end
  end

  test :chmod_with_failue do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chmod(fixture, 0100777) == {:error,:enoent}
  end

  test :chmod_with_failue! do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = %r"could not change mode for #{escape fixture}: no such file or directory"
    assert_raise File.Error, message, fn ->
      File.chmod!(fixture, 0100777)
    end
  end

  test :chgrp_with_failue do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chgrp(fixture, 1) == {:error,:enoent}
  end

  test :chgrp_with_failue! do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = %r"could not change group for #{escape fixture}: no such file or directory"
    assert_raise File.Error, message, fn ->
      File.chgrp!(fixture, 1)
    end
  end

  test :chown_with_failue do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chown(fixture, 1) == {:error,:enoent}
  end

  test :chown_with_failue! do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = %r"could not change owner for #{escape fixture}: no such file or directory"
    assert_raise File.Error, message, fn ->
      File.chown!(fixture, 1)
    end
  end

  defp last_year do
    last_year :calendar.local_time
  end

  defp last_year({ { year, month, day }, time }) do
    { { year - 1, month, day }, time }
  end

  defp is_io_error?(result) do
    {:error,errorcode} = result
    errorcode in [:enotdir, :eio, :enoent, :eisdir]
  end
end
