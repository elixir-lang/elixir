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
    on_exit(fn -> File.rm_rf(tmp_path) end)
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
      src  = fixture_path("file.txt")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        assert File.cp(src, dest) == {:error, :eisdir}
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
      assert File.cp(src, dest) == {:error, :eisdir}
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
        assert File.cp_r(src, dest) == {:ok, [dest]}
        assert File.exists?(dest)
      after
        File.rm(dest)
      end
    end

    test :cp_r_with_src_file_and_dest_dir do
      src   = fixture_path("file.txt")
      dest  = tmp_path("tmp")

      File.mkdir(dest)

      try do
        assert io_error? File.cp_r(src, dest)
      after
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_file_and_dest_unknown do
      src   = fixture_path("file.txt")
      dest  = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.cp_r(src, dest) == {:ok, [dest]}
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
        refute File.exists?(tmp_path("tmp/a/1.txt"))
        refute File.exists?(tmp_path("tmp/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/b/3.txt"))

        {:ok, files} = File.cp_r(src, dest)
        assert length(files) == 7
        assert tmp_path("tmp/a") in files
        assert tmp_path("tmp/a/1.txt") in files

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
        assert (File.cp_r(src, dest) |> io_error?)
      after
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_dir_and_dest_unknown do
      src  = fixture_path("cp_r")
      dest = tmp_path("tmp")

      try do
        refute File.exists?(tmp_path("tmp/a/1.txt"))
        refute File.exists?(tmp_path("tmp/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/b/3.txt"))

        {:ok, files} = File.cp_r(src, dest)
        assert length(files) == 7

        assert File.exists?(tmp_path("tmp/a/1.txt"))
        assert File.exists?(tmp_path("tmp/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/b/3.txt"))
      after
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_unknown do
      src  = fixture_path("unknown")
      dest = tmp_path("tmp")
      assert File.cp_r(src, dest) == {:error, :enoent, src}
    end

    test :cp_r_with_dir_and_file_conflict do
      src  = fixture_path("cp_r")
      dest = tmp_path("tmp")

      try do
        File.mkdir(dest)
        File.write!(Path.join(dest, "a"), "hello")
        assert io_error? File.cp_r(src, dest)
      after
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_dir_and_dest_dir_using_lists do
      src  = fixture_path("cp_r") |> to_char_list
      dest = tmp_path("tmp") |> to_char_list

      File.mkdir(dest)

      try do
        refute File.exists?(tmp_path("tmp/a/1.txt"))
        refute File.exists?(tmp_path("tmp/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/b/3.txt"))

        {:ok, files} = File.cp_r(src, dest)
        assert length(files) == 7
        assert Enum.all?(files, &is_binary/1)

        assert File.exists?(tmp_path("tmp/a/1.txt"))
        assert File.exists?(tmp_path("tmp/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/b/3.txt"))
      after
        File.rm_rf dest
      end
    end

    test :cp_r_with_src_with_file_conflict do
      src  = fixture_path("cp_r")
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
      src  = fixture_path("cp_r")
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
      src  = fixture_path("cp_r")
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
     %File.Stat{mode: src_mode} = File.stat! src
     %File.Stat{mode: dest_mode} = File.stat! dest
     assert src_mode == dest_mode

     # On overwrite
     File.cp! src, dest, fn(_, _) -> true end
     %File.Stat{mode: src_mode} = File.stat! src
     %File.Stat{mode: dest_mode} = File.stat! dest
     assert src_mode == dest_mode
    end

    defp io_error?(result) do
      elem(result, 1) in [:enotdir, :eio, :enoent, :eisdir]
    end
  end

  defmodule Queries do
    use ExUnit.Case

    test :regular do
      assert File.regular?(__ENV__.file)
      assert File.regular?(String.to_char_list(__ENV__.file))
      refute File.regular?("#{__ENV__.file}.unknown")
    end

    test :exists do
      assert File.exists?(__ENV__.file)
      assert File.exists?(fixture_path)
      assert File.exists?(fixture_path("file.txt"))

      refute File.exists?(fixture_path("missing.txt"))
      refute File.exists?("_missing.txt")
    end
  end

  test :ls do
    {:ok, value} = File.ls(fixture_path)
    assert "code_sample.exs" in value
    assert "file.txt" in value

    {:error, :enoent} = File.ls(fixture_path("non-existent-subdirectory"))
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
      assert {:ok, "FOO\n"} = File.read(fixture_path("file.txt"))
      assert {:error, :enoent} = File.read(fixture_path("missing.txt"))
    end

    test :read_with_list do
      assert {:ok, "FOO\n"} = File.read(Path.expand('fixtures/file.txt', __DIR__))
      assert {:error, :enoent} = File.read(Path.expand('fixtures/missing.txt', __DIR__))
    end

    test :read_with_utf8 do
      assert {:ok, "Русский\n日\n"} = File.read(Path.expand('fixtures/utf8.txt', __DIR__))
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
        assert File.read(fixture) == {:ok, "test text"}
      after
        File.rm(fixture)
      end
    end

    test :write_utf8 do
      fixture = tmp_path("tmp_test.txt")
      try do
        refute File.exists?(fixture)
        assert File.write(fixture, "Русский\n日\n") == :ok
        assert {:ok, "Русский\n日\n"} == File.read(fixture)
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
        assert {:ok, "Русский\n日\ntest text"} == File.read(fixture)
      after
        File.rm(fixture)
      end
    end

    test :open_file_without_modes do
      {:ok, file} = File.open(fixture_path("file.txt"))
      assert IO.gets(file, "") == "FOO\n"
      assert File.close(file) == :ok
    end

    test :open_file_with_char_list do
      {:ok, file} = File.open(fixture_path("file.txt"), [:char_list])
      assert IO.gets(file, "") == 'FOO\n'
      assert File.close(file) == :ok
    end

    test :open_utf8_by_default do
      {:ok, file} = File.open(fixture_path("utf8.txt"), [:utf8])
      assert IO.gets(file, "") == "Русский\n"
      assert File.close(file) == :ok
    end

    test :open_readonly_by_default do
      {:ok, file} = File.open(fixture_path("file.txt"))
      assert_raise ArgumentError, fn -> IO.write(file, "foo") end
      assert File.close(file) == :ok
    end

    test :open_with_write_permission do
      fixture = tmp_path("tmp_text.txt")
      try do
        {:ok, file} = File.open(fixture, [:write])
        assert IO.write(file, "foo") == :ok
        assert File.close(file) == :ok
        assert File.read(fixture) == {:ok, "foo"}
      after
        File.rm(fixture)
      end
    end

    test :open_with_binwrite_permission do
      fixture = tmp_path("tmp_text.txt")
      try do
        {:ok, file} = File.open(fixture, [:write])
        assert IO.binwrite(file, "Русский") == :ok
        assert File.close(file) == :ok
        assert File.read(fixture) == {:ok, "Русский"}
      after
        File.rm(fixture)
      end
    end

    test :open_utf8_and_charlist do
      {:ok, file} = File.open(fixture_path("utf8.txt"), [:char_list, :utf8])
      assert IO.gets(file, "") == [1056, 1091, 1089, 1089, 1082, 1080, 1081, 10]
      assert File.close(file) == :ok
    end

    test :open_respects_encoding do
      {:ok, file} = File.open(fixture_path("utf8.txt"), [{:encoding, :latin1}])
      assert IO.gets(file, "") == <<195, 144, 194, 160, 195, 145, 194, 131, 195, 145, 194, 129, 195, 145, 194, 129, 195, 144, 194, 186, 195, 144, 194, 184, 195, 144, 194, 185, 10>>
      assert File.close(file) == :ok
    end

    test :open_a_missing_file do
      assert File.open('missing.txt') == {:error, :enoent}
    end

    test :open_a_file_with_function do
      file = fixture_path("file.txt")
      assert File.open(file, &IO.read(&1, :line)) == {:ok, "FOO\n"}
    end

    test :open_a_missing_file! do
      message = "could not open missing.txt: no such file or directory"
      assert_raise File.Error, message, fn ->
        File.open!('missing.txt')
      end
    end

    test :open_a_file_with_function! do
      file = fixture_path("file.txt")
      assert File.open!(file, &IO.read(&1, :line)) == "FOO\n"
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
      assert io_error? File.mkdir(invalid)
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
      assert_raise File.Error, ~r"^could not make directory #{escape invalid}: (not a directory|no such file or directory)", fn ->
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
      assert io_error? File.mkdir(invalid)
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
      assert_raise File.Error, ~r"^could not make directory \(with -p\) #{escape invalid}: (not a directory|no such file or directory)", fn ->
        File.mkdir_p!(invalid)
      end
    end

    defp io_error?(result) do
      {:error, errorcode} = result
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

    test :rm_read_only_file do
      fixture = tmp_path("tmp_test.txt")
      File.write(fixture, "test")
      assert File.exists?(fixture)
      File.chmod(fixture, 0o100444)
      assert File.rm(fixture) == :ok
      refute File.exists?(fixture)
    end

    test :rm_file_with_dir do
      assert File.rm(fixture_path) == {:error, :eperm}
    end

    test :rm_nonexistent_file do
      assert File.rm('missing.txt') == {:error, :enoent}
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
      assert io_error? File.rmdir(fixture_path("file.txt"))
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
      assert_raise File.Error, ~r"^could not remove directory #{escape fixture}: (not a directory|I/O error)", fn ->
        File.rmdir!(fixture)
      end
    end

    test :rm_rf do
      fixture = tmp_path("tmp")
      File.mkdir(fixture)
      File.cp_r!(fixture_path("cp_r"), fixture)

      assert File.exists?(tmp_path("tmp/a/1.txt"))
      assert File.exists?(tmp_path("tmp/a/a/2.txt"))
      assert File.exists?(tmp_path("tmp/b/3.txt"))

      {:ok, files} = File.rm_rf(fixture)
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

      if File.exists?(from) or not windows? do
        assert File.exists?(from)

        {:ok, files} = File.rm_rf(from)
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
      File.cp_r!(fixture_path("cp_r"), fixture)

      assert File.exists?(tmp_path("tmp/a/1.txt"))
      assert File.exists?(tmp_path("tmp/a/a/2.txt"))
      assert File.exists?(tmp_path("tmp/b/3.txt"))

      {:ok, files} = File.rm_rf(fixture)
      assert length(files) == 7
      assert tmp_path("tmp") in files
      assert Enum.all?(files, &is_binary/1)

      refute File.exists?(tmp_path("tmp/a/1.txt"))
      refute File.exists?(tmp_path("tmp/a/a/2.txt"))
      refute File.exists?(tmp_path("tmp/b/3.txt"))
      refute File.exists?(fixture)
    end

    test :rm_rf_with_file do
      fixture = tmp_path("tmp")
      File.write(fixture, "hello")
      assert File.rm_rf(fixture) == {:ok, [fixture]}
    end

    test :rm_rf_with_unknown do
      fixture = tmp_path("tmp.unknown")
      assert File.rm_rf(fixture) == {:ok, []}
    end

    test :rm_rf_with_invalid do
      fixture = fixture_path "file.txt/path"
      assert File.rm_rf(fixture) == {:ok, []}
    end

    test :rm_rf! do
      fixture = tmp_path("tmp")
      File.mkdir(fixture)
      File.cp_r!(fixture_path("cp_r"), fixture)

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

    defp io_error?(result) do
      elem(result, 1) in [:enotdir, :eio, :enoent, :eisdir]
    end
  end

  test :stat do
    {:ok, info} = File.stat(__ENV__.file)
    assert info.mtime
  end

  test :stat! do
    assert File.stat!(__ENV__.file).mtime
  end

  test :stat_with_invalid_file do
    assert {:error, _} = File.stat("./invalid_file")
  end

  test :stat_with_invalid_file! do
    assert_raise File.Error, fn ->
      File.stat!("./invalid_file")
    end
  end

   test :lstat do
    {:ok, info} = File.lstat(__ENV__.file)
    assert info.mtime
  end

  test :lstat! do
    assert File.lstat!(__ENV__.file).mtime
  end

  test :lstat_with_invalid_file do
    invalid_file = tmp_path("invalid_file")
    assert {:error, _} = File.lstat(invalid_file)
  end

  test :lstat_with_invalid_file! do
    invalid_file = tmp_path("invalid_file")
    assert_raise File.Error, fn ->
      File.lstat!(invalid_file)
    end
  end

  test :lstat_with_dangling_symlink do
    invalid_file = tmp_path("invalid_file")
    dest = tmp_path("dangling_symlink")
    File.ln_s(invalid_file, dest)
    try do
      assert {:ok, info } = File.lstat(dest)
      assert info.type == :symlink
    after
      File.rm(dest)
    end
  end

  test :lstat_with_dangling_symlink! do
    invalid_file = tmp_path("invalid_file")
    dest = tmp_path("dangling_symlink")
    File.ln_s(invalid_file, dest)
    try do
     assert File.lstat!(dest).type == :symlink
    after
     File.rm(dest)
    end
  end


  test :io_stream_utf8 do
    src  = File.open! fixture_path("file.txt"), [:utf8]
    dest = tmp_path("tmp_test.txt")

    try do
      stream = IO.stream(src, :line)
      File.open dest, [:write], fn(target) ->
        Enum.into stream, IO.stream(target, :line), &String.replace(&1, "O", "A")
      end
      assert File.read(dest) == {:ok, "FAA\n"}
    after
      File.rm(dest)
    end
  end

  test :io_stream do
    src  = File.open! fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      stream = IO.binstream(src, :line)
      File.open dest, [:write], fn(target) ->
        Enum.into stream, IO.binstream(target, :line), &String.replace(&1, "O", "A")
      end
      assert File.read(dest) == {:ok, "FAA\n"}
    after
      File.rm(dest)
    end
  end

  test :stream_map do
    src = fixture_path("file.txt")
    stream = File.stream!(src)
    assert %File.Stream{} = stream
    assert stream.modes == [:raw, :read_ahead, :binary]
    assert stream.raw
    assert stream.line_or_bytes == :line

    src = fixture_path("file.txt")
    stream = File.stream!(src, [:utf8], 10)
    assert %File.Stream{} = stream
    assert stream.modes == [{:encoding, :utf8}, :binary]
    refute stream.raw
    assert stream.line_or_bytes == 10
  end

  test :stream_line_utf8 do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      stream = File.stream!(src)
      File.open dest, [:write, :utf8], fn(target) ->
        Enum.each stream, fn(line) ->
          IO.write target, String.replace(line, "O", "A")
        end
      end
      assert File.read(dest) == {:ok, "FAA\n"}
    after
      File.rm(dest)
    end
  end

  test :stream_bytes_utf8 do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      stream = File.stream!(src, [:utf8], 1)
      File.open dest, [:write], fn(target) ->
        Enum.each stream, fn(line) ->
          IO.write target, String.replace(line, "OO", "AA")
        end
      end
      assert File.read(dest) == {:ok, "FOO\n"}
    after
      File.rm(dest)
    end
  end

  test :stream_line do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      stream = File.stream!(src)
      File.open dest, [:write], fn(target) ->
        Enum.each stream, fn(line) ->
          IO.write target, String.replace(line, "O", "A")
        end
      end
      assert File.read(dest) == {:ok, "FAA\n"}
    after
      File.rm(dest)
    end
  end

  test :stream_bytes do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      stream = File.stream!(src, [], 1)
      File.open dest, [:write], fn(target) ->
        Enum.each stream, fn(line) ->
          IO.write target, String.replace(line, "OO", "AA")
        end
      end
      assert File.read(dest) == {:ok, "FOO\n"}
    after
      File.rm(dest)
    end
  end

  test :stream_into do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)

      original = File.stream!(dest)
      stream   = File.stream!(src)
                 |> Stream.map(&String.replace(&1, "O", "A"))
                 |> Enum.into(original)

      assert stream == original
      assert File.read(dest) == {:ok, "FAA\n"}
    after
      File.rm(dest)
    end
  end

  test :stream_into_append do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      original = File.stream!(dest, [:append])

      File.stream!(src, [:append])
      |> Stream.map(&String.replace(&1, "O", "A"))
      |> Enum.into(original)

      File.stream!(src, [:append])
      |> Enum.into(original)

      assert File.read(dest) == {:ok, "FAA\nFOO\n"}
    after
      File.rm(dest)
    end
  end

  test :ln_s do
    existing  = fixture_path("file.txt")
    new = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(new)
      assert File.ln_s(existing, new) == :ok
      assert File.read(new) == {:ok, "FOO\n"}
    after
      File.rm(new)
    end
  end

  test :ln_s_with_existing_destination do
    existing  = fixture_path("file.txt")
    assert File.ln_s(existing, existing) == {:error, :eexist}
  end

  test :copy do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy(src, dest) == {:ok, 4}
      assert File.read(dest) == {:ok, "FOO\n"}
    after
      File.rm(dest)
    end
  end

  test :copy_with_bytes_count do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy(src, dest, 2) == {:ok, 2}
      assert {:ok, "FO"} == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test :copy_with_invalid_file do
    src  = fixture_path("invalid.txt")
    dest = tmp_path("tmp_test.txt")
    assert File.copy(src, dest, 2) == {:error, :enoent}
  end

  test :copy! do
    src  = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")
    try do
      refute File.exists?(dest)
      assert File.copy!(src, dest) == 4
      assert {:ok, "FOO\n"} == File.read(dest)
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
      assert {:ok, "FO"} == File.read(dest)
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
    {:ok, current} = File.cwd
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
    assert io_error? File.cd(fixture_path("file.txt"))
  end

  test :invalid_cd! do
    message = ~r"^could not set current working directory to #{escape fixture_path("file.txt")}: (not a directory|no such file or directory)"
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
    time = {{2010, 4, 17}, {14, 0, 0}}

    try do
      refute File.exists?(fixture)
      assert File.touch(fixture, time) == :ok
      assert {:ok, ""} == File.read(fixture)
      assert File.stat!(fixture).mtime == time
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
    assert io_error? File.touch(fixture)
  end

  test :touch_with_success! do
    assert File.touch!(fixture_path) == :ok
  end

  test :touch_with_failure! do
    fixture = fixture_path("file.txt/bar")
    assert_raise File.Error, ~r"could not touch #{escape fixture}: (not a directory|no such file or directory)", fn ->
      File.touch!(fixture)
    end
  end

  test :chmod_with_success do
    fixture = tmp_path("tmp_test.txt")

    File.touch(fixture)
    try do
      assert File.chmod(fixture, 0o100666) == :ok
      stat = File.stat!(fixture)
      assert stat.mode == 0o100666

      unless windows? do
        assert File.chmod(fixture, 0o100777) == :ok
        stat = File.stat!(fixture)
        assert stat.mode == 0o100777
      end
    after
      File.rm(fixture)
    end
  end

  test :chmod_with_success! do
    fixture = tmp_path("tmp_test.txt")

    File.touch(fixture)
    try do
      assert File.chmod!(fixture, 0o100666) == :ok
      stat = File.stat!(fixture)
      assert stat.mode == 0o100666

      unless windows? do
        assert File.chmod!(fixture, 0o100777) == :ok
        stat = File.stat!(fixture)
        assert stat.mode == 0o100777
      end
    after
      File.rm(fixture)
    end
  end

  test :chmod_with_failure do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chmod(fixture, 0o100777) == {:error, :enoent}
  end

  test :chmod_with_failure! do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = ~r"could not change mode for #{escape fixture}: no such file or directory"
    assert_raise File.Error, message, fn ->
      File.chmod!(fixture, 0o100777)
    end
  end

  test :chgrp_with_failure do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chgrp(fixture, 1) == {:error, :enoent}
  end

  test :chgrp_with_failure! do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = ~r"could not change group for #{escape fixture}: no such file or directory"
    assert_raise File.Error, message, fn ->
      File.chgrp!(fixture, 1)
    end
  end

  test :chown_with_failure do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chown(fixture, 1) == {:error, :enoent}
  end

  test :chown_with_failure! do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = ~r"could not change owner for #{escape fixture}: no such file or directory"
    assert_raise File.Error, message, fn ->
      File.chown!(fixture, 1)
    end
  end

  defp last_year do
    last_year :calendar.local_time
  end

  defp last_year({{year, month, day}, time}) do
    {{year - 1, month, day}, time}
  end

  defp io_error?(result) do
    {:error, errorcode} = result
    errorcode in [:enotdir, :eio, :enoent, :eisdir]
  end
end
