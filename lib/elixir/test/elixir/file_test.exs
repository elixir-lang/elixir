Code.require_file("test_helper.exs", __DIR__)

defmodule Elixir.FileCase do
  use ExUnit.CaseTemplate
  import PathHelpers

  using do
    quote do
      import PathHelpers
    end
  end

  setup do
    File.mkdir_p!(tmp_path())
    on_exit(fn -> File.rm_rf(tmp_path()) end)
    :ok
  end
end

defmodule FileTest do
  use Elixir.FileCase

  defmodule Rename do
    # Following Erlang's underlying implementation
    #
    # Renaming files
    # :ok               -> rename file to existing file default behaviour
    # {:error, :eisdir} -> rename file to existing empty dir
    # {:error, :eisdir} -> rename file to existing non-empty dir
    # :ok               -> rename file to non-existing location
    # {:error, :eexist} -> rename file to existing file
    # :ok               -> rename file to itself

    # Renaming dirs
    # {:error, :enotdir} -> rename dir to existing file
    # :ok                -> rename dir to non-existing leaf location
    # {:error, ??}       -> rename dir to non-existing parent location
    # :ok                -> rename dir to itself
    # :ok                -> rename dir to existing empty dir default behaviour
    # {:error, :eexist}  -> rename dir to existing empty dir
    # {:error, :einval}  -> rename parent dir to existing sub dir
    # {:error, :einval}  -> rename parent dir to non-existing sub dir
    # {:error, :eexist}  -> rename dir to existing non-empty dir

    # other tests
    # {:error, :enoent} -> rename unknown source
    # :ok               -> rename preserves mode
    use Elixir.FileCase

    test "rename file to existing file default behaviour" do
      src = tmp_fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)
        assert File.rename(src, dest) == :ok
        refute File.exists?(src)
        assert File.read!(dest) == "FOO\n"
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename file to existing empty dir" do
      src = tmp_fixture_path("file.txt")
      dest = tmp_path("tmp")

      try do
        File.mkdir(dest)
        assert File.rename(src, dest) == {:error, :eisdir}
        assert File.exists?(src)
        refute File.exists?(tmp_path("tmp/file.txt"))
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename file to existing non-empty dir" do
      src = tmp_fixture_path("file.txt")
      dest = tmp_path("tmp")

      try do
        File.mkdir_p(Path.join(dest, "a"))
        assert File.rename(src, dest) == {:error, :eisdir}
        assert File.exists?(src)
        refute File.exists?(Path.join(dest, "file.txt"))
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename file to non-existing location" do
      src = tmp_fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.rename(src, dest) == :ok
        assert File.exists?(dest)
        refute File.exists?(src)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename file to existing file" do
      src = tmp_fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)
        assert File.rename(src, dest) == :ok
        refute File.exists?(src)
        assert File.read!(dest) == "FOO\n"
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename file to itself" do
      src = tmp_fixture_path("file.txt")
      dest = src

      try do
        assert File.exists?(src)
        assert File.rename(src, dest) == :ok
        assert File.exists?(src)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename! file to existing file default behaviour" do
      src = tmp_fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)
        assert File.rename!(src, dest) == :ok
        refute File.exists?(src)
        assert File.read!(dest) == "FOO\n"
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename! with invalid file" do
      src = tmp_fixture_path("invalid.txt")
      dest = tmp_path("tmp.file")

      message =
        "could not rename from #{inspect(src)} to #{inspect(dest)}: no such file or directory"

      assert_raise File.RenameError, message, fn ->
        File.rename!(src, dest)
      end
    end

    test "rename dir to existing file" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("tmp.file")

      try do
        File.touch(dest)
        assert File.rename(src, dest) == {:error, :enotdir}
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename dir to non-existing leaf location" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("tmp")

      try do
        refute File.exists?(tmp_path("tmp/a/1.txt"))
        refute File.exists?(tmp_path("tmp/a/a/2.txt"))
        refute File.exists?(tmp_path("tmp/b/3.txt"))

        assert File.rename(src, dest) == :ok
        {:ok, files} = File.ls(dest)
        assert length(files) == 2
        assert "a" in files

        {:ok, files} = File.ls(tmp_path("tmp/a"))
        assert length(files) == 2
        assert "1.txt" in files

        assert File.exists?(tmp_path("tmp/a/1.txt"))
        assert File.exists?(tmp_path("tmp/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/b/3.txt"))

        refute File.exists?(src)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename dir to non-existing parent location" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("tmp/a/b")

      try do
        assert File.rename(src, dest) == {:error, :enoent}
        assert File.exists?(src)
        refute File.exists?(dest)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename dir to itself" do
      src = tmp_fixture_path("cp_r")
      dest = src

      try do
        assert File.exists?(src)
        assert File.rename(src, dest) == :ok
        assert File.exists?(src)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename parent dir to existing sub dir" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("cp_r/a")

      try do
        assert File.exists?(src)
        assert File.rename(src, dest) == {:error, :einval}
        assert File.exists?(src)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename parent dir to non-existing sub dir" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("cp_r/x")

      try do
        assert File.exists?(src)
        assert File.rename(src, dest) == {:error, :einval}
        assert File.exists?(src)
        refute File.exists?(dest)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename dir to existing empty dir default behaviour" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        refute File.exists?(tmp_path("tmp/a"))

        assert File.rename(src, dest) == :ok
        {:ok, files} = File.ls(dest)
        assert length(files) == 2
        assert "a" in files

        {:ok, files} = File.ls(tmp_path("tmp/a"))
        assert length(files) == 2
        assert "1.txt" in files

        assert File.exists?(tmp_path("tmp/a/1.txt"))
        assert File.exists?(tmp_path("tmp/a/a/2.txt"))
        assert File.exists?(tmp_path("tmp/b/3.txt"))

        refute File.exists?(src)
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename dir to existing empty dir" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        assert File.exists?(dest)
        assert File.rename(src, dest) == :ok
        refute File.exists?(src)
        assert File.exists?(tmp_path("tmp/a"))
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename dir to existing non-empty dir" do
      src = tmp_fixture_path("cp_r")
      dest = tmp_path("tmp")

      File.mkdir_p(tmp_path("tmp/x"))

      try do
        assert File.exists?(tmp_path("tmp/x"))
        assert File.exists?(src)
        refute File.exists?(tmp_path("tmp/a"))

        assert File.rename(src, dest) == {:error, :eexist}

        assert File.exists?(tmp_path("tmp/x"))
        assert File.exists?(src)
        refute File.exists?(tmp_path("tmp/a"))
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    test "rename unknown source" do
      src = fixture_path("unknown")
      dest = tmp_path("tmp")

      try do
        assert File.rename(src, dest) == {:error, :enoent}
      after
        File.rm_rf(dest)
      end
    end

    test "rename preserves mode" do
      File.mkdir_p!(tmp_path("tmp"))
      src = tmp_fixture_path("cp_mode")
      dest = tmp_path("tmp/cp_mode")

      try do
        %File.Stat{mode: src_mode} = File.stat!(src)
        File.rename(src, dest)
        %File.Stat{mode: dest_mode} = File.stat!(dest)
        assert src_mode == dest_mode
      after
        File.rm_rf(src)
        File.rm_rf(dest)
      end
    end

    def tmp_fixture_path(extra) do
      src = fixture_path(extra)
      dest = tmp_path(extra)
      File.cp_r(src, dest)
      dest
    end
  end

  defmodule Cp do
    use Elixir.FileCase

    test "cp with src file and dest file" do
      src = fixture_path("file.txt")
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

    test "cp with src file and dest dir" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        assert File.cp(src, dest) == {:error, :eisdir}
      after
        File.rm_rf(dest)
      end
    end

    test "cp with src file and dest unknown" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.cp(src, dest) == :ok
        assert File.exists?(dest)
      after
        File.rm_rf(dest)
      end
    end

    test "cp with src dir" do
      src = fixture_path("cp_r")
      dest = tmp_path("tmp.file")
      assert File.cp(src, dest) == {:error, :eisdir}
    end

    test "cp with conflict" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)
        assert File.cp(src, dest) == :ok
        assert File.read!(dest) == "FOO\n"
      after
        File.rm_rf(dest)
      end
    end

    test "cp with conflict with function" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      File.write!(dest, "hello")

      try do
        assert File.exists?(dest)

        assert File.cp(src, dest, fn src_file, dest_file ->
                 assert src_file == src
                 assert dest_file == dest
                 false
               end) == :ok

        assert File.read!(dest) == "hello"
      after
        File.rm_rf(dest)
      end
    end

    test "cp! with src file and dest file" do
      src = fixture_path("file.txt")
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

    test "cp! with src dir" do
      src = fixture_path("cp_r")
      dest = tmp_path("tmp.file")

      message =
        "could not copy from #{inspect(src)} to #{inspect(dest)}: illegal operation on a directory"

      assert_raise File.CopyError, message, fn ->
        File.cp!(src, dest)
      end
    end

    test "copy file to itself" do
      src = dest = tmp_path("tmp.file")

      File.write!(src, "here")

      try do
        assert File.cp(src, dest) == :ok
        assert File.read!(dest) == "here"
        assert File.cp_r(src, dest) == {:ok, []}
      after
        File.rm(dest)
      end
    end

    test "cp_r raises on path with null byte" do
      assert_raise ArgumentError, ~r/null byte/, fn -> File.cp_r("source", "foo\0bar") end
      assert_raise ArgumentError, ~r/null byte/, fn -> File.cp_r("foo\0bar", "dest") end
    end

    test "cp_r with src file and dest file" do
      src = fixture_path("file.txt")
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

    test "cp_r with src file and dest dir" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp")

      File.mkdir(dest)

      try do
        assert io_error?(File.cp_r(src, dest))
      after
        File.rm_rf(dest)
      end
    end

    test "cp_r with src file and dest unknown" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp.file")

      try do
        refute File.exists?(dest)
        assert File.cp_r(src, dest) == {:ok, [dest]}
        assert File.exists?(dest)
      after
        File.rm_rf(dest)
      end
    end

    test "cp_r with src dir and dest dir" do
      src = fixture_path("cp_r")
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
        File.rm_rf(dest)
      end
    end

    test "cp_r with src dir and dest file" do
      src = fixture_path("cp_r")
      dest = tmp_path("tmp.file")

      try do
        File.touch!(dest)
        assert File.cp_r(src, dest) |> io_error?
      after
        File.rm_rf(dest)
      end
    end

    test "cp_r with src dir and dest unknown" do
      src = fixture_path("cp_r")
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
        File.rm_rf(dest)
      end
    end

    test "cp_r with src unknown" do
      src = fixture_path("unknown")
      dest = tmp_path("tmp")
      assert File.cp_r(src, dest) == {:error, :enoent, src}
    end

    test "cp_r with dir and file conflict" do
      src = fixture_path("cp_r")
      dest = tmp_path("tmp")

      try do
        File.mkdir(dest)
        File.write!(Path.join(dest, "a"), "hello")
        assert io_error?(File.cp_r(src, dest))
      after
        File.rm_rf(dest)
      end
    end

    test "cp_r with src dir and dest dir using lists" do
      src = fixture_path("cp_r") |> to_charlist
      dest = tmp_path("tmp") |> to_charlist

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
        File.rm_rf(dest)
      end
    end

    test "cp_r with src with file conflict" do
      src = fixture_path("cp_r")
      dest = tmp_path("tmp")

      File.mkdir_p(tmp_path("tmp/a"))
      File.write!(tmp_path("tmp/a/1.txt"), "hello")

      try do
        assert File.exists?(tmp_path("tmp/a/1.txt"))
        File.cp_r(src, dest)
        assert File.read!(tmp_path("tmp/a/1.txt")) == ""
      after
        File.rm_rf(dest)
      end
    end

    test "cp_r with src with file conflict callback" do
      src = fixture_path("cp_r")
      dest = tmp_path("tmp")

      File.mkdir_p(tmp_path("tmp/a"))
      File.write!(tmp_path("tmp/a/1.txt"), "hello")

      try do
        assert File.exists?(tmp_path("tmp/a/1.txt"))

        File.cp_r(src, dest, fn src_file, dest_file ->
          assert src_file == fixture_path("cp_r/a/1.txt")
          assert dest_file == tmp_path("tmp/a/1.txt")
          false
        end)

        assert File.read!(tmp_path("tmp/a/1.txt")) == "hello"
      after
        File.rm_rf(dest)
      end
    end

    test "cp_r!" do
      src = fixture_path("cp_r")
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
        File.rm_rf(dest)
      end
    end

    test "cp_r with src_unknown!" do
      src = fixture_path("unknown")
      dest = tmp_path("tmp")

      message =
        "could not copy recursively from #{inspect(src)} to #{inspect(dest)}. #{src}: no such file or directory"

      assert_raise File.CopyError, message, fn ->
        File.cp_r!(src, dest)
      end
    end

    test "cp preserves mode" do
      File.mkdir_p!(tmp_path("tmp"))
      src = fixture_path("cp_mode")
      dest = tmp_path("tmp/cp_mode")

      File.cp!(src, dest)
      %File.Stat{mode: src_mode} = File.stat!(src)
      %File.Stat{mode: dest_mode} = File.stat!(dest)
      assert src_mode == dest_mode

      # On overwrite
      File.cp!(src, dest, fn _, _ -> true end)
      %File.Stat{mode: src_mode} = File.stat!(src)
      %File.Stat{mode: dest_mode} = File.stat!(dest)
      assert src_mode == dest_mode
    end

    defp io_error?(result) do
      elem(result, 1) in [:enotdir, :eio, :enoent, :eisdir]
    end
  end

  defmodule Queries do
    use ExUnit.Case

    test "regular" do
      assert File.regular?(__ENV__.file)
      assert File.regular?(String.to_charlist(__ENV__.file))
      refute File.regular?("#{__ENV__.file}.unknown")
    end

    test "exists" do
      assert File.exists?(__ENV__.file)
      assert File.exists?(fixture_path())
      assert File.exists?(fixture_path("file.txt"))

      refute File.exists?(fixture_path("missing.txt"))
      refute File.exists?("_missing.txt")
    end

    test "exists with dangling symlink" do
      invalid_file = tmp_path("invalid_file")
      dest = tmp_path("dangling_symlink")
      File.ln_s(invalid_file, dest)

      try do
        refute File.exists?(dest)
      after
        File.rm(dest)
      end
    end
  end

  test "ls" do
    {:ok, value} = File.ls(fixture_path())
    assert "code_sample.exs" in value
    assert "file.txt" in value

    {:error, :enoent} = File.ls(fixture_path("non-existent-subdirectory"))
  end

  test "ls!" do
    value = File.ls!(fixture_path())
    assert "code_sample.exs" in value
    assert "file.txt" in value

    assert_raise File.Error, fn ->
      File.ls!(fixture_path("non-existent-subdirectory"))
    end
  end

  defmodule OpenReadWrite do
    use Elixir.FileCase

    test "read with binary" do
      assert {:ok, "FOO\n"} = File.read(fixture_path("file.txt"))
      assert {:error, :enoent} = File.read(fixture_path("missing.txt"))
    end

    test "read with list" do
      assert {:ok, "FOO\n"} = File.read(Path.expand('fixtures/file.txt', __DIR__))
      assert {:error, :enoent} = File.read(Path.expand('fixtures/missing.txt', __DIR__))
    end

    test "read with UTF-8" do
      assert {:ok, "Русский\n日\n"} = File.read(Path.expand('fixtures/utf8.txt', __DIR__))
    end

    test "read!" do
      assert File.read!(fixture_path("file.txt")) == "FOO\n"
      expected_message = "could not read file \"fixtures/missing.txt\": no such file or directory"

      assert_raise File.Error, expected_message, fn ->
        File.read!("fixtures/missing.txt")
      end
    end

    test "write ASCII content" do
      fixture = tmp_path("tmp_test.txt")

      try do
        refute File.exists?(fixture)
        assert File.write(fixture, 'test text') == :ok
        assert File.read(fixture) == {:ok, "test text"}
      after
        File.rm(fixture)
      end
    end

    test "write UTF-8" do
      fixture = tmp_path("tmp_test.txt")

      try do
        refute File.exists?(fixture)
        assert File.write(fixture, "Русский\n日\n") == :ok
        assert {:ok, "Русский\n日\n"} == File.read(fixture)
      after
        File.rm(fixture)
      end
    end

    test "write with options" do
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

    test "open file without modes" do
      {:ok, file} = File.open(fixture_path("file.txt"))
      assert IO.gets(file, "") == "FOO\n"
      assert File.close(file) == :ok
    end

    test "open file with charlist" do
      {:ok, file} = File.open(fixture_path("file.txt"), [:charlist])
      assert IO.gets(file, "") == 'FOO\n'
      assert File.close(file) == :ok
    end

    test "open UTF-8 by default" do
      {:ok, file} = File.open(fixture_path("utf8.txt"), [:utf8])
      assert IO.gets(file, "") == "Русский\n"
      assert File.close(file) == :ok
    end

    test "open readonly by default" do
      {:ok, file} = File.open(fixture_path("file.txt"))
      assert_raise ArgumentError, fn -> IO.write(file, "foo") end
      assert File.close(file) == :ok
    end

    test "open with write permission" do
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

    test "open with binwrite permission" do
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

    test "open UTF-8 and charlist" do
      {:ok, file} = File.open(fixture_path("utf8.txt"), [:charlist, :utf8])
      assert IO.gets(file, "") == [1056, 1091, 1089, 1089, 1082, 1080, 1081, 10]
      assert File.close(file) == :ok
    end

    test "open respects encoding" do
      {:ok, file} = File.open(fixture_path("utf8.txt"), [{:encoding, :latin1}])

      data =
        <<195, 144, 194, 160, 195, 145, 194, 131, 195, 145, 194, 129, 195, 145>> <>
          <<194, 129, 195, 144, 194, 186, 195, 144, 194, 184, 195, 144, 194, 185, 10>>

      assert IO.gets(file, "") == data
      assert File.close(file) == :ok
    end

    test "open a missing file" do
      assert File.open('missing.txt') == {:error, :enoent}
    end

    test "open a file with function" do
      file = fixture_path("file.txt")
      assert File.open(file, &IO.read(&1, :line)) == {:ok, "FOO\n"}
    end

    test "open! a missing file" do
      message = "could not open \"missing.txt\": no such file or directory"

      assert_raise File.Error, message, fn ->
        File.open!('missing.txt')
      end
    end

    test "open! a file with function" do
      file = fixture_path("file.txt")
      assert File.open!(file, &IO.read(&1, :line)) == "FOO\n"
    end
  end

  defmodule Mkdir do
    use Elixir.FileCase

    test "mkdir with binary" do
      fixture = tmp_path("tmp_test")

      try do
        refute File.exists?(fixture)
        assert File.mkdir(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rmdir(fixture)
      end
    end

    test "mkdir with list" do
      fixture = tmp_path("tmp_test") |> to_charlist

      try do
        refute File.exists?(fixture)
        assert File.mkdir(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rmdir(fixture)
      end
    end

    test "mkdir with invalid path" do
      fixture = fixture_path("file.txt")
      invalid = Path.join(fixture, "test")
      assert File.exists?(fixture)
      assert io_error?(File.mkdir(invalid))
      refute File.exists?(invalid)
    end

    test "mkdir!" do
      fixture = tmp_path("tmp_test")

      try do
        refute File.exists?(fixture)
        assert File.mkdir!(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rmdir(fixture)
      end
    end

    test "mkdir! with invalid path" do
      fixture = fixture_path("file.txt")
      invalid = Path.join(fixture, "test")
      assert File.exists?(fixture)

      message =
        ~r"\Acould not make directory #{inspect(invalid)}: (not a directory|no such file or directory)"

      assert_raise File.Error, message, fn ->
        File.mkdir!(invalid)
      end
    end

    test "mkdir_p with one directory" do
      fixture = tmp_path("tmp_test")

      try do
        refute File.exists?(fixture)
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rm_rf(fixture)
      end
    end

    test "mkdir_p with nested directory and binary" do
      base = tmp_path("tmp_test")
      fixture = Path.join(base, "test")
      refute File.exists?(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        File.rm_rf(base)
      end
    end

    test "mkdir_p with nested directory and list" do
      base = tmp_path("tmp_test") |> to_charlist
      fixture = Path.join(base, "test")
      refute File.exists?(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        File.rm_rf(base)
      end
    end

    test "mkdir_p with nested directory and existing parent" do
      base = tmp_path("tmp_test")
      fixture = Path.join(base, "test")

      File.mkdir(base)

      try do
        assert File.mkdir_p(fixture) == :ok
        assert File.exists?(base)
        assert File.exists?(fixture)
      after
        File.rm_rf(base)
      end
    end

    test "mkdir_p with invalid path" do
      assert File.exists?(fixture_path("file.txt"))
      invalid = Path.join(fixture_path("file.txt"), "test/foo")
      assert io_error?(File.mkdir(invalid))
      refute File.exists?(invalid)
    end

    test "mkdir_p!" do
      fixture = tmp_path("tmp_test")

      try do
        refute File.exists?(fixture)
        assert File.mkdir_p!(fixture) == :ok
        assert File.exists?(fixture)
      after
        File.rm_rf(fixture)
      end
    end

    test "mkdir_p! with invalid path" do
      fixture = fixture_path("file.txt")
      invalid = Path.join(fixture, "test")
      assert File.exists?(fixture)

      message =
        ~r"\Acould not make directory \(with -p\) #{inspect(invalid)}: (not a directory|no such file or directory)"

      assert_raise File.Error, message, fn ->
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

    test "rm file" do
      fixture = tmp_path("tmp_test.txt")
      File.write(fixture, "test")
      assert File.exists?(fixture)
      assert File.rm(fixture) == :ok
      refute File.exists?(fixture)
    end

    test "rm read only file" do
      fixture = tmp_path("tmp_test.txt")
      File.write(fixture, "test")
      assert File.exists?(fixture)
      File.chmod(fixture, 0o100444)
      assert File.rm(fixture) == :ok
      refute File.exists?(fixture)
    end

    test "rm file with dir" do
      assert File.rm(fixture_path()) == {:error, :eperm}
    end

    test "rm nonexistent file" do
      assert File.rm('missing.txt') == {:error, :enoent}
    end

    test "rm!" do
      fixture = tmp_path("tmp_test.txt")
      File.write(fixture, "test")
      assert File.exists?(fixture)
      assert File.rm!(fixture) == :ok
      refute File.exists?(fixture)
    end

    test "rm! with invalid file" do
      message = "could not remove file \"missing.file\": no such file or directory"

      assert_raise File.Error, message, fn ->
        File.rm!("missing.file")
      end
    end

    test "rmdir" do
      fixture = tmp_path("tmp_test")
      File.mkdir_p(fixture)
      assert File.dir?(fixture)
      assert File.rmdir(fixture) == :ok
      refute File.exists?(fixture)
    end

    test "rmdir with file" do
      assert io_error?(File.rmdir(fixture_path("file.txt")))
    end

    test "rmdir!" do
      fixture = tmp_path("tmp_test")
      File.mkdir_p(fixture)
      assert File.dir?(fixture)
      assert File.rmdir!(fixture) == :ok
      refute File.exists?(fixture)
    end

    test "rmdir! with file" do
      fixture = fixture_path("file.txt")
      message = ~r"\Acould not remove directory #{inspect(fixture)}: (not a directory|I/O error)"

      assert_raise File.Error, message, fn ->
        File.rmdir!(fixture)
      end
    end

    test "rmdir! error messages" do
      fixture = tmp_path("tmp_test")
      File.mkdir_p(fixture)
      File.touch(fixture <> "/file")

      # directory is not empty
      dir_not_empty_message =
        "could not remove directory #{inspect(fixture)}: directory is not empty"

      assert_raise File.Error, dir_not_empty_message, fn ->
        File.rmdir!(fixture)
      end

      # directory does not exist
      non_existent_dir = fixture <> "/non_existent_dir"

      non_existent_dir_message =
        ~r"\Acould not remove directory #{inspect(non_existent_dir)}: (not a directory|no such file or directory)"

      assert_raise File.Error, non_existent_dir_message, fn ->
        File.rmdir!(non_existent_dir)
      end

      File.rm_rf(fixture)
    end

    test "rm_rf" do
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

    test "rm_rf raises on path with null byte" do
      assert_raise ArgumentError, ~r/null byte/, fn -> File.rm_rf("foo\0bar") end
    end

    test "rm_rf with symlink" do
      from = tmp_path("tmp/from")
      to = tmp_path("tmp/to")

      File.mkdir_p!(to)
      File.write!(Path.join(to, "hello"), "world")
      :file.make_symlink(to, from)

      if File.exists?(from) or not windows?() do
        assert File.exists?(from)

        {:ok, files} = File.rm_rf(from)
        assert length(files) == 1

        assert File.exists?(Path.join(to, "hello"))
        refute File.exists?(from)
      end
    after
      File.rm(tmp_path("tmp/from"))
    end

    test "rm_rf with charlist" do
      fixture = tmp_path("tmp") |> to_charlist
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

    test "rm_rf with file" do
      fixture = tmp_path("tmp")
      File.write(fixture, "hello")
      assert File.rm_rf(fixture) == {:ok, [fixture]}
    end

    test "rm_rf with unknown" do
      fixture = tmp_path("tmp.unknown")
      assert File.rm_rf(fixture) == {:ok, []}
    end

    test "rm_rf with invalid" do
      fixture = fixture_path("file.txt/path")
      assert File.rm_rf(fixture) == {:ok, []}
    end

    test "rm_rf!" do
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

    test "rm_rf! with invalid path" do
      fixture = fixture_path("file.txt/path")
      assert File.rm_rf!(fixture) == []
    end

    defp io_error?(result) do
      elem(result, 1) in [:enotdir, :eio, :enoent, :eisdir]
    end
  end

  test "stat" do
    {:ok, info} = File.stat(__ENV__.file)
    assert info.mtime
  end

  test "stat!" do
    assert File.stat!(__ENV__.file).mtime
  end

  test "stat with invalid file" do
    assert {:error, _} = File.stat("./invalid_file")
  end

  test "stat! with invalid_file" do
    assert_raise File.Error, fn ->
      File.stat!("./invalid_file")
    end
  end

  test "lstat" do
    {:ok, info} = File.lstat(__ENV__.file)
    assert info.mtime
  end

  test "lstat!" do
    assert File.lstat!(__ENV__.file).mtime
  end

  test "lstat with invalid file" do
    invalid_file = tmp_path("invalid_file")
    assert {:error, _} = File.lstat(invalid_file)
  end

  test "lstat! with invalid file" do
    invalid_file = tmp_path("invalid_file")

    assert_raise File.Error, fn ->
      File.lstat!(invalid_file)
    end
  end

  test "lstat with dangling symlink" do
    invalid_file = tmp_path("invalid_file")
    dest = tmp_path("dangling_symlink")
    File.ln_s(invalid_file, dest)

    try do
      assert {:ok, info} = File.lstat(dest)
      assert info.type == :symlink
    after
      File.rm(dest)
    end
  end

  test "lstat! with dangling symlink" do
    invalid_file = tmp_path("invalid_file")
    dest = tmp_path("dangling_symlink")
    File.ln_s(invalid_file, dest)

    try do
      assert File.lstat!(dest).type == :symlink
    after
      File.rm(dest)
    end
  end

  test "read_link with regular file" do
    dest = tmp_path("symlink")
    File.touch(dest)

    try do
      assert File.read_link(dest) == {:error, :einval}
    after
      File.rm(dest)
    end
  end

  test "read_link with nonexistent file" do
    dest = tmp_path("does_not_exist")
    assert File.read_link(dest) == {:error, :enoent}
  end

  test "read_link! with nonexistent file" do
    dest = tmp_path("does_not_exist")
    assert_raise File.Error, fn -> File.read_link!(dest) end
  end

  unless windows?() do
    test "read_link with symlink" do
      target = tmp_path("does_not_need_to_exist")
      dest = tmp_path("symlink")
      File.ln_s(target, dest)

      try do
        assert File.read_link(dest) == {:ok, target}
      after
        File.rm(dest)
      end
    end

    test "read_link! with symlink" do
      target = tmp_path("does_not_need_to_exist")
      dest = tmp_path("symlink")
      File.ln_s(target, dest)

      try do
        assert File.read_link!(dest) == target
      after
        File.rm(dest)
      end
    end
  end

  test "IO stream UTF-8" do
    src = File.open!(fixture_path("file.txt"), [:utf8])
    dest = tmp_path("tmp_test.txt")

    try do
      stream = IO.stream(src, :line)

      File.open(dest, [:write], fn target ->
        Enum.into(stream, IO.stream(target, :line), &String.replace(&1, "O", "A"))
      end)

      assert File.read(dest) == {:ok, "FAA\n"}
    after
      File.rm(dest)
    end
  end

  test "IO stream" do
    src = File.open!(fixture_path("file.txt"))
    dest = tmp_path("tmp_test.txt")

    try do
      stream = IO.binstream(src, :line)

      File.open(dest, [:write], fn target ->
        Enum.into(stream, IO.binstream(target, :line), &String.replace(&1, "O", "A"))
      end)

      assert File.read(dest) == {:ok, "FAA\n"}
    after
      File.rm(dest)
    end
  end

  describe "file stream" do
    test "returns a struct" do
      src = fixture_path("file.txt")
      stream = File.stream!(src)
      assert %File.Stream{} = stream
      assert stream.modes == [:raw, :read_ahead, :binary]
      assert stream.raw
      assert stream.line_or_bytes == :line

      stream = File.stream!(src, read_ahead: false)
      assert %File.Stream{} = stream
      assert stream.modes == [:raw, :binary]
      assert stream.raw

      stream = File.stream!(src, read_ahead: 5000)
      assert %File.Stream{} = stream
      assert stream.modes == [:raw, {:read_ahead, 5000}, :binary]
      assert stream.raw

      stream = File.stream!(src, [:utf8], 10)
      assert %File.Stream{} = stream
      assert stream.modes == [{:encoding, :utf8}, :binary]
      refute stream.raw
      assert stream.line_or_bytes == 10
    end

    test "counts bytes/characters" do
      src = fixture_path("file.txt")
      stream = File.stream!(src)
      assert Enum.count(stream) == 1

      stream = File.stream!(src, [:utf8])
      assert Enum.count(stream) == 1

      stream = File.stream!(src, [], 2)
      assert Enum.count(stream) == 2
    end

    test "reads and writes lines" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp_test.txt")

      try do
        stream = File.stream!(src)

        File.open(dest, [:write], fn target ->
          Enum.each(stream, fn line ->
            IO.write(target, String.replace(line, "O", "A"))
          end)
        end)

        assert File.read(dest) == {:ok, "FAA\n"}
      after
        File.rm(dest)
      end
    end

    test "reads and writes bytes" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp_test.txt")

      try do
        stream = File.stream!(src, [], 1)

        File.open(dest, [:write], fn target ->
          Enum.each(stream, fn <<char>> ->
            IO.write(target, <<char + 1>>)
          end)
        end)

        assert File.read(dest) == {:ok, "GPP\v"}
      after
        File.rm(dest)
      end
    end

    test "is collectable" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp_test.txt")

      try do
        refute File.exists?(dest)
        original = File.stream!(dest)

        stream =
          File.stream!(src)
          |> Stream.map(&String.replace(&1, "O", "A"))
          |> Enum.into(original)

        assert stream == original
        assert File.read(dest) == {:ok, "FAA\n"}
      after
        File.rm(dest)
      end
    end

    test "is collectable with append" do
      src = fixture_path("file.txt")
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

    test "keeps BOM when raw" do
      src = fixture_path("utf8_bom.txt")

      assert src
             |> File.stream!([])
             |> Enum.take(1) == [<<239, 187, 191>> <> "Русский\n"]

      assert src
             |> File.stream!([], 1)
             |> Enum.take(5) == [<<239>>, <<187>>, <<191>>, <<208>>, <<160>>]

      assert src |> File.stream!([]) |> Enum.count() == 2
      assert src |> File.stream!([], 1) |> Enum.count() == 22
    end

    test "trims BOM via option when raw" do
      src = fixture_path("utf8_bom.txt")

      assert src
             |> File.stream!([:trim_bom])
             |> Enum.take(1) == ["Русский\n"]

      assert src
             |> File.stream!([:trim_bom], 1)
             |> Enum.take(5) == [<<208>>, <<160>>, <<209>>, <<131>>, <<209>>]

      assert src |> File.stream!([:trim_bom]) |> Enum.count() == 2
      assert src |> File.stream!([:trim_bom], 1) |> Enum.count() == 19
    end

    test "keeps BOM with utf8 encoding" do
      src = fixture_path("utf8_bom.txt")

      assert src
             |> File.stream!([{:encoding, :utf8}])
             |> Enum.take(1) == [<<239, 187, 191>> <> "Русский\n"]

      assert src
             |> File.stream!([{:encoding, :utf8}], 1)
             |> Enum.take(9) == ["\uFEFF", "Р", "у", "с", "с", "к", "и", "й", "\n"]
    end

    test "trims BOM via option with utf8 encoding" do
      src = fixture_path("utf8_bom.txt")

      assert src
             |> File.stream!([{:encoding, :utf8}, :trim_bom])
             |> Enum.take(1) == ["Русский\n"]

      assert src
             |> File.stream!([{:encoding, :utf8}, :trim_bom], 1)
             |> Enum.take(8) == ["Р", "у", "с", "с", "к", "и", "й", "\n"]
    end

    test "keeps BOM with UTF16 BE" do
      src = fixture_path("utf16_be_bom.txt")

      assert src
             |> File.stream!([{:encoding, {:utf16, :big}}])
             |> Enum.take(1) == ["\uFEFFРусский\n"]
    end

    test "keeps BOM with UTF16 LE" do
      src = fixture_path("utf16_le_bom.txt")

      assert src
             |> File.stream!([{:encoding, {:utf16, :little}}])
             |> Enum.take(1) == ["\uFEFFРусский\n"]
    end

    test "trims BOM via option with utf16 BE encoding" do
      src = fixture_path("utf16_be_bom.txt")

      assert src
             |> File.stream!([{:encoding, {:utf16, :big}}, :trim_bom])
             |> Enum.take(1) == ["Русский\n"]

      assert src
             |> File.stream!([{:encoding, {:utf16, :big}}, :trim_bom], 1)
             |> Enum.take(8) == ["Р", "у", "с", "с", "к", "и", "й", "\n"]
    end

    test "trims BOM via option with utf16 LE encoding" do
      src = fixture_path("utf16_le_bom.txt")

      assert src
             |> File.stream!([{:encoding, {:utf16, :little}}, :trim_bom])
             |> Enum.take(1) == ["Русский\n"]

      assert src
             |> File.stream!([{:encoding, {:utf16, :little}}, :trim_bom], 1)
             |> Enum.take(8) == ["Р", "у", "с", "с", "к", "и", "й", "\n"]
    end

    test "reads and writes line by line in UTF-8" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp_test.txt")

      try do
        stream = File.stream!(src)

        File.open(dest, [:write, :utf8], fn target ->
          Enum.each(stream, fn line ->
            IO.write(target, String.replace(line, "O", "A"))
          end)
        end)

        assert File.read(dest) == {:ok, "FAA\n"}
      after
        File.rm(dest)
      end
    end

    test "reads and writes character in UTF-8" do
      src = fixture_path("file.txt")
      dest = tmp_path("tmp_test.txt")

      try do
        stream = File.stream!(src, [:utf8], 1)

        File.open(dest, [:write], fn target ->
          Enum.each(stream, fn <<char::utf8>> ->
            IO.write(target, <<char + 1::utf8>>)
          end)
        end)

        assert File.read(dest) == {:ok, "GPP\v"}
      after
        File.rm(dest)
      end
    end
  end

  test "ln" do
    existing = fixture_path("file.txt")
    new = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(new)
      assert File.ln(existing, new) == :ok
      assert File.read(new) == {:ok, "FOO\n"}
    after
      File.rm(new)
    end
  end

  test "ln with existing destination" do
    existing = fixture_path("file.txt")
    assert File.ln(existing, existing) == {:error, :eexist}
  end

  test "ln! with existing destination" do
    assert_raise File.LinkError, fn ->
      existing = fixture_path("file.txt")
      File.ln!(existing, existing)
    end
  end

  test "ln_s" do
    existing = fixture_path("file.txt")
    new = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(new)
      assert File.ln_s(existing, new) == :ok
      assert File.read(new) == {:ok, "FOO\n"}
    after
      File.rm(new)
    end
  end

  test "ln_s with existing destination" do
    existing = fixture_path("file.txt")
    assert File.ln_s(existing, existing) == {:error, :eexist}
  end

  test "ln_s! with existing destination" do
    existing = fixture_path("file.txt")

    assert_raise File.LinkError, fn ->
      File.ln_s!(existing, existing)
    end
  end

  test "copy" do
    src = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      assert File.copy(src, dest) == {:ok, 4}
      assert File.read(dest) == {:ok, "FOO\n"}
    after
      File.rm(dest)
    end
  end

  test "copy with an io_device" do
    {:ok, src} = File.open(fixture_path("file.txt"))
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      assert File.copy(src, dest) == {:ok, 4}
      assert File.read(dest) == {:ok, "FOO\n"}
    after
      File.close(src)
      File.rm(dest)
    end
  end

  test "copy with raw io_device" do
    {:ok, src} = File.open(fixture_path("file.txt"), [:raw])
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      assert File.copy(src, dest) == {:ok, 4}
      assert File.read(dest) == {:ok, "FOO\n"}
    after
      File.close(src)
      File.rm(dest)
    end
  end

  test "copy with ram io_device" do
    {:ok, src} = File.open("FOO\n", [:ram])
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      assert File.copy(src, dest) == {:ok, 4}
      assert File.read(dest) == {:ok, "FOO\n"}
    after
      File.close(src)
      File.rm(dest)
    end
  end

  test "copy with bytes count" do
    src = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      assert File.copy(src, dest, 2) == {:ok, 2}
      assert {:ok, "FO"} == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test "copy with invalid file" do
    src = fixture_path("invalid.txt")
    dest = tmp_path("tmp_test.txt")
    assert File.copy(src, dest, 2) == {:error, :enoent}
  end

  test "copy!" do
    src = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      assert File.copy!(src, dest) == 4
      assert {:ok, "FOO\n"} == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test "copy! with bytes count" do
    src = fixture_path("file.txt")
    dest = tmp_path("tmp_test.txt")

    try do
      refute File.exists?(dest)
      assert File.copy!(src, dest, 2) == 2
      assert {:ok, "FO"} == File.read(dest)
    after
      File.rm(dest)
    end
  end

  test "copy! with invalid file" do
    src = fixture_path("invalid.txt")
    dest = tmp_path("tmp_test.txt")
    message = "could not copy from #{inspect(src)} to #{inspect(dest)}: no such file or directory"

    assert_raise File.CopyError, message, fn ->
      File.copy!(src, dest, 2)
    end
  end

  test "cwd and cd" do
    {:ok, current} = File.cwd()

    try do
      assert File.cd(fixture_path()) == :ok
      assert File.exists?("file.txt")
    after
      File.cd!(current)
    end
  end

  if :file.native_name_encoding() == :utf8 do
    test "cwd and cd with UTF-8" do
      File.mkdir_p(tmp_path("héllò"))

      File.cd!(tmp_path("héllò"), fn ->
        assert Path.basename(File.cwd!()) == "héllò"
      end)
    after
      File.rm_rf(tmp_path("héllò"))
    end
  end

  test "invalid cd" do
    assert io_error?(File.cd(fixture_path("file.txt")))
  end

  test "invalid_cd!" do
    message =
      ~r"\Acould not set current working directory to #{inspect(fixture_path("file.txt"))}: (not a directory|no such file or directory)"

    assert_raise File.Error, message, fn ->
      File.cd!(fixture_path("file.txt"))
    end
  end

  test "cd with function" do
    assert File.cd!(fixture_path(), fn ->
             assert File.exists?("file.txt")
             :cd_result
           end) == :cd_result
  end

  @tag :skip
  test "touch with no file" do
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

  test "touch with erlang timestamp" do
    fixture = tmp_path("tmp_erlang_touch.txt")

    try do
      assert File.touch!(fixture, :erlang.universaltime()) == :ok
      stat = File.stat!(fixture)

      assert File.touch!(fixture, last_year()) == :ok
      assert stat.mtime > File.stat!(fixture).mtime
    after
      File.rm(fixture)
    end
  end

  test "touch with posix timestamp" do
    fixture = tmp_path("tmp_posix_touch.txt")

    try do
      assert File.touch!(fixture, System.os_time(:second)) == :ok
      stat = File.stat!(fixture)

      assert File.touch!(fixture, last_year()) == :ok
      assert stat.mtime > File.stat!(fixture).mtime
    after
      File.rm(fixture)
    end
  end

  test "touch with dir" do
    assert File.touch(fixture_path()) == :ok
  end

  test "touch with failure" do
    fixture = fixture_path("file.txt/bar")
    assert io_error?(File.touch(fixture))
  end

  test "touch! raises" do
    fixture = fixture_path("file.txt/bar")

    message =
      ~r"\Acould not touch #{inspect(fixture)}: (not a directory|no such file or directory)"

    assert_raise File.Error, message, fn ->
      File.touch!(fixture)
    end
  end

  test "chmod with success" do
    fixture = tmp_path("tmp_test.txt")

    File.touch(fixture)

    try do
      assert File.chmod(fixture, 0o100666) == :ok
      stat = File.stat!(fixture)
      assert stat.mode == 0o100666

      unless windows?() do
        assert File.chmod(fixture, 0o100777) == :ok
        stat = File.stat!(fixture)
        assert stat.mode == 0o100777
      end
    after
      File.rm(fixture)
    end
  end

  test "chmod! with success" do
    fixture = tmp_path("tmp_test.txt")

    File.touch(fixture)

    try do
      assert File.chmod!(fixture, 0o100666) == :ok
      stat = File.stat!(fixture)
      assert stat.mode == 0o100666

      unless windows?() do
        assert File.chmod!(fixture, 0o100777) == :ok
        stat = File.stat!(fixture)
        assert stat.mode == 0o100777
      end
    after
      File.rm(fixture)
    end
  end

  test "chmod with failure" do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chmod(fixture, 0o100777) == {:error, :enoent}
  end

  test "chmod! with failure" do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = ~r"could not change mode for #{inspect(fixture)}: no such file or directory"

    assert_raise File.Error, message, fn ->
      File.chmod!(fixture, 0o100777)
    end
  end

  test "chgrp with failure" do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chgrp(fixture, 1) == {:error, :enoent}
  end

  test "chgrp! with failure" do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = ~r"could not change group for #{inspect(fixture)}: no such file or directory"

    assert_raise File.Error, message, fn ->
      File.chgrp!(fixture, 1)
    end
  end

  test "chown with failure" do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    assert File.chown(fixture, 1) == {:error, :enoent}
  end

  test "chown! with failure" do
    fixture = tmp_path("tmp_test.txt")
    File.rm(fixture)

    message = ~r"could not change owner for #{inspect(fixture)}: no such file or directory"

    assert_raise File.Error, message, fn ->
      File.chown!(fixture, 1)
    end
  end

  defp last_year do
    System.os_time(:second) - 365 * 24 * 60 * 60
  end

  defp io_error?(result) do
    {:error, errorcode} = result
    errorcode in [:enotdir, :eio, :enoent, :eisdir]
  end
end
