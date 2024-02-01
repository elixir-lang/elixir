Code.require_file("test_helper.exs", __DIR__)

defmodule PathTest do
  use ExUnit.Case, async: true
  doctest Path

  if :file.native_name_encoding() == :utf8 do
    @tag :tmp_dir
    test "wildcard with UTF-8", config do
      File.mkdir_p(Path.join(config.tmp_dir, "héllò"))

      assert Path.wildcard(Path.join(config.tmp_dir, "héllò")) ==
               [Path.join(config.tmp_dir, "héllò")]
    after
      File.rm_rf(Path.join(config.tmp_dir, "héllò"))
    end
  end

  @tag :tmp_dir
  test "wildcard/2", config do
    hello = Path.join(config.tmp_dir, "wildcard/.hello")
    world = Path.join(config.tmp_dir, "wildcard/.hello/world")
    File.mkdir_p(world)

    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/*/*")) == []
    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/**/*")) == []
    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/?hello/world")) == []

    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/*/*"), match_dot: true) ==
             [world]

    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/**/*"), match_dot: true) ==
             [hello, world]

    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/?hello/world"), match_dot: true) ==
             [world]
  after
    File.rm_rf(Path.join(config.tmp_dir, "wildcard"))
  end

  @tag :tmp_dir
  test "wildcard/2 follows ..", config do
    hello = Path.join(config.tmp_dir, "wildcard/hello")
    world = Path.join(config.tmp_dir, "wildcard/world")
    File.mkdir_p(hello)
    File.touch(world)

    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/w*/../h*")) == []

    assert Path.wildcard(Path.join(config.tmp_dir, "wildcard/h*/../w*")) ==
             [Path.join(config.tmp_dir, "wildcard/hello/../world")]
  after
    File.rm_rf(Path.join(config.tmp_dir, "wildcard"))
  end

  test "wildcard/2 raises on null byte" do
    assert_raise ArgumentError, ~r/null byte/, fn -> Path.wildcard("foo\0bar") end
  end

  describe "Windows" do
    @describetag :windows

    test "absname/1" do
      assert Path.absname("//host/path") == "//host/path"
      assert Path.absname("\\\\host\\path") == "//host/path"
      assert Path.absname("\\/host\\path") == "//host/path"
      assert Path.absname("/\\host\\path") == "//host/path"

      assert Path.absname("c:/") == "c:/"
      assert Path.absname("c:/host/path") == "c:/host/path"

      cwd = File.cwd!()
      assert Path.absname(cwd |> String.split("/") |> hd()) == cwd

      <<letter, _::binary>> = cwd
      random = Enum.random(Enum.to_list(?c..?z) -- [letter])
      assert Path.absname(<<random, ?:>>) == <<random, ?:, ?/>>
    end

    test "relative/1" do
      assert Path.relative("C:/usr/local/bin") == "usr/local/bin"
      assert Path.relative("C:\\usr\\local\\bin") == "usr\\local\\bin"
      assert Path.relative("C:usr\\local\\bin") == "usr\\local\\bin"

      assert Path.relative("/usr/local/bin") == "usr/local/bin"
      assert Path.relative("usr/local/bin") == "usr/local/bin"
      assert Path.relative("../usr/local/bin") == "../usr/local/bin"
    end

    test "relative_to/3" do
      # should give same relative paths for both force true and false
      for force <- [true, false] do
        assert Path.relative_to("//usr/local/foo", "//usr/", force: force) == "local/foo"

        assert Path.relative_to("D:/usr/local/foo", "D:/usr/", force: force) == "local/foo"
        assert Path.relative_to("D:/usr/local/foo", "d:/usr/", force: force) == "local/foo"
        assert Path.relative_to("d:/usr/local/foo", "D:/usr/", force: force) == "local/foo"
        assert Path.relative_to("D:/usr/local/foo", "d:/", force: force) == "usr/local/foo"
        assert Path.relative_to("D:/usr/local/foo", "D:/", force: force) == "usr/local/foo"

        assert Path.relative_to("d:/usr/local/foo/..", "d:/usr/local", force: force) == "."
        assert Path.relative_to("d:/usr/local/../foo", "d:/usr/foo", force: force) == "."
        assert Path.relative_to("d:/usr/local/../foo/bar", "d:/usr/foo", force: force) == "bar"
        assert Path.relative_to("d:/usr/local/../foo/./bar", "d:/usr/foo", force: force) == "bar"

        assert Path.relative_to("d:/usr/local/../foo/bar/..", "d:/usr/foo", force: force) == "."
        assert Path.relative_to("d:/usr/local/foo/..", "d:/usr/local/..", force: force) == "local"
        assert Path.relative_to("d:/usr/local/foo/..", "d:/usr/local/.", force: force) == "."
      end

      # different results for force: true
      assert Path.relative_to("d:/usr/local/../foo", "d:/usr/local") == "d:/usr/foo"
      assert Path.relative_to("d:/usr/local/../foo", "d:/usr/local", force: true) == "../foo"

      assert Path.relative_to("d:/usr/local/../foo/../bar", "d:/usr/foo") == "d:/usr/bar"
      assert Path.relative_to("d:/usr/local/../foo/../bar", "d:/usr/foo", force: true) == "../bar"

      # on different volumes with force: true it should return the original path
      assert Path.relative_to("d:/usr/local", "c:/usr/local", force: true) == "d:/usr/local"
      assert Path.relative_to("d:/usr/local", "c:/another/local", force: true) == "d:/usr/local"
    end

    test "type/1" do
      assert Path.type("C:/usr/local/bin") == :absolute
      assert Path.type(~c"C:\\usr\\local\\bin") == :absolute
      assert Path.type("C:usr\\local\\bin") == :volumerelative

      assert Path.type("/usr/local/bin") == :volumerelative
      assert Path.type(~c"usr/local/bin") == :relative
      assert Path.type("../usr/local/bin") == :relative

      assert Path.type("//host/path") == :absolute
      assert Path.type("\\\\host\\path") == :absolute
      assert Path.type("/\\host\\path") == :absolute
      assert Path.type("\\/host\\path") == :absolute
    end

    test "split/1" do
      assert Path.split("C:\\foo\\bar") == ["c:/", "foo", "bar"]
      assert Path.split("C:/foo/bar") == ["c:/", "foo", "bar"]
    end

    test "safe_relative/1" do
      assert Path.safe_relative("local/foo") == {:ok, "local/foo"}
      assert Path.safe_relative("D:/usr/local/foo") == :error
      assert Path.safe_relative("d:/usr/local/foo") == :error
      assert Path.safe_relative("foo/../..") == :error
    end

    test "safe_relative/2" do
      assert Path.safe_relative("local/foo/bar", "local") == {:ok, "local/foo/bar"}
      assert Path.safe_relative("foo/..", "local") == {:ok, ""}
      assert Path.safe_relative("..", "local/foo") == :error
      assert Path.safe_relative("d:/usr/local/foo", "D:/") == :error
      assert Path.safe_relative("D:/usr/local/foo", "d:/") == :error
    end
  end

  describe "Unix" do
    @describetag :unix

    test "relative/1" do
      assert Path.relative("/usr/local/bin") == "usr/local/bin"
      assert Path.relative("usr/local/bin") == "usr/local/bin"
      assert Path.relative("../usr/local/bin") == "../usr/local/bin"
      assert Path.relative("/") == "."
      assert Path.relative(~c"/") == "."
      assert Path.relative([~c"/usr", ?/, "local/bin"]) == "usr/local/bin"
    end

    test "relative_to/3" do
      # subpaths of cwd, should give the same result for both force true and false
      for force <- [false, true] do
        assert Path.relative_to("/usr/local/foo", "/usr/local", force: force) == "foo"
        assert Path.relative_to("/usr/local/foo", "/", force: force) == "usr/local/foo"
        assert Path.relative_to("/usr/local/foo", "/usr/local/foo", force: force) == "."
        assert Path.relative_to("/usr/local/foo/", "/usr/local/foo", force: force) == "."
        assert Path.relative_to("/usr/local/foo", "/usr/local/foo/", force: force) == "."

        assert Path.relative_to("/usr/local/foo/..", "/usr/local", force: force) == "."
        assert Path.relative_to("/usr/local/../foo", "/usr/foo", force: force) == "."
        assert Path.relative_to("/usr/local/../foo/bar", "/usr/foo", force: force) == "bar"
        assert Path.relative_to("/usr/local/../foo/./bar", "/usr/foo", force: force) == "bar"
        assert Path.relative_to("/usr/local/../foo/bar/..", "/usr/foo", force: force) == "."

        assert Path.relative_to("/usr/local/foo/..", "/usr/local/..", force: force) == "local"
        assert Path.relative_to("/usr/local/foo/..", "/usr/local/.", force: force) == "."
      end

      # With relative second argument
      assert Path.relative_to("/usr/local/foo", "etc") == "/usr/local/foo"
      assert Path.relative_to("/usr/local/foo", "etc", force: true) == "/usr/local/foo"

      # Different relative paths for force true/false
      assert Path.relative_to("/usr/local/foo", "/etc") == "/usr/local/foo"
      assert Path.relative_to("/usr/local/foo", "/etc", force: true) == "../usr/local/foo"

      assert Path.relative_to("/usr/local/../foo", "/usr/local") == "/usr/foo"
      assert Path.relative_to("/usr/local/../foo", "/usr/local", force: true) == "../foo"

      assert Path.relative_to("/usr/local/../foo/../bar", "/usr/foo") == "/usr/bar"
      assert Path.relative_to("/usr/local/../foo/../bar", "/usr/foo", force: true) == "../bar"

      # More tests with force: true
      assert Path.relative_to("/etc", "/usr/local/foo", force: true) == "../../../etc"
      assert Path.relative_to(~c"/usr/local/foo", "/etc", force: true) == "../usr/local/foo"
      assert Path.relative_to("/usr/local", "/usr/local/foo", force: true) == ".."
      assert Path.relative_to("/usr/local/..", "/usr/local", force: true) == ".."

      assert Path.relative_to("/usr/../etc/foo/../../bar", "/log/foo/../../usr/", force: true) ==
               "../bar"
    end

    test "type/1" do
      assert Path.type("/usr/local/bin") == :absolute
      assert Path.type("usr/local/bin") == :relative
      assert Path.type("../usr/local/bin") == :relative

      assert Path.type(~c"/usr/local/bin") == :absolute
      assert Path.type(~c"usr/local/bin") == :relative
      assert Path.type(~c"../usr/local/bin") == :relative

      assert Path.type([~c"/usr/", ~c"local/bin"]) == :absolute
      assert Path.type([~c"usr/", ~c"local/bin"]) == :relative
      assert Path.type([~c"../usr", ~c"/local/bin"]) == :relative
    end
  end

  test "relative_to_cwd/2" do
    assert Path.relative_to_cwd(__ENV__.file) == Path.relative_to(__ENV__.file, File.cwd!())

    assert Path.relative_to_cwd(to_charlist(__ENV__.file)) ==
             Path.relative_to(to_charlist(__ENV__.file), to_charlist(File.cwd!()))

    assert Path.relative_to_cwd(Path.dirname(File.cwd!()), force: true) == ".."

    [slash | splitted_cwd] = Path.split(File.cwd!())
    relative_to_root = List.duplicate("..", length(splitted_cwd))

    assert Path.relative_to_cwd(slash) == slash
    assert Path.relative_to_cwd(slash, force: true) == Path.join(relative_to_root)
  end

  test "absname/1,2" do
    assert Path.absname("/") |> strip_drive_letter_if_windows == "/"
    assert Path.absname("/foo") |> strip_drive_letter_if_windows == "/foo"
    assert Path.absname("/./foo") |> strip_drive_letter_if_windows == "/foo"
    assert Path.absname("/foo/bar") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.absname("/foo/bar/") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.absname("/foo/bar/../bar") |> strip_drive_letter_if_windows == "/foo/bar/../bar"

    assert Path.absname("bar", "/foo") == "/foo/bar"
    assert Path.absname("bar/", "/foo") == "/foo/bar"
    assert Path.absname("bar/.", "/foo") == "/foo/bar/."
    assert Path.absname("bar/../bar", "/foo") == "/foo/bar/../bar"
    assert Path.absname("bar/../bar", "foo") == "foo/bar/../bar"
    assert Path.absname(["bar/", ?., ?., ["/bar"]], "/foo") == "/foo/bar/../bar"
  end

  test "expand/1,2 with user home" do
    home = System.user_home!() |> Path.absname()

    assert home == Path.expand("~")
    assert home == Path.expand(~c"~")
    assert is_binary(Path.expand("~/foo"))
    assert is_binary(Path.expand(~c"~/foo"))

    assert Path.expand("~/file") == Path.join(home, "file")
    assert Path.expand("~/file", "whatever") == Path.join(home, "file")
    assert Path.expand("file", Path.expand("~")) == Path.join(home, "file")
    assert Path.expand("file", "~") == Path.join(home, "file")
    assert Path.expand("~file") == Path.join(File.cwd!(), "~file")
  end

  test "expand/1,2" do
    assert Path.expand("/") |> strip_drive_letter_if_windows == "/"
    assert Path.expand("/foo/../..") |> strip_drive_letter_if_windows == "/"
    assert Path.expand("/foo") |> strip_drive_letter_if_windows == "/foo"
    assert Path.expand("/./foo") |> strip_drive_letter_if_windows == "/foo"
    assert Path.expand("/../foo") |> strip_drive_letter_if_windows == "/foo"
    assert Path.expand("/foo/bar") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.expand("/foo/bar/") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.expand("/foo/bar/.") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.expand("/foo/bar/../bar") |> strip_drive_letter_if_windows == "/foo/bar"

    assert Path.expand("bar", "/foo") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.expand("bar/", "/foo") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.expand("bar/.", "/foo") |> strip_drive_letter_if_windows == "/foo/bar"
    assert Path.expand("bar/../bar", "/foo") |> strip_drive_letter_if_windows == "/foo/bar"

    drive_letter =
      Path.expand("../bar/../bar", "/foo/../foo/../foo") |> strip_drive_letter_if_windows

    assert drive_letter == "/bar"

    drive_letter =
      Path.expand([~c"..", ?/, "bar/../bar"], ~c"/foo/../foo/../foo")
      |> strip_drive_letter_if_windows

    assert "/bar" == drive_letter

    assert Path.expand("/..") |> strip_drive_letter_if_windows == "/"

    assert Path.expand("bar/../bar", "foo") == Path.expand("foo/bar")
  end

  test "relative_to/3 (with relative paths)" do
    # on cwd
    assert Path.relative_to("foo", File.cwd!()) == "foo"
    assert Path.relative_to("./foo", File.cwd!()) == "foo"
    assert Path.relative_to("./foo/.", File.cwd!()) == "foo"
    assert Path.relative_to("./foo/./bar/.", File.cwd!()) == "foo/bar"
    assert Path.relative_to("../foo/./bar/.", File.cwd!()) == "../foo/bar"
    assert Path.relative_to("../foo/./bar/..", File.cwd!()) == "../foo"
    assert Path.relative_to("../foo/../bar/..", File.cwd!()) == ".."
    assert Path.relative_to("./foo/../bar/..", File.cwd!()) == "."

    # both relative
    assert Path.relative_to("usr/local/foo", ".") == "usr/local/foo"
    assert Path.relative_to(".", "usr/local/foo") == "."
    assert Path.relative_to("usr/local/foo", "usr/local") == "foo"
    assert Path.relative_to("usr/local/foo", "etc") == "../usr/local/foo"
    assert Path.relative_to(~c"usr/local/foo", "etc") == "../usr/local/foo"
    assert Path.relative_to("usr/local/foo", "usr/local") == "foo"
    assert Path.relative_to(["usr", ?/, ~c"local/foo"], ~c"usr/local") == "foo"
  end

  test "safe_relative/1" do
    assert Path.safe_relative("foo/bar") == {:ok, "foo/bar"}
    assert Path.safe_relative("foo/..") == {:ok, ""}
    assert Path.safe_relative("./foo") == {:ok, "foo"}

    assert Path.safe_relative("/usr/local/foo") == :error
    assert Path.safe_relative("foo/../..") == :error
  end

  test "safe_relative/2" do
    assert Path.safe_relative("/usr/local/foo", "/usr/local") == :error
    assert Path.safe_relative("../../..", "foo/bar") == :error
    assert Path.safe_relative("../../..", "foo/bar") == :error
    assert Path.safe_relative("/usr/local/foo", "/") == :error
  end

  test "rootname/2" do
    assert Path.rootname("~/foo/bar.ex", ".ex") == "~/foo/bar"
    assert Path.rootname("~/foo/bar.exs", ".ex") == "~/foo/bar.exs"
    assert Path.rootname("~/foo/bar.old.ex", ".ex") == "~/foo/bar.old"
    assert Path.rootname([?~, ~c"/foo/bar", ".old.ex"], ~c".ex") == "~/foo/bar.old"
  end

  test "extname/1" do
    assert Path.extname("foo.erl") == ".erl"
    assert Path.extname("~/foo/bar") == ""

    assert Path.extname(~c"foo.erl") == ".erl"
    assert Path.extname(~c"~/foo/bar") == ""
  end

  test "dirname/1" do
    assert Path.dirname("/foo/bar.ex") == "/foo"
    assert Path.dirname("foo/bar.ex") == "foo"

    assert Path.dirname("~/foo/bar.ex") == "~/foo"
    assert Path.dirname("/foo/bar/baz/") == "/foo/bar/baz"

    assert Path.dirname([?~, "/foo", ~c"/bar.ex"]) == "~/foo"
  end

  test "basename/1,2" do
    assert Path.basename("foo") == "foo"
    assert Path.basename("/foo/bar") == "bar"
    assert Path.basename("/") == ""

    assert Path.basename("~/foo/bar.ex", ".ex") == "bar"
    assert Path.basename("~/foo/bar.exs", ".ex") == "bar.exs"
    assert Path.basename("~/for/bar.old.ex", ".ex") == "bar.old"

    assert Path.basename([?~, "/for/bar", ~c".old.ex"], ".ex") == "bar.old"
  end

  test "join/1" do
    assert Path.join([""]) == ""
    assert Path.join(["foo"]) == "foo"
    assert Path.join(["/", "foo", "bar"]) == "/foo/bar"
    assert Path.join(["/", "foo", "bar", "/"]) == "/foo/bar"
    assert Path.join(["~", "foo", "bar"]) == "~/foo/bar"
    assert Path.join([~c"/foo/", "/bar/"]) == "/foo/bar"
    assert Path.join(["/", ""]) == "/"
    assert Path.join(["/", "", "bar"]) == "/bar"
    assert Path.join([~c"foo", [?b, "a", ?r]]) == "foo/bar"
    assert Path.join([[?f, ~c"o", "o"]]) == "foo"
  end

  test "join/2" do
    assert Path.join("/foo", "bar") == "/foo/bar"
    assert Path.join("~", "foo") == "~/foo"

    assert Path.join("", "bar") == "bar"
    assert Path.join("bar", "") == "bar"
    assert Path.join("", "/bar") == "bar"
    assert Path.join("/bar", "") == "/bar"

    assert Path.join("foo", "/bar") == "foo/bar"
    assert Path.join("/foo", "/bar") == "/foo/bar"
    assert Path.join("/foo", "/bar") == "/foo/bar"
    assert Path.join("/foo", "./bar") == "/foo/./bar"

    assert Path.join("/foo", "/") == "/foo"
    assert Path.join("/foo", "/bar/zar/") == "/foo/bar/zar"

    assert Path.join([?/, "foo"], "./bar") == "/foo/./bar"
    assert Path.join(["/foo", "bar"], ["fiz", "buz"]) == "/foobar/fizbuz"
  end

  test "split/1" do
    assert Path.split("") == []
    assert Path.split("foo") == ["foo"]
    assert Path.split("/foo/bar") == ["/", "foo", "bar"]
    assert Path.split([?/, "foo/bar"]) == ["/", "foo", "bar"]
  end

  if PathHelpers.windows?() do
    defp strip_drive_letter_if_windows([_d, ?: | rest]), do: rest
    defp strip_drive_letter_if_windows(<<_d, ?:, rest::binary>>), do: rest
  else
    defp strip_drive_letter_if_windows(path), do: path
  end
end
