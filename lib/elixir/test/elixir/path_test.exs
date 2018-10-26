Code.require_file("test_helper.exs", __DIR__)

defmodule PathTest do
  use ExUnit.Case, async: true

  doctest Path

  import PathHelpers

  if :file.native_name_encoding() == :utf8 do
    test "wildcard with UTF-8" do
      File.mkdir_p(tmp_path("héllò"))
      assert Path.wildcard(tmp_path("héllò")) == [tmp_path("héllò")]
    after
      File.rm_rf(tmp_path("héllò"))
    end
  end

  test "wildcard/2" do
    hello = tmp_path("wildcard/.hello")
    world = tmp_path("wildcard/.hello/world")
    File.mkdir_p(world)

    assert Path.wildcard(tmp_path("wildcard/*/*")) == []
    assert Path.wildcard(tmp_path("wildcard/**/*")) == []
    assert Path.wildcard(tmp_path("wildcard/?hello/world")) == []

    assert Path.wildcard(tmp_path("wildcard/*/*"), match_dot: true) == [world]
    assert Path.wildcard(tmp_path("wildcard/**/*"), match_dot: true) == [hello, world]
    assert Path.wildcard(tmp_path("wildcard/?hello/world"), match_dot: true) == [world]
  after
    File.rm_rf(tmp_path("wildcard"))
  end

  test "wildcard/2 raises on null byte" do
    assert_raise ArgumentError, ~r/null byte/, fn -> Path.wildcard("foo\0bar") end
  end

  describe "Windows" do
    @describetag :windows

    test "relative/1" do
      assert Path.relative("C:/usr/local/bin") == "usr/local/bin"
      assert Path.relative("C:\\usr\\local\\bin") == "usr\\local\\bin"
      assert Path.relative("C:usr\\local\\bin") == "usr\\local\\bin"

      assert Path.relative("/usr/local/bin") == "usr/local/bin"
      assert Path.relative("usr/local/bin") == "usr/local/bin"
      assert Path.relative("../usr/local/bin") == "../usr/local/bin"
    end

    test "relative_to/2" do
      assert Path.relative_to("D:/usr/local/foo", "D:/usr/") == "local/foo"
      assert Path.relative_to("D:/usr/local/foo", "d:/usr/") == "local/foo"
      assert Path.relative_to("d:/usr/local/foo", "D:/usr/") == "local/foo"
      assert Path.relative_to("D:/usr/local/foo", "d:/") == "usr/local/foo"
      assert Path.relative_to("D:/usr/local/foo", "D:/") == "usr/local/foo"
      assert Path.relative_to("D:/usr/local/foo", "d:") == "D:/usr/local/foo"
      assert Path.relative_to("D:/usr/local/foo", "D:") == "D:/usr/local/foo"
    end

    test "type/1" do
      assert Path.type("C:/usr/local/bin") == :absolute
      assert Path.type('C:\\usr\\local\\bin') == :absolute
      assert Path.type("C:usr\\local\\bin") == :volumerelative

      assert Path.type("/usr/local/bin") == :volumerelative
      assert Path.type('usr/local/bin') == :relative
      assert Path.type("../usr/local/bin") == :relative
    end

    test "split/1" do
      assert Path.split("C:\\foo\\bar") == ["c:/", "foo", "bar"]
      assert Path.split("C:/foo/bar") == ["c:/", "foo", "bar"]
    end
  end

  describe "Unix" do
    @describetag :unix

    test "relative/1" do
      assert Path.relative("/usr/local/bin") == "usr/local/bin"
      assert Path.relative("usr/local/bin") == "usr/local/bin"
      assert Path.relative("../usr/local/bin") == "../usr/local/bin"
      assert Path.relative("/") == "."
      assert Path.relative('/') == "."
      assert Path.relative(['/usr', ?/, "local/bin"]) == "usr/local/bin"
    end

    test "type/1" do
      assert Path.type("/usr/local/bin") == :absolute
      assert Path.type("usr/local/bin") == :relative
      assert Path.type("../usr/local/bin") == :relative

      assert Path.type('/usr/local/bin') == :absolute
      assert Path.type('usr/local/bin') == :relative
      assert Path.type('../usr/local/bin') == :relative

      assert Path.type(['/usr/', 'local/bin']) == :absolute
      assert Path.type(['usr/', 'local/bin']) == :relative
      assert Path.type(['../usr', '/local/bin']) == :relative
    end
  end

  test "relative_to_cwd/1" do
    assert Path.relative_to_cwd(__ENV__.file) == Path.relative_to(__ENV__.file, File.cwd!())

    assert Path.relative_to_cwd(to_charlist(__ENV__.file)) ==
             Path.relative_to(to_charlist(__ENV__.file), to_charlist(File.cwd!()))
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
    assert home == Path.expand('~')
    assert is_binary(Path.expand("~/foo"))
    assert is_binary(Path.expand('~/foo'))

    assert Path.expand("~/file") == Path.join(home, "file")
    assert Path.expand("~/file", "whatever") == Path.join(home, "file")
    assert Path.expand("file", Path.expand("~")) == Path.expand("~/file")
    assert Path.expand("file", "~") == Path.join(home, "file")
    assert Path.expand("~file") == Path.join(File.cwd!(), "file")
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
      Path.expand(['..', ?/, "bar/../bar"], '/foo/../foo/../foo') |> strip_drive_letter_if_windows

    assert "/bar" == drive_letter

    assert Path.expand("/..") |> strip_drive_letter_if_windows == "/"

    assert Path.expand("bar/../bar", "foo") == Path.expand("foo/bar")
  end

  test "relative_to/2" do
    assert Path.relative_to("/usr/local/foo", "/usr/local") == "foo"
    assert Path.relative_to("/usr/local/foo", "/") == "usr/local/foo"
    assert Path.relative_to("/usr/local/foo", "/etc") == "/usr/local/foo"
    assert Path.relative_to("/usr/local/foo", "/usr/local/foo") == "/usr/local/foo"

    assert Path.relative_to("usr/local/foo", "usr/local") == "foo"
    assert Path.relative_to("usr/local/foo", "etc") == "usr/local/foo"
    assert Path.relative_to('usr/local/foo', "etc") == "usr/local/foo"

    assert Path.relative_to("usr/local/foo", "usr/local") == "foo"
    assert Path.relative_to(["usr", ?/, 'local/foo'], 'usr/local') == "foo"
  end

  test "rootname/2" do
    assert Path.rootname("~/foo/bar.ex", ".ex") == "~/foo/bar"
    assert Path.rootname("~/foo/bar.exs", ".ex") == "~/foo/bar.exs"
    assert Path.rootname("~/foo/bar.old.ex", ".ex") == "~/foo/bar.old"
    assert Path.rootname([?~, '/foo/bar', ".old.ex"], '.ex') == "~/foo/bar.old"
  end

  test "extname/1" do
    assert Path.extname("foo.erl") == ".erl"
    assert Path.extname("~/foo/bar") == ""

    assert Path.extname('foo.erl') == ".erl"
    assert Path.extname('~/foo/bar') == ""
  end

  test "dirname/1" do
    assert Path.dirname("/foo/bar.ex") == "/foo"
    assert Path.dirname("foo/bar.ex") == "foo"

    assert Path.dirname("~/foo/bar.ex") == "~/foo"
    assert Path.dirname("/foo/bar/baz/") == "/foo/bar/baz"

    assert Path.dirname([?~, "/foo", '/bar.ex']) == "~/foo"
  end

  test "basename/1,2" do
    assert Path.basename("foo") == "foo"
    assert Path.basename("/foo/bar") == "bar"
    assert Path.basename("/") == ""

    assert Path.basename("~/foo/bar.ex", ".ex") == "bar"
    assert Path.basename("~/foo/bar.exs", ".ex") == "bar.exs"
    assert Path.basename("~/for/bar.old.ex", ".ex") == "bar.old"

    assert Path.basename([?~, "/for/bar", '.old.ex'], ".ex") == "bar.old"
  end

  test "join/1" do
    assert Path.join([""]) == ""
    assert Path.join(["foo"]) == "foo"
    assert Path.join(["/", "foo", "bar"]) == "/foo/bar"
    assert Path.join(["~", "foo", "bar"]) == "~/foo/bar"
    assert Path.join(['/foo/', "/bar/"]) == "/foo/bar"
    assert Path.join(["/", ""]) == "/"
    assert Path.join(["/", "", "bar"]) == "/bar"
    assert Path.join(['foo', [?b, "a", ?r]]) == "foo/bar"
    assert Path.join([[?f, 'o', "o"]]) == "foo"
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

    assert Path.join([?/, "foo"], "./bar") == "/foo/./bar"
    assert Path.join(["/foo", "bar"], ["fiz", "buz"]) == "/foobar/fizbuz"
  end

  test "split/1" do
    assert Path.split("") == []
    assert Path.split("foo") == ["foo"]
    assert Path.split("/foo/bar") == ["/", "foo", "bar"]
    assert Path.split([?/, "foo/bar"]) == ["/", "foo", "bar"]
  end

  if windows?() do
    defp strip_drive_letter_if_windows([_d, ?: | rest]), do: rest
    defp strip_drive_letter_if_windows(<<_d, ?:, rest::binary>>), do: rest
  else
    defp strip_drive_letter_if_windows(path), do: path
  end
end
