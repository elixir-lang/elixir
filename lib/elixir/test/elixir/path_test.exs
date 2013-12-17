Code.require_file "test_helper.exs", __DIR__

defmodule PathTest do
  use ExUnit.Case, async: true
  import PathHelpers

  if :file.native_name_encoding == :utf8 do
    test :wildcard do
      File.mkdir_p(tmp_path("héllò"))
      assert Path.wildcard(tmp_path("héllò")) == [tmp_path("héllò")]
    after
      File.rm_rf tmp_path("héllò")
    end
  end

  if is_win? do
    test :relative do
      assert Path.relative("C:/usr/local/bin")    == "usr/local/bin"
      assert Path.relative("C:\\usr\\local\\bin") == "usr\\local\\bin"
      assert Path.relative("C:usr\\local\\bin")   == "usr\\local\\bin"

      assert Path.relative("/usr/local/bin")   == "usr/local/bin"
      assert Path.relative("usr/local/bin")    == "usr/local/bin"
      assert Path.relative("../usr/local/bin") == "../usr/local/bin"

      assert Path.relative('/usr/local/bin')   == 'usr/local/bin'
      assert Path.relative('usr/local/bin')    == 'usr/local/bin'
      assert Path.relative('../usr/local/bin') == '../usr/local/bin'

      assert List.flatten(Path.relative(['/usr/', 'local/bin']))   == 'usr/local/bin'
      assert List.flatten(Path.relative(['usr/', 'local/bin']))    == 'usr/local/bin'
      assert List.flatten(Path.relative(['../usr', '/local/bin'])) == '../usr/local/bin'
    end

    test :type do
      assert Path.type("C:/usr/local/bin")    == :absolute
      assert Path.type("C:\\usr\\local\\bin") == :absolute
      assert Path.type("C:usr\\local\\bin")   == :volumerelative

      assert Path.type("/usr/local/bin")   == :volumerelative
      assert Path.type("usr/local/bin")    == :relative
      assert Path.type("../usr/local/bin") == :relative

      assert Path.type('/usr/local/bin')   == :volumerelative
      assert Path.type('usr/local/bin')    == :relative
      assert Path.type('../usr/local/bin') == :relative

      assert Path.type(['/usr/', 'local/bin'])   == :volumerelative
      assert Path.type(['usr/', 'local/bin'])    == :relative
      assert Path.type(['../usr', '/local/bin']) == :relative
    end
  else
    test :relative do
      assert Path.relative("/usr/local/bin")   == "usr/local/bin"
      assert Path.relative("usr/local/bin")    == "usr/local/bin"
      assert Path.relative("../usr/local/bin") == "../usr/local/bin"

      assert Path.relative('/usr/local/bin')   == 'usr/local/bin'
      assert Path.relative('usr/local/bin')    == 'usr/local/bin'
      assert Path.relative('../usr/local/bin') == '../usr/local/bin'

      assert List.flatten(Path.relative(['/usr/', 'local/bin']))   == 'usr/local/bin'
      assert List.flatten(Path.relative(['usr/', 'local/bin']))    == 'usr/local/bin'
      assert List.flatten(Path.relative(['../usr', '/local/bin'])) == '../usr/local/bin'
    end

    test :type do
      assert Path.type("/usr/local/bin")   == :absolute
      assert Path.type("usr/local/bin")    == :relative
      assert Path.type("../usr/local/bin") == :relative

      assert Path.type('/usr/local/bin')   == :absolute
      assert Path.type('usr/local/bin')    == :relative
      assert Path.type('../usr/local/bin') == :relative

      assert Path.type(['/usr/', 'local/bin'])   == :absolute
      assert Path.type(['usr/', 'local/bin'])    == :relative
      assert Path.type(['../usr', '/local/bin']) == :relative
    end
  end

  test :relative_to_cwd do
    assert Path.relative_to_cwd(__FILE__) ==
           Path.relative_to(__FILE__, System.cwd!)

    assert Path.relative_to_cwd(to_char_list(__FILE__)) ==
           Path.relative_to(to_char_list(__FILE__), to_char_list(System.cwd!))
  end

  test :absname_with_binary do
    assert (Path.absname("/foo/bar") |> strip_drive_letter_if_windows) == "/foo/bar"
    assert (Path.absname("/foo/bar/") |> strip_drive_letter_if_windows)  == "/foo/bar"
    assert (Path.absname("/foo/bar/../bar")  |> strip_drive_letter_if_windows) == "/foo/bar/../bar"

    assert Path.absname("bar", "/foo") == "/foo/bar"
    assert Path.absname("bar/", "/foo") == "/foo/bar"
    assert Path.absname("bar/.", "/foo") == "/foo/bar/."
    assert Path.absname("bar/../bar", "/foo") == "/foo/bar/../bar"
    assert Path.absname("bar/../bar", "foo") == "foo/bar/../bar"
  end

  test :absname_with_list do
    assert (Path.absname('/foo/bar') |> strip_drive_letter_if_windows)  == '/foo/bar'
    assert (Path.absname('/foo/bar/') |> strip_drive_letter_if_windows)  == '/foo/bar'
    assert (Path.absname('/foo/bar/.')  |> strip_drive_letter_if_windows)  == '/foo/bar/.'
    assert (Path.absname('/foo/bar/../bar')  |> strip_drive_letter_if_windows) == '/foo/bar/../bar'
  end

  test :expand_path_with_user_home do
    home = System.user_home!

    assert home == Path.expand("~")
    assert is_binary Path.expand("~/foo")

    assert (home |> String.to_char_list!) == Path.expand('~')
    assert is_list Path.expand('~/foo')

    assert Path.expand("~/file") == Path.join(home, "file")
    assert Path.expand("~/file", "whatever") == Path.join(home, "file")
    assert Path.expand("file", Path.expand("~")) == Path.expand("~/file")
    assert Path.expand("file", "~") == Path.join(home, "file")
  end

  test :expand_path_with_binary do
    assert (Path.expand("/foo/bar") |> strip_drive_letter_if_windows) == "/foo/bar"
    assert (Path.expand("/foo/bar/")  |> strip_drive_letter_if_windows) == "/foo/bar"
    assert (Path.expand("/foo/bar/.")  |> strip_drive_letter_if_windows)== "/foo/bar"
    assert (Path.expand("/foo/bar/../bar")  |> strip_drive_letter_if_windows) == "/foo/bar"

    assert (Path.expand("bar", "/foo") |> strip_drive_letter_if_windows)== "/foo/bar"
    assert (Path.expand("bar/", "/foo") |> strip_drive_letter_if_windows)== "/foo/bar"
    assert (Path.expand("bar/.", "/foo") |> strip_drive_letter_if_windows)== "/foo/bar"
    assert (Path.expand("bar/../bar", "/foo") |> strip_drive_letter_if_windows)== "/foo/bar"
    assert (Path.expand("../bar/../bar", "/foo/../foo/../foo")|> strip_drive_letter_if_windows) == "/bar"

    full = Path.expand("foo/bar")
    assert Path.expand("bar/../bar", "foo") == full
  end

  test :expand_path_with_list do
    assert (Path.expand('/foo/bar')|> strip_drive_letter_if_windows)  == '/foo/bar'
    assert (Path.expand('/foo/bar/')|> strip_drive_letter_if_windows)  == '/foo/bar'
    assert (Path.expand('/foo/bar/.')|> strip_drive_letter_if_windows)  == '/foo/bar'
    assert (Path.expand('/foo/bar/../bar')|> strip_drive_letter_if_windows)  == '/foo/bar'
  end

  test :relative_to_with_binary do
    assert Path.relative_to("/usr/local/foo", "/usr/local") == "foo"
    assert Path.relative_to("/usr/local/foo", "/") == "usr/local/foo"
    assert Path.relative_to("/usr/local/foo", "/etc") == "/usr/local/foo"
    assert Path.relative_to("/usr/local/foo", "/usr/local/foo") == "/usr/local/foo"

    assert Path.relative_to("usr/local/foo", "usr/local") == "foo"
    assert Path.relative_to("usr/local/foo", "etc") == "usr/local/foo"
  end

  test :relative_to_with_list do
    assert Path.relative_to('/usr/local/foo', '/usr/local') == 'foo'
    assert Path.relative_to('/usr/local/foo', '/') == 'usr/local/foo'
    assert Path.relative_to('/usr/local/foo', '/etc') == '/usr/local/foo'

    assert Path.relative_to("usr/local/foo", 'usr/local') == "foo"
    assert Path.relative_to('usr/local/foo', "etc") == "usr/local/foo"
  end

  test :rootname_with_binary do
    assert Path.rootname("~/foo/bar.ex", ".ex") == "~/foo/bar"
    assert Path.rootname("~/foo/bar.exs", ".ex") == "~/foo/bar.exs"
    assert Path.rootname("~/foo/bar.old.ex", ".ex") == "~/foo/bar.old"
  end

  test :rootname_with_list do
    assert Path.rootname('~/foo/bar.ex', '.ex') == '~/foo/bar'
    assert Path.rootname('~/foo/bar.exs', '.ex') == '~/foo/bar.exs'
    assert Path.rootname('~/foo/bar.old.ex', '.ex') == '~/foo/bar.old'
  end

  test :extname_with_binary do
    assert Path.extname("foo.erl") == ".erl"
    assert Path.extname("~/foo/bar") == ""
  end

  test :extname_with_list do
    assert Path.extname('foo.erl') == '.erl'
    assert Path.extname('~/foo/bar') == ''
  end

  test :dirname_with_binary do
    assert Path.dirname("/foo/bar.ex") == "/foo"
    assert Path.dirname("foo/bar.ex") == "foo"
    assert Path.dirname("~/foo/bar.ex") == "~/foo"
    assert Path.dirname("/foo/bar/baz/") == "/foo/bar/baz"
  end

  test :dirname_with_list do
    assert Path.dirname('/foo/bar.ex') == '/foo'
    assert Path.dirname('~/foo/bar.ex') == '~/foo'
    assert Path.dirname('/foo/bar/baz/') == '/foo/bar/baz'
  end

  test :basename_with_binary do
    assert Path.basename("foo") == "foo"
    assert Path.basename("/foo/bar") == "bar"
    assert Path.basename("/") == ""

    assert Path.basename("~/foo/bar.ex", ".ex") == "bar"
    assert Path.basename("~/foo/bar.exs", ".ex") == "bar.exs"
    assert Path.basename("~/for/bar.old.ex", ".ex") == "bar.old"
  end

  test :basename_with_list do
    assert Path.basename('foo') == 'foo'
    assert Path.basename('/foo/bar') == 'bar'
    assert Path.basename('/') == ''

    assert Path.basename('~/foo/bar.ex', '.ex') == 'bar'
    assert Path.basename('~/foo/bar.exs', '.ex') == 'bar.exs'
    assert Path.basename('~/for/bar.old.ex', '.ex') == 'bar.old'
  end

  test :join_with_binary do
    assert Path.join([""]) == ""
    assert Path.join(["foo"]) == "foo"
    assert Path.join(["/", "foo", "bar"]) == "/foo/bar"
    assert Path.join(["~", "foo", "bar"]) == "~/foo/bar"
  end

  test :join_with_list do
    assert Path.join(['']) == ''
    assert Path.join(['foo']) == 'foo'
    assert Path.join(['/', 'foo', 'bar']) == '/foo/bar'
    assert Path.join(['~', 'foo', 'bar']) == '~/foo/bar'
  end

  test :join_two_with_binary do
    assert Path.join("/foo", "bar") == "/foo/bar"
    assert Path.join("~", "foo") == "~/foo"

    assert Path.join("", "bar") == "/bar"
    assert Path.join("/foo", "/bar") == "/foo/bar"
    assert Path.join("/foo", "./bar") == "/foo/bar"
  end

  test :join_two_with_list do
    assert Path.join('/foo', 'bar') == '/foo/bar'
    assert Path.join('~', 'foo') == '~/foo'
  end

  test :split_with_binary do
    assert Path.split("") == []
    assert Path.split("foo") == ["foo"]
    assert Path.split("/foo/bar") == ["/", "foo", "bar"]
  end

  test :split_with_list do
    assert Path.split('') == []
    assert Path.split('foo') == ['foo']
    assert Path.split('/foo/bar') == ['/', 'foo', 'bar']
  end

  test :from_char_list do
    assert Path.from_char_list('simple/path') == { :ok, "simple/path" }
    assert Path.from_char_list!('simple/path') == "simple/path"
    assert Path.from_char_list('hêllõ', :latin1) == { :ok, list_to_bitstring('hêllõ') }
    assert Path.from_char_list!('hêllõ', :utf8) == "hêllõ"
    assert match?({ :error,  _, _ }, Path.from_char_list([256], :latin1))
    assert_raise Path.ConversionError, fn -> Path.from_char_list!([256], :latin1) end
  end

  test :to_char_list_with_list do
    assert Path.to_char_list('simple/path') == { :ok, 'simple/path' }
    assert Path.to_char_list!('simple/path') == 'simple/path'
    assert Path.to_char_list('hêllõ', :latin1) == { :ok, 'hêllõ' }
    assert Path.to_char_list!('hêllõ', :utf8) == 'hêllõ'
  end

  test :to_char_list_with_binary do
    assert Path.to_char_list("simple/path") == { :ok, 'simple/path' }
    assert Path.to_char_list!("simple/path") == 'simple/path'
    assert Path.to_char_list("hêllõ", :latin1) == { :ok, bitstring_to_list("hêllõ") }
    assert Path.to_char_list!("hêllõ", :utf8) == 'hêllõ'
    assert match?({ :error, _, _ }, Path.to_char_list(<<129>>, :utf8))
    assert_raise Path.ConversionError, fn -> Path.to_char_list!(<<129>>, :utf8) end
    assert match?({ :incomplete, _, _}, Path.to_char_list(<<195>>, :utf8))
    assert_raise Path.ConversionError, fn -> Path.to_char_list!(<<195>>, :utf8) end
  end

  test :to_char_list_with_atom do
    assert Path.to_char_list(:"simple/path") == { :ok, 'simple/path' }
    assert Path.to_char_list!(:"simple/path") == 'simple/path'
    assert Path.to_char_list(:"hêllõ", :latin1) == { :ok, 'hêllõ' }
    assert Path.to_char_list!(:"hêllõ", :utf8) == 'hêllõ'
  end

  test :to_char_list_with_deep do
    assert Path.to_char_list(['simple', [:"/", ['pat']], 'h']) == { :ok, 'simple/path' }
    assert Path.to_char_list!(['simple', [:"/", ['pat']], 'h']) == 'simple/path'
    assert Path.to_char_list(['h', [:"ê", ['l', :l, ['õ']]]], :latin1) == { :ok, 'hêllõ' }
    assert Path.to_char_list!(['h', [:"ê", ['l', :l, ['õ']]]], :utf8) == 'hêllõ'
  end

  test :to_binary_with_list do
    assert Path.to_binary('simple/path') == { :ok, "simple/path" }
    assert Path.to_binary!('simple/path') == "simple/path"
    assert Path.to_binary('hêllõ', :latin1) == { :ok, list_to_bitstring('hêllõ') }
    assert Path.to_binary!('hêllõ', :utf8) == "hêllõ"
    assert match?({ :error,  _, _ }, Path.to_binary([256], :latin1))
    assert_raise Path.ConversionError, fn -> Path.to_binary!([256], :latin1) end
  end

  test :to_binary_with_binary do
    assert Path.to_binary("simple/path") == { :ok, "simple/path" }
    assert Path.to_binary!("simple/path") == "simple/path"
    assert Path.to_binary("hêllõ", :latin1) == { :ok, "hêllõ" }
    assert Path.to_binary!("hêllõ", :utf8) == "hêllõ"
    assert Path.to_binary!(<<129>>, :utf8) == <<129>>
  end

  test :to_binary_with_atom do
    assert Path.to_binary(:"simple/path") == { :ok, "simple/path" }
    assert Path.to_binary!(:"simple/path") == "simple/path"
    assert Path.to_binary(:"hêllõ", :latin1) == { :ok, list_to_bitstring('hêllõ') }
    assert Path.to_binary!(:"hêllõ", :utf8) == "hêllõ"
  end

  test :to_binary_with_deep do
    assert Path.to_binary(['simple', [:"/", ['pat']], 'h']) == { :ok, "simple/path" }
    assert Path.to_binary!(['simple', [:"/", ['pat']], 'h']) == "simple/path"
    assert Path.to_binary(['h', [:"ê", ['l', :l, ['õ']]]], :latin1) == { :ok, list_to_bitstring('hêllõ') }
    assert Path.to_binary!(['h', [:"ê", ['l', :l, ['õ']]]], :utf8) == "hêllõ"
  end

  if is_win? do
    defp strip_drive_letter_if_windows([_d,?:|rest]), do: rest
    defp strip_drive_letter_if_windows(<<_d,?:,rest::binary>>), do: rest
  else
    defp strip_drive_letter_if_windows(path), do: path
  end
end
