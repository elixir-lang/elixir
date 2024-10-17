Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.BinaryTest do
  use ExUnit.Case, async: true

  test "heredoc" do
    assert 7 == __ENV__.line

    assert "foo\nbar\n" == """
           foo
           bar
           """

    assert 14 == __ENV__.line

    assert "foo\nbar \"\"\"\n" == """
           foo
           bar \"""
           """
  end

  test "aligned heredoc" do
    assert "foo\nbar\n" == """
           foo
           bar
           """
  end

  test "heredoc with interpolation" do
    assert "31\n" == """
           #{__ENV__.line}
           """

    assert "\n36\n" == """

           #{__ENV__.line}
           """
  end

  test "heredoc in call" do
    assert "foo\nbar" ==
             Kernel.<>(
               """
               foo
               """,
               "bar"
             )
  end

  test "heredoc with heredoc inside interpolation" do
    assert """
           1
           #{"""
           2
           """}
           """ == "1\n2\n\n"
  end

  test "UTF-8" do
    assert byte_size(" ゆんゆん") == 13
  end

  test "UTF-8 char" do
    assert ?ゆ == 12422
  end

  test "size outside match" do
    x = 16
    assert <<0::size(x)>> == <<0, 0>>
  end

  test "string concatenation as match" do
    "foo" <> x = "foobar"
    assert x == "bar"

    <<"foo">> <> x = "foobar"
    assert x == "bar"

    <<"f", "oo">> <> x = "foobar"
    assert x == "bar"

    <<x::binary-size(3)>> <> _ = "foobar"
    assert x == "foo"

    size = 3
    <<x::binary-size(^size)>> <> _ = "foobar"
    assert x == "foo"

    size = 3
    <<size, x::binary-size(^size)>> <> _ = <<10, "foobar">>
    assert x == "foo"
    assert size == 10

    <<x::6*4-binary>> <> _ = "foobar"
    assert x == "foo"

    <<x::3-bytes>> <> _ = "foobar"
    assert x == "foo"

    <<x::24-bits>> <> _ = "foobar"
    assert x == "foo"

    <<x::utf8>> <> _ = "foobar"
    assert x == ?f
  end

  test "string concatenation outside match" do
    x = "bar"
    assert "foobar" = "foo" <> x
    assert "barfoo" = x <> "foo"
  end

  test "invalid string concatenation arguments" do
    assert_raise ArgumentError, ~r"expected binary argument in <> operator but got: :bar", fn ->
      Code.eval_string(~s["foo" <> :bar])
    end

    assert_raise ArgumentError, ~r"expected binary argument in <> operator but got: 1", fn ->
      Code.eval_string(~s["foo" <> 1])
    end

    message = ~r"cannot perform prefix match because the left operand of <> has unknown size."

    assert_raise ArgumentError, message, fn ->
      Code.eval_string(~s[a <> "b" = "ab"])
    end

    assert_raise ArgumentError, message, fn ->
      Code.eval_string(~s["a" <> b <> "c" = "abc"])
    end
  end

  test "hex" do
    assert "\x76" == "v"
    assert "\u00FF" == "ÿ"
    assert "\u{A}" == "\n"
    assert "\u{E9}" == "é"
    assert "\u{10F}" == <<196, 143>>
    assert "\u{10FF}" == <<225, 131, 191>>
    assert "\u{10FFF}" == <<240, 144, 191, 191>>
    assert "\u{10FFFF}" == <<244, 143, 191, 191>>
  end

  test "match" do
    assert match?(<<?a, _::binary>>, "ab")
    refute match?(<<?a, _::binary>>, "cd")
    assert match?(<<_::utf8>> <> _, "éf")
  end

  test "interpolation" do
    res = "hello \\abc"
    assert "hello #{"\\abc"}" == res
    assert "hello #{"\\abc" <> ""}" == res
  end

  test "pattern match" do
    s = 16
    assert <<_a, _b::size(^s)>> = "foo"
  end

  test "pattern match with splice" do
    assert <<1, <<2, 3, 4>>, 5>> = <<1, 2, 3, 4, 5>>
  end

  test "partial application" do
    assert (&<<&1, 2>>).(1) == <<1, 2>>
    assert (&<<&1, &2>>).(1, 2) == <<1, 2>>
    assert (&<<&2, &1>>).(2, 1) == <<1, 2>>
  end

  test "literal" do
    assert <<106, 111, 115, 195, 169>> == <<"josé">>
    assert <<106, 111, 115, 195, 169>> == <<"#{:josé}">>
    assert <<106, 111, 115, 195, 169>> == <<"josé"::binary>>
    assert <<106, 111, 115, 195, 169>> == <<"josé"::bits>>
    assert <<106, 111, 115, 195, 169>> == <<"josé"::bitstring>>
    assert <<106, 111, 115, 195, 169>> == <<"josé"::bytes>>

    assert <<106, 111, 115, 195, 169>> == <<"josé"::utf8>>
    assert <<0, 106, 0, 111, 0, 115, 0, 233>> == <<"josé"::utf16>>
    assert <<106, 0, 111, 0, 115, 0, 233, 0>> == <<"josé"::little-utf16>>
    assert <<0, 0, 0, 106, 0, 0, 0, 111, 0, 0, 0, 115, 0, 0, 0, 233>> == <<"josé"::utf32>>
  end

  test "literal errors" do
    message = "conflicting type specification for bit field"

    assert_compile_error(message, fn ->
      Code.eval_string(~s[<<"foo"::integer>>])
    end)

    assert_compile_error(message, fn ->
      Code.eval_string(~s[<<"foo"::float>>])
    end)
  end

  @bitstring <<"foo", 16::4>>

  test "bitstring attribute" do
    assert @bitstring == <<"foo", 16::4>>
  end

  @binary "new "

  test "bitsyntax expansion" do
    assert <<@binary, "world">> == "new world"
  end

  test "bitsyntax translation" do
    refb = "sample"
    sec_data = "another"

    <<
      byte_size(refb)::size(1)-big-signed-integer-unit(8),
      refb::binary,
      byte_size(sec_data)::1*16-big-signed-integer,
      sec_data::binary
    >>
  end

  test "bitsyntax size shortcut" do
    assert <<1::3>> == <<1::size(3)>>
    assert <<1::3*8>> == <<1::size(3)-unit(8)>>
  end

  test "bitsyntax variable size" do
    x = 8
    assert <<_, _::size(^x)>> = <<?a, ?b>>
    assert (fn <<_, _::size(^x)>> -> true end).(<<?a, ?b>>)
  end

  test "bitsyntax size using expressions" do
    x = 8
    assert <<1::size(x - 5)>>

    foo = %{bar: 5}
    assert <<1::size(foo.bar)>>
    assert <<1::size(length(~c"abcd"))>>
    assert <<255::size(hd(List.flatten([3])))>>
  end

  test "bitsyntax size using guard expressions in match context" do
    x = 8
    assert <<1::size(^x - 5)>> = <<1::3>>
    assert <<1::size(^x - 5)-unit(8)>> = <<1::3*8>>
    assert <<1::size(length(~c"abcd"))>> = <<1::4>>

    foo = %{bar: 5}
    assert <<1::size((^foo).bar)>> = <<1::5>>
  end

  test "bitsyntax size with pinned integer" do
    a = 1
    b = <<2, 3>>
    assert <<^a, ^b::binary>> = <<1, 2, 3>>
  end

  test "automatic size computation of matched bitsyntax variable" do
    var = "foo"
    <<^var::binary, rest::binary>> = "foobar"
    assert rest == "bar"

    <<^var::bytes, rest::bytes>> = "foobar"
    assert rest == "bar"

    ^var <> rest = "foobar"
    assert rest == "bar"

    var = <<0, 1>>
    <<^var::bitstring, rest::bitstring>> = <<0, 1, 2, 3>>
    assert rest == <<2, 3>>

    <<^var::bits, rest::bits>> = <<0, 1, 2, 3>>
    assert rest == <<2, 3>>

    ^var <> rest = <<0, 1, 2, 3>>
    assert rest == <<2, 3>>
  end

  defmacro signed_16 do
    quote do
      big - signed - integer - unit(16)
    end
  end

  defmacro refb_spec do
    quote do
      1 * 8 - big - signed - integer
    end
  end

  test "bitsyntax macro" do
    refb = "sample"
    sec_data = "another"

    <<
      byte_size(refb)::refb_spec(),
      refb::binary,
      byte_size(sec_data)::size(1)-signed_16(),
      sec_data::binary
    >>
  end

  test "bitsyntax macro is expanded with a warning" do
    assert capture_err(fn ->
             Code.eval_string("<<1::refb_spec>>", [], __ENV__)
           end) =~
             "bitstring specifier \"refb_spec\" does not exist and is being expanded to \"refb_spec()\""

    assert capture_err(fn ->
             Code.eval_string("<<1::size(1)-signed_16>>", [], __ENV__)
           end) =~
             "bitstring specifier \"signed_16\" does not exist and is being expanded to \"signed_16()\""
  end

  test "bitsyntax with extra parentheses warns" do
    assert capture_err(fn ->
             Code.eval_string("<<1::big()>>")
           end) =~ "extra parentheses on a bitstring specifier \"big()\" have been deprecated"

    assert capture_err(fn ->
             Code.eval_string("<<1::size(8)-integer()>>")
           end) =~ "extra parentheses on a bitstring specifier \"integer()\" have been deprecated"
  end

  defp capture_err(fun) do
    ExUnit.CaptureIO.capture_io(:stderr, fun)
  end

  defp assert_compile_error(message, fun) do
    assert capture_err(fn -> assert_raise CompileError, fun end) =~ message
  end
end
