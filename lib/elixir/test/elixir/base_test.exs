Code.require_file "test_helper.exs", __DIR__

defmodule BaseTest do
  use ExUnit.Case, async: true

  doctest Base

  import Base

  test "encode16" do
    assert "" == encode16("")
    assert "66" == encode16("f")
    assert "666F" == encode16("fo")
    assert "666F6F" == encode16("foo")
    assert "666F6F62" == encode16("foob")
    assert "666F6F6261" == encode16("fooba")
    assert "666F6F626172" == encode16("foobar")
    assert "A1B2C3D4E5F67891" == encode16(<<161, 178, 195, 212, 229, 246, 120, 145>>)

    assert "a1b2c3d4e5f67891" == encode16(<<161, 178, 195, 212, 229, 246, 120, 145>>, case: :lower)
  end

  test "decode16" do
    assert {:ok, ""} == decode16("")
    assert {:ok, "f"} == decode16("66")
    assert {:ok, "fo"} == decode16("666F")
    assert {:ok, "foo"} == decode16("666F6F")
    assert {:ok, "foob"} == decode16("666F6F62")
    assert {:ok, "fooba"} == decode16("666F6F6261")
    assert {:ok, "foobar"} == decode16("666F6F626172")
    assert {:ok, <<161, 178, 195, 212, 229, 246, 120, 145>>} == decode16("A1B2C3D4E5F67891")

    assert {:ok, <<161, 178, 195, 212, 229, 246, 120, 145>>} == decode16("a1b2c3d4e5f67891", case: :lower)
    assert {:ok, <<161, 178, 195, 212, 229, 246, 120, 145>>} == decode16("a1B2c3D4e5F67891", case: :mixed)
  end

  test "decode16!" do
    assert "" == decode16!("")
    assert "f" == decode16!("66")
    assert "fo" == decode16!("666F")
    assert "foo" == decode16!("666F6F")
    assert "foob" == decode16!("666F6F62")
    assert "fooba" == decode16!("666F6F6261")
    assert "foobar" == decode16!("666F6F626172")
    assert <<161, 178, 195, 212, 229, 246, 120, 145>> == decode16!("A1B2C3D4E5F67891")

    assert <<161, 178, 195, 212, 229, 246, 120, 145>> == decode16!("a1b2c3d4e5f67891", case: :lower)
    assert  <<161, 178, 195, 212, 229, 246, 120, 145>> == decode16!("a1B2c3D4e5F67891", case: :mixed)
  end

  test "decode16 non-alphabet digit" do
    assert :error == decode16("66KF")
    assert :error == decode16("66ff")
    assert :error == decode16("66FF", case: :lower)
  end

  test "decode16! non-alphabet digit" do
    assert_raise ArgumentError, "non-alphabet digit found: \"K\" (byte 75)", fn ->
      decode16!("66KF")
    end
    assert_raise ArgumentError, "non-alphabet digit found: \"f\" (byte 102)", fn ->
      decode16!("66ff")
    end
    assert_raise ArgumentError, "non-alphabet digit found: \"F\" (byte 70)", fn ->
      decode16!("66FF", case: :lower)
    end
  end

  test "decode16 odd-length string" do
    assert :error == decode16("666")
  end

  test "decode16! odd-length string" do
    assert_raise ArgumentError, "odd-length string", fn ->
      decode16!("666")
    end
  end

  test "encode64 empty" do
    assert "" == encode64("")
  end

  test "encode64 two pads" do
    assert "QWxhZGRpbjpvcGVuIHNlc2FtZQ==" == encode64("Aladdin:open sesame")
  end

  test "encode64 one pad" do
    assert "SGVsbG8gV29ybGQ=" == encode64("Hello World")
  end

  test "encode64 no pad" do
    assert "QWxhZGRpbjpvcGVuIHNlc2Ft" == encode64("Aladdin:open sesam")
    assert "MDEyMzQ1Njc4OSFAIzBeJiooKTs6PD4sLiBbXXt9" == encode64(<<"0123456789!@#0^&*();:<>,. []{}">>)
  end

  test "encode64 omit padding one pad" do
    assert "SGVsbG8gV29ybGQ" == encode64("Hello World", padding: false)
  end

  test "encode64 omit padding two pads" do
    assert "QWxhZGRpbjpvcGVuIHNlc2FtZQ" == encode64("Aladdin:open sesame", padding: false)
  end

  test "encode64 omit padding no pad" do
    assert "QWxhZGRpbjpvcGVuIHNlc2Ft" == encode64("Aladdin:open sesam", padding: false)
  end

  test "decode64 empty" do
    assert {:ok, ""} == decode64("")
  end

  test "decode64! empty" do
    assert "" == decode64!("")
  end

  test "decode64 two pads" do
    assert {:ok, "Aladdin:open sesame"} == decode64("QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
  end

  test "decode64! two pads" do
    assert "Aladdin:open sesame" == decode64!("QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
  end

  test "decode64 one pad" do
    assert {:ok, "Hello World"} == decode64("SGVsbG8gV29ybGQ=")
  end

  test "decode64! one pad" do
    assert "Hello World" == decode64!("SGVsbG8gV29ybGQ=")
  end

  test "decode64 no pad" do
    assert {:ok, "Aladdin:open sesam"} == decode64("QWxhZGRpbjpvcGVuIHNlc2Ft")
  end

  test "decode64! no pad" do
    assert "Aladdin:open sesam" == decode64!("QWxhZGRpbjpvcGVuIHNlc2Ft")
  end

  test "decode64 non-alphabet digit" do
    assert :error == decode64("Zm9)")
  end

  test "decode64! non-alphabet digit" do
    assert_raise ArgumentError, "non-alphabet digit found: \")\" (byte 41)", fn ->
      decode64!("Zm9)")
    end
  end

  test "decode64 whitespace" do
    assert :error == decode64("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t")
    assert {:ok, "Aladdin:open sesam"} == decode64("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t", ignore: :whitespace)
  end

  test "decode64! whitespace" do
    assert_raise ArgumentError, "non-alphabet digit found: \"\\n\" (byte 10)", fn ->
      decode64!("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t")
    end
    assert "Aladdin:open sesam" == decode64!("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t", ignore: :whitespace)
  end

  test "decode64 incorrect padding" do
    assert :error == decode64("SGVsbG8gV29ybGQ")
  end

  test "decode64! incorrect padding" do
    assert_raise ArgumentError, "incorrect padding", fn ->
      decode64!("SGVsbG8gV29ybGQ")
    end
  end

  test "decode64 ignore padding two pads" do
    assert {:ok, "Aladdin:open sesame"} == decode64("QWxhZGRpbjpvcGVuIHNlc2FtZQ", padding: false)
  end

  test "decode64! ignore padding two pads" do
    assert "Aladdin:open sesame" == decode64!("QWxhZGRpbjpvcGVuIHNlc2FtZQ", padding: false)
  end

  test "decode64 ignore padding one pad" do
    assert {:ok, "Hello World"} == decode64("SGVsbG8gV29ybGQ", padding: false)
  end

  test "decode64! ignore padding one pad" do
    assert "Hello World" == decode64!("SGVsbG8gV29ybGQ", padding: false)
  end

  test "decode64 ignore padding no pad" do
    assert {:ok, "Aladdin:open sesam"} == decode64("QWxhZGRpbjpvcGVuIHNlc2Ft", padding: false)
  end

  test "decode64! ignore padding no pad" do
    assert "Aladdin:open sesam" == decode64!("QWxhZGRpbjpvcGVuIHNlc2Ft", padding: false)
  end

  test "decode64 ignore padding incorrect padding" do
    assert {:ok, "Hello World"} == decode64("SGVsbG8gV29ybGQ", padding: false)
  end

  test "decode64! ignore padding incorrect padding" do
    assert "Hello World" == decode64!("SGVsbG8gV29ybGQ", padding: false)
  end

  test "url_encode64 empty" do
    assert "" == url_encode64("")
  end

  test "url_encode64 two pads" do
    assert "QWxhZGRpbjpvcGVuIHNlc2FtZQ==" == url_encode64("Aladdin:open sesame")
  end

  test "url_encode64 one pad" do
    assert "SGVsbG8gV29ybGQ=" == url_encode64("Hello World")
  end

  test "url_encode64 no pad" do
    assert "QWxhZGRpbjpvcGVuIHNlc2Ft" == url_encode64("Aladdin:open sesam")
    assert "MDEyMzQ1Njc4OSFAIzBeJiooKTs6PD4sLiBbXXt9" == url_encode64(<<"0123456789!@#0^&*();:<>,. []{}">>)
  end

  test "url_encode64 omit padding two pads" do
    assert "QWxhZGRpbjpvcGVuIHNlc2FtZQ" == url_encode64("Aladdin:open sesame", padding: false)
  end

  test "url_encode64 omit padding one pad" do
    assert "SGVsbG8gV29ybGQ" == url_encode64("Hello World", padding: false)
  end

  test "url_encode64 omit padding no pad" do
    assert "QWxhZGRpbjpvcGVuIHNlc2Ft" == url_encode64("Aladdin:open sesam", padding: false)
  end

  test "url_encode64 no URL unsafe characters" do
    refute "/3/+/A==" == url_encode64(<<255, 127, 254, 252>>)
    assert "_3_-_A==" == url_encode64(<<255, 127, 254, 252>>)
  end

  test "url_decode64 empty" do
    assert {:ok, ""} == url_decode64("")
  end

  test "url_decode64! empty" do
    assert "" == url_decode64!("")
  end

  test "url_decode64 two pads" do
    assert {:ok, "Aladdin:open sesame"} == url_decode64("QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
  end

  test "url_decode64! two pads" do
    assert "Aladdin:open sesame" == url_decode64!("QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
  end

  test "url_decode64 one pad" do
    assert {:ok, "Hello World"} == url_decode64("SGVsbG8gV29ybGQ=")
  end

  test "url_decode64! one pad" do
    assert "Hello World" == url_decode64!("SGVsbG8gV29ybGQ=")
  end

  test "url_decode64 no pad" do
    assert {:ok, "Aladdin:open sesam"} == url_decode64("QWxhZGRpbjpvcGVuIHNlc2Ft")
  end

  test "url_decode64! no pad" do
    assert "Aladdin:open sesam" == url_decode64!("QWxhZGRpbjpvcGVuIHNlc2Ft")
  end

  test "url_decode64 whitespace" do
    assert :error == url_decode64("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t")
    assert {:ok, "Aladdin:open sesam"} == url_decode64("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t", ignore: :whitespace)
  end

  test "url_decode64! whitespace" do
    assert_raise ArgumentError, "non-alphabet digit found: \"\\n\" (byte 10)", fn ->
      url_decode64!("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t")
    end
    assert "Aladdin:open sesam" == url_decode64!("\nQWxhZGRp bjpvcGVu\sIHNlc2Ft\t", ignore: :whitespace)
  end

  test "url_decode64 non-alphabet digit" do
    assert :error == url_decode64("Zm9)")
  end

  test "url_decode64! non-alphabet digit" do
    assert_raise ArgumentError, "non-alphabet digit found: \")\" (byte 41)", fn ->
      url_decode64!("Zm9)")
    end
  end

  test "url_decode64 incorrect padding" do
    assert :error == url_decode64("SGVsbG8gV29ybGQ")
  end

  test "url_decode64! incorrect padding" do
    assert_raise ArgumentError, "incorrect padding", fn ->
      url_decode64!("SGVsbG8gV29ybGQ")
    end
  end

  test "url_decode64 ignore padding two pads" do
    assert {:ok, "Aladdin:open sesame"} == url_decode64("QWxhZGRpbjpvcGVuIHNlc2FtZQ", padding: false)
  end

  test "url_decode64! ignore padding two pads" do
    assert "Aladdin:open sesame" == url_decode64!("QWxhZGRpbjpvcGVuIHNlc2FtZQ", padding: false)
  end

  test "url_decode64 ignore padding one pad" do
    assert {:ok, "Hello World"} == url_decode64("SGVsbG8gV29ybGQ", padding: false)
  end

  test "url_decode64! ignore padding one pad" do
    assert "Hello World" == url_decode64!("SGVsbG8gV29ybGQ", padding: false)
  end

  test "url_decode64 ignore padding no pad" do
    assert {:ok, "Aladdin:open sesam"} == url_decode64("QWxhZGRpbjpvcGVuIHNlc2Ft", padding: false)
  end

  test "url_decode64! ignore padding no pad" do
    assert "Aladdin:open sesam" == url_decode64!("QWxhZGRpbjpvcGVuIHNlc2Ft", padding: false)
  end

  test "url_decode64 ignore padding incorrect padding" do
    assert {:ok, "Hello World"} == url_decode64("SGVsbG8gV29ybGQ", padding: false)
  end

  test "url_decode64! ignore padding incorrect padding" do
    assert "Hello World" == url_decode64!("SGVsbG8gV29ybGQ", padding: false)
  end

  test "encode32 empty" do
    assert "" == encode32("")
  end

  test "encode32 one pad" do
    assert "MZXW6YQ=" == encode32("foob")
  end

  test "encode32 three pads" do
    assert "MZXW6===" == encode32("foo")
  end

  test "encode32 four pads" do
    assert "MZXQ====" == encode32("fo")
  end

  test "encode32 six pads" do
    assert "MZXW6YTBOI======" == encode32("foobar")
    assert "MY======" == encode32("f")
  end

  test "encode32 no pads" do
    assert "MZXW6YTB" == encode32("fooba")
  end

  test "encode32 omit padding one pad" do
    assert "MZXW6YQ" == encode32("foob", padding: false)
  end

  test "encode32 omit padding three pads" do
    assert "MZXW6" == encode32("foo", padding: false)
  end

  test "encode32 omit padding four pads" do
    assert "MZXQ" == encode32("fo", padding: false)
  end

  test "encode32 omit padding six pads" do
    assert "MZXW6YTBOI" == encode32("foobar", padding: false)
  end

  test "encode32 omit padding no pads" do
    assert "MZXW6YTB" == encode32("fooba", padding: false)
  end

  test "encode32 lowercase" do
    assert "mzxw6ytb" == encode32("fooba", case: :lower)
  end

  test "decode32 empty" do
    assert {:ok, ""} == decode32("")
  end

  test "decode32! empty" do
    assert "" == decode32!("")
  end

  test "decode32 one pad" do
    assert {:ok, "foob"} == decode32("MZXW6YQ=")
  end

  test "decode32! one pad" do
    assert "foob" == decode32!("MZXW6YQ=")
  end

  test "decode32 three pads" do
    assert {:ok, "foo"} == decode32("MZXW6===")
  end

  test "decode32! three pads" do
    assert "foo" == decode32!("MZXW6===")
  end

  test "decode32 four pads" do
    assert {:ok, "fo"} == decode32("MZXQ====")
  end

  test "decode32! four pads" do
    assert "fo" == decode32!("MZXQ====")
  end

  test "decode32 lowercase" do
    assert {:ok, "fo"} == decode32("mzxq====", case: :lower)
  end

  test "decode32! lowercase" do
    assert "fo" == decode32!("mzxq====", case: :lower)
  end

  test "decode32 mixed case" do
    assert {:ok, "fo"} == decode32("mZXq====", case: :mixed)
  end

  test "decode32! mixed case" do
    assert "fo" == decode32!("mZXq====", case: :mixed)
  end

  test "decode32 six pads" do
    assert {:ok, "foobar"} == decode32("MZXW6YTBOI======")
    assert {:ok, "f"} == decode32("MY======")
  end

  test "decode32! six pads" do
    assert "foobar" == decode32!("MZXW6YTBOI======")
    assert "f" == decode32!("MY======")
  end

  test "decode32 no pads" do
    assert {:ok, "fooba"} == decode32("MZXW6YTB")
  end

  test "decode32! no pads" do
    assert "fooba" == decode32!("MZXW6YTB")
  end

  test "decode32 non-alphabet digit" do
    assert :error == decode32("MZX)6YTB")
    assert :error == decode32("66ff")
    assert :error == decode32("66FF", case: :lower)
  end

  test "decode32! non-alphabet digit" do
    assert_raise ArgumentError, "non-alphabet digit found: \")\" (byte 41)", fn ->
      decode32!("MZX)6YTB")
    end
    assert_raise ArgumentError, "non-alphabet digit found: \"m\" (byte 109)", fn ->
      decode32!("mzxw6ytboi======")
    end
    assert_raise ArgumentError, "non-alphabet digit found: \"M\" (byte 77)", fn ->
      decode32!("MZXW6YTBOI======", case: :lower)
    end
  end

  test "decode32 incorrect padding" do
    assert :error == decode32("MZXW6YQ")
  end

  test "decode32! incorrect padding" do
    assert_raise ArgumentError, "incorrect padding", fn ->
      decode32!("MZXW6YQ")
    end
  end

  test "decode32 ignore padding one pad" do
    assert {:ok, "foob"} == decode32("MZXW6YQ", padding: false)
  end

  test "decode32! ignore padding one pad" do
    assert "foob" == decode32!("MZXW6YQ", padding: false)
  end

  test "decode32 ignore padding three pads" do
    assert {:ok, "foo"} == decode32("MZXW6", padding: false)
  end

  test "decode32! ignore padding three pads" do
    assert "foo" == decode32!("MZXW6", padding: false)
  end

  test "decode32 ignore padding four pads" do
    assert {:ok, "fo"} == decode32("MZXQ", padding: false)
  end

  test "decode32! ignore padding four pads" do
    assert "fo" == decode32!("MZXQ", padding: false)
  end

  test "decode32 ignore padding lowercase" do
    assert {:ok, "fo"} == decode32("mzxq", case: :lower, padding: false)
  end

  test "decode32! ignore padding lowercase" do
    assert "fo" == decode32!("mzxq", case: :lower, padding: false)
  end

  test "decode32 ignore padding mixed case" do
    assert {:ok, "fo"} == decode32("mZXq", case: :mixed, padding: false)
  end

  test "decode32! ignore padding mixed case" do
    assert "fo" == decode32!("mZXq", case: :mixed, padding: false)
  end

  test "decode32 ignore padding six pads" do
    assert {:ok, "foobar"} == decode32("MZXW6YTBOI", padding: false)
  end

  test "decode32! ignore padding six pads" do
    assert "foobar" == decode32!("MZXW6YTBOI", padding: false)
  end

  test "decode32 ignore padding no pads" do
    assert {:ok, "fooba"} == decode32("MZXW6YTB", padding: false)
  end

  test "decode32! ignore padding no pads" do
    assert "fooba" == decode32!("MZXW6YTB", padding: false)
  end

  test "decode32 ignore padding incorrect padding" do
    assert {:ok, "foob"} == decode32("MZXW6YQ", padding: false)
  end

  test "decode32! ignore padding incorrect padding" do
    "foob" = decode32!("MZXW6YQ", padding: false)
  end

  test "hex_encode32 empty" do
    assert "" == hex_encode32("")
  end

  test "hex_encode32 one pad" do
    assert "CPNMUOG=" == hex_encode32("foob")
  end

  test "hex_encode32 three pads" do
    assert "CPNMU===" == hex_encode32("foo")
  end

  test "hex_encode32 four pads" do
    assert "CPNG====" == hex_encode32("fo")
  end

  test "hex_encode32 six pads" do
    assert "CPNMUOJ1E8======" == hex_encode32("foobar")
    assert "CO======" == hex_encode32("f")
  end

  test "hex_encode32 no pads" do
    assert "CPNMUOJ1" == hex_encode32("fooba")
  end

  test "hex_encode32 omit padding one pad" do
    assert "CPNMUOG" == hex_encode32("foob", padding: false)
  end

  test "hex_encode32 omit padding three pads" do
    assert "CPNMU" == hex_encode32("foo", padding: false)
  end

  test "hex_encode32 omit padding four pads" do
    assert "CPNG" == hex_encode32("fo", padding: false)
  end

  test "hex_encode32 omit padding six pads" do
    assert "CPNMUOJ1E8" == hex_encode32("foobar", padding: false)
  end

  test "hex_encode32 omit padding no pads" do
    assert "CPNMUOJ1" == hex_encode32("fooba", padding: false)
  end

  test "hex_encode32 lowercase" do
    assert "cpnmuoj1" == hex_encode32("fooba", case: :lower)
  end

  test "hex_decode32 empty" do
    assert {:ok, ""} == hex_decode32("")
  end

  test "hex_decode32! empty" do
    assert "" == hex_decode32!("")
  end

  test "hex_decode32 one pad" do
    assert {:ok, "foob"} == hex_decode32("CPNMUOG=")
  end

  test "hex_decode32! one pad" do
    assert "foob" == hex_decode32!("CPNMUOG=")
  end

  test "hex_decode32 three pads" do
    assert {:ok, "foo"} == hex_decode32("CPNMU===")
  end

  test "hex_decode32! three pads" do
    assert "foo" == hex_decode32!("CPNMU===")
  end

  test "hex_decode32 four pads" do
    assert {:ok, "fo"} == hex_decode32("CPNG====")
  end

  test "hex_decode32! four pads" do
    assert "fo" == hex_decode32!("CPNG====")
  end

  test "hex_decode32 six pads" do
    assert {:ok, "foobar"} == hex_decode32("CPNMUOJ1E8======")
    assert {:ok, "f"} == hex_decode32("CO======")
  end

  test "hex_decode32! six pads" do
    assert "foobar" == hex_decode32!("CPNMUOJ1E8======")
    assert "f" == hex_decode32!("CO======")
  end

  test "hex_decode32 no pads" do
    assert {:ok, "fooba"} == hex_decode32("CPNMUOJ1")
  end

  test "hex_decode32! no pads" do
    assert "fooba" == hex_decode32!("CPNMUOJ1")
  end

  test "hex_decode32 non-alphabet digit" do
    assert :error == hex_decode32("CPN)UOJ1")
    assert :error == hex_decode32("66f")
    assert :error == hex_decode32("66F", case: :lower)
  end

  test "hex_decode32! non-alphabet digit" do
    assert_raise ArgumentError, "non-alphabet digit found: \")\" (byte 41)", fn ->
      hex_decode32!("CPN)UOJ1")
    end
    assert_raise ArgumentError, "non-alphabet digit found: \"c\" (byte 99)", fn ->
      hex_decode32!("cpnmuoj1e8======")
    end
    assert_raise ArgumentError, "non-alphabet digit found: \"C\" (byte 67)", fn ->
      hex_decode32!("CPNMUOJ1E8======", case: :lower)
    end
  end

  test "hex_decode32 incorrect padding" do
    assert :error == hex_decode32("CPNMUOG")
  end

  test "hex_decode32! incorrect padding" do
    assert_raise ArgumentError, "incorrect padding", fn ->
      hex_decode32!("CPNMUOG")
    end
  end

  test "hex_decode32 lowercase" do
    assert {:ok, "fo"} == hex_decode32("cpng====", case: :lower)
  end

  test "hex_decode32! lowercase" do
    assert "fo" == hex_decode32!("cpng====", case: :lower)
  end

  test "hex_decode32 mixed case" do
    assert {:ok, "fo"} == hex_decode32("cPNg====", case: :mixed)
  end

  test "hex_decode32! mixed case" do
    assert "fo" == hex_decode32!("cPNg====", case: :mixed)
  end

  test "decode non UTF-8 char" do
    assert_raise ArgumentError, "non-alphabet digit found: \"\\0\" (byte 0)", fn ->
      decode16!("012" <> <<0>>)
    end
  end

  test "hex_decode32 ignore padding one pad" do
    assert {:ok, "foob"} == hex_decode32("CPNMUOG", padding: false)
  end

  test "hex_decode32! ignore padding one pad" do
    assert "foob" == hex_decode32!("CPNMUOG", padding: false)
  end

  test "hex_decode32 ignore padding three pads" do
    assert {:ok, "foo"} == hex_decode32("CPNMU", padding: false)
  end

  test "hex_decode32! ignore padding three pads" do
    assert "foo" == hex_decode32!("CPNMU", padding: false)
  end

  test "hex_decode32 ignore padding four pads" do
    assert {:ok, "fo"} == hex_decode32("CPNG", padding: false)
  end

  test "hex_decode32! ignore padding four pads" do
    assert "fo" == hex_decode32!("CPNG", padding: false)
  end

  test "hex_decode32 ignore padding six pads" do
    assert {:ok, "foobar"} == hex_decode32("CPNMUOJ1E8", padding: false)
  end

  test "hex_decode32! ignore padding six pads" do
    assert "foobar" == hex_decode32!("CPNMUOJ1E8", padding: false)
  end

  test "hex_decode32 ignore padding no pads" do
    assert {:ok, "fooba"} == hex_decode32("CPNMUOJ1", padding: false)
  end

  test "hex_decode32! ignore padding no pads" do
    assert "fooba" == hex_decode32!("CPNMUOJ1", padding: false)
  end

  test "hex_decode32 ignore padding incorrect padding" do
    assert {:ok, "foob"} == hex_decode32("CPNMUOG", padding: false)
  end

  test "hex_decode32! ignore padding incorrect padding" do
    "foob" = hex_decode32!("CPNMUOG", padding: false)
  end

  test "hex_decode32 ignoring padding lowercase" do
    assert {:ok, "fo"} == hex_decode32("cpng", case: :lower, padding: false)
  end

  test "hex_decode32! ignore padding lowercase" do
    assert "fo" == hex_decode32!("cpng", case: :lower, padding: false)
  end

  test "hex_decode32 ignore padding mixed case" do
    assert {:ok, "fo"} == hex_decode32("cPNg====", case: :mixed, padding: false)
  end

  test "hex_decode32! ignore padding mixed case" do
    assert "fo" == hex_decode32!("cPNg", case: :mixed, padding: false)
  end
end
