Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.StringTokenizerTest do
  use ExUnit.Case, async: true

  defp var({var, _, nil}), do: var
  defp aliases({:__aliases__, _, [alias]}), do: alias

  test "tokenizes vars" do
    assert Code.string_to_quoted!("_12") |> var() == :_12
    assert Code.string_to_quoted!("ola") |> var() == :ola
    assert Code.string_to_quoted!("ólá") |> var() == :ólá
    assert Code.string_to_quoted!("óLÁ") |> var() == :óLÁ
    assert Code.string_to_quoted!("ólá?") |> var() == :ólá?
    assert Code.string_to_quoted!("ólá!") |> var() == :ólá!
    assert Code.string_to_quoted!("こんにちは世界") |> var() == :こんにちは世界
    assert {:error, _} = Code.string_to_quoted("v@r")
    assert {:error, _} = Code.string_to_quoted("1var")
  end

  test "tokenizes atoms" do
    assert Code.string_to_quoted!(":_12") == :_12
    assert Code.string_to_quoted!(":ola") == :ola
    assert Code.string_to_quoted!(":ólá") == :ólá
    assert Code.string_to_quoted!(":ólá?") == :ólá?
    assert Code.string_to_quoted!(":ólá!") == :ólá!
    assert Code.string_to_quoted!(":ól@") == :ól@
    assert Code.string_to_quoted!(":ól@!") == :ól@!
    assert Code.string_to_quoted!(":ó@@!") == :ó@@!
    assert Code.string_to_quoted!(":Ola") == :Ola
    assert Code.string_to_quoted!(":Ólá") == :Ólá
    assert Code.string_to_quoted!(":ÓLÁ") == :ÓLÁ
    assert Code.string_to_quoted!(":ÓLÁ?") == :ÓLÁ?
    assert Code.string_to_quoted!(":ÓLÁ!") == :ÓLÁ!
    assert Code.string_to_quoted!(":ÓL@!") == :ÓL@!
    assert Code.string_to_quoted!(":Ó@@!") == :Ó@@!
    assert Code.string_to_quoted!(":こんにちは世界") == :こんにちは世界
    assert {:error, _} = Code.string_to_quoted(":123")
    assert {:error, _} = Code.string_to_quoted(":@123")
  end

  test "tokenizes keywords" do
    assert Code.string_to_quoted!("[_12: 0]") == [_12: 0]
    assert Code.string_to_quoted!("[ola: 0]") == [ola: 0]
    assert Code.string_to_quoted!("[ólá: 0]") == [ólá: 0]
    assert Code.string_to_quoted!("[ólá?: 0]") == [ólá?: 0]
    assert Code.string_to_quoted!("[ólá!: 0]") == [ólá!: 0]
    assert Code.string_to_quoted!("[ól@: 0]") == [ól@: 0]
    assert Code.string_to_quoted!("[ól@!: 0]") == [ól@!: 0]
    assert Code.string_to_quoted!("[ó@@!: 0]") == [ó@@!: 0]
    assert Code.string_to_quoted!("[Ola: 0]") == [Ola: 0]
    assert Code.string_to_quoted!("[Ólá: 0]") == [Ólá: 0]
    assert Code.string_to_quoted!("[ÓLÁ: 0]") == [ÓLÁ: 0]
    assert Code.string_to_quoted!("[ÓLÁ?: 0]") == [ÓLÁ?: 0]
    assert Code.string_to_quoted!("[ÓLÁ!: 0]") == [ÓLÁ!: 0]
    assert Code.string_to_quoted!("[ÓL@!: 0]") == [ÓL@!: 0]
    assert Code.string_to_quoted!("[Ó@@!: 0]") == [Ó@@!: 0]
    assert Code.string_to_quoted!("[こんにちは世界: 0]") == [こんにちは世界: 0]
    assert {:error, _} = Code.string_to_quoted("[123: 0]")
    assert {:error, _} = Code.string_to_quoted("[@123: 0]")
  end

  test "tokenizes aliases" do
    assert Code.string_to_quoted!("Ola") |> aliases() == String.to_atom("Ola")
    assert Code.string_to_quoted!("M_123") |> aliases() == String.to_atom("M_123")
    assert {:error, _} = Code.string_to_quoted("Óla")
    assert {:error, _} = Code.string_to_quoted("Olá")
    assert {:error, _} = Code.string_to_quoted("Ol@")
    assert {:error, _} = Code.string_to_quoted("Ola?")
    assert {:error, _} = Code.string_to_quoted("Ola!")
  end

  describe "script mixing" do
    test "prevents Restricted codepoints in identifiers" do
      exception = assert_raise SyntaxError, fn -> Code.string_to_quoted!("_shibㅤ = 1") end

      assert Exception.message(exception) =~
               "unexpected token: \"ㅤ\" (column 6, code point U+3164)"
    end

    test "prevents unsafe mixing in identifiers" do
      exception =
        assert_raise SyntaxError, fn ->
          Code.string_to_quoted!("if аdmin_, do: :ok, else: :err")
        end

      assert Exception.message(exception) =~ "nofile:1:9:"
      assert Exception.message(exception) =~ "invalid mixed-script identifier found: аdmin"

      for s <- [
            "\\u0430 а {Cyrillic}",
            "\\u0064 d {Latin}",
            "\\u006D m {Latin}",
            "\\u0069 i {Latin}",
            "\\u006E n {Latin}",
            "\\u005F _"
          ] do
        assert Exception.message(exception) =~ s
      end

      # includes suggestion about what to change
      assert Exception.message(exception) =~ """
             Hint: You could write the above in a similar way that is accepted by Elixir:
             """

      assert Exception.message(exception) =~ """
             "admin_" (code points 0x00061 0x00064 0x0006D 0x00069 0x0006E 0x0005F)
             """

      # a is in cyrillic
      assert_raise SyntaxError, ~r/mixed/, fn -> Code.string_to_quoted!("[аdmin: 1]") end
      assert_raise SyntaxError, ~r/mixed/, fn -> Code.string_to_quoted!("[{:аdmin, 1}]") end
      assert_raise SyntaxError, ~r/mixed/, fn -> Code.string_to_quoted!("quote do: аdmin(1)") end

      # c is Latin
      assert_raise SyntaxError, ~r/mixed/, fn -> Code.string_to_quoted!("http_cервер = 1") end

      # T is in cyrillic
      assert_raise SyntaxError, ~r/mixed/, fn -> Code.string_to_quoted!("[Тシャツ: 1]") end
    end

    test "allows legitimate script mixing" do
      # Mixed script with supersets, numbers, and underscores
      assert Code.eval_string("幻ㄒㄧㄤ = 1") == {1, [幻ㄒㄧㄤ: 1]}
      assert Code.eval_string("幻ㄒㄧㄤ1 = 1") == {1, [幻ㄒㄧㄤ1: 1]}
      assert Code.eval_string("__सवव_1? = 1") == {1, [__सवव_1?: 1]}

      # Elixir's normalizations combine scriptsets of the 'from' and 'to' characters,
      # ex: {Common} MICRO => {Greek} MU == {Common, Greek}; Common intersects w/all
      assert Code.eval_string("μs = 1") == {1, [μs: 1]}

      # Mixed scripts in variables
      assert Code.eval_string("http_сервер = 1") == {1, [http_сервер: 1]}
      assert Code.eval_string("сервер_http = 1") == {1, [сервер_http: 1]}

      # Mixed scripts in atoms
      assert Code.eval_string(":T_シャツ") == {:T_シャツ, []}
    end

    test "bidi" do
      # test that the implementation of String.Tokenizer.Security.unbidify/1 agrees
      # w/Unicode Bidi Algo (UAX9) for these (identifier-specific, no-bracket) examples
      #
      # you can create new examples with: https://util.unicode.org/UnicodeJsps/bidic.jsp?s=foo_%D9%84%D8%A7%D9%85%D8%AF%D8%A7_baz&b=0&u=140&d=2
      # inspired by (none of these are directly usable for our idents): https://www.unicode.org/Public/UCD/latest/ucd/BidiCharacterTest.txt
      #
      # there's a spurious ;A; after the identifier, because the semicolon is dir-neutral, and
      # deleting it makes these examples hard to read in many/most editors!
      """
      foo;A;0066 006F 006F;0 1 2
      _foo_ ;A;005F 0066 006F 006F 005F;0 1 2 3 4
      __foo__ ;A;005F 005F 0066 006F 006F 005F 005F;0 1 2 3 4 5 6
      لامدا_foo ;A;0644 0627 0645 062F 0627 005F 0066 006F 006F;4 3 2 1 0 5 6 7 8
      foo_لامدا_baz ;A;0066 006F 006F 005F 0644 0627 0645 062F 0627 005F 0062 0061 007A;0 1 2 3 8 7 6 5 4 9 10 11 12
      foo_لامدا ;A;0066 006F 006F 005F 0644 0627 0645 062F 0627;0 1 2 3 8 7 6 5 4
      foo_لامدا1 ;A;0066 006F 006F 005F 0644 0627 0645 062F 0627 0031;0 1 2 3 9 8 7 6 5 4
      foo_لامدا_حدد ;A;0066 006F 006F 005F 0644 0627 0645 062F 0627 005F 062D 062F 062F;0 1 2 3 12 11 10 9 8 7 6 5 4
      foo_لامدا_حدد1 ;A;0066 006F 006F 005F 0644 0627 0645 062F 0627 005F 062D 062F 062F 0031;0 1 2 3 13 12 11 10 9 8 7 6 5 4
      foo_لامدا_حدد1_bar ;A; 0066 006F 006F 005F 0644 0627 0645 062F 0627 005F 062D 062F 062F 0031 005F 0062 0061 0072;0 1 2 3 13 12 11 10 9 8 7 6 5 4 14 15 16 17
      foo_لامدا_حدد1_bar1 ;A;0066 006F 006F 005F 0644 0627 0645 062F 0627 005F 062D 062F 062F 0031 005F 0062 0061 0072 0031;0 1 2 3 13 12 11 10 9 8 7 6 5 4 14 15 16 17 18
      """
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split(&1, ";", trim: true))
      |> Enum.each(fn
        [ident, _, bytes, indices | _rest] ->
          bytes = String.split(bytes, " ", trim: true) |> Enum.map(&String.to_integer(&1, 16))
          indices = String.split(indices, " ", trim: true) |> Enum.map(&String.to_integer/1)
          display_ordered = for i <- indices, do: Enum.at(bytes, i)
          unbidified = String.Tokenizer.Security.unbidify(bytes)

          if display_ordered != unbidified do
            raise """
            Failing String.Tokenizer.Security.unbidify/1 case for: '#{ident}'
              bytes        : #{bytes |> Enum.map(&Integer.to_string(&1, 16)) |> Enum.join(" ")}
              byte order   : #{bytes |> Enum.intersperse(32)}
              uax9 order   : #{display_ordered |> Enum.intersperse(32)}
              uax9 indices : #{indices |> Enum.join(" ")}
              unbidify/1   : #{unbidified |> Enum.intersperse(32)}
            """
          end
      end)
    end
  end
end
