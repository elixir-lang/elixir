Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Normalizer.FormatterASTTest do
  use ExUnit.Case, async: true

  defmacro assert_same(good, opts \\ []) do
    quote bind_quoted: [good: good, opts: opts], location: :keep do
      assert IO.iodata_to_binary(Code.format_string!(good, opts)) ==
               string_to_string(good, opts)
    end
  end

  def string_to_string(good, opts) do
    line_length = Keyword.get(opts, :line_length, 98)
    good = String.trim(good)

    to_quoted_opts =
      [
        literal_encoder: &{:ok, {:__block__, &2, [&1]}},
        token_metadata: true,
        unescape: false
      ] ++ opts

    {quoted, comments} = Code.string_to_quoted_with_comments!(good, to_quoted_opts)

    to_algebra_opts = [comments: comments, escape: false] ++ opts

    quoted
    |> Code.quoted_to_algebra(to_algebra_opts)
    |> Inspect.Algebra.format(line_length)
    |> IO.iodata_to_binary()
  end

  describe "integers" do
    test "in decimal base" do
      assert_same "0"
      assert_same "100"
      assert_same "007"
      assert_same "10000"
      assert_same "100_00"
    end

    test "in binary base" do
      assert_same "0b0"
      assert_same "0b1"
      assert_same "0b101"
      assert_same "0b01"
      assert_same "0b111_111"
    end

    test "in octal base" do
      assert_same "0o77"
      assert_same "0o0"
      assert_same "0o01"
      assert_same "0o777_777"
    end

    test "in hex base" do
      assert_same "0x1"
      assert_same "0x01"
    end

    test "as chars" do
      assert_same "?a"
      assert_same "?1"
      assert_same "?è"
      assert_same "??"
      assert_same "?\\\\"
      assert_same "?\\s"
      assert_same "?🎾"
    end
  end

  describe "floats" do
    test "with normal notation" do
      assert_same "0.0"
      assert_same "1.0"
      assert_same "123.456"
      assert_same "0.0000001"
      assert_same "001.100"
      assert_same "0_10000_0.000_000"
    end

    test "with scientific notation" do
      assert_same "1.0e1"
      assert_same "1.0e-1"
      assert_same "1.0e01"
      assert_same "1.0e-01"
      assert_same "001.100e-010"
      assert_same "0_100_0000.100e-010"
    end
  end

  describe "atoms" do
    test "true, false, nil" do
      assert_same "nil"
      assert_same "true"
      assert_same "false"
    end

    test "without escapes" do
      assert_same ~S[:foo]
    end

    test "with escapes" do
      assert_same ~S[:"f\a\b\ro"]
    end

    test "with unicode" do
      assert_same ~S[:ólá]
    end

    test "does not reformat aliases" do
      assert_same ~S[:"Elixir.String"]
    end

    test "quoted operators" do
      assert_same ~S[:"::"]
      assert_same ~S[:"..//"]
      assert_same ~S{["..//": 1]}
    end

    test "with interpolation" do
      assert_same ~S[:"one #{2} three"]
    end

    test "with escapes and interpolation" do
      assert_same ~S[:"one\n\"#{2}\"\nthree"]
    end
  end

  describe "strings" do
    test "without escapes" do
      assert_same ~S["foo"]
    end

    test "with escapes" do
      assert_same ~S["\x0A"]
      assert_same ~S["f\a\b\ro"]
      assert_same ~S["double \" quote"]
    end

    test "keeps literal new lines" do
      assert_same """
      "fo
      o"
      """
    end

    test "with interpolation" do
      assert_same ~S["one #{} three"]
      assert_same ~S["one #{2} three"]
    end

    test "with escaped interpolation" do
      assert_same ~S["one\#{two}three"]
    end

    test "with escapes and interpolation" do
      assert_same ~S["one\n\"#{2}\"\nthree"]
    end
  end

  describe "charlists" do
    test "without escapes" do
      assert_same ~S['']
      assert_same ~S[' ']
      assert_same ~S['foo']
    end

    test "with escapes" do
      assert_same ~S['f\a\b\ro']
      assert_same ~S['single \' quote']
    end

    test "keeps literal new lines" do
      assert_same """
      'fo
      o'
      """
    end

    test "with interpolation" do
      assert_same ~S['one #{2} three']
    end

    test "with escape and interpolation" do
      assert_same ~S['one\n\'#{2}\'\nthree']
    end
  end

  describe "string heredocs" do
    test "without escapes" do
      assert_same ~S'''
      """
      hello
      """
      '''
    end

    test "with escapes" do
      assert_same ~S'''
      """
      f\a\b\ro
      """
      '''

      assert_same ~S'''
      """
      multiple "\"" quotes
      """
      '''
    end

    test "with interpolation" do
      assert_same ~S'''
      """
      one
      #{2}
      three
      """
      '''

      assert_same ~S'''
      """
      one
      "
      #{2}
      "
      three
      """
      '''
    end

    test "nested with empty lines" do
      assert_same ~S'''
      nested do
        """

        foo


        bar

        """
      end
      '''
    end

    test "nested with empty lines and interpolation" do
      assert_same ~S'''
      nested do
        """

        #{foo}


        #{bar}

        """
      end
      '''

      assert_same ~S'''
      nested do
        """
        #{foo}


        #{bar}
        """
      end
      '''
    end

    test "with escaped new lines" do
      assert_same ~S'''
      """
      one\
      #{"two"}\
      three\
      """
      '''
    end
  end

  describe "charlist heredocs" do
    test "without escapes" do
      assert_same ~S"""
      '''
      hello
      '''
      """
    end

    test "with escapes" do
      assert_same ~S"""
      '''
      f\a\b\ro
      '''
      """

      assert_same ~S"""
      '''
      multiple "\"" quotes
      '''
      """
    end

    test "with interpolation" do
      assert_same ~S"""
      '''
      one
      #{2}
      three
      '''
      """

      assert_same ~S"""
      '''
      one
      "
      #{2}
      "
      three
      '''
      """
    end
  end

  describe "keyword list" do
    test "blocks" do
      assert_same ~S"""
      defmodule Example do
        def sample, do: :ok
      end
      """
    end

    test "omitting brackets" do
      assert_same ~S"""
      @type foo :: a when b: :c
      """
    end
  end

  describe "preserves user choice on parenthesis" do
    test "in functions with do blocks" do
      assert_same(~S"""
      foo Bar do
        :ok
      end
      """)

      assert_same(~S"""
      foo(Bar) do
        :ok
      end
      """)
    end
  end

  describe "preserves formatting for sigils" do
    test "without interpolation" do
      assert_same ~S[~s(foo)]
      assert_same ~S[~s{foo bar}]
      assert_same ~S[~r/Bar Baz/]
      assert_same ~S[~w<>]
      assert_same ~S[~W()]
    end

    test "with escapes" do
      assert_same ~S[~s(foo \) bar)]
      assert_same ~S[~s(f\a\b\ro)]

      assert_same ~S"""
      ~S(foo\
      bar)
      """
    end

    test "with nested new lines" do
      assert_same ~S"""
      foo do
        ~S(foo\
      bar)
      end
      """

      assert_same ~S"""
      foo do
        ~s(#{bar}
      )
      end
      """
    end

    test "with interpolation" do
      assert_same ~S[~s(one #{2} three)]
    end

    test "with modifiers" do
      assert_same ~S[~w(one two three)a]
      assert_same ~S[~z(one two three)foo]
    end

    test "with heredoc syntax" do
      assert_same ~S"""
      ~s'''
      one\a
      #{:two}\r
      three\0
      '''
      """

      assert_same ~S'''
      ~s"""
      one\a
      #{:two}\r
      three\0
      """
      '''
    end

    test "with heredoc syntax and modifier" do
      assert_same ~S"""
      ~s'''
      foo
      '''rsa
      """
    end
  end

  describe "preserves comments formatting" do
    test "before and after expressions" do
      assert_same """
      # before comment
      :hello
      """

      assert_same """
      :hello
      # after comment
      """

      assert_same """
      # before comment
      :hello
      # after comment
      """
    end

    test "empty comment" do
      assert_same """
      #
      :foo
      """
    end

    test "before and after expressions with newlines" do
      assert_same """
      # before comment
      # second line

      :hello

      # middle comment 1

      #

      # middle comment 2

      :world

      # after comment
      # second line
      """
    end

    test "interpolation with comment outside before and after" do
      assert_same ~S"""
      # comment
      IO.puts("Hello #{world}")
      """

      assert_same ~S"""
      IO.puts("Hello #{world}")
      # comment
      """
    end

    test "blocks with keyword list" do
      assert_same ~S"""
      defp sample do
        [
          # comment
          {:a, "~> 1.2"}
        ]
      end
      """

      assert_same ~S"""
      defp sample do
        [
          # comment
          {:a, "~> 1.2"},
          {:b, "~> 1.2"}
        ]
      end
      """
    end
  end
end
