Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.LiteralsTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  @short_length [line_length: 10]

  describe "integers" do
    test "in decimal base" do
      assert_same "0"
      assert_same "100"
      assert_same "007"
      assert_same "10000"
      assert_same "100_00"
      assert_format "100000", "100_000"
      assert_format "1000000", "1_000_000"
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
      assert_format "0xabcdef", "0xABCDEF"
      assert_same "0x01"
      assert_format "0xfff_fff", "0xFFF_FFF"
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
      assert_format "0100000.000000", "0_100_000.000000"
    end

    test "with scientific notation" do
      assert_same "1.0e1"
      assert_same "1.0e-1"
      assert_same "1.0e01"
      assert_same "1.0e-01"
      assert_same "001.100e-010"
      assert_same "0_100_0000.100e-010"
      assert_format "0100000.0e-5", "0_100_000.0e-5"

      assert_format "1.0E01", "1.0e01"
      assert_format "1.0E-01", "1.0e-01"
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
      assert_format ~S[:'f\a\b\ro'], ~S[:"f\a\b\ro"]
      assert_format ~S[:'single \' quote'], ~S[:"single ' quote"]
      assert_format ~S[:"double \" quote"], ~S[:"double \" quote"]
    end

    test "with unicode" do
      assert_same ~S[:ólá]
    end

    test "does not reformat aliases" do
      assert_same ~S[:"Elixir.String"]
    end

    test "removes quotes when they are not necessary" do
      assert_format ~S[:"foo"], ~S[:foo]
      assert_format ~S[:"++"], ~S[:++]
    end

    test "quoted operators" do
      assert_same ~S[:"::"]
      assert_same ~S[:"..//"]
      assert_format ~S[:..//], ~S[:"..//"]
      assert_format ~S{[..//: 1]}, ~S{["..//": 1]}
      assert_same ~S{["..//": 1]}
    end

    test "uses double quotes even when single quotes are used" do
      assert_format ~S[:'foo bar'], ~S[:"foo bar"]
    end

    test "with interpolation" do
      assert_same ~S[:"one #{2} three"]
    end

    test "with escapes and interpolation" do
      assert_same ~S[:"one\n\"#{2}\"\nthree"]
    end

    test "with interpolation on line limit" do
      assert_same ~S"""
                  :"one #{"two"} three"
                  """,
                  @short_length
    end
  end

  describe "strings" do
    test "without escapes" do
      assert_same ~S["foo"]
    end

    test "with escapes" do
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

    test "with interpolation uses block content" do
      assert_format ~S["one #{@two(three)}"], ~S["one #{@two three}"]
    end

    test "with interpolation on line limit" do
      assert_same ~S"""
                  "one #{"two"} three"
                  """,
                  @short_length
    end

    test "with escaped interpolation" do
      assert_same ~S["one\#{two}three"]
    end

    test "with escapes and interpolation" do
      assert_same ~S["one\n\"#{2}\"\nthree"]
    end

    test "is measured in graphemes" do
      assert_same ~S"""
                  "áá#{0}áá"
                  """,
                  @short_length
    end

    test "literal new lines don't count towards line limit" do
      assert_same ~S"""
                  "one
                  #{"two"}
                  three"
                  """,
                  @short_length
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

    test "with interpolation on line limit" do
      assert_same ~S"""
                  'one #{"two"} three'
                  """,
                  @short_length
    end

    test "literal new lines don't count towards line limit" do
      assert_same ~S"""
                  'one
                  #{"two"}
                  three'
                  """,
                  @short_length
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

    test "with interpolation on line limit" do
      assert_same ~S'''
                  """
                  one #{"two two"} three
                  """
                  ''',
                  @short_length
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

    test "literal new lines don't count towards line limit" do
      assert_same ~S'''
                  """
                  one
                  #{"two"}
                  three
                  """
                  ''',
                  @short_length
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

    test "with interpolation on line limit" do
      assert_same ~S"""
                  '''
                  one #{"two two"} three
                  '''
                  """,
                  @short_length
    end

    test "literal new lines don't count towards line limit" do
      assert_same ~S"""
                  '''
                  one
                  #{"two"}
                  three
                  '''
                  """,
                  @short_length
    end
  end
end
