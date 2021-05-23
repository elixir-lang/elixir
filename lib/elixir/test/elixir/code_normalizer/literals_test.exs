Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Normalizer.LiteralsTest do
  use ExUnit.Case, async: true

  import CodeNormalizerHelpers

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
end
