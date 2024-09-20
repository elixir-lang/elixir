Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.MigrationTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  @short_length [line_length: 10]

  describe "migrate_bitstring_modifiers: true" do
    @opts [migrate_bitstring_modifiers: true]

    test "normalizes bitstring modifiers" do
      assert_format "<<foo::binary()>>", "<<foo::binary>>", @opts
      assert_same "<<foo::binary>>", @opts

      assert_format "<<foo::custom_type>>", "<<foo::custom_type()>>", @opts
      assert_same "<<foo::custom_type()>>", @opts

      assert_format "<<x::binary()-(13 * 6)-custom>>", "<<x::binary-(13 * 6)-custom()>>", @opts
      assert_same "<<x::binary-(13 * 6)-custom()>>", @opts
      assert_same "<<0::size*unit, bytes::binary>>", @opts
      assert_format "<<0::size*unit, bytes::custom>>", "<<0::size*unit, bytes::custom()>>", @opts

      assert_format "<<0, 1::2-integer() <- x>>", "<<0, 1::2-integer <- x>>", @opts
      assert_same "<<0, 1::2-integer <- x>>", @opts
    end
  end

  describe "migrate_charlists_as_sigils: true" do
    @opts [migrate_charlists_as_sigils: true]

    test "without escapes" do
      assert_format ~S[''], ~S[~c""], @opts
      assert_format ~S[' '], ~S[~c" "], @opts
      assert_format ~S['foo'], ~S[~c"foo"], @opts
    end

    test "with escapes" do
      assert_format ~S['f\a\b\ro'], ~S[~c"f\a\b\ro"], @opts
      assert_format ~S['single \' quote'], ~S[~c"single ' quote"], @opts
      assert_format ~S['double " quote'], ~S[~c'double " quote'], @opts
      assert_format ~S['escaped \" quote'], ~S[~c'escaped \" quote'], @opts
      assert_format ~S['\\"'], ~S[~c'\\"'], @opts
    end

    test "keeps literal new lines" do
      assert_format """
                    'fo
                    o'
                    """,
                    """
                    ~c"fo
                    o"
                    """,
                    @opts
    end

    test "with interpolation" do
      assert_format ~S['one #{2} three'], ~S[~c"one #{2} three"], @opts
      assert_format ~S['#{1}\n \\ " \"'], ~S[~c'#{1}\n \\ " \"'], @opts
    end

    test "with escape and interpolation" do
      assert_format ~S['one\n\'#{2}\'\nthree'], ~S[~c"one\n'#{2}'\nthree"], @opts
      assert_format ~S['one\n"#{2}"\nthree'], ~S[~c'one\n"#{2}"\nthree'], @opts
    end

    test "with interpolation on line limit" do
      assert_format ~S"""
                    'one #{"two"} three'
                    """,
                    ~S"""
                    ~c"one #{"two"} three"
                    """,
                    @short_length ++ @opts
    end

    test "literal new lines don't count towards line limit" do
      assert_format ~S"""
                    'one
                    #{"two"}
                    three'
                    """,
                    ~S"""
                    ~c"one
                    #{"two"}
                    three"
                    """,
                    @short_length ++ @opts
    end

    test "heredocs without escapes" do
      assert_format ~S"""
                    '''
                    hello
                    '''
                    """,
                    ~S'''
                    ~c"""
                    hello
                    """
                    ''',
                    @opts
    end

    test "heredocs with escapes" do
      assert_format ~S"""
                    '''
                    f\a\b\ro
                    '''
                    """,
                    ~S'''
                    ~c"""
                    f\a\b\ro
                    """
                    ''',
                    @opts

      assert_format ~S"""
                    '''
                    multiple "\"" quotes
                    '''
                    """,
                    ~S'''
                    ~c"""
                    multiple "\"" quotes
                    """
                    ''',
                    @opts
    end

    test "heredocs with interpolation" do
      assert_format ~S"""
                    '''
                    one
                    #{2}
                    three
                    '''
                    """,
                    ~S'''
                    ~c"""
                    one
                    #{2}
                    three
                    """
                    ''',
                    @opts

      assert_format ~S"""
                    '''
                    one
                    "
                    #{2}
                    "
                    three
                    '''
                    """,
                    ~S'''
                    ~c"""
                    one
                    "
                    #{2}
                    "
                    three
                    """
                    ''',
                    @opts
    end

    test "heredocs with interpolation on line limit" do
      assert_format ~S"""
                    '''
                    one #{"two two"} three
                    '''
                    """,
                    ~S'''
                    ~c"""
                    one #{"two two"} three
                    """
                    ''',
                    @short_length ++ @opts
    end

    test "heredocs literal new lines don't count towards line limit" do
      assert_format ~S"""
                    '''
                    one
                    #{"two"}
                    three
                    '''
                    """,
                    ~S'''
                    ~c"""
                    one
                    #{"two"}
                    three
                    """
                    ''',
                    @short_length ++ @opts
    end
  end

  describe "migrate: true" do
    test "enables :migrate_bitstring_modifiers" do
      assert_format "<<foo::binary()>>", "<<foo::binary>>", migrate: true
    end

    test "enables :migrate_charlists_as_sigils" do
      assert_format ~S['abc'], ~S[~c"abc"], migrate: true
    end
  end
end
