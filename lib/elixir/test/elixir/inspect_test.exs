Code.require_file "test_helper.exs", __DIR__

defmodule Inspect.AtomTest do
  use ExUnit.Case, async: true

  doctest Inspect

  test "basic" do
    assert inspect(:foo) == ":foo"
  end

  test "empty" do
    assert inspect(:"") == ":\"\""
  end

  test "true false nil" do
    assert inspect(false) == "false"
    assert inspect(true) == "true"
    assert inspect(nil) == "nil"
  end

  test "with uppercase" do
    assert inspect(:fOO) == ":fOO"
    assert inspect(:FOO) == ":FOO"
  end

  test "alias atom" do
    assert inspect(Foo) == "Foo"
    assert inspect(Foo.Bar) == "Foo.Bar"
    assert inspect(Elixir) == "Elixir"
    assert inspect(Elixir.Elixir) == "Elixir.Elixir"
  end

  test "with integers" do
    assert inspect(User1)  == "User1"
    assert inspect(:user1) == ":user1"
  end

  test "with punctuation" do
    assert inspect(:foo?) == ":foo?"
    assert inspect(:bar!) == ":bar!"
  end

  test "op" do
    assert inspect(:+) == ":+"
    assert inspect(:<~) == ":<~"
    assert inspect(:~>) == ":~>"
    assert inspect(:&&&) == ":&&&"
    assert inspect(:~~~) == ":~~~"
    assert inspect(:<<~) == ":<<~"
    assert inspect(:~>>) == ":~>>"
    assert inspect(:<~>) == ":<~>"
    assert inspect(:<|>) == ":<|>"
  end

  test :... do
    assert inspect(:...) == ":..."
  end

  test :@ do
    assert inspect(:@) == ":@"
    assert inspect(:foo@bar) == ":foo@bar"
    assert inspect(:foo@bar@) == ":foo@bar@"
    assert inspect(:foo@bar@baz) == ":foo@bar@baz"
  end

  test "others" do
    assert inspect(:<<>>) == ":<<>>"
    assert inspect(:{})   == ":{}"
    assert inspect(:%{})  == ":%{}"
    assert inspect(:%)    == ":%"
  end

  test "colors" do
    opts = [syntax_colors: [atom: :red]]
    assert inspect(:hello, opts) == "\e[31m:hello\e[0m"
    opts = [syntax_colors: [reset: :cyan]]
    assert inspect(:hello, opts) == ":hello"
  end
end

defmodule Inspect.BitStringTest do
  use ExUnit.Case, async: true

  test "bitstring" do
    assert inspect(<<1 :: size(12)-integer-signed>>) == "<<0, 1::size(4)>>"
  end

  test "binary" do
    assert inspect("foo") == "\"foo\""
    assert inspect(<<?a, ?b, ?c>>) == "\"abc\""
  end

  test "escape" do
    assert inspect("f\no") == "\"f\\no\""
    assert inspect("f\\o") == "\"f\\\\o\""
    assert inspect("f\ao") == "\"f\\ao\""
  end

  test "UTF-8" do
    assert inspect(" ゆんゆん") == "\" ゆんゆん\""
  end

  test "all escapes" do
    assert inspect("\a\b\d\e\f\n\r\s\t\v") ==
           "\"\\a\\b\\d\\e\\f\\n\\r \\t\\v\""
  end

  test "opt infer" do
    assert inspect(<<"john", 193, "doe">>, binaries: :infer) == ~s(<<106, 111, 104, 110, 193, 100, 111, 101>>)
    assert inspect(<<"john">>, binaries: :infer) == ~s("john")
    assert inspect(<<193>>, binaries: :infer) == ~s(<<193>>)
  end

  test "opt as strings" do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_strings) == ~s("john\\xC1doe")
    assert inspect(<<"john">>, binaries: :as_strings) == ~s("john")
    assert inspect(<<193>>, binaries: :as_strings) == ~s("\\xC1")
  end

  test "opt as binaries" do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_binaries) == "<<106, 111, 104, 110, 193, 100, 111, 101>>"
    assert inspect(<<"john">>, binaries: :as_binaries) == "<<106, 111, 104, 110>>"
    assert inspect(<<193>>, binaries: :as_binaries) == "<<193>>"
    # base: :hex is recognized
    assert inspect("abc", binaries: :as_binary, base: :hex) == "<<0x61, 0x62, 0x63>>"
    # any base other than :decimal implies binaries: :as_binaries
    assert inspect("abc", base: :hex) == "<<0x61, 0x62, 0x63>>"
    assert inspect("abc", base: :octal) == "<<0o141, 0o142, 0o143>>"
    # size is still represented as decimal
    assert inspect(<<10, 11, 12::4>>, base: :hex) == "<<0xA, 0xB, 0xC::size(4)>>"
  end

  test "unprintable with opts" do
    assert inspect(<<193, 193, 193, 193>>, limit: 3) == "<<193, 193, 193, ...>>"
  end
end

defmodule Inspect.NumberTest do
  use ExUnit.Case, async: true

  test "integer" do
    assert inspect(100) == "100"
  end

  test "decimal" do
    assert inspect(100, base: :decimal) == "100"
  end

  test "hex" do
    assert inspect(100, base: :hex) == "0x64"
  end

  test "octal" do
    assert inspect(100, base: :octal) == "0o144"
  end

  test "binary" do
    assert inspect(86, base: :binary) == "0b1010110"
  end

  test "float" do
    assert inspect(1.0) == "1.0"
    assert inspect(1.0E10) == "1.0e10"
    assert inspect(1.0e10) == "1.0e10"
    assert inspect(1.0e-10) == "1.0e-10"
  end

  test "integer colors" do
    opts = [syntax_colors: [number: :red]]
    assert inspect(123, opts) == "\e[31m123\e[0m"
    opts = [syntax_colors: [reset: :cyan]]
    assert inspect(123, opts) == "123"
  end

  test "float colors" do
    opts = [syntax_colors: [number: :red]]
    assert inspect(1.3, opts) == "\e[31m1.3\e[0m"
    opts = [syntax_colors: [reset: :cyan]]
    assert inspect(1.3, opts) == "1.3"
  end
end

defmodule Inspect.TupleTest do
  use ExUnit.Case

  test "basic" do
    assert inspect({1, "b", 3}) == "{1, \"b\", 3}"
    assert inspect({1, "b", 3}, [pretty: true, width: 1]) == "{1,\n \"b\",\n 3}"
  end

  test "empty" do
    assert inspect({}) == "{}"
  end

  test "with limit" do
    assert inspect({1, 2, 3, 4}, limit: 3) == "{1, 2, 3, ...}"
  end

  test "colors" do
    opts = [syntax_colors: []]
    assert inspect({}, opts) == "{}"

    opts = [syntax_colors: [reset: :cyan]]
    assert inspect({}, opts) == "{}"
    assert inspect({:X, :Y}, opts) == "{:X, :Y}"

    opts = [syntax_colors: [reset: :cyan, atom: :red]]
    assert inspect({}, opts) == "{}"
    assert inspect({:X, :Y}, opts) ==
      "{\e[31m:X\e[36m, \e[31m:Y\e[36m}"

    opts = [syntax_colors: [tuple: :green, reset: :cyan, atom: :red]]
    assert inspect({}, opts) == "\e[32m{\e[36m\e[32m}\e[36m"
    assert inspect({:X, :Y}, opts) ==
      "\e[32m{\e[36m" <>
      "\e[31m:X\e[36m" <>
      "\e[32m,\e[36m " <>
      "\e[31m:Y\e[36m" <>
      "\e[32m}\e[36m"
  end
end

defmodule Inspect.ListTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert inspect([ 1, "b", 3 ]) == "[1, \"b\", 3]"
    assert inspect([ 1, "b", 3 ], [pretty: true, width: 1]) == "[1,\n \"b\",\n 3]"
  end

  test "printable" do
    assert inspect('abc') == "'abc'"
  end

  test "keyword" do
    assert inspect([a: 1]) == "[a: 1]"
    assert inspect([a: 1, b: 2]) == "[a: 1, b: 2]"
    assert inspect([a: 1, a: 2, b: 2]) == "[a: 1, a: 2, b: 2]"
    assert inspect(["123": 1]) == ~s(["123": 1])

    assert inspect([foo: [1, 2, 3, :bar], bazzz: :bat], [pretty: true, width: 30]) ==
           "[foo: [1, 2, 3, :bar],\n bazzz: :bat]"
  end

  test "opt infer" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :infer) == "[106, 111, 104, 110, 0, 100, 111, 101]"
    assert inspect('john', charlists: :infer) == "'john'"
    assert inspect([0], charlists: :infer) == "[0]"
  end

  test "opt as strings" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :as_charlists) == "'john\\0doe'"
    assert inspect('john', charlists: :as_charlists) == "'john'"
    assert inspect([0], charlists: :as_charlists) == "'\\0'"
  end

  test "opt as lists" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :as_lists) == "[106, 111, 104, 110, 0, 100, 111, 101]"
    assert inspect('john', charlists: :as_lists) == "[106, 111, 104, 110]"
    assert inspect([0], charlists: :as_lists) == "[0]"
  end

  test "non printable" do
    assert inspect([{:b, 1}, {:a, 1}]) == "[b: 1, a: 1]"
  end

  test "improper" do
    assert inspect([:foo | :bar]) == "[:foo | :bar]"

    assert inspect([1, 2, 3, 4, 5 | 42], [pretty: true, width: 1]) == "[1,\n 2,\n 3,\n 4,\n 5 |\n 42]"
  end

  test "nested" do
    assert inspect(Enum.reduce(1..100, [0], &[&2, Integer.to_string(&1)]), [limit: 5]) ==
           "[[[[[[...], ...], \"97\"], \"98\"], \"99\"], \"100\"]"
    assert inspect(Enum.reduce(1..100, [0], &[&2 | Integer.to_string(&1)]), [limit: 5]) ==
           "[[[[[[...] | \"96\"] | \"97\"] | \"98\"] | \"99\"] | \"100\"]"
  end

  test "codepoints" do
    assert inspect('é') == "[233]"
  end

  test "empty" do
    assert inspect([]) == "[]"
  end

  test "with limit" do
    assert inspect([ 1, 2, 3, 4 ], limit: 3) == "[1, 2, 3, ...]"
  end

  test "colors" do
    opts = [syntax_colors: []]
    assert inspect([], opts) == "[]"

    opts = [syntax_colors: [reset: :cyan]]
    assert inspect([], opts) == "[]"
    assert inspect([:X, :Y], opts) ==
      "[:X, :Y]"

    opts = [syntax_colors: [reset: :cyan, atom: :red]]
    assert inspect([], opts) == "[]"
    assert inspect([:X, :Y], opts) ==
      "[\e[31m:X\e[36m, \e[31m:Y\e[36m]"

    opts = [syntax_colors: [reset: :cyan, atom: :red, list: :green]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([:X, :Y], opts) ==
      "\e[32m[\e[36m" <>
      "\e[31m:X\e[36m" <>
      "\e[32m,\e[36m " <>
      "\e[31m:Y\e[36m" <>
      "\e[32m]\e[36m"
  end

  test "keyword with colors" do
    opts = [syntax_colors: [reset: :cyan, list: :green, number: :blue]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([a: 9999], opts) ==
      "\e[32m[\e[36m" <>
      "a: " <>
      "\e[34m9999\e[36m" <>
      "\e[32m]\e[36m"

    opts = [syntax_colors: [reset: :cyan, atom: :red, list: :green, number: :blue]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([a: 9999], opts) ==
      "\e[32m[\e[36m" <>
      "\e[31ma: \e[36m" <>
      "\e[34m9999\e[36m" <>
      "\e[32m]\e[36m"
  end

  test "limit with colors" do
    opts = [limit: 1, syntax_colors: [reset: :cyan, list: :green, atom: :red]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([:X, :Y], opts) ==
      "\e[32m[\e[36m" <>
      "\e[31m:X\e[36m" <>
      "\e[32m,\e[36m " <>
      "..." <>
      "\e[32m]\e[36m"
  end
end

defmodule Inspect.MapTest do
  use ExUnit.Case

  test "basic" do
    assert inspect(%{1 => "b"}) == "%{1 => \"b\"}"
    assert inspect(%{1 => "b", 2 => "c"}, [pretty: true, width: 1]) == "%{1 => \"b\",\n  2 => \"c\"}"
  end

  test "keyword" do
    assert inspect(%{a: 1}) == "%{a: 1}"
    assert inspect(%{a: 1, b: 2}) == "%{a: 1, b: 2}"
    assert inspect(%{a: 1, b: 2, c: 3}) == "%{a: 1, b: 2, c: 3}"
  end

  test "with limit" do
    assert inspect(%{1 => 1, 2 => 2, 3 => 3, 4 => 4}, limit: 3) == "%{1 => 1, 2 => 2, 3 => 3, ...}"
  end

  defmodule Public do
    defstruct key: 0
  end

  defmodule Private do
  end

  test "public struct" do
    assert inspect(%Public{key: 1}) == "%Inspect.MapTest.Public{key: 1}"
  end

  test "public modified struct" do
    public = %Public{key: 1}
    assert inspect(Map.put(public, :foo, :bar)) ==
           "%{__struct__: Inspect.MapTest.Public, foo: :bar, key: 1}"
  end

  test "private struct" do
    assert inspect(%{__struct__: Private, key: 1}) == "%{__struct__: Inspect.MapTest.Private, key: 1}"
  end

  defmodule Failing do
    defstruct key: 0

    defimpl Inspect do
      def inspect(struct, _) do
        struct.unknown
      end
    end
  end

  test "bad implementation unsafe" do
    msg = "got KeyError with message \"key :unknown not found in: " <>
          "%{__struct__: Inspect.MapTest.Failing, key: 0}\" while " <>
          "inspecting %{__struct__: Inspect.MapTest.Failing, key: 0}"

    assert_raise Inspect.Error, msg, fn ->
      inspect(%Failing{}, safe: false)
    end

    assert [{Inspect.Inspect.MapTest.Failing, :inspect, 2, _} | _] = System.stacktrace
  end

  test "bad implementation safe" do
    msg = "got KeyError with message \"key :unknown not found in: " <>
          "%{__struct__: Inspect.MapTest.Failing, key: 0}\" while " <>
          "inspecting %{__struct__: Inspect.MapTest.Failing, key: 0}"

    assert inspect(%Failing{}) ==
           inspect(%Inspect.Error{message: "#{msg}"})
  end

  test "exception" do
    assert inspect(%RuntimeError{message: "runtime error"}) ==
           "%RuntimeError{message: \"runtime error\"}"
  end

  test "colors" do
    opts = [syntax_colors: [reset: :cyan, atom: :red, number: :magenta]]
    assert inspect(%{1 => 2}, opts) ==
      "%{\e[35m1\e[36m => \e[35m2\e[36m}"

    assert inspect(%{a: 1}, opts) ==
      "%{\e[31ma: \e[36m\e[35m1\e[36m}"

    assert inspect(%Public{key: 1}, opts) ==
      "%Inspect.MapTest.Public{\e[31mkey: \e[36m\e[35m1\e[36m}"

    opts = [syntax_colors: [reset: :cyan, atom: :red, map: :green, number: :blue]]
    assert inspect(%{a: 9999}, opts) ==
      "\e[32m%{\e[36m" <>
      "\e[31ma: \e[36m" <>
      "\e[34m9999\e[36m" <>
      "\e[32m}\e[36m"
  end
end

defmodule Inspect.OthersTest do
  use ExUnit.Case, async: true

  def fun() do
    fn() -> :ok end
  end

  test "external elixir funs" do
    bin = inspect(&Enum.map/2)
    assert bin == "&Enum.map/2"
  end

  test "external Erlang funs" do
    bin = inspect(&:lists.map/2)
    assert bin == "&:lists.map/2"
  end

  test "outdated functions" do
    defmodule V do
      def fun do
        fn -> 1 end
      end
    end

    Application.put_env(:elixir, :anony, V.fun)
    Application.put_env(:elixir, :named, &V.fun/0)

    :code.delete(V)
    :code.purge(V)

    anony = Application.get_env(:elixir, :anony)
    named = Application.get_env(:elixir, :named)

    assert inspect(anony) =~ ~r"#Function<0.\d+/0 in Inspect.OthersTest.V>"
    assert inspect(named) =~ ~r"&Inspect.OthersTest.V.fun/0"
  after
    Application.delete_env(:elixir, :anony)
    Application.delete_env(:elixir, :named)
  end

  test "other funs" do
    assert "#Function<" <> _ = inspect(fn(x) -> x + 1 end)
    assert "#Function<" <> _ = inspect(fun())
    opts = [syntax_colors: []]
    assert "#Function<" <> _ = inspect(fun(), opts)
    opts = [syntax_colors: [reset: :red]]
    assert "#Function<" <> _ = inspect(fun(), opts)
    assert String.ends_with?(inspect(fun(), opts), ">")
  end

  test "map set" do
    assert "#MapSet<" <> _ = inspect(MapSet.new)
  end

  test "PIDs" do
    assert "#PID<" <> _ = inspect(self())
    opts = [syntax_colors: []]
    assert "#PID<" <> _ = inspect(self(), opts)
    opts = [syntax_colors: [reset: :cyan]]
    assert "#PID" <> _ = inspect(self(), opts)
    assert String.ends_with?(inspect(self(), opts), ">")
  end

  test "references" do
    assert "#Reference<" <> _ = inspect(make_ref())
  end

  test "regex" do
    assert inspect(~r(foo)m) == "~r/foo/m"
    assert inspect(Regex.compile!("\a\b\d\e\f\n\r\s\t\v/")) ==
      "~r/\\a\\x08\\x7F\\x1B\\f\\n\\r \\t\\v\\//"
    assert inspect(~r<\a\b\d\e\f\n\r\s\t\v/>) ==
      "~r/\\a\\b\\d\\e\\f\\n\\r\\s\\t\\v\\//"
    opts = [syntax_colors: [regex: :red]]
    assert inspect(~r/hi/, opts) ==
      "\e[31m~r/hi/\e[0m"
  end
end
