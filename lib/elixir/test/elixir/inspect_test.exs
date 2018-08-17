Code.require_file("test_helper.exs", __DIR__)

defmodule Inspect.AtomTest do
  use ExUnit.Case, async: true

  doctest Inspect

  test "basic" do
    assert inspect(:foo) == ":foo"
  end

  test "empty" do
    assert inspect(:"") == ":\"\""
  end

  test "true, false, nil" do
    assert inspect(false) == "false"
    assert inspect(true) == "true"
    assert inspect(nil) == "nil"
  end

  test "with uppercase letters" do
    assert inspect(:fOO) == ":fOO"
    assert inspect(:FOO) == ":FOO"
  end

  test "aliases" do
    assert inspect(Foo) == "Foo"
    assert inspect(Foo.Bar) == "Foo.Bar"
    assert inspect(Elixir) == "Elixir"
    assert inspect(Elixir.Foo) == "Foo"
    assert inspect(Elixir.Elixir) == "Elixir.Elixir"
    assert inspect(Elixir.Elixir.Foo) == "Elixir.Elixir.Foo"
  end

  test "with integers" do
    assert inspect(User1) == "User1"
    assert inspect(:user1) == ":user1"
  end

  test "with trailing ? or !" do
    assert inspect(:foo?) == ":foo?"
    assert inspect(:bar!) == ":bar!"
    assert inspect(:Foo?) == ":Foo?"
  end

  test "operators" do
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

  test "with @" do
    assert inspect(:@) == ":@"
    assert inspect(:foo@bar) == ":foo@bar"
    assert inspect(:foo@bar@) == ":foo@bar@"
    assert inspect(:foo@bar@baz) == ":foo@bar@baz"
  end

  test "others" do
    assert inspect(:...) == ":..."
    assert inspect(:<<>>) == ":<<>>"
    assert inspect(:{}) == ":{}"
    assert inspect(:%{}) == ":%{}"
    assert inspect(:%) == ":%"
    assert inspect(:->) == ":->"
  end

  test "escaping" do
    assert inspect(:"hy-phen") == ~s(:"hy-phen")
    assert inspect(:"@hello") == ~s(:"@hello")
    assert inspect(:"Wat!?") == ~s(:"Wat!?")
    assert inspect(:"'quotes' and \"double quotes\"") == ~S(:"'quotes' and \"double quotes\"")
  end

  test "colors" do
    opts = [syntax_colors: [atom: :red]]
    assert inspect(:hello, opts) == "\e[31m:hello\e[0m"
    opts = [syntax_colors: [reset: :cyan]]
    assert inspect(:hello, opts) == ":hello"
  end

  test "unicode" do
    assert inspect(:olá) == ":olá"
    assert inspect(:Olá) == ":Olá"
    assert inspect(:Ólá) == ":Ólá"
    assert inspect(:こんにちは世界) == ":こんにちは世界"

    nfd = :unicode.characters_to_nfd_binary("olá")
    assert inspect(String.to_atom(nfd)) == ":\"#{nfd}\""
  end
end

defmodule Inspect.BitStringTest do
  use ExUnit.Case, async: true

  test "bitstring" do
    assert inspect(<<1::12-integer-signed>>) == "<<0, 1::size(4)>>"
    assert inspect(<<1, 2, 3, 4, 5>>, pretty: true, width: 10) == "<<1, 2, 3,\n  4, 5>>"
  end

  test "binary" do
    assert inspect("foo") == "\"foo\""
    assert inspect(<<?a, ?b, ?c>>) == "\"abc\""
  end

  test "escaping" do
    assert inspect("f\no") == "\"f\\no\""
    assert inspect("f\\o") == "\"f\\\\o\""
    assert inspect("f\ao") == "\"f\\ao\""

    assert inspect("\a\b\d\e\f\n\r\s\t\v") == "\"\\a\\b\\d\\e\\f\\n\\r \\t\\v\""
  end

  test "UTF-8" do
    assert inspect(" ゆんゆん") == "\" ゆんゆん\""
    # BOM
    assert inspect("\uFEFFhello world") == "\"\\uFEFFhello world\""
  end

  test "infer" do
    assert inspect(<<"john", 193, "doe">>, binaries: :infer) ==
             ~s(<<106, 111, 104, 110, 193, 100, 111, 101>>)

    assert inspect(<<"john">>, binaries: :infer) == ~s("john")
    assert inspect(<<193>>, binaries: :infer) == ~s(<<193>>)
  end

  test "as strings" do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_strings) == ~s("john\\xC1doe")
    assert inspect(<<"john">>, binaries: :as_strings) == ~s("john")
    assert inspect(<<193>>, binaries: :as_strings) == ~s("\\xC1")
  end

  test "as binaries" do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_binaries) ==
             "<<106, 111, 104, 110, 193, 100, 111, 101>>"

    assert inspect(<<"john">>, binaries: :as_binaries) == "<<106, 111, 104, 110>>"
    assert inspect(<<193>>, binaries: :as_binaries) == "<<193>>"

    # Any base other than :decimal implies "binaries: :as_binaries"
    assert inspect("abc", base: :hex) == "<<0x61, 0x62, 0x63>>"
    assert inspect("abc", base: :octal) == "<<0o141, 0o142, 0o143>>"

    # Size is still represented as decimal
    assert inspect(<<10, 11, 12::4>>, base: :hex) == "<<0xA, 0xB, 0xC::size(4)>>"
  end

  test "unprintable with limit" do
    assert inspect(<<193, 193, 193, 193>>, limit: 3) == "<<193, 193, 193, ...>>"
  end

  test "printable limit" do
    assert inspect("hello world", printable_limit: 4) == ~s("hell" <> ...)

    # Non-printable characters after the limit don't matter
    assert inspect("hello world" <> <<0>>, printable_limit: 4) == ~s("hell" <> ...)

    # Non printable strings aren't affected by printable limit
    assert inspect(<<0, 1, 2, 3, 4>>, printable_limit: 3) == ~s(<<0, 1, 2, 3, 4>>)
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
    assert inspect(-100, base: :hex) == "-0x64"
  end

  test "octal" do
    assert inspect(100, base: :octal) == "0o144"
    assert inspect(-100, base: :octal) == "-0o144"
  end

  test "binary" do
    assert inspect(86, base: :binary) == "0b1010110"
    assert inspect(-86, base: :binary) == "-0b1010110"
  end

  test "float" do
    assert inspect(1.0) == "1.0"
    assert inspect(1.0e10) == "1.0e10"
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
    assert inspect({1, "b", 3}, pretty: true, width: 1) == "{1,\n \"b\",\n 3}"
    assert inspect({1, "b", 3}, pretty: true, width: 10) == "{1, \"b\",\n 3}"
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
    assert inspect({:x, :y}, opts) == "{:x, :y}"

    opts = [syntax_colors: [reset: :cyan, atom: :red]]
    assert inspect({}, opts) == "{}"
    assert inspect({:x, :y}, opts) == "{\e[31m:x\e[36m, \e[31m:y\e[36m}"

    opts = [syntax_colors: [tuple: :green, reset: :cyan, atom: :red]]
    assert inspect({}, opts) == "\e[32m{\e[36m\e[32m}\e[36m"

    assert inspect({:x, :y}, opts) ==
             "\e[32m{\e[36m\e[31m:x\e[36m\e[32m,\e[36m \e[31m:y\e[36m\e[32m}\e[36m"
  end
end

defmodule Inspect.ListTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert inspect([1, "b", 3]) == "[1, \"b\", 3]"
    assert inspect([1, "b", 3], pretty: true, width: 1) == "[1,\n \"b\",\n 3]"
  end

  test "printable" do
    assert inspect('abc') == "'abc'"
  end

  test "printable limit" do
    assert inspect('hello world', printable_limit: 4) == ~s('hell' ++ ...)
    # Non printable characters after the limit don't matter
    assert inspect('hello world' ++ [0], printable_limit: 4) == ~s('hell' ++ ...)
    # Non printable strings aren't affected by printable limit
    assert inspect([0, 1, 2, 3, 4], printable_limit: 3) == ~s([0, 1, 2, 3, 4])
  end

  test "keyword" do
    assert inspect(a: 1) == "[a: 1]"
    assert inspect(a: 1, b: 2) == "[a: 1, b: 2]"
    assert inspect(a: 1, a: 2, b: 2) == "[a: 1, a: 2, b: 2]"
    assert inspect("123": 1) == ~s(["123": 1])

    assert inspect([foo: [1, 2, 3], baz: [4, 5, 6]], pretty: true, width: 20) ==
             "[\n  foo: [1, 2, 3],\n  baz: [4, 5, 6]\n]"
  end

  test "opt infer" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :infer) ==
             "[106, 111, 104, 110, 0, 100, 111, 101]"

    assert inspect('john', charlists: :infer) == "'john'"
    assert inspect([0], charlists: :infer) == "[0]"
  end

  test "opt as strings" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :as_charlists) == "'john\\0doe'"
    assert inspect('john', charlists: :as_charlists) == "'john'"
    assert inspect([0], charlists: :as_charlists) == "'\\0'"
  end

  test "opt as lists" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :as_lists) ==
             "[106, 111, 104, 110, 0, 100, 111, 101]"

    assert inspect('john', charlists: :as_lists) == "[106, 111, 104, 110]"
    assert inspect([0], charlists: :as_lists) == "[0]"
  end

  test "non printable" do
    assert inspect([{:b, 1}, {:a, 1}]) == "[b: 1, a: 1]"
  end

  test "improper" do
    assert inspect([:foo | :bar]) == "[:foo | :bar]"

    assert inspect([1, 2, 3, 4, 5 | 42], pretty: true, width: 1) ==
             "[1,\n 2,\n 3,\n 4,\n 5 |\n 42]"
  end

  test "nested" do
    assert inspect(Enum.reduce(1..100, [0], &[&2, Integer.to_string(&1)]), limit: 5) ==
             "[[[[[[...], ...], \"97\"], \"98\"], \"99\"], \"100\"]"

    assert inspect(Enum.reduce(1..100, [0], &[&2 | Integer.to_string(&1)]), limit: 5) ==
             "[[[[[[...] | \"96\"] | \"97\"] | \"98\"] | \"99\"] | \"100\"]"
  end

  test "codepoints" do
    assert inspect('é') == "[233]"
  end

  test "empty" do
    assert inspect([]) == "[]"
  end

  test "with limit" do
    assert inspect([1, 2, 3, 4], limit: 3) == "[1, 2, 3, ...]"
  end

  test "colors" do
    opts = [syntax_colors: []]
    assert inspect([], opts) == "[]"

    opts = [syntax_colors: [reset: :cyan]]
    assert inspect([], opts) == "[]"
    assert inspect([:x, :y], opts) == "[:x, :y]"

    opts = [syntax_colors: [reset: :cyan, atom: :red]]
    assert inspect([], opts) == "[]"
    assert inspect([:x, :y], opts) == "[\e[31m:x\e[36m, \e[31m:y\e[36m]"

    opts = [syntax_colors: [reset: :cyan, atom: :red, list: :green]]
    assert inspect([], opts) == "\e[32m[]\e[36m"

    assert inspect([:x, :y], opts) ==
             "\e[32m[\e[36m\e[31m:x\e[36m\e[32m,\e[36m \e[31m:y\e[36m\e[32m]\e[36m"
  end

  test "keyword with colors" do
    opts = [syntax_colors: [reset: :cyan, list: :green, number: :blue]]
    assert inspect([], opts) == "\e[32m[]\e[36m"

    assert inspect([a: 9999], opts) == "\e[32m[\e[36ma: \e[34m9999\e[36m\e[32m]\e[36m"

    opts = [syntax_colors: [reset: :cyan, atom: :red, list: :green, number: :blue]]
    assert inspect([], opts) == "\e[32m[]\e[36m"

    assert inspect([a: 9999], opts) == "\e[32m[\e[36m\e[31ma:\e[36m \e[34m9999\e[36m\e[32m]\e[36m"
  end

  test "limit with colors" do
    opts = [limit: 1, syntax_colors: [reset: :cyan, list: :green, atom: :red]]
    assert inspect([], opts) == "\e[32m[]\e[36m"

    assert inspect([:x, :y], opts) == "\e[32m[\e[36m\e[31m:x\e[36m\e[32m,\e[36m ...\e[32m]\e[36m"
  end
end

defmodule Inspect.MapTest do
  use ExUnit.Case

  test "basic" do
    assert inspect(%{1 => "b"}) == "%{1 => \"b\"}"

    assert inspect(%{1 => "b", 2 => "c"}, pretty: true, width: 1) ==
             "%{\n  1 => \"b\",\n  2 => \"c\"\n}"
  end

  test "keyword" do
    assert inspect(%{a: 1}) == "%{a: 1}"
    assert inspect(%{a: 1, b: 2}) == "%{a: 1, b: 2}"
    assert inspect(%{a: 1, b: 2, c: 3}) == "%{a: 1, b: 2, c: 3}"
  end

  test "with limit" do
    assert inspect(%{1 => 1, 2 => 2, 3 => 3, 4 => 4}, limit: 3) ==
             "%{1 => 1, 2 => 2, 3 => 3, ...}"
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
    assert inspect(%{__struct__: Private, key: 1}) ==
             "%{__struct__: Inspect.MapTest.Private, key: 1}"
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
    msg =
      "got KeyError with message \"key :unknown not found in: " <>
        "%{__struct__: Inspect.MapTest.Failing, key: 0}\" while " <>
        "inspecting %{__struct__: Inspect.MapTest.Failing, key: 0}"

    try do
      inspect(%Failing{}, safe: false)
    rescue
      e in Inspect.Error ->
        assert Exception.message(e) =~ msg
        assert [{Inspect.Inspect.MapTest.Failing, :inspect, 2, _} | _] = __STACKTRACE__
    else
      _ -> flunk("expected failure")
    end
  end

  test "bad implementation safe" do
    msg =
      "got KeyError with message \"key :unknown not found in: " <>
        "%{__struct__: Inspect.MapTest.Failing, key: 0}\" while " <>
        "inspecting %{__struct__: Inspect.MapTest.Failing, key: 0}"

    assert inspect(%Failing{}) == inspect(%Inspect.Error{message: "#{msg}"})
  end

  test "bad implementation safe disables colors" do
    msg =
      "got KeyError with message \\\"key :unknown not found in: " <>
        "%{__struct__: Inspect.MapTest.Failing, key: 0}\\\" while " <>
        "inspecting %{__struct__: Inspect.MapTest.Failing, key: 0}"

    assert inspect(%Failing{}, syntax_colors: [atom: [:green]]) =~ msg
  end

  test "exception" do
    assert inspect(%RuntimeError{message: "runtime error"}) ==
             "%RuntimeError{message: \"runtime error\"}"
  end

  test "colors" do
    opts = [syntax_colors: [reset: :cyan, atom: :red, number: :magenta]]
    assert inspect(%{1 => 2}, opts) == "%{\e[35m1\e[36m => \e[35m2\e[36m}"

    assert inspect(%{a: 1}, opts) == "%{\e[31ma:\e[36m \e[35m1\e[36m}"

    assert inspect(%Public{key: 1}, opts) ==
             "%Inspect.MapTest.Public{\e[31mkey:\e[36m \e[35m1\e[36m}"

    opts = [syntax_colors: [reset: :cyan, atom: :red, map: :green, number: :blue]]

    assert inspect(%{a: 9999}, opts) ==
             "\e[32m%{\e[36m" <> "\e[31ma:\e[36m " <> "\e[34m9999\e[36m" <> "\e[32m}\e[36m"
  end

  defmodule StructWithoutOptions do
    @derive Inspect
    defstruct [:a, :b, :c, :d]
  end

  test "struct without options" do
    struct = %StructWithoutOptions{a: 1, b: 2, c: 3, d: 4}
    assert inspect(struct) == "%Inspect.MapTest.StructWithoutOptions{a: 1, b: 2, c: 3, d: 4}"

    assert inspect(struct, pretty: true, width: 1) ==
             "%Inspect.MapTest.StructWithoutOptions{\n  a: 1,\n  b: 2,\n  c: 3,\n  d: 4\n}"
  end

  defmodule StructWithOnly do
    @derive {Inspect, only: [:b, :c]}
    defstruct [:a, :b, :c, :d]
  end

  test "struct with :only option" do
    struct = %StructWithOnly{a: 1, b: 2, c: 3, d: 4}
    assert inspect(struct) == "#Inspect.MapTest.StructWithOnly<b: 2, c: 3, ...>"

    assert inspect(struct, pretty: true, width: 1) ==
             "#Inspect.MapTest.StructWithOnly<\n  b: 2,\n  c: 3,\n  ...\n>"
  end

  defmodule StructWithExcept do
    @derive {Inspect, except: [:b, :c]}
    defstruct [:a, :b, :c, :d]
  end

  test "struct with :except option" do
    struct = %StructWithExcept{a: 1, b: 2, c: 3, d: 4}
    assert inspect(struct) == "#Inspect.MapTest.StructWithExcept<a: 1, d: 4, ...>"

    assert inspect(struct, pretty: true, width: 1) ==
             "#Inspect.MapTest.StructWithExcept<\n  a: 1,\n  d: 4,\n  ...\n>"
  end

  defmodule StructWithBothOnlyAndExcept do
    @derive {Inspect, only: [:a, :b], except: [:b, :c]}
    defstruct [:a, :b, :c, :d]
  end

  test "struct with both :only and :except options" do
    struct = %StructWithBothOnlyAndExcept{a: 1, b: 2, c: 3, d: 4}
    assert inspect(struct) == "#Inspect.MapTest.StructWithBothOnlyAndExcept<a: 1, ...>"

    assert inspect(struct, pretty: true, width: 1) ==
             "#Inspect.MapTest.StructWithBothOnlyAndExcept<\n  a: 1,\n  ...\n>"
  end
end

defmodule Inspect.OthersTest do
  use ExUnit.Case, async: true

  def fun() do
    fn -> :ok end
  end

  def unquote(:"weirdly named/fun-")() do
    fn -> :ok end
  end

  test "external Elixir funs" do
    bin = inspect(&Enum.map/2)
    assert bin == "&Enum.map/2"

    assert inspect(&__MODULE__."weirdly named/fun-"/0) ==
             ~s(&Inspect.OthersTest."weirdly named/fun-"/0)
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

    Application.put_env(:elixir, :anony, V.fun())
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
    assert "#Function<" <> _ = inspect(fn x -> x + 1 end)
    assert "#Function<" <> _ = inspect(fun())
    opts = [syntax_colors: []]
    assert "#Function<" <> _ = inspect(fun(), opts)
    opts = [syntax_colors: [reset: :red]]
    assert "#Function<" <> rest = inspect(fun(), opts)
    assert String.ends_with?(rest, ">")

    inspected = inspect(__MODULE__."weirdly named/fun-"())
    assert inspected =~ ~r(#Function<\d+\.\d+/0 in Inspect\.OthersTest\."weirdly named/fun-"/0>)
  end

  test "map set" do
    assert "#MapSet<" <> _ = inspect(MapSet.new())
  end

  test "PIDs" do
    assert "#PID<" <> _ = inspect(self())
    opts = [syntax_colors: []]
    assert "#PID<" <> _ = inspect(self(), opts)
    opts = [syntax_colors: [reset: :cyan]]
    assert "#PID<" <> rest = inspect(self(), opts)
    assert String.ends_with?(rest, ">")
  end

  test "references" do
    assert "#Reference<" <> _ = inspect(make_ref())
  end

  test "regex" do
    assert inspect(~r(foo)m) == "~r/foo/m"

    assert inspect(Regex.compile!("\a\b\d\e\f\n\r\s\t\v/")) ==
             "~r/\\a\\x08\\x7F\\x1B\\f\\n\\r \\t\\v\\//"

    assert inspect(~r<\a\b\d\e\f\n\r\s\t\v/>) == "~r/\\a\\b\\d\\e\\f\\n\\r\\s\\t\\v\\//"
    assert inspect(~r" \\/ ") == "~r/ \\\\\\/ /"
    assert inspect(~r/hi/, syntax_colors: [regex: :red]) == "\e[31m~r/hi/\e[0m"
  end
end
