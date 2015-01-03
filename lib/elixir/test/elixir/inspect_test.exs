Code.require_file "test_helper.exs", __DIR__

defmodule Inspect.AtomTest do
  use ExUnit.Case, async: true

  test :basic do
    assert inspect(:foo) == ":foo"
  end

  test :empty do
    assert inspect(:"") == ":\"\""
  end

  test :true_false_nil do
    assert inspect(false) == "false"
    assert inspect(true) == "true"
    assert inspect(nil) == "nil"
  end

  test :with_uppercase do
    assert inspect(:fOO) == ":fOO"
    assert inspect(:FOO) == ":FOO"
  end

  test :alias_atom do
    assert inspect(Foo) == "Foo"
    assert inspect(Foo.Bar) == "Foo.Bar"
    assert inspect(Elixir) == "Elixir"
    assert inspect(Elixir.Elixir) == "Elixir.Elixir"
  end

  test :with_integers do
    assert inspect(User1)  == "User1"
    assert inspect(:user1) == ":user1"
  end

  test :with_punctuation do
    assert inspect(:foo?) == ":foo?"
    assert inspect(:bar!) == ":bar!"
  end

  test :op do
    assert inspect(:+)   == ":+"
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

  test :others do
    assert inspect(:<<>>) == ":<<>>"
    assert inspect(:{})   == ":{}"
    assert inspect(:%{})  == ":%{}"
    assert inspect(:%)    == ":%"
  end
end

defmodule Inspect.BitStringTest do
  use ExUnit.Case, async: true

  test :bitstring do
    assert inspect(<<1 :: size(12)-integer-signed>>) == "<<0, 1::size(4)>>"
  end

  test :binary do
    assert inspect("foo") == "\"foo\""
    assert inspect(<<?a, ?b, ?c>>) == "\"abc\""
  end

  test :escape do
    assert inspect("f\no") == "\"f\\no\""
    assert inspect("f\\o") == "\"f\\\\o\""
    assert inspect("f\ao") == "\"f\\ao\""
  end

  test :utf8 do
    assert inspect(" ゆんゆん") == "\" ゆんゆん\""
  end

  test :all_escapes do
    assert inspect("\a\b\d\e\f\n\r\s\t\v") ==
           "\"\\a\\b\\d\\e\\f\\n\\r \\t\\v\""
  end

  test :opt_infer do
    assert inspect(<<"john", 193, "doe">>, binaries: :infer) == ~s(<<106, 111, 104, 110, 193, 100, 111, 101>>)
    assert inspect(<<"john">>, binaries: :infer) == ~s("john")
    assert inspect(<<193>>, binaries: :infer) == ~s(<<193>>)
  end

  test :opt_as_strings do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_strings) == ~s("john\\xC1doe")
    assert inspect(<<"john">>, binaries: :as_strings) == ~s("john")
    assert inspect(<<193>>, binaries: :as_strings) == ~s("\\xC1")
  end

  test :opt_as_binaries do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_binaries) == "<<106, 111, 104, 110, 193, 100, 111, 101>>"
    assert inspect(<<"john">>, binaries: :as_binaries) == "<<106, 111, 104, 110>>"
    assert inspect(<<193>>, binaries: :as_binaries) == "<<193>>"
  end

  test :unprintable_with_opts do
    assert inspect(<<193, 193, 193, 193>>, limit: 3) == "<<193, 193, 193, ...>>"
  end
end

defmodule Inspect.NumberTest do
  use ExUnit.Case, async: true

  test :integer do
    assert inspect(100) == "100"
  end

  test :decimal do
    assert inspect(100, base: :decimal) == "100"
  end

  test :hex do
    assert inspect(100, base: :hex) == "0x64"
  end

  test :octal do
    assert inspect(100, base: :octal) == "0o144"
  end

  test :binary do
    assert inspect(86, base: :binary) == "0b1010110"
  end

  test :float do
    assert inspect(1.0) == "1.0"
    assert inspect(1.0E10) == "1.0e10"
    assert inspect(1.0e10) == "1.0e10"
    assert inspect(1.0e-10) == "1.0e-10"
  end
end

defmodule Inspect.TupleTest do
  use ExUnit.Case

  test :basic do
    assert inspect({1, "b", 3}) == "{1, \"b\", 3}"
    assert inspect({1, "b", 3}, [pretty: true, width: 1]) == "{1,\n \"b\",\n 3}"
  end

  test :empty do
    assert inspect({}) == "{}"
  end

  test :with_limit do
    assert inspect({1, 2, 3, 4}, limit: 3) == "{1, 2, 3, ...}"
  end
end

defmodule Inspect.ListTest do
  use ExUnit.Case, async: true

  test :basic do
    assert inspect([ 1, "b", 3 ]) == "[1, \"b\", 3]"
    assert inspect([ 1, "b", 3 ], [pretty: true, width: 1]) == "[1,\n \"b\",\n 3]"
  end

  test :printable do
    assert inspect('abc') == "'abc'"
  end

  test :keyword do
    assert inspect([a: 1]) == "[a: 1]"
    assert inspect([a: 1, b: 2]) == "[a: 1, b: 2]"
    assert inspect([a: 1, a: 2, b: 2]) == "[a: 1, a: 2, b: 2]"
    assert inspect(["123": 1]) == ~s(["123": 1])

    assert inspect([foo: [1,2,3,:bar], bazzz: :bat], [pretty: true, width: 30]) ==
           "[foo: [1, 2, 3, :bar],\n bazzz: :bat]"
  end

  test :opt_infer do
    assert inspect('john' ++ [0] ++ 'doe', char_lists: :infer) == "[106, 111, 104, 110, 0, 100, 111, 101]"
    assert inspect('john', char_lists: :infer) == "'john'"
    assert inspect([0], char_lists: :infer) == "[0]"
  end

  test :opt_as_strings do
    assert inspect('john' ++ [0] ++ 'doe', char_lists: :as_char_lists) == "'john\\0doe'"
    assert inspect('john', char_lists: :as_char_lists) == "'john'"
    assert inspect([0], char_lists: :as_char_lists) == "'\\0'"
  end

  test :opt_as_lists do
    assert inspect('john' ++ [0] ++ 'doe', char_lists: :as_lists) == "[106, 111, 104, 110, 0, 100, 111, 101]"
    assert inspect('john', char_lists: :as_lists) == "[106, 111, 104, 110]"
    assert inspect([0], char_lists: :as_lists) == "[0]"
  end

  test :non_printable do
    assert inspect([{:b, 1}, {:a, 1}]) == "[b: 1, a: 1]"
  end

  test :improper do
    assert inspect([:foo | :bar]) == "[:foo | :bar]"

    assert inspect([1,2,3,4,5|42], [pretty: true, width: 1]) == "[1,\n 2,\n 3,\n 4,\n 5 |\n 42]"
  end

  test :nested do
    assert inspect(Enum.reduce(1..100, [0], &[&2 , Integer.to_string(&1)]), [limit: 5]) ==
           "[[[[[[...], ...], \"97\"], \"98\"], \"99\"], \"100\"]"
    assert inspect(Enum.reduce(1..100, [0], &[&2 | Integer.to_string(&1)]), [limit: 5]) ==
           "[[[[[[...] | \"96\"] | \"97\"] | \"98\"] | \"99\"] | \"100\"]"
  end

  test :codepoints do
    assert inspect('é') == "[233]"
  end

  test :empty do
    assert inspect([]) == "[]"
  end

  test :with_limit do
    assert inspect([ 1, 2, 3, 4 ], limit: 3) == "[1, 2, 3, ...]"
  end
end

defmodule Inspect.MapTest do
  use ExUnit.Case

  test :basic do
    assert inspect(%{1 => "b"}) == "%{1 => \"b\"}"
    assert inspect(%{1 => "b", 2 => "c"}, [pretty: true, width: 1]) == "%{1 => \"b\",\n  2 => \"c\"}"
  end

  test :keyword do
    assert inspect(%{a: 1}) == "%{a: 1}"
    assert inspect(%{a: 1, b: 2}) == "%{a: 1, b: 2}"
    assert inspect(%{a: 1, b: 2, c: 3}) == "%{a: 1, b: 2, c: 3}"
  end

  test :with_limit do
    assert inspect(%{1 => 1, 2 => 2, 3 => 3, 4 => 4}, limit: 3) == "%{1 => 1, 2 => 2, 3 => 3, ...}"
  end

  defmodule Public do
    def __struct__ do
      %{key: 0, __struct__: Public}
    end
  end

  defmodule Private do
  end

  test :public_struct do
    assert inspect(%Public{key: 1}) == "%Inspect.MapTest.Public{key: 1}"
  end

  test :public_modified_struct do
    public = %Public{key: 1}
    assert inspect(Map.put(public, :foo, :bar)) ==
           "%{__struct__: Inspect.MapTest.Public, foo: :bar, key: 1}"
  end

  test :private_struct do
    assert inspect(%{__struct__: Private, key: 1}) == "%{__struct__: Inspect.MapTest.Private, key: 1}"
  end

  defmodule Failing do
    def __struct__ do
      %{key: 0}
    end

    defimpl Inspect do
      def inspect(struct, _) do
        struct.unknown
      end
    end
  end

  test :bad_implementation do
    msg = "Got KeyError with message \"key :unknown not found in: " <>
          "%{__struct__: Inspect.MapTest.Failing, key: 0}\" while " <>
          "inspecting %{__struct__: Inspect.MapTest.Failing, key: 0}"

    assert_raise ArgumentError, msg, fn ->
      inspect(%Failing{})
    end

    assert [{Inspect.Inspect.MapTest.Failing, :inspect, 2, _}|_] = System.stacktrace
  end

  test :exception do
    assert inspect(%RuntimeError{message: "runtime error"}) ==
           "%RuntimeError{message: \"runtime error\"}"
  end
end

defmodule Inspect.OthersTest do
  use ExUnit.Case, async: true

  def f do
    fn() -> :ok end
  end

  test :external_elixir_funs do
    bin = inspect(&Enum.map/2)
    assert bin == "&Enum.map/2"
  end

  test :external_erlang_funs do
    bin = inspect(&:lists.map/2)
    assert bin == "&:lists.map/2"
  end

  test :outdated_functions do
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

  test :other_funs do
    assert "#Function<" <> _ = inspect(fn(x) -> x + 1 end)
    assert "#Function<" <> _ = inspect(f)
  end

  test :hash_dict_set do
    assert "#HashDict<" <> _ = inspect(HashDict.new)
    assert "#HashSet<" <> _ = inspect(HashSet.new)
  end

  test :pids do
    assert "#PID<" <> _ = inspect(self)
  end

  test :references do
    assert "#Reference<" <> _ = inspect(make_ref)
  end

  test :regex do
    "~r/foo/m" = inspect(~r(foo)m)
    "~r/\\a\\x08\\x7F\\x1B\\f\\n\\r \\t\\v\\//" = inspect(Regex.compile!("\a\b\d\e\f\n\r\s\t\v/"))
    "~r/\\a\\b\\d\\e\\f\\n\\r\\s\\t\\v\\//" = inspect(~r<\a\b\d\e\f\n\r\s\t\v/>)
  end
end
