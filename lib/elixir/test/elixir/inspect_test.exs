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
    assert inspect(Foo.Bar) == "Foo.Bar"
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
    assert inspect(:&&&) == ":&&&"
    assert inspect(:~~~) == ":~~~"
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

  test :container do
    assert inspect(:<<>>) == ":<<>>"
    assert inspect(:{})   == ":{}"
  end
end

defmodule Inspect.BitStringTest do
  use ExUnit.Case, async: true

  test :bitstring do
    assert inspect(<<1 :: [size(12), integer, signed]>>) == "<<0, 1::size(4)>>"
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

  test :opt_infer do
    assert inspect(<<"eric", 193, "mj">>, binaries: :infer) == ~s(<<101, 114, 105, 99, 193, 109, 106>>)
    assert inspect(<<"eric">>, binaries: :infer) == ~s("eric")
    assert inspect(<<193>>, binaries: :infer) == ~s(<<193>>)
  end

  test :opt_as_strings do
    assert inspect(<<"eric", 193, "mj">>, binaries: :as_strings) == ~s("eric\\301mj")
    assert inspect(<<"eric">>, binaries: :as_strings) == ~s("eric")
    assert inspect(<<193>>, binaries: :as_strings) == ~s("\\301")
  end

  test :opt_as_binaries do
    assert inspect(<<"eric", 193, "mj">>, binaries: :as_binaries) == "<<101, 114, 105, 99, 193, 109, 106>>"
    assert inspect(<<"eric">>, binaries: :as_binaries) == "<<101, 114, 105, 99>>"
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

  test :float do
    assert inspect(1.0) == "1.0"
    assert inspect(1.0e10) == "1.0e10"
    assert inspect(1.0e-10) == "1.0e-10"
  end
end

defmodule Inspect.TupleTest do
  use ExUnit.Case

  test :basic do
    assert inspect({ 1, "b", 3 }) == "{1, \"b\", 3}"
    assert inspect({ 1, "b", 3 }, [pretty: true, width: 1]) == "{1,\n \"b\",\n 3}"
  end

  test :record_like do
    assert inspect({ :foo, :bar }) == "{:foo, :bar}"
  end

  test :with_builtin_like_record do
    assert inspect({ :list, 1 }) == "{:list, 1}"
  end

  test :with_record_like_tuple do
    assert inspect({ List, 1 }) == "{List, 1}"
  end

  test :with_record_like_pseudo_exception do
    assert inspect({ Other, :__exception__, 1 }) == "{Other, :__exception__, 1}"
  end

  defrecord Config, a: 1, b: []

  test :with_record do
    assert inspect(Config.new) == "Inspect.TupleTest.Config[a: 1, b: []]"
  end

  test :with_tuple_matching_record_name_but_not_length do
    assert inspect({ExUnit.Server.Config}) == "{ExUnit.Server.Config}"
  end

  test :exception do
    assert inspect(RuntimeError.new) == "RuntimeError[message: \"runtime error\"]"
  end

  defrecord Rec, value: 1

  test :two_items_record do
    assert inspect({ Rec[value: 1], 1 }) == "{Inspect.TupleTest.Rec[value: 1], 1}"
  end

  test :false_positives do
    import ExUnit.CaptureIO

    assert capture_io(:stderr, fn ->
      assert inspect({ Range, nil }) == "{Range, nil}"
    end) =~ "** (Inspect.Error) Got FunctionClauseError with message no function clause matching in Inspect.Range.inspect/2"
  end

  test :empty do
    assert inspect({}) == "{}"
  end

  test :with_limit do
    assert inspect({ 1, 2, 3, 4 }, limit: 3) == "{1, 2, 3, ...}"
  end

  test :with_records_false do
    assert inspect(Config.new, records: false) == "{Inspect.TupleTest.Config, 1, []}"
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
    assert inspect('eric' ++ [0] ++ 'mj', char_lists: :infer) == "[101, 114, 105, 99, 0, 109, 106]"
    assert inspect('eric', char_lists: :infer) == "'eric'"
    assert inspect([0], char_lists: :infer) == "[0]"
  end

  test :opt_as_strings do
    assert inspect('eric' ++ [0] ++ 'mj', char_lists: :as_char_lists) == "'eric\\000mj'"
    assert inspect('eric', char_lists: :as_char_lists) == "'eric'"
    assert inspect([0], char_lists: :as_char_lists) == "'\\000'"
  end

  test :opt_as_lists do
    assert inspect('eric' ++ [0] ++ 'mj', char_lists: :as_lists) == "[101, 114, 105, 99, 0, 109, 106]"
    assert inspect('eric', char_lists: :as_lists) == "[101, 114, 105, 99]"
    assert inspect([0], char_lists: :as_lists) == "[0]"
  end

  test :non_printable do
    assert inspect([{:b, 1}, {:a, 1}]) == "[b: 1, a: 1]"
  end

  test :unproper do
    assert inspect([:foo | :bar]) == "[:foo | :bar]"

    assert inspect([1,2,3,4,5|42], [pretty: true, width: 1]) == "[1,\n 2,\n 3,\n 4,\n 5 |\n 42]"
  end

  test :codepoints do
    assert inspect('é') == "'é'"
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
    assert inspect(%{ 1 => "b" }) == "%{1 => \"b\"}"
    assert inspect(%{ 1 => "b", 2 => "c"}, [pretty: true, width: 1]) == "%{1 => \"b\",\n 2 => \"c\"}"
  end

  test :keyword do
    assert inspect(%{a: 1}) == "%{a: 1}"
    assert inspect(%{a: 1, b: 2}) == "%{a: 1, b: 2}"
    assert inspect(%{a: 1, b: 2, c: 3}) == "%{a: 1, b: 2, c: 3}"
  end

  test :with_limit do
    assert inspect(%{ 1 => 1, 2 => 2, 3 => 3, 4 => 4 }, limit: 3) == "%{1 => 1, 2 => 2, 3 => 3, ...}"
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
      def inspect(_, _) do
        raise "failing"
      end
    end
  end

  test :bad_implementation do
    import ExUnit.CaptureIO

    assert capture_io(:stderr, fn ->
      inspect(%Failing{})
    end) =~ ("** (Inspect.Error) Got RuntimeError with message failing while inspecting " <>
             "%{__struct__: Inspect.MapTest.Failing, key: 0}")
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

    :application.set_env(:elixir, :anony, V.fun)
    :application.set_env(:elixir, :named, &V.fun/0)

    :code.delete(V)
    :code.purge(V)

    { :ok, anony } = :application.get_env(:elixir, :anony)
    { :ok, named } = :application.get_env(:elixir, :named)

    assert inspect(anony) =~ ~r"#Function<0.\d+/0 in Inspect.OthersTest.V>"
    assert inspect(named) =~ ~r"&Inspect.OthersTest.V.fun/0"
  after
    :application.unset_env(:elixir, :anony)
    :application.unset_env(:elixir, :named)
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
    "~r\"foo\"m" = inspect(~r(foo)m)
  end
end
