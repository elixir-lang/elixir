Code.require_file "../../test_helper.exs", __FILE__

defmodule Binary.Inspect.AtomTest do
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
    assert inspect(:@)   == ":@"
    assert inspect(:&&&) == ":&&&"
  end

  test :impl do
    assert Binary.Inspect.Atom.__impl__ == Binary.Inspect
  end
end

defmodule Binary.Inspect.BitStringTest do
  use ExUnit.Case, async: true

  test :bitstring do
    assert inspect(<<1 :: [size(12), integer, signed]>>) == "<<0,1|4>>"
  end

  test :binary do
    assert inspect("foo") == "\"foo\""
    assert inspect(<<?a, ?b, ?c>>) == "\"abc\""
  end

  test :escape do
    assert inspect("f\no") == "\"f\\no\""
    assert inspect("f\\o") == "\"f\\\\o\""
  end

  test :utf8 do
    assert inspect(" ゆんゆん") == "\" ゆんゆん\""
  end

  test :unprintable do
    assert inspect(<<193>>) == "<<193>>"
  end
end

defmodule Binary.Inspect.NumberTest do
  use ExUnit.Case, async: true

  test :integer do
    assert inspect(100) == "100"
  end

  test :float do
    assert inspect(1.0) == "1.00000000000000000000e+00"
    assert inspect(1.0e10) == "1.00000000000000000000e+10"
    assert inspect(1.0e+10) == "1.00000000000000000000e+10"
  end
end

defmodule Binary.Inspect.TupleTest do
  use ExUnit.Case, async: true

  test :basic do
    assert inspect({ 1, "b", 3 }) == "{1,\"b\",3}"
  end

  test :record_like do
    assert inspect({ :foo, :bar }) == "{:foo,:bar}"
  end

  test :with_builtin_like_record do
    assert inspect({ :list, 1 }) == "{:list,1}"
  end

  test :with_record_like_tuple do
    assert inspect({ List, 1 }) == "{List,1}"
  end

  defrecord Config, a: 1, b: []

  test :with_record do
    assert inspect(Config.new) == "Binary.Inspect.TupleTest.Config[a: 1, b: []]"
  end

  test :with_tuple_matching_record_name_but_not_length do
    assert inspect({ExUnit.Server.Config}) == "{ExUnit.Server.Config}"
  end

  test :exception do
    assert inspect(RuntimeError.new) == "RuntimeError[message: \"runtime error\"]"
  end

  test :empty do
    assert inspect({}) == "{}"
  end
end

defmodule Binary.Inspect.ListTest do
  use ExUnit.Case, async: true

  test :basic do
    assert inspect([ 1, "b", 3 ]) == "[1,\"b\",3]"
  end

  test :printable do
    assert inspect('abc') == "'abc'"
  end

  test :non_printable do
    assert inspect([{:a,1}]) == "[{:a,1}]"
  end

  test :unproper do
    assert inspect([:foo | :bar]) == "[:foo|:bar]"
  end

  test :codepoints do
    assert inspect('é') == "[195,169]"
  end

  test :empty do
    assert inspect([]) == "[]"
  end
end

defmodule Binary.Inspect.AnyTest do
  use ExUnit.Case, async: true

  test :funs do
    bin = inspect(fn(x) -> x + 1 end)
    assert '#Fun<' ++ _ = binary_to_list(bin)
  end
end

defmodule Binary.Inspect.RegexTest do
  use ExUnit.Case, async: true

  test :regex do
    "%r\"foo\"m" = inspect(%r(foo)m)
  end
end
