Code.require_file "../../test_helper", __FILE__

defmodule String::Inspect::AtomTest do
  use ExUnit::Case

  test :basic do
    assert_equal ":foo", inspect(:foo)
  end

  test :empty do
    assert_equal ":\"\"", inspect(:"")
  end

  test :true_false_nil do
    assert_equal "false", inspect(false)
    assert_equal "true", inspect(true)
    assert_equal "nil", inspect(nil)
  end

  test :with_uppercase do
    assert_equal ":fOO", inspect(:fOO)
    assert_equal ":FOO", inspect(:FOO)
  end

  test :reference_atom do
    assert_equal "::Foo::Bar", inspect(::Foo::Bar)
  end

  test :impl do
    assert_equal String::Inspect, String::Inspect::Atom.__impl__
  end
end

defmodule String::Inspect::BitStringTest do
  use ExUnit::Case

  test :bitstring do
    assert_equal "<<0,1|4>>", inspect(<<1|12-integer-signed>>)
  end

  test :binary do
    assert_equal "\"foo\"", inspect("foo")
    assert_equal "\"abc\"", inspect(<<?a, ?b, ?c>>)
  end

  test :escape do
    assert_equal "\"f\\no\"" , inspect("f\no")
    assert_equal "\"f\\\\o\"", inspect("f\\o")
  end

  test :unprintable do
    assert_equal "<<1>>", inspect(<<1>>)
  end
end

defmodule String::Inspect::NumberTest do
  use ExUnit::Case

  test :integer do
    assert_equal "100", inspect(100)
  end

  test :float do
    assert_equal "1.00000000000000000000e+00", inspect(1.0)
    assert_equal "1.00000000000000000000e+10", inspect(1.0e10)
    assert_equal "1.00000000000000000000e+10", inspect(1.0e+10)
  end
end

defmodule String::Inspect::TupleTest do
  use ExUnit::Case

  test :basic do
    assert_equal "{1,\"b\",3}", inspect({ 1, "b", 3 })
  end

  test :record_like do
    assert_equal "{:foo,:bar}", inspect({ :foo, :bar })
  end

  test :with_builtin_like_record do
    assert_equal "{:list,1}", inspect({ :list, 1 })
  end

  test :exception do
    assert_equal "::RuntimeError{\"runtime error\"}", inspect(RuntimeError.new)
  end

  test :empty do
    assert_equal "{}", inspect({})
  end
end

defmodule String::Inspect::ListTest do
  use ExUnit::Case

  test :basic do
    assert_equal "[1,\"b\",3]", inspect([ 1, "b", 3 ])
  end

  test :printable do
    assert_equal "'abc'", inspect('abc')
  end

  test :non_printable do
    assert_equal "[{:a,1}]", inspect([{:a,1}])
  end

  test :unproper do
    assert_equal "[:foo|:bar]", inspect([:foo | :bar])
  end

  test :empty do
    assert_equal "[]", inspect([])
  end
end

defmodule String::Inspect::AnyTest do
  use ExUnit::Case

  test :funs do
    bin = inspect(fn(x, do: x + 1))
    '#Fun<' ++ _ = binary_to_list(bin)
  end
end
