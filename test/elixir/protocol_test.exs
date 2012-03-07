Code.require_file "../test_helper", __FILE__

defprotocol ProtocolTest::WithAll, [blank(thing)]
defprotocol ProtocolTest::WithExcept, [blank(thing)], except: [Atom, Number, List]
defprotocol ProtocolTest::WithOnly, [blank(thing)], only: [Record, Function]

defrecord ProtocolTest::Foo, a: 0, b: 0

defimpl WithAll, for: Foo do
  def blank(record) do
    record.a + record.b == 0
  end
end

defimpl WithOnly, for: Foo do
  def blank(record) do
    record.a + record.b == 0
  end
end

defmodule ProtocolTest do
  use ExUnit::Case

  test :protocol_with_all do
    assert_undef(ProtocolTest::WithAll, Builtin::Atom, :foo)
    assert_undef(ProtocolTest::WithAll, Builtin::Function, fn(x, do: x))
    assert_undef(ProtocolTest::WithAll, Builtin::Number, 1)
    assert_undef(ProtocolTest::WithAll, Builtin::Number, 1.1)
    assert_undef(ProtocolTest::WithAll, Builtin::List, [])
    assert_undef(ProtocolTest::WithAll, Builtin::List, [1,2,3])
    assert_undef(ProtocolTest::WithAll, Builtin::Tuple, {})
    assert_undef(ProtocolTest::WithAll, Builtin::Tuple, {1,2,3})
    assert_undef(ProtocolTest::WithAll, Builtin::Tuple, {Bar,2,3})
    assert_undef(ProtocolTest::WithAll, Builtin::BitString, "foo")
    assert_undef(ProtocolTest::WithAll, Builtin::BitString, <<1>>)
    assert_undef(ProtocolTest::WithAll, Builtin::PID, Process.self)
    assert_undef(ProtocolTest::WithAll, Builtin::Port, hd(:erlang.ports))
    assert_undef(ProtocolTest::WithAll, Builtin::Reference, make_ref)
  end

  test :protocol_with_except do
    assert_undef(ProtocolTest::WithExcept, Builtin::Any, :foo)
    assert_undef(ProtocolTest::WithExcept, Builtin::Any, 1)
    assert_undef(ProtocolTest::WithExcept, Builtin::Any, [1,2,3])
    assert_undef(ProtocolTest::WithExcept, Builtin::Function, fn(x, do: x))
    assert_undef(ProtocolTest::WithExcept, Builtin::Tuple, {})
  end

  test :protocol_with_only do
    assert_undef ProtocolTest::WithOnly, Builtin::Function, fn(x, do: x)
    assert_equal true, ProtocolTest::WithOnly.blank(ProtocolTest::Foo.new)
  end

  test :protocol_with_only_with_undefined do
    assert_equal nil, ProtocolTest::WithOnly.__protocol_for__(:foo)

    assert_raises Protocol::UndefinedError, "protocol ::ProtocolTest::WithOnly not implemented for :foo", fn ->
      ProtocolTest::WithOnly.blank(:foo)
    end
  end

  test :protocol_with_record do
    true  = ProtocolTest::WithAll.blank(ProtocolTest::Foo.new)
    false = ProtocolTest::WithAll.blank(ProtocolTest::Foo.new(a: 1))
  end

  test :protocol_for do
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Atom, :foo)
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Function, fn(x, do: x))
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Number, 1)
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Number, 1.1)
    assert_protocol_for(ProtocolTest::WithAll, Builtin::List, [])
    assert_protocol_for(ProtocolTest::WithAll, Builtin::List, [1,2,3])
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Tuple, {})
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Tuple, {1,2,3})
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Record, {Bar,2,3})
    assert_protocol_for(ProtocolTest::WithAll, Builtin::BitString, "foo")
    assert_protocol_for(ProtocolTest::WithAll, Builtin::BitString, <<1>>)
    assert_protocol_for(ProtocolTest::WithAll, Builtin::PID, Process.self)
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Port, hd(:erlang.ports))
    assert_protocol_for(ProtocolTest::WithAll, Builtin::Reference, make_ref)
  end

  # Assert that the given protocol is going to be dispatched.
  defp assert_protocol_for(target, impl, thing) do
    joined  = target::impl
    assert_equal joined, target.__protocol_for__ thing
  end

  # Dispatch `blank(thing)` to the given `target`
  # and check if it will dispatch (and successfully fail)
  # to the proper implementation `impl`.
  defp assert_undef(target, impl, thing) do
    try do
      target.blank(thing)
      raise "Expected invocation to fail"
    catch: :error, :undef, [stack|_]
      ref = target :: impl
      case hd(stack) do
      match: { ^ref, :blank, [^thing] }
        :ok
      match: { ^ref, :blank, [^thing], []}
        :ok
      else:
        raise "Invalid stack #{inspect stack}. Expected: { #{ref}, :blank, [#{inspect thing}] }"
      end
    end
  end
end
