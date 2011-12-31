module ProtocolTest do
  use ExUnit::Case

  module __MODULE__ :: WithAll do
    defprotocol [blank(thing)]
  end

  def test_protocol_with_all do
    assert_undef(ProtocolTest::WithAll, Atom, :foo)
    assert_undef(ProtocolTest::WithAll, Function, fn(x, do: x))
    assert_undef(ProtocolTest::WithAll, Number, 1)
    assert_undef(ProtocolTest::WithAll, Number, 1.1)
    assert_undef(ProtocolTest::WithAll, List, [])
    assert_undef(ProtocolTest::WithAll, List, [1,2,3])
    assert_undef(ProtocolTest::WithAll, Tuple, {})
    assert_undef(ProtocolTest::WithAll, Tuple, {1,2,3})
    assert_undef(ProtocolTest::WithAll, BitString, "foo")
    assert_undef(ProtocolTest::WithAll, BitString, bitstr(1))
    assert_undef(ProtocolTest::WithAll, PID, self())
    assert_undef(ProtocolTest::WithAll, Port, hd(:erlang.ports))
    assert_undef(ProtocolTest::WithAll, Reference, make_ref)
  end

  defp assert_undef(target, impl, thing) do
    try do
      target.blank(thing)
      error("Expected invocation to fail")
    catch: { :error, :undef, [stack|_] }
      ref = target :: impl
      { ref, :blank, [thing] } = hd(stack)
    end
  end
end