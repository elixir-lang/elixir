Code.require_file "../test_helper", __FILE__

defprotocol ProtocolTest.WithAll do
  def blank(thing)
end

defprotocol ProtocolTest.WithExcept do
  @except [Atom, Number, List]
  def blank(thing)
end

defprotocol ProtocolTest.WithOnly do
  @only [Record, Function]
  def blank(thing)
end

defprotocol ProtocolTest.Plus do
  @only [Number]
  def plus(thing)
  def plus(thing, other)
end

defrecord ProtocolTest.Foo, a: 0, b: 0

defimpl ProtocolTest.WithAll, for: ProtocolTest.Foo do
  def blank(record) do
    record.a + record.b == 0
  end
end

defimpl ProtocolTest.WithOnly, for: ProtocolTest.Foo do
  def blank(record) do
    record.a + record.b == 0
  end
end

defimpl ProtocolTest.Plus, for: Number do
  def plus(thing), do: thing + 1
  def plus(thing, other), do: thing + other
end

defmodule ProtocolTest do
  use ExUnit.Case

  test :protocol_with_all do
    assert_undef(ProtocolTest.WithAll, Atom, :foo)
    assert_undef(ProtocolTest.WithAll, Function, fn(x) -> x end)
    assert_undef(ProtocolTest.WithAll, Number, 1)
    assert_undef(ProtocolTest.WithAll, Number, 1.1)
    assert_undef(ProtocolTest.WithAll, List, [])
    assert_undef(ProtocolTest.WithAll, List, [1,2,3])
    assert_undef(ProtocolTest.WithAll, Tuple, {})
    assert_undef(ProtocolTest.WithAll, Tuple, {1,2,3})
    assert_undef(ProtocolTest.WithAll, Tuple, {Bar,2,3})
    assert_undef(ProtocolTest.WithAll, BitString, "foo")
    assert_undef(ProtocolTest.WithAll, BitString, <<1>>)
    assert_undef(ProtocolTest.WithAll, PID, Process.self)
    assert_undef(ProtocolTest.WithAll, Port, hd(:erlang.ports))
    assert_undef(ProtocolTest.WithAll, Reference, make_ref)
  end

  test :protocol_with_except do
    assert_undef(ProtocolTest.WithExcept, Any, :foo)
    assert_undef(ProtocolTest.WithExcept, Any, 1)
    assert_undef(ProtocolTest.WithExcept, Any, [1,2,3])
    assert_undef(ProtocolTest.WithExcept, Function, fn(x) -> x end)
    assert_undef(ProtocolTest.WithExcept, Tuple, {})
  end

  test :protocol_with_only do
    assert_undef ProtocolTest.WithOnly, Function, fn(x) -> x end
    assert ProtocolTest.WithOnly.blank(ProtocolTest.Foo.new) == true
  end

  test :protocol_with_only_with_undefined do
    assert ProtocolTest.WithOnly.__impl_for__(:foo) == nil

    assert_raise Protocol.UndefinedError, "protocol ProtocolTest.WithOnly not implemented for :foo", fn ->
      ProtocolTest.WithOnly.blank(:foo)
    end
  end

  test :protocol_with_record do
    true  = ProtocolTest.WithAll.blank(ProtocolTest.Foo.new)
    false = ProtocolTest.WithAll.blank(ProtocolTest.Foo.new(a: 1))
  end

  test :protocol_for do
    assert_protocol_for(ProtocolTest.WithAll, Atom, :foo)
    assert_protocol_for(ProtocolTest.WithAll, Function, fn(x) -> x end)
    assert_protocol_for(ProtocolTest.WithAll, Number, 1)
    assert_protocol_for(ProtocolTest.WithAll, Number, 1.1)
    assert_protocol_for(ProtocolTest.WithAll, List, [])
    assert_protocol_for(ProtocolTest.WithAll, List, [1,2,3])
    assert_protocol_for(ProtocolTest.WithAll, Tuple, {})
    assert_protocol_for(ProtocolTest.WithAll, Tuple, {1,2,3})
    assert_protocol_for(ProtocolTest.WithAll, Tuple, {Bar,2,3})
    assert_protocol_for(ProtocolTest.WithAll, BitString, "foo")
    assert_protocol_for(ProtocolTest.WithAll, BitString, <<1>>)
    assert_protocol_for(ProtocolTest.WithAll, PID, Process.self)
    assert_protocol_for(ProtocolTest.WithAll, Port, hd(:erlang.ports))
    assert_protocol_for(ProtocolTest.WithAll, Reference, make_ref)
  end

  test :protocol_with_two_items do
    assert ProtocolTest.Plus.plus(1) == 2
    assert ProtocolTest.Plus.plus(1, 2) == 3
  end

  # Assert that the given protocol is going to be dispatched.
  defp assert_protocol_for(target, impl, thing) do
    joined  = Module.concat(target, impl)
    assert target.__impl_for__(thing) == joined
  end

  # Dispatch `blank(thing)` to the given `target`
  # and check if it will dispatch (and successfully fail)
  # to the proper implementation `impl`.
  defp assert_undef(target, impl, thing) do
    try do
      target.blank(thing)
      raise "Expected invocation to fail"
    catch
      :error, :undef, [stack|_] ->
        ref = Module.concat target, impl
        case hd(stack) do
          { ^ref, :blank, [^thing], _} -> :ok
          _ ->
            raise "Invalid stack #{inspect stack}. Expected: { #{ref}, :blank, [#{inspect thing}], _ }"
        end
    end
  end
end
