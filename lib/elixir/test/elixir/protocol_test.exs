Code.require_file "test_helper.exs", __DIR__

defprotocol ProtocolTest.Sample do
  @type t :: any
  @doc "Blank"
  @spec blank(t) :: boolean
  def blank(thing)
end

defprotocol ProtocolTest.Prioritized do
  @prioritize [Tuple]
  @doc "Blank"
  def blank(thing)
end

defrecord ProtocolTest.Foo, a: 0, b: 0

defimpl ProtocolTest.Sample, for: ProtocolTest.Foo do
  def blank(record) do
    record.a + record.b == 0
  end
end

defrecord ProtocolTest.Bar, a: 0 do
  defimpl ProtocolTest.Sample do
    def blank(record) do
      Unknown.undefined(record)
    end
  end
end

defimpl ProtocolTest.Prioritized, for: ProtocolTest.Foo do
  def blank(record) do
    record.a + record.b == 0
  end
end

defimpl ProtocolTest.Prioritized, for: Tuple do
  def blank(_tuple) do
    false
  end
end

defimpl ProtocolTest.Prioritized, for: Any do
  def blank(_any) do
    true
  end
end

defmodule ProtocolTest do
  use ExUnit.Case, async: true

  test :protocol_impl_for do
    assert nil? ProtocolTest.Sample.impl_for(:foo)
    assert nil? ProtocolTest.Sample.impl_for(fn(x) -> x end)
    assert nil? ProtocolTest.Sample.impl_for(1)
    assert nil? ProtocolTest.Sample.impl_for(1.1)
    assert nil? ProtocolTest.Sample.impl_for([])
    assert nil? ProtocolTest.Sample.impl_for([1, 2, 3])
    assert nil? ProtocolTest.Sample.impl_for({})
    assert nil? ProtocolTest.Sample.impl_for({1, 2, 3})
    assert nil? ProtocolTest.Sample.impl_for({Bar, 2, 3})
    assert nil? ProtocolTest.Sample.impl_for("foo")
    assert nil? ProtocolTest.Sample.impl_for(<<1>>)
    assert nil? ProtocolTest.Sample.impl_for(self)
    assert nil? ProtocolTest.Sample.impl_for(hd(:erlang.ports))
    assert nil? ProtocolTest.Sample.impl_for(make_ref)

    assert ProtocolTest.Sample.impl_for(ProtocolTest.Foo[]) ==
           ProtocolTest.Sample.ProtocolTest.Foo
    assert ProtocolTest.Sample.impl_for(ProtocolTest.Bar[]) ==
           ProtocolTest.Sample.ProtocolTest.Bar
  end

  test :protocol_impl_for_prioritized do
    # Has higher priority
    assert ProtocolTest.Prioritized.impl_for(ProtocolTest.Foo[]) ==
           ProtocolTest.Prioritized.Tuple

    # Has fallback
    assert ProtocolTest.Prioritized.impl_for(self) ==
           ProtocolTest.Prioritized.Any
  end

  test :protocol_not_implemented do
    assert_raise Protocol.UndefinedError, "protocol ProtocolTest.Sample not implemented for :foo", fn ->
      ProtocolTest.Sample.blank(:foo)
    end
  end

  test :protocol_with_nil_record do
    assert_raise UndefinedFunctionError, fn ->
      ProtocolTest.WithOnly.blank({:nil})
    end
  end

  test :protocol_docs do
    docs = ProtocolTest.Sample.__info__(:docs)
    assert { { :blank, 1 }, _, :def, [{ :thing, _, nil }], "Blank" } =
           List.keyfind(docs, { :blank, 1 }, 0)
  end

  test :protocol_avoids_false_negatives do
    assert_raise UndefinedFunctionError, fn ->
      ProtocolTest.WithAll.blank(ProtocolTest.Bar.new)
    end
  end

  test :protocol_callback do
    assert get_callbacks(ProtocolTest.Sample, :blank, 1) ==
      [{:type, 6, :fun, [{:type, 6, :product, [{:type, 6, :t, []}]}, {:type, 6, :boolean, []}]}]

    assert get_callbacks(ProtocolTest.Prioritized, :blank, 1) ==
      [{:type, 13, :fun, [{:type, 13, :product, [{:type, 13, :t, []}]}, {:type, 13, :term, []}]}]
  end

  test :defimpl do
    alias ProtocolTest.Foo

    defprotocol Attribute do
      def test(thing)
    end

    defimpl Attribute, for: Foo do
      def test(_) do
        { @protocol, @for }
      end
    end

    assert Attribute.test(Foo[]) == { Attribute, Foo }
    assert ProtocolTest.Attribute.ProtocolTest.Foo.__impl__(:protocol) == Attribute
    assert ProtocolTest.Attribute.ProtocolTest.Foo.__impl__(:for) == Foo
  end

  defp get_callbacks(module, name, arity) do
    callbacks = lc { :callback, info } inlist module.__info__(:attributes), do: hd(info)
    List.keyfind(callbacks, { name, arity }, 0) |> elem(1)
  end

  test :defimpl_multi do
    defprotocol Multi do
      def test(a)
    end

    defimpl Multi, for: [Atom, Number] do
      def test(a), do: a
    end

    assert Multi.test(1) == 1
    assert Multi.test(:a) == :a
  end
end
