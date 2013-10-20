Code.require_file "test_helper.exs", __DIR__

defmodule ProtocolTest do
  use ExUnit.Case, async: true

  defprotocol Sample do
    @type t :: any
    @doc "Blank"
    @spec blank(t) :: boolean
    def blank(thing)
  end

  defprotocol WithAny do
    @fallback_to_any true
    @doc "Blank"
    def blank(thing)
  end

  defrecord Foo, a: 0, b: 0

  defimpl Sample, for: Foo do
    def blank(record) do
      record.a + record.b == 0
    end
  end

  defrecord Bar, a: 0 do
    defimpl Sample do
      def blank(record) do
        Unknown.undefined(record)
      end
    end
  end

  defimpl WithAny, for: Foo do
    def blank(record) do
      record.a + record.b == 0
    end
  end

  defimpl WithAny, for: Tuple do
    def blank(_tuple) do
      false
    end
  end

  defimpl WithAny, for: Any do
    def blank(_any) do
      true
    end
  end

  test :protocol_impl_for do
    assert nil? Sample.impl_for(:foo)
    assert nil? Sample.impl_for(fn(x) -> x end)
    assert nil? Sample.impl_for(1)
    assert nil? Sample.impl_for(1.1)
    assert nil? Sample.impl_for([])
    assert nil? Sample.impl_for([1, 2, 3])
    assert nil? Sample.impl_for({})
    assert nil? Sample.impl_for({1, 2, 3})
    assert nil? Sample.impl_for("foo")
    assert nil? Sample.impl_for(<<1>>)
    assert nil? Sample.impl_for(self)
    assert nil? Sample.impl_for(hd(:erlang.ports))
    assert nil? Sample.impl_for(make_ref)
    assert nil? Sample.impl_for(Range[])

    assert Sample.impl_for(Foo[]) ==
           Sample.ProtocolTest.Foo
    assert Sample.impl_for(Bar[]) ==
           Sample.ProtocolTest.Bar
  end

  test :protocol_priority_does_not_override_records do
    assert WithAny.impl_for(Foo[])    == WithAny.ProtocolTest.Foo
    assert WithAny.impl_for(Range[])  == WithAny.Tuple
    assert WithAny.impl_for({ :foo }) == WithAny.Tuple
    assert WithAny.impl_for({})       == WithAny.Tuple
  end

  test :protocol_with_fallback do
    assert WithAny.impl_for(self) == WithAny.Any
  end

  test :protocol_not_implemented do
    assert_raise Protocol.UndefinedError, "protocol ProtocolTest.Sample not implemented for :foo", fn ->
      Sample.blank(:foo)
    end
  end

  test :protocol_with_nil_record do
    assert_raise UndefinedFunctionError, fn ->
      WithOnly.blank({:nil})
    end
  end

  test :protocol_docs do
    docs = Sample.__info__(:docs)
    assert { { :blank, 1 }, _, :def, [{ :thing, _, nil }], "Blank" } =
           List.keyfind(docs, { :blank, 1 }, 0)
  end

  test :protocol_avoids_false_negatives do
    assert_raise UndefinedFunctionError, fn ->
      WithAll.blank(Bar.new)
    end
  end

  test :protocol_callback do
    assert get_callbacks(Sample, :blank, 1) ==
      [{:type, 9, :fun, [{:type, 9, :product, [{:type, 9, :t, []}]}, {:type, 9, :boolean, []}]}]

    assert get_callbacks(WithAny, :blank, 1) ==
      [{:type, 16, :fun, [{:type, 16, :product, [{:type, 16, :t, []}]}, {:type, 16, :term, []}]}]
  end

  test :protocol_attribute do
    assert Sample.__info__(:attributes)[:protocol] == [fallback_to_any: false, consolidated: false]
    assert WithAny.__info__(:attributes)[:protocol] == [fallback_to_any: true, consolidated: false]
  end

  defp get_callbacks(module, name, arity) do
    callbacks = lc { :callback, info } inlist module.__info__(:attributes), do: hd(info)
    List.keyfind(callbacks, { name, arity }, 0) |> elem(1)
  end

  test :defimpl do
    defprotocol Attribute do
      def test(thing)
    end

    defimpl Attribute, for: Foo do
      def test(_) do
        { @protocol, @for }
      end
    end

    assert Attribute.test(Foo[]) == { Attribute, Foo }
    assert Attribute.ProtocolTest.Foo.__impl__(:protocol) == Attribute
    assert Attribute.ProtocolTest.Foo.__impl__(:for) == Foo
    assert Attribute.ProtocolTest.Foo.__info__(:attributes)[:impl] ==
           [protocol: Attribute, for: Foo]
  end

  test :defimpl_multi do
    defprotocol Multi do
      def test(a)
    end

    defimpl Multi, for: [Atom, Integer] do
      def test(a), do: a
    end

    assert Multi.test(1) == 1
    assert Multi.test(:a) == :a
  end
end

## Those protocols needs to be the same as above.
path = Path.expand("../ebin", __DIR__)

compile = fn { :module, module, binary, _ } ->
  :code.purge(module)
  :code.delete(module)
  File.write!("#{path}/#{module}.beam", binary)
end

defmodule Protocol.ConsolidationTest do
  use ExUnit.Case, async: true

  compile.(
    defprotocol Sample do
      @type t :: any
      @doc "Blank"
      @spec blank(t) :: boolean
      def blank(thing)
    end
  )

  compile.(
    defprotocol WithAny do
      @fallback_to_any true
      @doc "Blank"
      def blank(thing)
    end
  )

  defrecord Foo, a: 0, b: 0
  defrecord Bar, a: 0

  Code.append_path(path)

  # Any is ignored because there is no fallback
  { :ok, binary } = Protocol.Consolidation.apply_to(Sample, [Any, Foo, Bar])
  :code.load_binary(Sample, 'protocol_test.exs', binary)

  # Any should be moved to the end
  { :ok, binary } = Protocol.Consolidation.apply_to(WithAny, [Any, Foo, Tuple])
  :code.load_binary(WithAny, 'protocol_test.exs', binary)

  test :consolidated_impl_for do
    assert nil? Sample.impl_for(:foo)
    assert nil? Sample.impl_for(fn(x) -> x end)
    assert nil? Sample.impl_for(1)
    assert nil? Sample.impl_for(1.1)
    assert nil? Sample.impl_for([])
    assert nil? Sample.impl_for([1, 2, 3])
    assert nil? Sample.impl_for({})
    assert nil? Sample.impl_for({1, 2, 3})
    assert nil? Sample.impl_for("foo")
    assert nil? Sample.impl_for(<<1>>)
    assert nil? Sample.impl_for(self)
    assert nil? Sample.impl_for(hd(:erlang.ports))
    assert nil? Sample.impl_for(make_ref)
    assert nil? Sample.impl_for(Range[])

    assert Sample.impl_for(Foo[]) ==
           Sample.Protocol.ConsolidationTest.Foo
    assert Sample.impl_for(Bar[]) ==
           Sample.Protocol.ConsolidationTest.Bar
  end

  test :consolidated_impl_for_prioritized do
    assert WithAny.impl_for(Foo[])    == WithAny.Protocol.ConsolidationTest.Foo
    assert WithAny.impl_for(Range[])  == WithAny.Tuple
    assert WithAny.impl_for({ :foo }) == WithAny.Tuple
    assert WithAny.impl_for({})       == WithAny.Tuple
  end

  test :consolidated_fallback do
    assert WithAny.impl_for(self) == WithAny.Any
  end

  test :consolidated_docs do
    docs = Sample.__info__(:docs)
    assert { { :blank, 1 }, _, :def, [{ :thing, _, nil }], "Blank" } =
           List.keyfind(docs, { :blank, 1 }, 0)
  end

  test :consolidated_callback do
    callbacks = lc { :callback, info } inlist Sample.__info__(:attributes), do: hd(info)
    assert callbacks != []
  end

  test :consolidation_errors do
    defprotocol NoBeam, do: nil
    assert Protocol.Consolidation.apply_to(String, [])  == { :error, :not_a_protocol }
    assert Protocol.Consolidation.apply_to(NoBeam, [])  == { :error, :no_beam_info }
  end

  test :consolidated_attribute do
    assert Sample.__info__(:attributes)[:protocol] == [fallback_to_any: false, consolidated: true]
    assert WithAny.__info__(:attributes)[:protocol] == [fallback_to_any: true, consolidated: true]
  end

  test :protocols_extraction do
    protos = Protocol.Consolidation.extract_protocols([:code.lib_dir(:elixir, :ebin)])
    assert Enumerable in protos
    assert Inspect in protos
  end

  test :impls_extraction do
    protos = Protocol.Consolidation.extract_impls(Enumerable, [:code.lib_dir(:elixir, :ebin)])
    assert List in protos
    assert Function in protos
  end
end
