Code.require_file "test_helper.exs", __DIR__

defmodule ProtocolTest do
  use ExUnit.Case, async: true

  defprotocol Sample do
    @type t :: any
    @doc "Ok"
    @spec ok(t) :: boolean
    def ok(thing)
  end

  defprotocol WithAny do
    @fallback_to_any true
    @doc "Ok"
    def ok(thing)
  end

  defrecord NoImplRec, a: 0, b: 0

  defrecord ImplRec, a: 0, b: 0 do
    defimpl Sample do
      def ok(record) do
        Unknown.undefined(record)
      end
    end
  end

  defimpl WithAny, for: ImplRec do
    def ok(_record) do
      :ok
    end
  end

  defimpl WithAny, for: Tuple do
    def ok(_tuple) do
      :ok
    end
  end

  defimpl WithAny, for: Any do
    def ok(_any) do
      :ok
    end
  end

  test "protocol implementations without any" do
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
    assert nil? Sample.impl_for(%{})
    assert nil? Sample.impl_for(self)
    assert nil? Sample.impl_for(hd(:erlang.ports))
    assert nil? Sample.impl_for(make_ref)

    assert Sample.impl_for(ImplRec[]) ==
           Sample.ProtocolTest.ImplRec
    assert Sample.impl_for(NoImplRec[]) ==
           nil
  end

  test "protocol implementation with any and records fallback" do
    assert WithAny.impl_for(ImplRec[])   == WithAny.ProtocolTest.ImplRec
    assert WithAny.impl_for(NoImplRec[]) == WithAny.Tuple
    assert WithAny.impl_for({ :foo })    == WithAny.Tuple
    assert WithAny.impl_for({})          == WithAny.Tuple
    assert WithAny.impl_for(self)        == WithAny.Any
  end

  test "protocol not implemented" do
    assert_raise Protocol.UndefinedError, "protocol ProtocolTest.Sample not implemented for :foo", fn ->
      Sample.ok(:foo)
    end
  end

  test "protocol documentation" do
    docs = Sample.__info__(:docs)
    assert { { :ok, 1 }, _, :def, [{ :thing, _, nil }], "Ok" } =
           List.keyfind(docs, { :ok, 1 }, 0)
  end

  test "protocol keeps underlying UndefinedFunctionError" do
    assert_raise UndefinedFunctionError, fn ->
      WithAll.ok(Foo.new)
    end
  end

  test "protocol defines callbacks" do
    assert get_callbacks(Sample, :ok, 1) ==
      [{:type, 9, :fun, [{:type, 9, :product, [{:type, 9, :t, []}]}, {:type, 9, :boolean, []}]}]

    assert get_callbacks(WithAny, :ok, 1) ==
      [{:type, 16, :fun, [{:type, 16, :product, [{:type, 16, :t, []}]}, {:type, 16, :term, []}]}]
  end

  test "protocol defines attributes" do
    assert Sample.__info__(:attributes)[:protocol] == [fallback_to_any: false, consolidated: false]
    assert WithAny.__info__(:attributes)[:protocol] == [fallback_to_any: true, consolidated: false]
  end

  test "defimpl" do
    defprotocol Attribute do
      def test(thing)
    end

    defimpl Attribute, for: ImplRec do
      def test(_) do
        { @protocol, @for }
      end
    end

    assert Attribute.test(ImplRec[]) == { Attribute, ImplRec }
    assert Attribute.ProtocolTest.ImplRec.__impl__(:protocol) == Attribute
    assert Attribute.ProtocolTest.ImplRec.__impl__(:for) == ImplRec
    assert Attribute.ProtocolTest.ImplRec.__info__(:attributes)[:impl] ==
           [protocol: Attribute, for: ImplRec]
  end

  test "defimpl with multiple for" do
    defprotocol Multi do
      def test(a)
    end

    defimpl Multi, for: [Atom, Integer] do
      def test(a), do: a
    end

    assert Multi.test(1) == 1
    assert Multi.test(:a) == :a
  end

  defp get_callbacks(module, name, arity) do
    callbacks = lc { :callback, info } inlist module.__info__(:attributes), do: hd(info)
    List.keyfind(callbacks, { name, arity }, 0) |> elem(1)
  end
end

# Consolidation is temporarily disabled because we can't
# consolidate records and structs at once!

# path = Path.expand("../ebin", __DIR__)
# File.mkdir_p!(path)
#
# compile = fn { :module, module, binary, _ } ->
#   :code.purge(module)
#   :code.delete(module)
#   File.write!("#{path}/#{module}.beam", binary)
# end
#
# defmodule Protocol.ConsolidationTest do
#   use ExUnit.Case, async: true
#
#   compile.(
#     defprotocol Sample do
#       @type t :: any
#       @doc "Ok"
#       @spec ok(t) :: boolean
#       def ok(thing)
#     end
#   )
#
#   compile.(
#     defprotocol WithAny do
#       @fallback_to_any true
#       @doc "Ok"
#       def ok(thing)
#     end
#   )
#
#   defrecord ImplRec, a: 0, b: 0
#   defrecord NoImplRec, a: 0
#
#   Code.append_path(path)
#
#   # Any is ignored because there is no fallback
#   { :ok, binary } = Protocol.Consolidation.apply_to(Sample, [Any, ImplRec, NoImplRec])
#   :code.load_binary(Sample, 'protocol_test.exs', binary)
#
#   # Any should be moved to the end
#   { :ok, binary } = Protocol.Consolidation.apply_to(WithAny, [Any, ImplRec, Tuple])
#   :code.load_binary(WithAny, 'protocol_test.exs', binary)
#
#   test "consolidated implementations without any" do
#     assert nil? Sample.impl_for(:foo)
#     assert nil? Sample.impl_for(fn(x) -> x end)
#     assert nil? Sample.impl_for(1)
#     assert nil? Sample.impl_for(1.1)
#     assert nil? Sample.impl_for([])
#     assert nil? Sample.impl_for([1, 2, 3])
#     assert nil? Sample.impl_for({})
#     assert nil? Sample.impl_for({1, 2, 3})
#     assert nil? Sample.impl_for("foo")
#     assert nil? Sample.impl_for(<<1>>)
#     assert nil? Sample.impl_for(self)
#     assert nil? Sample.impl_for(%{})
#     assert nil? Sample.impl_for(hd(:erlang.ports))
#     assert nil? Sample.impl_for(make_ref)
#     assert nil? Sample.impl_for(Macro.Env[])
#
#     assert Sample.impl_for(ImplRec[]) ==
#            Sample.Protocol.ConsolidationTest.ImplRec
#     assert Sample.impl_for(NoImplRec[]) ==
#            nil
#   end
#
#   test "consolidated implementations with any and tuple fallback" do
#     assert WithAny.impl_for(ImplRec[])   == WithAny.Protocol.ConsolidationTest.ImplRec
#     assert WithAny.impl_for(NoImplRec[]) == WithAny.Tuple
#     assert WithAny.impl_for({ :foo })    == WithAny.Tuple
#     assert WithAny.impl_for({})          == WithAny.Tuple
#     assert WithAny.impl_for(self)        == WithAny.Any
#   end
#
#   test "consolidation keeps docs" do
#     docs = Sample.__info__(:docs)
#     assert { { :ok, 1 }, _, :def, [{ :thing, _, nil }], "Ok" } =
#            List.keyfind(docs, { :ok, 1 }, 0)
#   end
#
#   test "consolidated keeps callbacks" do
#     callbacks = lc { :callback, info } inlist Sample.__info__(:attributes), do: hd(info)
#     assert callbacks != []
#   end
#
#   test "consolidation errors on missing beams" do
#     defprotocol NoBeam, do: nil
#     assert Protocol.Consolidation.apply_to(String, [])  == { :error, :not_a_protocol }
#     assert Protocol.Consolidation.apply_to(NoBeam, [])  == { :error, :no_beam_info }
#   end
#
#   test "consolidation updates attributes" do
#     assert Sample.__info__(:attributes)[:protocol] == [fallback_to_any: false, consolidated: true]
#     assert WithAny.__info__(:attributes)[:protocol] == [fallback_to_any: true, consolidated: true]
#   end
#
#   test "consolidation extracts protocols" do
#     protos = Protocol.Consolidation.extract_protocols([:code.lib_dir(:elixir, :ebin)])
#     assert Enumerable in protos
#     assert Inspect in protos
#   end
#
#   test "consolidation extracts implementations" do
#     protos = Protocol.Consolidation.extract_impls(Enumerable, [:code.lib_dir(:elixir, :ebin)])
#     assert List in protos
#     assert Function in protos
#   end
# end
