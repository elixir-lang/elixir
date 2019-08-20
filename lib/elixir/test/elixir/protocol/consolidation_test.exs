Code.require_file("../test_helper.exs", __DIR__)

path = Path.expand("../../ebin", __DIR__)
File.mkdir_p!(path)

files = Path.wildcard(PathHelpers.fixture_path("consolidation/*"))
Kernel.ParallelCompiler.compile_to_path(files, path)

defmodule Protocol.ConsolidationTest do
  use ExUnit.Case, async: true
  alias Protocol.ConsolidationTest.{Sample, WithAny}

  defimpl WithAny, for: Map do
    def ok(map) do
      {:ok, map}
    end
  end

  defimpl WithAny, for: Any do
    def ok(any) do
      {:ok, any}
    end
  end

  defmodule NoImplStruct do
    defstruct a: 0, b: 0
  end

  defmodule ImplStruct do
    @derive [WithAny]
    defstruct a: 0, b: 0

    defimpl Sample do
      @compile {:no_warn_undefined, Unknown}

      def ok(struct) do
        Unknown.undefined(struct)
      end
    end
  end

  Code.append_path(path)

  # Any is ignored because there is no fallback
  :code.purge(Sample)
  :code.delete(Sample)
  {:ok, binary} = Protocol.consolidate(Sample, [Any, ImplStruct])
  :code.load_binary(Sample, 'protocol_test.exs', binary)

  @sample_binary binary

  # Any should be moved to the end
  :code.purge(WithAny)
  :code.delete(WithAny)
  {:ok, binary} = Protocol.consolidate(WithAny, [Any, ImplStruct, Map])
  :code.load_binary(WithAny, 'protocol_test.exs', binary)

  test "consolidated?/1" do
    assert Protocol.consolidated?(WithAny)
    refute Protocol.consolidated?(Enumerable)
  end

  test "consolidation prevents new implementations" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defimpl WithAny, for: Integer do
          def ok(_any), do: :ok
        end
      end)

    assert output =~ ~r"the .+WithAny protocol has already been consolidated"
  after
    :code.purge(WithAny.Atom)
    :code.delete(WithAny.Atom)
  end

  test "consolidated implementations without any" do
    assert is_nil(Sample.impl_for(:foo))
    assert is_nil(Sample.impl_for(fn x -> x end))
    assert is_nil(Sample.impl_for(1))
    assert is_nil(Sample.impl_for(1.1))
    assert is_nil(Sample.impl_for([]))
    assert is_nil(Sample.impl_for([1, 2, 3]))
    assert is_nil(Sample.impl_for({}))
    assert is_nil(Sample.impl_for({1, 2, 3}))
    assert is_nil(Sample.impl_for("foo"))
    assert is_nil(Sample.impl_for(<<1>>))
    assert is_nil(Sample.impl_for(self()))
    assert is_nil(Sample.impl_for(%{}))
    assert is_nil(Sample.impl_for(hd(:erlang.ports())))
    assert is_nil(Sample.impl_for(make_ref()))

    assert Sample.impl_for(%ImplStruct{}) == Sample.Protocol.ConsolidationTest.ImplStruct
    assert Sample.impl_for(%NoImplStruct{}) == nil
  end

  test "consolidated implementations with any and tuple fallback" do
    assert WithAny.impl_for(%NoImplStruct{}) == WithAny.Any
    # Derived
    assert WithAny.impl_for(%ImplStruct{}) == WithAny.Any
    assert WithAny.impl_for(%{__struct__: "foo"}) == WithAny.Map
    assert WithAny.impl_for(%{}) == WithAny.Map
    assert WithAny.impl_for(self()) == WithAny.Any
  end

  test "consolidation keeps docs" do
    {:ok, {Sample, [{'Docs', docs_bin}]}} = :beam_lib.chunks(@sample_binary, ['Docs'])
    {:docs_v1, _, _, _, _, _, docs} = :erlang.binary_to_term(docs_bin)
    ok_doc = List.keyfind(docs, {:function, :ok, 1}, 0)

    assert {{:function, :ok, 1}, _, ["ok(term)"], %{"en" => "Ok"}, _} = ok_doc
  end

  test "consolidation keeps chunks" do
    deprecated = [{{:ok, 1}, "Reason"}]
    assert deprecated == Sample.__info__(:deprecated)

    {:ok, {Sample, [{'ExCk', check_bin}]}} = :beam_lib.chunks(@sample_binary, ['ExCk'])
    assert {:elixir_checker_v1, contents} = :erlang.binary_to_term(check_bin)
    export_info = %{deprecated_reason: "Reason", kind: :def, type: [[var: 0]]}
    assert {{:ok, 1}, export_info} in contents.exports
  end

  test "consolidation keeps source" do
    assert Sample.__info__(:compile)[:source]
  end

  test "consolidated keeps callbacks" do
    {:ok, callbacks} = Code.Typespec.fetch_callbacks(@sample_binary)
    assert callbacks != []
  end

  test "consolidation errors on missing BEAM files" do
    defprotocol(NoBeam, do: nil)
    assert Protocol.consolidate(String, []) == {:error, :not_a_protocol}
    assert Protocol.consolidate(NoBeam, []) == {:error, :no_beam_info}
  end

  test "consolidation updates attributes" do
    assert Sample.__protocol__(:consolidated?)
    assert Sample.__protocol__(:impls) == {:consolidated, [ImplStruct]}
    assert WithAny.__protocol__(:consolidated?)
    assert WithAny.__protocol__(:impls) == {:consolidated, [Any, ImplStruct, Map]}
  end

  test "consolidation extracts protocols" do
    protos = Protocol.extract_protocols([:code.lib_dir(:elixir, :ebin)])
    assert Enumerable in protos
    assert Inspect in protos
  end

  test "consolidation extracts implementations with charlist path" do
    protos = Protocol.extract_impls(Enumerable, [:code.lib_dir(:elixir, :ebin)])
    assert List in protos
    assert Function in protos
  end

  test "consolidation extracts implementations with binary path" do
    protos = Protocol.extract_impls(Enumerable, [Application.app_dir(:elixir, "ebin")])
    assert List in protos
    assert Function in protos
  end

  test "protocol not implemented" do
    message =
      "protocol Protocol.ConsolidationTest.Sample not implemented for :foo of type Atom. " <>
        "This protocol is implemented for the following type(s): Protocol.ConsolidationTest.ImplStruct"

    assert_raise Protocol.UndefinedError, message, fn ->
      sample = Sample
      sample.ok(:foo)
    end
  end
end
