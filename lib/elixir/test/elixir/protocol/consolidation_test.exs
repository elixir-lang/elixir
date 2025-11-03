# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

path = Path.expand("../../ebin", __DIR__)
File.mkdir_p!(path)

files = Path.wildcard(PathHelpers.fixture_path("consolidation/*"))
Kernel.ParallelCompiler.compile_to_path(files, path, return_diagnostics: true)

defmodule Protocol.ConsolidationTest do
  use ExUnit.Case, async: true
  alias Protocol.ConsolidationTest.{Sample, WithAny, NoImpl}

  defimpl WithAny, for: Map do
    def ok(map, _opts) do
      {:ok, map}
    end
  end

  defimpl WithAny, for: Any do
    def ok(any, _opts) do
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
  :code.load_binary(Sample, ~c"protocol_test.exs", binary)

  defp sample_binary, do: unquote(binary)

  # Any should be moved to the end
  :code.purge(WithAny)
  :code.delete(WithAny)
  {:ok, binary} = Protocol.consolidate(WithAny, [Any, ImplStruct, Map])
  :code.load_binary(WithAny, ~c"protocol_test.exs", binary)

  defp with_any_binary, do: unquote(binary)

  # No Any
  :code.purge(NoImpl)
  :code.delete(NoImpl)
  {:ok, binary} = Protocol.consolidate(NoImpl, [])
  :code.load_binary(NoImpl, ~c"protocol_test.exs", binary)

  defp no_impl_binary, do: unquote(binary)

  test "consolidated?/1" do
    assert Protocol.consolidated?(WithAny)
    refute Protocol.consolidated?(Enumerable)
  end

  test "consolidation warns on new implementations" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defimpl WithAny, for: Integer do
          def ok(_any, _opts), do: :ok
        end
      end)

    assert output =~ ~r"the .+WithAny protocol has already been consolidated"
  after
    :code.purge(WithAny.Integer)
    :code.delete(WithAny.Integer)
  end

  test "consolidation warns on new implementations unless disabled" do
    Code.put_compiler_option(:ignore_already_consolidated, true)

    defimpl WithAny, for: Integer do
      def ok(_any), do: :ok
    end
  after
    Code.put_compiler_option(:ignore_already_consolidated, false)
    :code.purge(WithAny.Integer)
    :code.delete(WithAny.Integer)
  end

  test "consolidated implementations without fallback to any" do
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

  test "consolidated implementations with fallback to any" do
    assert WithAny.impl_for(%NoImplStruct{}) == WithAny.Any

    # Derived
    assert WithAny.impl_for(%ImplStruct{}) ==
             Protocol.ConsolidationTest.WithAny.Protocol.ConsolidationTest.ImplStruct

    assert WithAny.impl_for(%{__struct__: "foo"}) == WithAny.Map
    assert WithAny.impl_for(%{}) == WithAny.Map
    assert WithAny.impl_for(self()) == WithAny.Any
  end

  test "consolidation keeps docs" do
    {:ok, {Sample, [{~c"Docs", docs_bin}]}} = :beam_lib.chunks(sample_binary(), [~c"Docs"])
    {:docs_v1, _, _, _, _, _, docs} = :erlang.binary_to_term(docs_bin)
    ok_doc = List.keyfind(docs, {:function, :ok, 1}, 0)

    assert {{:function, :ok, 1}, _, ["ok(term)"], %{"en" => "Ok"}, _} = ok_doc
  end

  @tag :requires_source
  test "consolidation keeps source" do
    assert Sample.__info__(:compile)[:source]
  end

  test "consolidated keeps callbacks" do
    {:ok, callbacks} = Code.Typespec.fetch_callbacks(sample_binary())
    assert callbacks != []
  end

  test "consolidation updates attributes" do
    assert Sample.__protocol__(:consolidated?)
    assert Sample.__protocol__(:impls) == {:consolidated, [ImplStruct]}
    assert WithAny.__protocol__(:consolidated?)
    assert WithAny.__protocol__(:impls) == {:consolidated, [Any, Map, ImplStruct]}
    assert NoImpl.__protocol__(:consolidated?)
    assert NoImpl.__protocol__(:impls) == {:consolidated, []}
  end

  describe "exports" do
    import Module.Types.Descr
    alias Module.Types.Of

    defp exports(binary) do
      {:ok, {_, [{~c"ExCk", check_bin}]}} = :beam_lib.chunks(binary, [~c"ExCk"])
      assert {:elixir_checker_v3, contents} = :erlang.binary_to_term(check_bin)
      Map.new(contents.exports)
    end

    test "keeps deprecations" do
      deprecated = [{{:ok, 1}, "Reason"}]
      assert deprecated == Sample.__info__(:deprecated)

      assert %{{:ok, 1} => %{deprecated: "Reason", sig: _}} = exports(sample_binary())
    end

    test "defines signatures without fallback to any" do
      exports = exports(sample_binary())

      assert %{{:impl_for, 1} => %{sig: {:strong, domain, clauses}}} = exports
      assert domain == [term()]

      assert clauses == [
               {[Of.impl(ImplStruct)], atom([Sample.Protocol.ConsolidationTest.ImplStruct])},
               {[negation(Of.impl(ImplStruct))], atom([nil])}
             ]

      assert %{{:impl_for!, 1} => %{sig: {:strong, domain, clauses}}} = exports
      assert domain == [Of.impl(ImplStruct)]

      assert clauses == [
               {[Of.impl(ImplStruct)], atom([Sample.Protocol.ConsolidationTest.ImplStruct])}
             ]

      assert %{{:ok, 1} => %{sig: {:strong, nil, clauses}}} = exports

      assert clauses == [
               {[Of.impl(ImplStruct)], dynamic()}
             ]
    end

    test "defines signatures with fallback to any" do
      exports = exports(with_any_binary())

      assert %{
               {:impl_for, 1} => %{sig: {:strong, domain, clauses}},
               {:impl_for!, 1} => %{sig: {:strong, domain, clauses}}
             } = exports

      assert domain == [term()]

      assert clauses == [
               {[Of.impl(Map)], atom([WithAny.Map])},
               {[Of.impl(ImplStruct)], atom([WithAny.Protocol.ConsolidationTest.ImplStruct])},
               {[negation(union(Of.impl(ImplStruct), Of.impl(Map)))], atom([WithAny.Any])}
             ]

      assert %{{:ok, 2} => %{sig: {:strong, nil, clauses}}} = exports

      assert clauses == [
               {[term(), term()], dynamic()}
             ]
    end

    test "defines signatures without implementation" do
      exports = exports(no_impl_binary())

      assert %{{:impl_for, 1} => %{sig: {:strong, domain, clauses}}} = exports
      assert domain == [term()]
      assert clauses == [{[term()], atom([nil])}]

      assert %{{:impl_for!, 1} => %{sig: {:strong, domain, clauses}}} = exports
      assert domain == [none()]
      assert clauses == [{[none()], none()}]

      assert %{{:ok, 1} => %{sig: {:strong, nil, clauses}}} = exports
      assert clauses == [{[none()], dynamic()}]
    end
  end

  test "consolidation errors on missing BEAM files" do
    import PathHelpers

    write_beam(
      defmodule ExampleModule do
      end
    )

    defprotocol NoBeam do
      def example(arg)
    end

    assert Protocol.consolidate(ExampleModule, []) == {:error, :not_a_protocol}
    assert Protocol.consolidate(NoBeam, []) == {:error, :no_beam_info}
  end

  test "protocol not implemented" do
    message =
      "protocol Protocol.ConsolidationTest.Sample not implemented for Atom. " <>
        "This protocol is implemented for: Protocol.ConsolidationTest.ImplStruct" <>
        "\n\nGot value:\n\n    :foo\n"

    assert_raise Protocol.UndefinedError, message, fn ->
      sample = String.to_atom("Elixir.Protocol.ConsolidationTest.Sample")
      sample.ok(:foo)
    end
  end

  describe "extraction" do
    test "protocols" do
      protos = Protocol.extract_protocols([Application.app_dir(:elixir, "ebin")])
      assert Enumerable in protos
      assert Inspect in protos
    end

    test "implementations with charlist path" do
      protos =
        Protocol.extract_impls(Enumerable, [to_charlist(Application.app_dir(:elixir, "ebin"))])

      assert List in protos
      assert Function in protos
    end

    test "implementations with binary path" do
      protos = Protocol.extract_impls(Enumerable, [Application.app_dir(:elixir, "ebin")])
      assert List in protos
      assert Function in protos
    end
  end
end
