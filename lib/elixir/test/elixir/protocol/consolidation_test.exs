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
  alias Protocol.ConsolidationTest.{Sample, WithAny, NoImpl, Multi}

  defimpl Multi, for: Atom do
    def first(atom), do: {:first, :atom, atom}
    def second(atom, arg), do: {:second, :atom, atom, arg}
  end

  defimpl Multi, for: Map do
    def first(map), do: {:first, :map, map}
    def second(map, arg), do: {:second, :map, map, arg}
  end

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

    defimpl Multi do
      def first(struct), do: {:first, :struct, struct}
      def second(struct, arg), do: {:second, :struct, struct, arg}
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

  # Multiple functions
  :code.purge(Multi)
  :code.delete(Multi)
  {:ok, binary} = Protocol.consolidate(Multi, [Atom, ImplStruct, Map])
  :code.load_binary(Multi, ~c"protocol_test.exs", binary)

  defp multi_binary, do: unquote(binary)

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

  test "consolidated dispatch without fallback to any" do
    assert Multi.first(:foo) == {:first, :atom, :foo}
    assert Multi.second(:foo, 1) == {:second, :atom, :foo, 1}
    assert Multi.first(%{}) == {:first, :map, %{}}
    assert Multi.second(%{}, 2) == {:second, :map, %{}, 2}
    assert Multi.first(%ImplStruct{}) == {:first, :struct, %ImplStruct{}}
    assert Multi.second(%ImplStruct{}, 3) == {:second, :struct, %ImplStruct{}, 3}

    # Maps with a non-atom __struct__ are not structs
    assert Multi.first(%{__struct__: "foo"}) == {:first, :map, %{__struct__: "foo"}}

    # Structs do not fall back to the Map implementation
    assert_raise Protocol.UndefinedError, fn -> Multi.first(%NoImplStruct{}) end
    assert_raise Protocol.UndefinedError, fn -> Multi.second(%NoImplStruct{}, 1) end
    assert_raise Protocol.UndefinedError, fn -> Multi.second(1, 1) end
    assert_raise Protocol.UndefinedError, fn -> NoImpl.ok(:foo) end
  end

  test "consolidated dispatch with fallback to any" do
    assert WithAny.ok(%{}, :opts) == {:ok, %{}}
    assert WithAny.ok(%NoImplStruct{}, :opts) == {:ok, %NoImplStruct{}}
    assert WithAny.ok(%ImplStruct{}, :opts) == {:ok, %ImplStruct{}}
    assert WithAny.ok(self(), :opts) == {:ok, self()}
  end

  test "consolidates an already consolidated protocol" do
    path = PathHelpers.tmp_path("consolidation/Elixir.Protocol.ConsolidationTest.Multi.beam")
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, multi_binary())

    :code.purge(Multi)
    {:module, Multi} = :code.load_abs(String.to_charlist(Path.rootname(path)))
    assert {:ok, binary} = Protocol.consolidate(Multi, [Atom, ImplStruct, Map])

    :code.purge(Multi)
    :code.load_binary(Multi, ~c"protocol_test.exs", binary)
    assert Multi.second(:foo, 1) == {:second, :atom, :foo, 1}
    assert Multi.first(%ImplStruct{}) == {:first, :struct, %ImplStruct{}}
    assert Multi.impl_for(%ImplStruct{}) == Multi.Protocol.ConsolidationTest.ImplStruct
  after
    :code.purge(Multi)
    :code.load_binary(Multi, ~c"protocol_test.exs", multi_binary())
  end

  test "consolidation replaces struct_impl_for/1" do
    for binary <- [sample_binary(), with_any_binary(), no_impl_binary(), multi_binary()] do
      {:ok, {_, [debug_info: {:debug_info_v1, :elixir_erl, {:elixir_v1, map, _}}]}} =
        :beam_lib.chunks(binary, [:debug_info])

      refute List.keymember?(map.definitions, {:struct_impl_for, 1}, 0)
      refute {:inline, [struct_impl_for: 1]} in map.compile_opts
    end
  end

  test "consolidated dispatch does not look up implementations at runtime" do
    for binary <- [sample_binary(), with_any_binary(), no_impl_binary(), multi_binary()] do
      {:beam_file, _, _, _, _, functions} = :beam_disasm.file(binary)

      for {:function, name, arity, _, instructions} <- functions,
          instruction <- instructions,
          is_tuple(instruction) do
        refute elem(instruction, 0) in [:apply, :apply_last],
               "#{name}/#{arity} has #{inspect(instruction)}"
      end
    end
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
      assert {:elixir_checker_v10, contents} = :erlang.binary_to_term(check_bin)
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
               {[Of.impl(ImplStruct, :open)],
                atom([Sample.Protocol.ConsolidationTest.ImplStruct])},
               {[opt_negation(Of.impl(ImplStruct, :open))], atom([nil])}
             ]

      assert %{{:impl_for!, 1} => %{sig: {:strong, domain, clauses}}} = exports
      assert domain == [Of.impl(ImplStruct, :open)]

      assert clauses == [
               {[Of.impl(ImplStruct, :open)],
                atom([Sample.Protocol.ConsolidationTest.ImplStruct])}
             ]

      assert %{{:ok, 1} => %{sig: {:strong, nil, clauses}}} = exports

      assert clauses == [
               {[Of.impl(ImplStruct, :open)], dynamic()}
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
               {[Of.impl(Map, :open)], atom([WithAny.Map])},
               {[Of.impl(ImplStruct, :open)],
                atom([WithAny.Protocol.ConsolidationTest.ImplStruct])},
               {[opt_negation(opt_union(Of.impl(ImplStruct, :open), Of.impl(Map, :open)))],
                atom([WithAny.Any])}
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
      sample = String.to_unsafe_atom("Elixir.Protocol.ConsolidationTest.Sample")
      sample.ok(:foo)
    end
  end

  describe "extraction" do
    test "protocols" do
      protos = Protocol.extract_protocols([Application.app_dir(:elixir, "ebin")])
      assert Enumerable in protos
      assert Inspect in protos
    end

    test "protocols with expanded path" do
      path = to_charlist(Application.app_dir(:elixir, "ebin"))
      {:ok, mods} = :file.list_dir(path)
      protos = Protocol.extract_protocols([{path, mods}])
      assert Enumerable in protos
      assert Inspect in protos
    end

    test "implementations with charlist path" do
      impls =
        Protocol.extract_impls(Enumerable, [to_charlist(Application.app_dir(:elixir, "ebin"))])

      assert List in impls
      assert Function in impls
    end

    test "implementations with binary path" do
      impls = Protocol.extract_impls(Enumerable, [Application.app_dir(:elixir, "ebin")])
      assert List in impls
      assert Function in impls
    end

    test "implementations with expanded path" do
      path = to_charlist(Application.app_dir(:elixir, "ebin"))
      {:ok, mods} = :file.list_dir(path)
      impls = Protocol.extract_impls(Enumerable, [{path, mods}])
      assert List in impls
      assert Function in impls
    end
  end
end
