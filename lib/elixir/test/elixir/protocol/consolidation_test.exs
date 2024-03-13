Code.require_file("../test_helper.exs", __DIR__)

path = Path.expand("../../ebin", __DIR__)
File.mkdir_p!(path)

files = Path.wildcard(PathHelpers.fixture_path("consolidation/*"))
Kernel.ParallelCompiler.compile_to_path(files, path)

defmodule Protocol.ConsolidationTest do
  use ExUnit.Case, async: true

  alias Protocol.ConsolidationTest.{
    Sample,
    WithAny,
    WithDeclaredTypes,
    WithExplicitTypeT
  }

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
  :code.load_binary(Sample, ~c"protocol_test.exs", binary)

  @sample_binary binary

  # Any should be moved to the end
  :code.purge(WithAny)
  :code.delete(WithAny)
  {:ok, binary} = Protocol.consolidate(WithAny, [Any, ImplStruct, Map])
  :code.load_binary(WithAny, ~c"protocol_test.exs", binary)

  test "consolidated?/1" do
    assert Protocol.consolidated?(WithAny)
    refute Protocol.consolidated?(Enumerable)
  end

  test "consolidation warns on new implementations" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defimpl WithAny, for: Integer do
          def ok(_any), do: :ok
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
    assert WithAny.impl_for(%ImplStruct{}) ==
             Protocol.ConsolidationTest.WithAny.Protocol.ConsolidationTest.ImplStruct

    assert WithAny.impl_for(%{__struct__: "foo"}) == WithAny.Map
    assert WithAny.impl_for(%{}) == WithAny.Map
    assert WithAny.impl_for(self()) == WithAny.Any
  end

  test "consolidation keeps docs" do
    {:ok, {Sample, [{~c"Docs", docs_bin}]}} = :beam_lib.chunks(@sample_binary, [~c"Docs"])
    {:docs_v1, _, _, _, _, _, docs} = :erlang.binary_to_term(docs_bin)
    ok_doc = List.keyfind(docs, {:function, :ok, 1}, 0)

    assert {{:function, :ok, 1}, _, ["ok(term)"], %{"en" => "Ok"}, _} = ok_doc
  end

  test "consolidation keeps chunks" do
    deprecated = [{{:ok, 1}, "Reason"}]
    assert deprecated == Sample.__info__(:deprecated)

    {:ok, {Sample, [{~c"ExCk", check_bin}]}} = :beam_lib.chunks(@sample_binary, [~c"ExCk"])
    assert {:elixir_checker_v1, contents} = :erlang.binary_to_term(check_bin)
    export_info = %{deprecated_reason: "Reason", kind: :def}
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
    defprotocol NoBeam do
      def example(arg)
    end

    assert Protocol.consolidate(String, []) == {:error, :not_a_protocol}
    assert Protocol.consolidate(NoBeam, []) == {:error, :no_beam_info}
  end

  test "consolidation updates attributes" do
    assert Sample.__protocol__(:consolidated?)
    assert Sample.__protocol__(:impls) == {:consolidated, [ImplStruct]}
    assert WithAny.__protocol__(:consolidated?)
    assert WithAny.__protocol__(:impls) == {:consolidated, [Any, Map, ImplStruct]}
  end

  test "consolidation extracts protocols" do
    protos = Protocol.extract_protocols([Application.app_dir(:elixir, "ebin")])
    assert Enumerable in protos
    assert Inspect in protos
  end

  test "consolidation extracts implementations with charlist path" do
    protos =
      Protocol.extract_impls(Enumerable, [to_charlist(Application.app_dir(:elixir, "ebin"))])

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

  describe "consolidation of type t()" do
    test "does not change type t() if it has been exlicitly declared (when protocol has multiple impls)" do
      {:ok, types} = Code.Typespec.fetch_types(WithExplicitTypeT)
      original_type_t = extract_type(types, :t)

      assert {:type, anno, :atom, []} = original_type_t

      {:ok, binary} = Protocol.consolidate(WithExplicitTypeT, impl_types())
      {:ok, types} = Code.Typespec.fetch_types(binary)
      consolidated_type_t = extract_type(types, :t)

      assert {:type, ^anno, :atom, []} = consolidated_type_t
    end

    test "does not change type t() if it has been exlicitly declared (when protocol has single impl)" do
      {:ok, types} = Code.Typespec.fetch_types(WithExplicitTypeT)
      original_type_t = extract_type(types, :t)

      assert {:type, anno, :atom, []} = original_type_t

      {:ok, binary} = Protocol.consolidate(WithExplicitTypeT, [Tuple])
      {:ok, types} = Code.Typespec.fetch_types(binary)
      consolidated_type_t = extract_type(types, :t)

      assert {:type, ^anno, :atom, []} = consolidated_type_t
    end

    test "does not change type t() if protocol has no implementations" do
      {:ok, types} = Code.Typespec.fetch_types(WithDeclaredTypes)
      original_type_t = extract_type(types, :t)

      assert original_type_t == {:type, 1, :term, []}

      {:ok, binary} = Protocol.consolidate(WithDeclaredTypes, [])
      {:ok, types} = Code.Typespec.fetch_types(binary)
      consolidated_type_t = extract_type(types, :t)

      assert consolidated_type_t == {:type, 1, :term, []}
    end

    test "does not change type t() if Any is an implementing type" do
      {:ok, types} = Code.Typespec.fetch_types(WithDeclaredTypes)
      original_type_t = extract_type(types, :t)

      assert original_type_t == {:type, 1, :term, []}

      {:ok, binary} = Protocol.consolidate(WithDeclaredTypes, [Any] ++ impl_types())
      {:ok, types} = Code.Typespec.fetch_types(binary)
      consolidated_type_t = extract_type(types, :t)

      assert consolidated_type_t == {:type, 1, :term, []}
    end

    test "produces ASTs that match compiled typespecs, for all possible impl types" do
      {:ok, compiled_types} = Code.Typespec.fetch_types(WithDeclaredTypes)

      impl_types()
      |> Enum.each(fn type ->
        compiled_type = extract_type(compiled_types, declared_type_name(type))

        {:ok, binary} = Protocol.consolidate(WithDeclaredTypes, [type])
        {:ok, consolidated_types} = Code.Typespec.fetch_types(binary)
        consolidated_type_t = extract_type(consolidated_types, :t)

        assert consolidated_type_t == strip_anno(compiled_type)
        assert consolidated_type_t == ast_for(type)
      end)
    end

    test "constructs a union type when the protocol has multiple implementations" do
      {:ok, original_types} = Code.Typespec.fetch_types(WithDeclaredTypes)
      compiled_union_type = extract_type(original_types, :impl_types)

      {:ok, binary} = Protocol.consolidate(WithDeclaredTypes, impl_types())
      {:ok, consolidated_types} = Code.Typespec.fetch_types(binary)
      type_t = extract_type(consolidated_types, :t)

      assert elem(type_t, 2) == :union
      assert elem(compiled_union_type, 2) == :union

      actual_member_types =
        elem(type_t, 3)
        |> Enum.sort()

      expected_member_types =
        elem(compiled_union_type, 3)
        |> Enum.map(&strip_anno/1)
        |> Enum.sort()

      assert length(actual_member_types) == length(impl_types())
      assert actual_member_types == expected_member_types
    end

    defp extract_type(types, type_name) do
      types
      |> Enum.find_value(fn
        {:type, {^type_name, type, _args}} ->
          type

        _ ->
          false
      end)
    end

    def strip_anno({:type, _anno1, :map, [{:type, _anno2, :map_field_exact, args}]}) do
      {:type, 1, :map, [{:type, 1, :map_field_exact, args}]}
    end

    def strip_anno({:type, _anno, type, args}) do
      {:type, 1, type, args}
    end

    defp impl_types() do
      [
        Tuple,
        Atom,
        List,
        BitString,
        Integer,
        Float,
        Function,
        PID,
        Map,
        Port,
        Reference,
        ImplStruct
      ]
    end

    defp declared_type_name(module_name) do
      module_name
      |> Atom.to_string()
      |> String.split(".")
      |> List.last()
      |> String.downcase()
      |> Kernel.<>("_type")
      |> String.to_atom()
    end

    defp ast_for(impl_type) do
      %{
        Tuple => {:type, 1, :tuple, :any},
        Atom => {:type, 1, :atom, []},
        List => {:type, 1, :list, []},
        BitString => {:type, 1, :bitstring, []},
        Integer => {:type, 1, :integer, []},
        Float => {:type, 1, :float, []},
        Function => {:type, 1, :function, []},
        PID => {:type, 1, :pid, []},
        Map => {:type, 1, :map, :any},
        Port => {:type, 1, :port, []},
        Reference => {:type, 1, :reference, []},
        ImplStruct =>
          {:type, 1, :map,
           [
             {:type, 1, :map_field_exact,
              [{:atom, 0, :__struct__}, {:atom, 0, Protocol.ConsolidationTest.ImplStruct}]}
           ]}
      }
      |> Map.get(impl_type)
    end
  end
end
