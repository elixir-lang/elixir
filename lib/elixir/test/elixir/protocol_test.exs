Code.require_file "test_helper.exs", __DIR__

defmodule ProtocolTest do
  use ExUnit.Case, async: true

  doctest Protocol

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

  defprotocol Derivable do
    def ok(a)
  end

  defimpl Derivable, for: Any do
    defmacro __deriving__(module, struct, options) do
      quote do
        defimpl Derivable, for: unquote(module) do
          def ok(arg) do
            {:ok, arg, unquote(Macro.escape(struct)), unquote(options)}
          end
        end
      end
    end

    def ok(arg) do
      {:ok, arg}
    end
  end

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
    @derive [WithAny, Derivable]
    defstruct a: 0, b: 0

    defimpl Sample do
      def ok(struct) do
        Unknown.undefined(struct)
      end
    end
  end

  test "protocol implementations without any" do
    assert is_nil Sample.impl_for(:foo)
    assert is_nil Sample.impl_for(fn(x) -> x end)
    assert is_nil Sample.impl_for(1)
    assert is_nil Sample.impl_for(1.1)
    assert is_nil Sample.impl_for([])
    assert is_nil Sample.impl_for([1, 2, 3])
    assert is_nil Sample.impl_for({})
    assert is_nil Sample.impl_for({1, 2, 3})
    assert is_nil Sample.impl_for("foo")
    assert is_nil Sample.impl_for(<<1>>)
    assert is_nil Sample.impl_for(%{})
    assert is_nil Sample.impl_for(self)
    assert is_nil Sample.impl_for(hd(:erlang.ports))
    assert is_nil Sample.impl_for(make_ref)

    assert Sample.impl_for(%ImplStruct{}) ==
           Sample.ProtocolTest.ImplStruct
    assert Sample.impl_for(%NoImplStruct{}) ==
           nil
  end

  test "protocol implementation with any and structs fallback" do
    assert WithAny.impl_for(%NoImplStruct{})      == WithAny.Any
    assert WithAny.impl_for(%ImplStruct{})        == WithAny.Any # Derived
    assert WithAny.impl_for(%{__struct__: "foo"}) == WithAny.Map
    assert WithAny.impl_for(%{})                  == WithAny.Map
    assert WithAny.impl_for(self)                 == WithAny.Any
  end

  test "protocol not implemented" do
    assert_raise Protocol.UndefinedError, "protocol ProtocolTest.Sample not implemented for :foo", fn ->
      Sample.ok(:foo)
    end
  end

  test "protocol documentation" do
    import PathHelpers

    write_beam(defprotocol SampleDocsProto do
      @type t :: any
      @doc "Ok"
      @spec ok(t) :: boolean
      def ok(thing)
    end)

    docs = Code.get_docs(SampleDocsProto, :docs)
    assert {{:ok, 1}, _, :def, [{:thing, _, nil}], "Ok"} =
           List.keyfind(docs, {:ok, 1}, 0)
  end

  test "protocol keeps underlying UndefinedFunctionError" do
    assert_raise UndefinedFunctionError, fn ->
      WithAll.ok(%ImplStruct{})
    end
  end

  test "protocol defines callbacks" do
    assert get_callbacks(Sample, :ok, 1) ==
      [{:type, [11], :fun, [{:type, [11], :product, [{:user_type, [11], :t, []}]}, {:type, [11], :boolean, []}]}]

    assert get_callbacks(WithAny, :ok, 1) ==
      [{:type, [18], :fun, [{:type, [18], :product, [{:user_type, [18], :t, []}]}, {:type, [18], :term, []}]}]
  end

  test "protocol defines functions and attributes" do
    assert Sample.__protocol__(:module) == Sample
    assert Sample.__protocol__(:functions) == [ok: 1]
    refute Sample.__protocol__(:consolidated?)
    assert Sample.__info__(:attributes)[:protocol] == [fallback_to_any: false]

    assert WithAny.__protocol__(:module) == WithAny
    assert WithAny.__protocol__(:functions) == [ok: 1]
    refute WithAny.__protocol__(:consolidated?)
    assert WithAny.__info__(:attributes)[:protocol] == [fallback_to_any: true]
  end

  test "defimpl" do
    module = Module.concat(Sample, ImplStruct)
    assert module.__impl__(:for) == ImplStruct
    assert module.__impl__(:target) == module
    assert module.__impl__(:protocol) == Sample
    assert module.__info__(:attributes)[:impl] ==
           [protocol: Sample, for: ImplStruct]
  end

  test "defimpl with implicit derive" do
    module = Module.concat(WithAny, ImplStruct)
    assert module.__impl__(:for) == ImplStruct
    assert module.__impl__(:target) == WithAny.Any
    assert module.__impl__(:protocol) == WithAny
    assert module.__info__(:attributes)[:impl] ==
           [protocol: WithAny, for: ImplStruct]
  end

  test "defimpl with explicit derive" do
    module = Module.concat(Derivable, ImplStruct)
    assert module.__impl__(:for) == ImplStruct
    assert module.__impl__(:target) == module
    assert module.__impl__(:protocol) == Derivable
    assert module.__info__(:attributes)[:impl] ==
           [protocol: Derivable, for: ImplStruct]
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
    callbacks = for {:callback, info} <- module.__info__(:attributes), do: hd(info)
    List.keyfind(callbacks, {name, arity}, 0) |> elem(1)
  end

  test "derives protocol implicitly" do
    struct = %ImplStruct{a: 1, b: 1}
    assert WithAny.ok(struct) == {:ok, struct}

    struct = %NoImplStruct{a: 1, b: 1}
    assert WithAny.ok(struct) == {:ok, struct}
  end

  test "derives protocol explicitly" do
    struct = %ImplStruct{a: 1, b: 1}
    assert Derivable.ok(struct) == {:ok, struct, %ImplStruct{}, []}

    assert_raise Protocol.UndefinedError, fn ->
      struct = %NoImplStruct{a: 1, b: 1}
      Derivable.ok(struct)
    end
  end

  test "derives protocol explicitly with options" do
    defmodule AnotherStruct do
      @derive [{Derivable, :ok}]
      @derive [WithAny]
      defstruct a: 0, b: 0
    end

    struct = struct AnotherStruct, a: 1, b: 1
    assert Derivable.ok(struct) ==
           {:ok, struct, struct(AnotherStruct), :ok}
  end

  test "derive protocol explicitly via API" do
    defmodule InlineStruct do
      defstruct a: 0, b: 0
    end

    require Protocol
    assert Protocol.derive(Derivable, InlineStruct, :oops) == :ok

    struct = struct InlineStruct, a: 1, b: 1
    assert Derivable.ok(struct) ==
           {:ok, struct, struct(InlineStruct), :oops}
  end

  test "derived implementation keeps local file/line info" do
    assert ProtocolTest.WithAny.ProtocolTest.ImplStruct.__info__(:compile)[:source] ==
           String.to_char_list(__ENV__.file)
  end

  test "cannot derive without any implementation" do
    assert_raise ArgumentError,
        ~r"#{inspect Sample.Any} is not available, cannot derive #{inspect Sample}", fn ->
      defmodule NotCompiled do
        @derive [Sample]
        defstruct hello: :world
      end
    end
  end
end

path = Path.expand("../ebin", __DIR__)
File.mkdir_p!(path)

compile = fn {:module, module, binary, _} ->
  File.write!("#{path}/#{module}.beam", binary)
end

defmodule Protocol.ConsolidationTest do
  use ExUnit.Case, async: true

  compile.(
    defprotocol Sample do
      @type t :: any
      @doc "Ok"
      @spec ok(t) :: boolean
      def ok(thing)
    end
  )

  compile.(
    defprotocol WithAny do
      @fallback_to_any true
      @doc "Ok"
      def ok(thing)
    end
  )

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
    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
      defimpl WithAny, for: Integer do
        def ok(_any), do: :ok
      end
    end) =~ ~r"warning: the .+WithAny protocol has already been consolidated"
  after
    :code.purge(WithAny.Atom)
    :code.delete(WithAny.Atom)
  end

  test "consolidated implementations without any" do
    assert is_nil Sample.impl_for(:foo)
    assert is_nil Sample.impl_for(fn(x) -> x end)
    assert is_nil Sample.impl_for(1)
    assert is_nil Sample.impl_for(1.1)
    assert is_nil Sample.impl_for([])
    assert is_nil Sample.impl_for([1, 2, 3])
    assert is_nil Sample.impl_for({})
    assert is_nil Sample.impl_for({1, 2, 3})
    assert is_nil Sample.impl_for("foo")
    assert is_nil Sample.impl_for(<<1>>)
    assert is_nil Sample.impl_for(self)
    assert is_nil Sample.impl_for(%{})
    assert is_nil Sample.impl_for(hd(:erlang.ports))
    assert is_nil Sample.impl_for(make_ref)

    assert Sample.impl_for(%ImplStruct{}) ==
           Sample.Protocol.ConsolidationTest.ImplStruct
    assert Sample.impl_for(%NoImplStruct{}) ==
           nil
  end

  test "consolidated implementations with any and tuple fallback" do
    assert WithAny.impl_for(%NoImplStruct{})      == WithAny.Any
    assert WithAny.impl_for(%ImplStruct{})        == WithAny.Any # Derived
    assert WithAny.impl_for(%{__struct__: "foo"}) == WithAny.Map
    assert WithAny.impl_for(%{})                  == WithAny.Map
    assert WithAny.impl_for(self)                 == WithAny.Any
  end

  test "consolidation keeps docs" do
    docs = Code.get_docs(Sample, :docs)
    assert {{:ok, 1}, _, :def, [{:thing, _, nil}], "Ok"} =
           List.keyfind(docs, {:ok, 1}, 0)
  end

  test "consolidated keeps callbacks" do
    callbacks = for {:callback, info} <- Sample.__info__(:attributes), do: hd(info)
    assert callbacks != []
  end

  test "consolidation errors on missing beams" do
    defprotocol NoBeam, do: nil
    assert Protocol.consolidate(String, [])  == {:error, :not_a_protocol}
    assert Protocol.consolidate(NoBeam, [])  == {:error, :no_beam_info}
  end

  test "consolidation updates attributes" do
    assert Sample.__protocol__(:consolidated?)
    assert WithAny.__protocol__(:consolidated?)
  end

  test "consolidation extracts protocols" do
    protos = Protocol.extract_protocols([:code.lib_dir(:elixir, :ebin)])
    assert Enumerable in protos
    assert Inspect in protos
  end

  test "consolidation extracts implementations" do
    protos = Protocol.extract_impls(Enumerable, [:code.lib_dir(:elixir, :ebin)])
    assert List in protos
    assert Function in protos
  end
end
