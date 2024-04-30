Code.require_file("test_helper.exs", __DIR__)

defmodule ProtocolTest do
  use ExUnit.Case, async: true

  doctest Protocol

  {_, _, sample_binary, _} =
    defprotocol Sample do
      @type t :: any
      @doc "Ok"
      @deprecated "Reason"
      @spec ok(t) :: boolean
      def ok(term)
    end

  @sample_binary sample_binary

  {_, _, with_any_binary, _} =
    defprotocol WithAny do
      @fallback_to_any true
      @doc "Ok"
      def ok(term)
    end

  @with_any_binary with_any_binary

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
      @compile {:no_warn_undefined, Unknown}

      def ok(struct) do
        Unknown.undefined(struct)
      end
    end
  end

  defmodule ImplStructExplicitFor do
    defstruct a: 0, b: 0

    defimpl Sample, for: __MODULE__ do
      def ok(_struct), do: true
    end
  end

  test "protocol implementations without any" do
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
    assert is_nil(Sample.impl_for(%{}))
    assert is_nil(Sample.impl_for(self()))
    assert is_nil(Sample.impl_for(hd(:erlang.ports())))
    assert is_nil(Sample.impl_for(make_ref()))

    assert Sample.impl_for(%ImplStruct{}) == Sample.ProtocolTest.ImplStruct
    assert Sample.impl_for(%ImplStructExplicitFor{}) == Sample.ProtocolTest.ImplStructExplicitFor
    assert Sample.impl_for(%NoImplStruct{}) == nil
  end

  test "protocol implementation with Any and struct fallbacks" do
    assert WithAny.impl_for(%NoImplStruct{}) == WithAny.Any
    # Derived
    assert WithAny.impl_for(%ImplStruct{}) == ProtocolTest.WithAny.ProtocolTest.ImplStruct
    assert WithAny.impl_for(%{__struct__: "foo"}) == WithAny.Map
    assert WithAny.impl_for(%{}) == WithAny.Map
    assert WithAny.impl_for(self()) == WithAny.Any
  end

  test "protocol not implemented" do
    message = "protocol ProtocolTest.Sample not implemented for :foo of type Atom"

    assert_raise Protocol.UndefinedError, message, fn ->
      sample = String.to_atom("Elixir.ProtocolTest.Sample")
      sample.ok(:foo)
    end
  end

  test "protocol documentation and deprecated" do
    import PathHelpers

    write_beam(
      defprotocol SampleDocsProto do
        @doc "Ok"
        @deprecated "Reason"
        @spec ok(t) :: boolean
        def ok(term)
      end
    )

    write_beam(
      defimpl SampleDocsProto, for: List do
        def ok(_), do: true
      end
    )

    write_beam(
      defimpl SampleDocsProto, for: Map do
        @moduledoc "for map"

        def ok(_), do: true
      end
    )

    {:docs_v1, _, _, _, _, _, docs} = Code.fetch_docs(SampleDocsProto)

    assert {{:type, :t, 0}, _, [], %{"en" => type_doc}, _} = List.keyfind(docs, {:type, :t, 0}, 0)
    assert type_doc =~ "All the types that implement this protocol"

    assert {{:function, :ok, 1}, _, ["ok(term)"], %{"en" => "Ok"}, _} =
             List.keyfind(docs, {:function, :ok, 1}, 0)

    deprecated = SampleDocsProto.__info__(:deprecated)
    assert [{{:ok, 1}, "Reason"}] = deprecated

    {:docs_v1, _, _, _, :hidden, _, _} = Code.fetch_docs(SampleDocsProto.List)
    {:docs_v1, _, _, _, moduledoc, _, _} = Code.fetch_docs(SampleDocsProto.Map)
    assert moduledoc == %{"en" => "for map"}
  end

  @compile {:no_warn_undefined, WithAll}

  test "protocol keeps underlying UndefinedFunctionError" do
    assert_raise UndefinedFunctionError, fn ->
      WithAll.ok(%ImplStruct{})
    end
  end

  test "protocol defines callbacks" do
    assert [{:type, {13, 19}, :fun, args}] = get_callbacks(@sample_binary, :ok, 1)

    assert args == [
             {:type, {13, 19}, :product, [{:user_type, {13, 16}, :t, []}]},
             {:type, {13, 22}, :boolean, []}
           ]

    assert [{:type, 23, :fun, args}] = get_callbacks(@with_any_binary, :ok, 1)
    assert args == [{:type, 23, :product, [{:user_type, 23, :t, []}]}, {:type, 23, :term, []}]
  end

  test "protocol defines t/0 type with documentation" do
    assert {:type, {:t, {_, _, :any, []}, []}} = get_type(@sample_binary, :t, 0)
  end

  test "protocol defines functions and attributes" do
    assert Sample.__protocol__(:module) == Sample
    assert Sample.__protocol__(:functions) == [ok: 1]
    refute Sample.__protocol__(:consolidated?)
    assert Sample.__protocol__(:impls) == :not_consolidated
    assert Sample.__info__(:attributes)[:__protocol__] == [fallback_to_any: false]

    assert WithAny.__protocol__(:module) == WithAny
    assert WithAny.__protocol__(:functions) == [ok: 1]
    refute WithAny.__protocol__(:consolidated?)
    assert WithAny.__protocol__(:impls) == :not_consolidated
    assert WithAny.__info__(:attributes)[:__protocol__] == [fallback_to_any: true]
  end

  test "defimpl" do
    module = Module.concat(Sample, ImplStruct)
    assert module.__impl__(:for) == ImplStruct
    assert module.__impl__(:protocol) == Sample
    assert module.__info__(:attributes)[:__impl__] == [protocol: Sample, for: ImplStruct]
  end

  test "defimpl with implicit derive" do
    module = Module.concat(WithAny, ImplStruct)
    assert module.__impl__(:for) == ImplStruct
    assert module.__impl__(:protocol) == WithAny
    assert module.__info__(:attributes)[:__impl__] == [protocol: WithAny, for: ImplStruct]
  end

  test "defimpl with explicit derive" do
    module = Module.concat(Derivable, ImplStruct)
    assert module.__impl__(:for) == ImplStruct
    assert module.__impl__(:protocol) == Derivable
    assert module.__info__(:attributes)[:__impl__] == [protocol: Derivable, for: ImplStruct]
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

  test "defimpl without :for option when outside a module" do
    msg = "defimpl/3 expects a :for option when declared outside a module"

    assert_raise ArgumentError, msg, fn ->
      ast =
        quote do
          defimpl Sample do
            def ok(_term), do: true
          end
        end

      Code.eval_quoted(ast, [], %{__ENV__ | module: nil})
    end
  end

  defp get_callbacks(beam, name, arity) do
    {:ok, callbacks} = Code.Typespec.fetch_callbacks(beam)
    List.keyfind(callbacks, {name, arity}, 0) |> elem(1)
  end

  defp get_type(beam, name, arity) do
    {:ok, types} = Code.Typespec.fetch_types(beam)

    assert {:value, value} =
             :lists.search(&match?({_, {^name, _, args}} when length(args) == arity, &1), types)

    value
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

    struct = struct(AnotherStruct, a: 1, b: 1)
    assert Derivable.ok(struct) == {:ok, struct, struct(AnotherStruct), :ok}
  end

  test "derive protocol explicitly via API" do
    defmodule InlineStruct do
      defstruct a: 0, b: 0
    end

    require Protocol
    assert Protocol.derive(Derivable, InlineStruct, :oops) == :ok

    struct = struct(InlineStruct, a: 1, b: 1)
    assert Derivable.ok(struct) == {:ok, struct, struct(InlineStruct), :oops}
  end

  test "derived implementation keeps local file/line info" do
    assert ProtocolTest.WithAny.ProtocolTest.ImplStruct.__info__(:compile)[:source] ==
             String.to_charlist(__ENV__.file)
  end

  describe "warnings" do
    import ExUnit.CaptureIO

    test "with no definitions" do
      assert capture_io(:stderr, fn ->
               defprotocol SampleWithNoDefinitions do
               end
             end) =~ "protocols must define at least one function, but none was defined"
    end

    test "when @callbacks and friends are defined inside a protocol" do
      message =
        capture_io(:stderr, fn ->
          defprotocol SampleWithCallbacks do
            @spec with_specs(any(), keyword()) :: tuple()
            def with_specs(term, options \\ [])

            @spec with_specs_and_when(any(), opts) :: tuple() when opts: keyword
            def with_specs_and_when(term, options \\ [])

            def without_specs(term, options \\ [])

            @callback foo :: {:ok, term}
            @callback foo(term) :: {:ok, term}
            @callback foo(term, keyword) :: {:ok, term, keyword}

            @callback foo_when :: {:ok, x} when x: term
            @callback foo_when(x) :: {:ok, x} when x: term
            @callback foo_when(x, opts) :: {:ok, x, opts} when x: term, opts: keyword

            @macrocallback bar(term) :: {:ok, term}
            @macrocallback bar(term, keyword) :: {:ok, term, keyword}

            @optional_callbacks [foo: 1, foo: 2]
            @optional_callbacks [without_specs: 2]
          end
        end)

      assert message =~
               "cannot define @callback foo/0 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @callback foo/1 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @callback foo/2 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @callback foo_when/0 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @callback foo_when/1 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @callback foo_when/2 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @macrocallback bar/1 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @macrocallback bar/2 inside protocol, use def/1 to outline your protocol definition"

      assert message =~
               "cannot define @optional_callbacks inside protocol, all of the protocol definitions are required"
    end

    test "when deriving after struct" do
      assert capture_io(:stderr, fn ->
               defmodule DeriveTooLate do
                 defstruct []
                 @derive [{Derivable, :ok}]
               end
             end) =~
               "module attribute @derive was set after defstruct, all @derive calls must come before defstruct"
    end

    test "when deriving with no struct" do
      assert capture_io(:stderr, fn ->
               defmodule DeriveNeverUsed do
                 @derive [{Derivable, :ok}]
               end
             end) =~
               "module attribute @derive was set but never used (it must come before defstruct)"
    end
  end

  describe "errors" do
    test "cannot derive without any implementation" do
      assert_raise ArgumentError,
                   ~r"could not load module #{inspect(Sample.Any)} due to reason :nofile, cannot derive #{inspect(Sample)}",
                   fn ->
                     defmodule NotCompiled do
                       @derive [Sample]
                       defstruct hello: :world
                     end
                   end
    end
  end
end

defmodule Protocol.DebugInfoTest do
  use ExUnit.Case

  test "protocols always keep debug_info" do
    Code.compiler_options(debug_info: false)

    {:module, _, binary, _} =
      defprotocol DebugInfoProto do
        def example(info)
      end

    assert {:ok, {DebugInfoProto, [debug_info: debug_info]}} =
             :beam_lib.chunks(binary, [:debug_info])

    assert {:debug_info_v1, :elixir_erl, {:elixir_v1, _, _}} = debug_info
  after
    Code.compiler_options(debug_info: true)
  end
end
