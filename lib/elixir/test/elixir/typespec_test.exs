Code.require_file "test_helper.exs", __DIR__

defmodule Typespec.TypeTest do
  use ExUnit.Case, async: true

  # This macro allows us to focus on the result of the
  # definition and not on the hassles of handling test
  # module
  defmacrop test_module([{:do, block}]) do
    quote do
      { :module, T, _binary, result } = defmodule T do
        unquote(block)
      end
      :code.delete(T)
      :code.purge(T)
      result
    end
  end

  test "invalid type specification" do
    assert_raise CompileError, %r"invalid type specification mytype = 1", fn ->
      test_module do
        @type mytype = 1
      end
    end
    assert_raise CompileError, %r"invalid type specification mytype = 1", fn ->
      test_module do
        @typep mytype = 1
      end
    end
    assert_raise CompileError, %r"invalid type specification mytype = 1", fn ->
      test_module do
        @opaque mytype = 1
      end
    end
  end

  test "invalid function specification" do
    assert_raise CompileError, %r"invalid function type specification myfun = 1", fn ->
      test_module do
        @spec myfun = 1
      end
    end
  end

  test "@type with no body (defaults to 'term')" do
    spec = test_module do
      @type mytype
    end
    assert {:mytype, {:type, _, :term, []}, []} = spec
  end

  test "@type with a single type" do
    spec = test_module do
      @type mytype :: term
    end
    assert {:mytype, {:type, _, :term, []}, []} = spec
  end

  test "@type with an atom" do
    spec = test_module do
      @type mytype :: :atom
    end
    assert {:mytype, {:atom, _, :atom}, []} = spec
  end

  test "@type with an atom alias" do
    spec = test_module do
      @type mytype :: Atom
    end
    assert {:mytype, {:atom, _, Atom}, []} = spec
  end

  test "@type with an integer" do
    spec = test_module do
      @type mytype :: 10
    end
    assert {:mytype, {:integer, _, 10}, []} = spec
  end

  test "@type with a negative integer" do
    spec = test_module do
      @type mytype :: -10
    end
    assert {:mytype, {:op, _, :-, {:integer, _, 10}}, []} = spec
  end

  test "@type with a remote type" do
    {spec1, spec2} = test_module do
      t1 = @type mytype :: Remote.Some.type
      t2 = @type mytype_arg :: Remote.type(integer)
      {t1, t2}
    end
    assert {:mytype, {:remote_type, _, [{:atom, _, Remote.Some}, {:atom, _, :type}, []]}, []} = spec1
    assert {:mytype_arg, {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :type}, [{:type, _, :integer, []}]]}, []} = spec2
  end

  test "@type with a binary" do
    spec = test_module do
      @type mytype :: binary
    end
    assert {:mytype, {:type, _, :binary, []}, []} = spec
  end

  test "@type with an empty binary" do
    spec = test_module do
      @type mytype :: <<>>
    end
    assert {:mytype, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 0}]}, []} = spec
  end

  test "@type with a binary with a base size" do
    spec = test_module do
      @type mytype :: <<_ :: 3>>
    end
    assert {:mytype, {:type, _, :binary, [{:integer, _, 3}, {:integer, _, 0}]}, []} = spec
  end

  test "@type with a binary with a unit size" do
    spec = test_module do
      @type mytype :: <<_ :: _ * 8>>
    end
    assert {:mytype, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 8}]}, []} = spec
  end

  test "@type with a range" do
    spec = test_module do
      @type mytype :: range(1, 10)
    end
    assert {:mytype, {:type, _, :range, [{:integer, _, 1}, {:integer, _, 10}]}, []} = spec
  end

  test "@type with a range op" do
    spec = test_module do
      @type mytype :: 1..10
    end
    assert {:mytype, {:type, _, :range, [{:integer, _, 1}, {:integer, _, 10}]}, []} = spec
  end

  test "@type with a tuple" do
    {spec1, spec2} = test_module do
      t1 = @type mytype :: tuple
      t2 = @type mytype1 :: {}
      {t1, t2}
    end
    assert {:mytype, {:type, _, :tuple, :any}, []} = spec1
    assert {:mytype1, {:type, _, :tuple, :any}, []} = spec2
  end

  test "@type with list shortcuts" do
    {spec1, spec2, spec3} = test_module do
      t1 = @type mytype :: []
      t2 = @type mytype1 :: [integer]
      t3 = @type mytype2 :: [integer, ...]
      {t1, t2, t3}
    end
    assert {:mytype, {:type, _, :nil, []}, []} = spec1
    assert {:mytype1, {:type, _, :list, [{:type, _, :integer, []}]}, []} = spec2
    assert {:mytype2, {:type, _, :nonempty_list, [{:type, _, :integer, []}]}, []} = spec3
  end

  test "@type with a fun" do
    spec = test_module do
      @type mytype :: (... -> any)
    end
    assert {:mytype, {:type, _, :fun, []}, []} = spec
  end

  test "@type with a fun with multiple arguments and return type" do
    t = test_module do
      @type mytype :: (integer, integer -> integer)
    end
    assert {:mytype, {:type, _, :fun, [{:type, _, :product,
             [{:type, _, :integer, []}, {:type, _, :integer, []}]},
             {:type, _, :integer, []}]}, []} = t
  end

  test "@type with a fun with no arguments and return type" do
    spec = test_module do
      @type mytype :: (() -> integer)
    end
    assert {:mytype, {:type, _, :fun, [{:type, _, :product, []},
             {:type, _, :integer, []}]}, []} = spec
  end

  test "@type with a fun with any arity and return type" do
    spec = test_module do
      @type mytype :: (... -> integer)
    end
    assert {:mytype, {:type, _, :fun, [{:type, _, :any},
             {:type, _, :integer, []}]}, []} = spec
  end

  test "@type with a union" do
    spec = test_module do
      @type mytype :: integer | char_list
                    | atom
    end
    assert {:mytype, {:type, _, :union, [{:type, _, :integer, []},
             {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :char_list}, []]},
             {:type, _, :atom, []}]}, []} = spec
  end

  test "@type with an access macro" do
    spec = test_module do
      @type mytype :: Range[first: integer]
    end
    assert {:mytype, {:type, _, :tuple,
             [{:atom, _, Range}, {:type, _, :integer, []}, {:type, _, :any, []}]}, []} = spec
  end

  test "@type with keywords" do
    spec = test_module do
      @type mytype :: [first: integer, last: integer]
    end
    assert {:mytype, {:type, _, :list, [
      {:type, _, :union, [
        {:type, _, :tuple, [{:atom, _, :first}, {:type, _, :integer, []}]},
        {:type, _, :tuple, [{:atom, _, :last}, {:type, _, :integer, []}]}
      ]}
    ]}, []} = spec
  end

  test "@type with parameters" do
    {spec1, spec2, spec3} = test_module do
      t1 = @type mytype(x) :: x
      t2 = @type mytype1(x) :: list(x)
      t3 = @type mytype2(x, y) :: {x, y}
      {t1, t2, t3}
    end
    assert {:mytype, {:var, _, :x}, [{:var, _, :x}]} = spec1
    assert {:mytype1, {:type, _, :list, [{:var, _, :x}]}, [{:var, _, :x}]} = spec2
    assert {:mytype2, {:type, _, :tuple, [{:var, _, :x}, {:var, _, :y}]}, [{:var, _, :x}, {:var, _, :y}]} = spec3
  end

  test "@type with annotations" do
    {spec1, spec2} = test_module do
      t1 = @type mytype :: named :: integer
      t2 = @type mytype1 :: (a :: integer -> integer)
      {t1, t2}
    end
    assert {:mytype, {:ann_type, _, [{:var, _, :named}, {:type, _, :integer, []}]}, []} = spec1
    assert {:mytype1, {:type, _, :fun, [{:type, _, :product, [{:ann_type, _, [{:var, _, :a}, {:type, _, :integer, []}]}]}, {:type, _, :integer, []}]}, []} = spec2
  end

  test "@opaque(type)" do
    spec = test_module do
      @opaque mytype(x) :: x
    end
    assert {:mytype, {:var, _, :x}, [{:var, _, :x}]} = spec
  end

  test "@type + opaque" do
    types = test_module do
      @type mytype :: tuple
      @opaque mytype1 :: {}
      @type ++ @opaque
    end
    assert [{:mytype, _, []}, {:mytype1, _, []}] = types
  end

  test "@type from defrecordp" do
    types = test_module do
      defrecordp :user, name: nil, age: 0 :: integer
      @opaque user :: user_t
      @type
    end

    assert [{:user_t, {:type, _, :tuple,
             [{:atom, _, :user}, {:type, _, :term, []}, {:type, _, :integer, []}]}, []}] = types
  end

  test "@type from defrecordp with custom tag" do
    {types, module} = test_module do
      defrecordp :user, __MODULE__, name: nil, age: 0 :: integer
      @opaque user :: user_t
      {@type, __MODULE__}
    end

    assert [{:user_t, {:type, _, :tuple,
             [{:atom, _, ^module}, {:type, _, :term, []}, {:type, _, :integer, []}]}, []}] = types
  end

  test "defines_type?" do
    test_module do
      @type mytype :: tuple
      @type mytype(a) :: [a]
      assert Kernel.Typespec.defines_type?(__MODULE__, :mytype, 0)
      assert Kernel.Typespec.defines_type?(__MODULE__, :mytype, 1)
      refute Kernel.Typespec.defines_type?(__MODULE__, :mytype, 2)
    end
  end

  test "@spec(spec)" do
    {spec1, spec2, spec3} = test_module do
      def myfun(x), do: x
      def myfun(), do: :ok
      def myfun(x, y), do: {x, y}
      t1 = @spec myfun(integer) :: integer
      t2 = @spec myfun() :: integer
      t3 = @spec myfun(integer, integer) :: {integer, integer}
      {t1, t2, t3}
    end
    assert {{:myfun, 1}, {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}} = spec1
    assert {{:myfun, 0}, {:type, _, :fun, [{:type, _, :product, []}, {:type, _, :integer, []}]}} = spec2
    assert {{:myfun, 2}, {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]}, {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]}]}} = spec3
  end

  test "@spec(spec) with guards" do
    spec1 = test_module do
      def myfun(x), do: x
      @spec myfun(x) when is_subtype(x, integer) :: boolean
    end
    assert {{:myfun, 1}, {:type, _, :bounded_fun, [{:type, _, :fun, [{:type, _, :product, [{:var, _, :x}]}, {:type, _, :boolean, []}]}, [{:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, :x}, {:type, _, :integer, []}]]}]]}} = spec1
  end

  test "@callback(callback)" do
    {spec1, spec2, spec3} = test_module do
      t1 = @callback myfun(integer) :: integer
      t2 = @callback myfun() :: integer
      t3 = @callback myfun(integer, integer) :: {integer, integer}
      {t1, t2, t3}
    end
    assert {{:myfun, 1}, {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}} = spec1
    assert {{:myfun, 0}, {:type, _, :fun, [{:type, _, :product, []}, {:type, _, :integer, []}]}} = spec2
    assert {{:myfun, 2}, {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]}, {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]}]}} = spec3
  end

  test "@spec + @callback" do
    { specs, callbacks } = test_module do
      def myfun(x), do: x
      @spec myfun(integer)   :: integer
      @spec myfun(char_list) :: char_list
      @callback cb(integer)  :: integer
      { @spec, @callback }
    end

    assert [
      { {:cb, 1}, {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]} }
    ] = Enum.sort(callbacks)

    assert [
      { {:myfun, 1}, {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]} },
      { {:myfun, 1}, {:type, _, :fun, [{:type, _, :product, [
                      {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :char_list}, []]}]},
                      {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :char_list}, []]}]} }
    ] = Enum.sort(specs)
  end

  test "block handling" do
    spec = test_module do
      spec = @spec foo((() -> [ integer ])) :: integer
      def foo(_), do: 1
      spec
    end
    assert {{:foo, 1},
            {:type, _, :fun, [{:type, _, :product, [
                     {:type, _, :fun, [{:type, _, :product, []}, {:type, _, :list, [{:type, _, :integer, []}]}]}]},
                     {:type, _, :integer, []}]}} = spec
  end

  # Conversion to AST

  test "type_to_ast" do
    quoted = [
      (quote do: @type with_ann() :: (t :: atom())),
      (quote do: @type empty_tuple_type() :: {}),
      (quote do: @type imm_type_1() :: 1),
      (quote do: @type imm_type_2() :: :atom),
      (quote do: @type simple_type() :: integer()),
      (quote do: @type param_type(p) :: [p]),
      (quote do: @type union_type() :: integer() | binary() | boolean()),
      (quote do: @type binary_type1() :: <<_ :: _ * 8>>),
      (quote do: @type binary_type2() :: <<_ :: 3 * 8>>),
      (quote do: @type binary_type3() :: <<_ :: 3>>),
      (quote do: @type tuple_type() :: {integer()}),
      (quote do: @type ftype() :: (() -> any()) | (() -> integer()) | ((integer() -> integer()))),
      (quote do: @type cl() :: char_list()),
      (quote do: @type ab() :: as_boolean(term())),
      (quote do: @type vaf() :: (... -> any())),
      (quote do: @type rng() :: 1 .. 10),
      (quote do: @type opts() :: [first: integer(), last: integer()]),
    ]

    types = test_module do
      Module.eval_quoted __MODULE__, quote do: (unquote_splicing(quoted))
      @type ++ @opaque
    end

    types = Enum.reverse(types)

    lc { type, definition } inlist Enum.zip(types, quoted) do
      ast = Kernel.Typespec.type_to_ast(type)
      assert Macro.to_string(quote do: @type unquote(ast)) == Macro.to_string(definition)
    end
  end

  test "type_to_ast for records" do
    record_type = { { :record, :my_record },
                    [
                      { :typed_record_field,
                        { :record_field, 0, { :atom, 0, :field1 }},
                        { :type, 0, :atom, [] } },
                      { :typed_record_field,
                        { :record_field, 0, { :atom, 0, :field2 }},
                        { :type, 0, :integer, [] } },
                    ],
                    []}
    assert Kernel.Typespec.type_to_ast(record_type) ==
      { :::, [], [
        { :my_record, [], [] },
        { :{}, [], [:my_record,
          { :::, [line: 0], [
            {:field1, 0, nil},
            {:atom, [line: 0], []}
          ] },
          { :::, [line: 0], [
            {:field2, 0, nil},
            {:integer, [line: 0], []}
          ] }
        ] }
      ] }
  end

  test "type_to_ast for paren_type" do
    type = {:my_type, {:paren_type, 0, [{:type, 0, :integer, []}]}, []}
    assert Kernel.Typespec.type_to_ast(type) ==
      { :::, [], [{:my_type, [], []}, {:integer, [line: 0], []}] }
  end

  test "spec_to_ast" do
    specs = [
      (quote do: @spec a() :: integer()),
      (quote do: @spec a(atom()) :: integer()),
      (quote do: @spec a(b) when is_subtype(b, integer()) :: integer()),
    ]

    compiled = test_module do
      def a, do: 1
      def a(a), do: a
      Module.eval_quoted __MODULE__, quote do: (unquote_splicing(specs))
      Enum.reverse @spec
    end

    lc { { { _, _ }, spec }, definition } inlist Enum.zip(compiled, specs) do
      quoted = quote do: @spec unquote(Kernel.Typespec.spec_to_ast(:a, spec))
      assert Macro.to_string(quoted) == Macro.to_string(definition)
    end
  end

  # types/specs retrieval

  test "specs retrieval" do
    Code.compiler_options debug_info: true

    { :module, T, binary, _ } = defmodule T do
      @spec a :: any
      def a, do: nil
    end

    :code.delete(T)
    :code.purge(T)

    assert [{{:a, _}, [{:type, _, :fun, [{:type, _, :product, []}, {:type, _, :any, []}]}]}] = Kernel.Typespec.beam_specs(binary)
  after
    Code.compiler_options debug_info: false
  end

  test "types retrieval" do
    Code.compiler_options debug_info: true

    { :module, T, binary, _ } = defmodule T do
      @type a :: any
      @typep b :: any
      @spec t(b) :: b
      def t(b), do: b
      @opaque c :: atom
    end

    :code.delete(T)
    :code.purge(T)

    assert [
      {:opaque, {:c, {:type, _, :atom, []}, []}},
      {:type, {:a, {:type, _, :any, []}, []}},
      {:typep, {:b, {:type, _, :any, []}, []}},
    ] = Kernel.Typespec.beam_types(binary)
  after
    Code.compiler_options debug_info: false
  end

  test "typedoc retrieval" do
    Code.compiler_options debug_info: true

    { :module, T, binary, _ } = defmodule T do
      @typedoc "A"
      @type a :: any
      @typep b :: any
      @typedoc "C"
      @opaque c(x, y) :: {x, y}
      @type d :: any
      @spec uses_b() :: b
      def uses_b(), do: nil
    end

    :code.delete(T)
    :code.purge(T)

    assert [
      {{:c, 2}, "C"},
      {{:a, 0}, "A"}
    ] = Kernel.Typespec.beam_typedocs(binary)
  after
    Code.compiler_options debug_info: false
  end
end
