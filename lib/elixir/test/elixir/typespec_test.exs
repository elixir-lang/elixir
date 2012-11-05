Code.require_file "../test_helper.exs", __FILE__

defmodule Typespec.Test.Type do
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

  test "@type with no body (defaults to 'term')" do
    spec = test_module do
      @type mytype
    end
    assert {:type,{:mytype,{:type,_,:term,[]},[]}} = spec
  end

  test "@type with a single type" do
    spec = test_module do
      @type mytype :: term
    end
    assert {:type,{:mytype,{:type,_,:term,[]},[]}} = spec
  end

  test "@type with an atom" do
    spec = test_module do
      @type mytype :: :atom
    end
    assert {:type,{:mytype,{:atom,_,:atom},[]}} = spec
  end

  test "@type with an atom alias" do
    spec = test_module do
      @type mytype :: Atom
    end
    assert {:type,{:mytype,{:atom,_,Atom},[]}} = spec
  end

  test "@type with an integer" do
    spec = test_module do
      @type mytype :: 10
    end
    assert {:type,{:mytype,{:integer,_,10},[]}} = spec
  end

  test "@type with a negative integer" do
    spec = test_module do
      @type mytype :: -10
    end
    assert {:type,{:mytype,{:op, _, :-, {:integer,_,10}},[]}} = spec
  end

  test "@type with a remote type" do
    {spec1, spec2} = test_module do
      t1 = @type mytype :: Remote.Some.type
      t2 = @type mytype_arg :: Remote.type(integer)
      {t1, t2}
    end
    assert {:type,{:mytype,{:remote_type, _, [{:atom, _, Remote.Some},{:atom, _, :type}, []]},[]}} = spec1
    assert {:type,{:mytype_arg,{:remote_type, _, [{:atom, _, Remote},{:atom, _, :type},[{:type,_, :integer,[]}]]},[]}} = spec2
  end

  test "@type with a binary" do
    spec = test_module do
      @type mytype :: binary
    end
    assert {:type,{:mytype,{:type,_,:binary, []},[]}} = spec
  end

  test "@type with an empty binary" do
    spec = test_module do
      @type mytype :: <<>>
    end
    assert {:type,{:mytype,{:type,_,:binary, [{:integer, _, 0},{:integer, _, 0}]},[]}} = spec
  end

  test "@type with a binary with a base size" do
    spec = test_module do
      @type mytype :: <<_ :: 3>>
    end
    assert {:type,{:mytype,{:type,_,:binary, [{:integer, _, 3},{:integer, _, 0}]},[]}} = spec
  end

  test "@type with a binary with a unit size" do
    spec = test_module do
      @type mytype :: <<_ :: _ * 8>>
    end
    assert {:type,{:mytype,{:type,_,:binary, [{:integer, _, 0},{:integer, _, 8}]},[]}} = spec
  end

  test "@type with a range" do
    spec = test_module do
      @type mytype :: range(1,10)
    end
    assert {:type,{:mytype,{:type,_,:range, [{:integer, _, 1},{:integer, _, 10}]},[]}} = spec
  end

  test "@type with a range op" do
    spec = test_module do
      @type mytype :: 1..10
    end
    assert {:type,{:mytype,{:type,_,:range, [{:integer, _, 1},{:integer, _, 10}]},[]}} = spec
  end

  test "@type with a tuple" do
    {spec1, spec2} = test_module do
      t1 = @type mytype :: tuple
      t2 = @type mytype1 :: {}
      {t1, t2}
    end
    assert {:type,{:mytype,{:type,_,:tuple, :any},[]}} = spec1
    assert {:type,{:mytype1,{:type,_,:tuple, :any},[]}} = spec2
  end

  test "@type with a fun" do
    spec = test_module do
      @type mytype :: fun
    end
    assert {:type,{:mytype,{:type,_,:fun, []},[]}} = spec
  end

  test "@type with a fun with arguments and return type" do
    spec = test_module do
      @type mytype :: fun(integer, integer, do: integer)
    end
    assert {:type,{:mytype,{:type,_,:fun, [{:type, _, :product,
                         [{:type, _, :integer, []}, {:type, _, :integer, []}]},
                         {:type, _, :integer, []}]},[]}} = spec
  end

  test "@type with a fun with no arguments and return type" do
    spec = test_module do
      @type mytype :: fun(do: integer)
    end
    assert {:type,{:mytype,{:type,_,:fun, [{:type, _, :product, []},
                         {:type, _, :integer, []}]}, []}} = spec
  end

  test "@type with a fun with any arity and return type" do
    spec = test_module do
      @type mytype :: fun(..., do: integer)
    end
    assert {:type,{:mytype,{:type,_,:fun, [{:type, _, :any},
                         {:type, _, :integer, []}]}, []}} = spec
  end

  test "@type with a union" do
    spec = test_module do
      @type mytype :: integer | char_list
                    | atom
    end
    assert {:type,{:mytype,{:type,_,:union, [{:type, _, :integer, []},
                         {:remote_type, _, [{:atom, _, :elixir},{:atom, _, :char_list}, []]},
                         {:type, _, :atom, []}]}, []}} = spec
  end

  test "@type with an access macro" do
    spec = test_module do
      @type mytype :: Range[first: integer]
    end
    assert {:type,{:mytype,{:type, _, :tuple,
              [{:atom, _, Range}, {:type, _, :integer, []}, {:type, _, :any, []}]}, []}} = spec
  end

  test "@type with parameters" do
    {spec1, spec2, spec3} = test_module do
      t1 = @type mytype(x) :: x
      t2 = @type mytype1(x) :: list(x)
      t3 = @type mytype2(x,y) :: {x,y}
      {t1,t2,t3}
    end
    assert {:type,{:mytype,{:var,_,:x},[{:var,_,:x}]}} = spec1
    assert {:type,{:mytype1,{:type,_,:list,[{:var,_,:x}]},[{:var,_,:x}]}} = spec2
    assert {:type,{:mytype2,{:type,_,:tuple,[{:var,_,:x},{:var,_,:y}]},[{:var,_,:x},{:var, _, :y}]}} = spec3
  end

  test "@type with annotations" do
    {spec1, spec2} = test_module do
      t1 = @type mytype :: named :: integer
      t2 = @type mytype1 :: fun(a :: integer, do: integer)
      {t1,t2}
    end
    assert {:type, {:mytype, {:ann_type, _, [{:var, _, :named}, {:type, _, :integer, []}]}, []}} = spec1
    assert {:type, {:mytype1,{:type,_,:fun,[{:type,_,:product,[{:ann_type,_,[{:var,_,:a},{:type,_,:integer,[]}]}]},{:type,_,:integer,[]}]},[]}} = spec2
  end

  test "@opaque(type)" do
    spec = test_module do
      @opaque mytype(x) :: x
    end
    assert {:opaque,{:mytype,{:var,_,:x},[{:var,_,:x}]}} = spec
  end

  test "@type + opaque" do
    types = test_module do
      @type mytype :: tuple
      @opaque mytype1 :: {}
      @type ++ @opaque
    end
    assert [{:mytype,_,[]},{:mytype1,_,[]}] = types
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
      def myfun(x,y), do: {x,y}
      t1 = @spec myfun(integer), do: integer
      t2 = @spec myfun(), do: integer
      t3 = @spec myfun(integer, integer), do: {integer, integer}
      {t1,t2,t3}
    end
    assert {{:myfun,1},{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]}} = spec1
    assert {{:myfun,0},{:type,_,:fun,[{:type,_,:product,[]},{:type,_,:integer,[]}]}} = spec2
    assert {{:myfun,2},{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]},{:type,_,:integer,[]}]},{:type,_,:tuple,[{:type,_,:integer,[]},{:type,_,:integer,[]}]}]}} = spec3
  end

  test "@callback(callback)" do
    {spec1, spec2, spec3} = test_module do
      t1 = @callback myfun(integer), do: integer
      t2 = @callback myfun(), do: integer
      t3 = @callback myfun(integer, integer), do: {integer, integer}
      {t1,t2,t3}
    end
    assert {{:myfun,1},{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]}} = spec1
    assert {{:myfun,0},{:type,_,:fun,[{:type,_,:product,[]},{:type,_,:integer,[]}]}} = spec2
    assert {{:myfun,2},{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]},{:type,_,:integer,[]}]},{:type,_,:tuple,[{:type,_,:integer,[]},{:type,_,:integer,[]}]}]}} = spec3
  end

  test "@spec + @callback" do
    { specs, callbacks } = test_module do
      def myfun(x), do: x
      @spec myfun(integer), do: integer
      @spec myfun(char_list),  do: char_list
      @callback cb(integer), do: integer
      { @spec, @callback }
    end

    assert [
      { {:cb, 1}, {:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]} }
    ] = List.sort(callbacks)

    assert [
      { {:myfun,1}, {:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]} },
      { {:myfun,1}, {:type,_,:fun,[{:type,_,:product,[
                      {:remote_type, _, [{:atom, _, :elixir},{:atom, _, :char_list}, []]}]},
                      {:remote_type, _, [{:atom, _, :elixir},{:atom, _, :char_list}, []]}]} }
    ] = List.sort(specs)
  end

  test "defines a callback from a spec" do
    callbacks = test_module do
      def myfun(x), do: x
      @spec myfun(integer), do: integer
      @spec myfun(char_list),  do: char_list
      Kernel.Typespec.callback_from_spec(__MODULE__, :myfun, 1)
      @callback
    end

    assert [
      { {:myfun,1}, {:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]} },
      { {:myfun,1}, {:type,_,:fun,[{:type,_,:product,[
                      {:remote_type, _, [{:atom, _, :elixir},{:atom, _, :char_list}, []]}]},
                      {:remote_type, _, [{:atom, _, :elixir},{:atom, _, :char_list}, []]}]} }
    ] = List.sort(callbacks)
  end

  # to_binary

  test "to_binary types" do
    types = [
      (quote do: @type with_ann() :: (t :: atom())),
      (quote do: @type empty_tuple_type() :: {}),    
      (quote do: @type imm_type_1() :: 1),
      (quote do: @type imm_type_2() :: :atom),
      (quote do: @type simple_type() :: integer()),
      (quote do: @type param_type(p) :: list(p)),
      (quote do: @type union_type() :: integer() | binary() | boolean()),
      (quote do: @type binary_type1() :: <<_ :: _ * 8>>),
      (quote do: @type binary_type2() :: <<_ :: 3 * 8>>),
      (quote do: @type binary_type3() :: <<_ :: 3>>),
      (quote do: @type tuple_type() :: {integer()}),
      (quote do: @type ftype() :: fun() | fun(do: integer()) | fun(integer(), do: integer())),
    ]

    spec = test_module do
      Module.eval_quoted __MODULE__, quote do: unquote_splicing(types)
      @type ++ @opaque
    end

    spec = Enum.reverse(spec)

    lc { typespec, definition } inlist Enum.zip(spec, types) do
      assert Kernel.Typespec.to_binary({ :type, typespec }) == Macro.to_binary(definition)
    end
  end

  test "to_binary spec" do
    specs = [
      (quote do: @spec a(), do: integer()),
      (quote do: @spec a(atom()), do: integer()),
    ]

    compiled = test_module do
      def a, do: 1
      def a(a), do: a
      Module.eval_quoted __MODULE__, quote do: unquote_splicing(specs)
      Enum.reverse @spec
    end

    lc { { tuple, spec }, definition } inlist Enum.zip(compiled, specs) do
      assert Kernel.Typespec.to_binary({ { :spec, tuple }, [spec] }) == Macro.to_binary(definition)
    end
  end

  # test/spec retrieval

  test "specs retrieval" do
    Code.compiler_options debug_info: true
    { :module, T, binary, _ } = defmodule T do
      @spec a, do: any
      def a, do: nil
    end
    Code.compiler_options debug_info: false
    :code.delete(T)
    :code.purge(T)
    assert [{{:spec,{:a,_}},[{:type,_,:fun,[{:type,_,:product,[]},{:type,_,:any,[]}]}]}] = Kernel.Typespec.specs(binary)
    assert [{{:spec,{:a,_}},[{:type,_,:fun,[{:type,_,:product,[]},{:type,_,:any,[]}]}]}] = Kernel.Typespec.specs(binary, :a, 0)
  end

  test "types retrieval" do
    Code.compiler_options debug_info: true
    { :module, T, binary, _ } = defmodule T do
      @type a :: any
      @typep b :: any
      @spec t(b), do: b
      def t(b), do: b
      @opaque c :: any
    end

    assert [
            {:opaque, {:c,{:type,_,:any,[]},[]}},
            {:type, {:a,{:type,_,:any,[]},[]}},
            {:typep, {:b,{:type,_,:any,[]},[]}},
           ] = Kernel.Typespec.types(binary)
    assert [
            {:type, {:a,{:type,_,:any,[]},[]}},
           ] = Kernel.Typespec.types(binary, :a, 0)
    assert [
            {:typep, {:b,{:type,_,:any,[]},[]}},
           ] = Kernel.Typespec.types(binary, :b, 0)
    assert [
            {:opaque, {:c,{:type,_,:any,[]},[]}},
           ] = Kernel.Typespec.types(binary, :c, 0)
  after
    Code.compiler_options debug_info: false
    :code.delete(T)
    :code.purge(T)
  end
end