Code.require_file "../test_helper", __FILE__

defmodule Typespec.Test.Type do
  use ExUnit.Case, async: true

  # This macro allows us to focus on the result of the
  # definition and not on the hassles of handling test
  # module
  defmacro test_module([{:do, block}]) do
    quote do
      result = defmodule T do
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
      @type mytype :: <<_|3>>
    end
    assert {:type,{:mytype,{:type,_,:binary, [{:integer, _, 3},{:integer, _, 0}]},[]}} = spec
  end

  test "@type with a binary with a unit size" do
    spec = test_module do
      @type mytype :: <<_|_ * 8>>
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
      @type mytype :: integer | string | atom
    end
    assert {:type,{:mytype,{:type,_,:union, [{:type, _, :integer, []},
                         {:type, _, :string, []},
                         {:type, _, :atom, []}]}, []}} = spec
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

  test "opaque @type" do
    spec = test_module do
      @type mytype(x) :: x, opaque: true
    end
    assert {:opaque,{:mytype,{:var,_,:x},[{:var,_,:x}]}} = spec
  end

  test "get_types" do
    types = test_module do
      import Kernel.Typespec
      @type mytype :: tuple
      @type mytype1 :: {}, opaque: true
      get_types(__MODULE__)
    end
    assert [{:mytype,_,[]},{:mytype1,_,[]}] = types
  end

  test "@spec" do
    {spec1, spec2, spec3} = test_module do
      def myfun(x), do: x
      def myfun(), do: :ok
      def myfun(x,y), do: {x,y}
      t1 = @spec myfun(integer), do: integer
      t2 = @spec myfun(), do: integer
      t3 = @spec myfun(integer, integer), do: {integer, integer}
      {t1,t2,t3}
    end
    assert {{:spec, {:myfun,1}},[{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]}]} = spec1
    assert {{:spec, {:myfun,0}},[{:type,_,:fun,[{:type,_,:product,[]},{:type,_,:integer,[]}]}]} = spec2
    assert {{:spec, {:myfun,2}},[{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]},{:type,_,:integer,[]}]},{:type,_,:tuple,[{:type,_,:integer,[]},{:type,_,:integer,[]}]}]}]} = spec3
  end

  test "@callback" do
    {spec1, spec2, spec3} = test_module do
      t1 = @callback myfun(integer), do: integer
      t2 = @callback myfun(), do: integer
      t3 = @callback myfun(integer, integer), do: {integer, integer}
      {t1,t2,t3}
    end
    assert {{:callback, {:myfun,1}},[{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]}]} = spec1
    assert {{:callback, {:myfun,0}},[{:type,_,:fun,[{:type,_,:product,[]},{:type,_,:integer,[]}]}]} = spec2
    assert {{:callback, {:myfun,2}},[{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]},{:type,_,:integer,[]}]},{:type,_,:tuple,[{:type,_,:integer,[]},{:type,_,:integer,[]}]}]}]} = spec3
  end

  test "get_specs" do
    specs = test_module do
      import Kernel.Typespec
      def myfun(x), do: x
      @spec myfun(integer), do: integer
      @spec myfun(string),  do: string
      @callback cb(integer), do: integer
      get_specs(__MODULE__)
    end

    assert [
            {{:callback, {:cb, 1}},[
              {:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]}
            ]},
            {{:spec, {:myfun,1}},[
              {:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]},
              {:type,_,:fun,[{:type,_,:product,[{:type,_,:string,[]}]},{:type,_,:string,[]}]}]}
            ] = List.sort(specs)
  end
end