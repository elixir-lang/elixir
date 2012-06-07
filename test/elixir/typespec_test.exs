Code.require_file "../test_helper", __FILE__


defmodule Typespec.Test.Type do
  use ExUnit.Case
  
  #
  # This macro allows us to focus on the result of the definition
  # and not on the hassles of handling test module
  #
  defmacro test_module([{:do, block}]) do
    quote do
      result =
      defmodule T do 
        use Typespec
        unquote(block)
      end
      :code.delete(T)
      :code.purge(T)
      result
    end
  end 
  ##

  test "deftype with no body (defaults to 'term')" do
    spec = 
    test_module do
      deftype mytype
    end
    assert {:type,{:mytype,{:type,_,:term,[]},[]}} = spec
  end

  test "deftype with a single type" do
    spec = 
    test_module do
      deftype mytype :: term
    end
    assert {:type,{:mytype,{:type,_,:term,[]},[]}} = spec
  end

  test "deftype with an atom" do
    spec = 
    test_module do
      deftype mytype :: :atom
    end
    assert {:type,{:mytype,{:atom,_,:atom},[]}} = spec
  end

  test "deftype with an atom alias" do
    spec = 
    test_module do
      deftype mytype :: Atom
    end
    assert {:type,{:mytype,{:atom,_,Atom},[]}} = spec
  end

  test "deftype with an integer" do
    spec = 
    test_module do
      deftype mytype :: 10
    end
    assert {:type,{:mytype,{:integer,_,10},[]}} = spec
  end

  test "deftype with a negative integer" do
    spec = 
    test_module do
      deftype mytype :: -10
    end
    assert {:type,{:mytype,{:op, _, :-, {:integer,_,10}},[]}} = spec
  end

  test "deftype with a remote type" do
    {spec1, spec2} = 
    test_module do
      t1 = deftype mytype :: Remote.Some.type
      t2 = deftype mytype_arg :: Remote.type(integer)
      {t1, t2}
    end
    assert {:type,{:mytype,{:remote_type, _, [{:atom, _, Remote.Some},{:atom, _, :type}, []]},[]}} = spec1
    assert {:type,{:mytype_arg,{:remote_type, _, [{:atom, _, Remote},{:atom, _, :type},[{:type,_, :integer,[]}]]},[]}} = spec2
  end

  test "deftype with a binary" do
    spec =
    test_module do
      deftype mytype :: binary
    end
    assert {:type,{:mytype,{:type,_,:binary, []},[]}} = spec
  end

  test "deftype with an empty binary" do
    spec =
    test_module do
      deftype mytype :: <<>>
    end
    assert {:type,{:mytype,{:type,_,:binary, [{:integer, _, 0},{:integer, _, 0}]},[]}} = spec
  end

  test "deftype with a binary with a base size" do
    spec =
    test_module do
      deftype mytype :: <<_|3>>
    end
    assert {:type,{:mytype,{:type,_,:binary, [{:integer, _, 3},{:integer, _, 0}]},[]}} = spec
  end

  test "deftype with a binary with a unit size" do
    spec =
    test_module do
      deftype mytype :: <<_|_ * 8>>
    end
    assert {:type,{:mytype,{:type,_,:binary, [{:integer, _, 0},{:integer, _, 8}]},[]}} = spec
  end
  
  test "deftype with a range" do
    spec =
    test_module do
      deftype mytype :: range(1,10)
    end
    assert {:type,{:mytype,{:type,_,:range, [{:integer, _, 1},{:integer, _, 10}]},[]}} = spec
  end

  test "deftype with a tuple" do
    {spec1, spec2} =
    test_module do
      t1 = deftype mytype :: tuple
      t2 = deftype mytype1 :: {}
      {t1, t2}
    end
    assert {:type,{:mytype,{:type,_,:tuple, :any},[]}} = spec1
    assert {:type,{:mytype1,{:type,_,:tuple, :any},[]}} = spec2
  end

  
  test "deftype with a fun" do
    spec = 
    test_module do
      deftype mytype :: fun
    end
    assert {:type,{:mytype,{:type,_,:fun, []},[]}} = spec
  end

  test "deftype with a fun with arguments and return type" do
    spec = 
    test_module do
      deftype mytype :: fun(integer, integer, do: integer)
    end
    assert {:type,{:mytype,{:type,_,:fun, [{:type, _, :product, 
                         [{:type, _, :integer, []}, {:type, _, :integer, []}]},
                         {:type, _, :integer, []}]},[]}} = spec
  end

  test "deftype with a fun with no arguments and return type" do
    spec = 
    test_module do
      deftype mytype :: fun(do: integer)
    end
    assert {:type,{:mytype,{:type,_,:fun, [{:type, _, :product, []},
                         {:type, _, :integer, []}]}, []}} = spec
  end

  test "deftype with a fun with any arity and return type" do
    spec = 
    test_module do
      deftype mytype :: fun(..., do: integer)
    end
    assert {:type,{:mytype,{:type,_,:fun, [{:type, _, :any},
                         {:type, _, :integer, []}]}, []}} = spec
  end

  test "deftype with a union" do
    spec = 
    test_module do
      deftype mytype :: integer | string | atom
    end
    assert {:type,{:mytype,{:type,_,:union, [{:type, _, :integer, []},
                         {:type, _, :string, []},
                         {:type, _, :atom, []}]}, []}} = spec
  end

  test "deftype with parameters" do
    {spec1, spec2, spec3} = 
    test_module do
      t1 = deftype mytype(x) :: x
      t2 = deftype mytype1(x) :: list(x)
      t3 = deftype mytype2(x,y) :: {x,y}
      {t1,t2,t3}
    end
    assert {:type,{:mytype,{:var,_,:x},[{:var,_,:x}]}} = spec1
    assert {:type,{:mytype1,{:type,_,:list,[{:var,_,:x}]},[{:var,_,:x}]}} = spec2
    assert {:type,{:mytype2,{:type,_,:tuple,[{:var,_,:x},{:var,_,:y}]},[{:var,_,:x},{:var, _, :y}]}} = spec3
  end

  test "deftype with annotations" do
    {spec1, spec2} = 
    test_module do
      t1 = deftype mytype :: named :: integer
      t2 = deftype mytype1 :: fun(a :: integer, do: integer)
      {t1,t2}
    end
    assert {:type, {:mytype, {:ann_type, _, [{:var, _, :named}, {:type, _, :integer, []}]}, []}} = spec1
    assert {:type, {:mytype1,{:type,_,:fun,[{:type,_,:product,[{:ann_type,_,[{:var,_,:a},{:type,_,:integer,[]}]}]},{:type,_,:integer,[]}]},[]}} = spec2
  end

  test "opaque deftype" do
    spec =
    test_module do
      deftype mytype(x) :: x, opaque: true
    end
    assert {:opaque,{:mytype,{:var,_,:x},[{:var,_,:x}]}} = spec
  end

  test "defspec" do
    {spec1, spec2, spec3} = 
    test_module do
      def myfun(x), do: x
      def myfun(), do: :ok
      def myfun(x,y), do: {x,y}
      t1 = defspec myfun(integer), do: integer
      t2 = defspec myfun(), do: integer
      t3 = defspec myfun(integer, integer), do: {integer, integer}
      {t1,t2,t3}
    end
    assert {:spec,{{:myfun,1},[{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]}]}} = spec1
    assert {:spec,{{:myfun,0},[{:type,_,:fun,[{:type,_,:product,[]},{:type,_,:integer,[]}]}]}} = spec2
    assert {:spec,{{:myfun,2},[{:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]},{:type,_,:integer,[]}]},{:type,_,:tuple,[{:type,_,:integer,[]},{:type,_,:integer,[]}]}]}]}} = spec3
  end

  test "defspec with multiple clauses" do
    defmodule T do 
      use Typespec, keep_data: true
      def myfun(x), do: x
      defspec myfun(integer), do: integer
      defspec myfun(string), do: string
    end
    specs = T.__specs__
    assert [{{:myfun,1},[
                    {:type,_,:fun,[{:type,_,:product,[{:type,_,:integer,[]}]},{:type,_,:integer,[]}]},
                    {:type,_,:fun,[{:type,_,:product,[{:type,_,:string,[]}]},{:type,_,:string,[]}]}]}] = specs

    # cleanup
    :code.delete(T)
    :code.purge(T)
  end

end