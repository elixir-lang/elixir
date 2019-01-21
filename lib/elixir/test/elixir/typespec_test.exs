Code.require_file("test_helper.exs", __DIR__)

# Holds tests for both Kernel.Typespec and Code.Typespec
defmodule TypespecTest do
  use ExUnit.Case, async: true
  alias TypespecTest.TypespecSample

  defstruct [:hello]

  defmacrop test_module(do: block) do
    quote do
      {:module, _, bytecode, _} =
        defmodule TypespecSample do
          unquote(block)
        end

      :code.delete(TypespecSample)
      :code.purge(TypespecSample)
      bytecode
    end
  end

  defp types(bytecode) do
    bytecode
    |> Code.Typespec.fetch_types()
    |> elem(1)
    |> Enum.sort()
  end

  @skip_specs [__info__: 1]

  defp specs(bytecode) do
    bytecode
    |> Code.Typespec.fetch_specs()
    |> elem(1)
    |> Enum.reject(fn {sign, _} -> sign in @skip_specs end)
    |> Enum.sort()
  end

  defp callbacks(bytecode) do
    bytecode
    |> Code.Typespec.fetch_callbacks()
    |> elem(1)
    |> Enum.sort()
  end

  describe "Kernel.Typespec errors" do
    test "invalid type specification" do
      assert_raise CompileError, ~r"invalid type specification: my_type = 1", fn ->
        test_module do
          @type my_type = 1
        end
      end
    end

    test "unexpected expression in typespec" do
      assert_raise CompileError, ~r"unexpected expression in typespec: \"foobar\"", fn ->
        test_module do
          @type my_type :: "foobar"
        end
      end
    end

    test "invalid function specification" do
      assert_raise CompileError, ~r"invalid type specification: \"not a spec\"", fn ->
        test_module do
          @spec "not a spec"
        end
      end

      assert_raise CompileError, ~r"invalid type specification: 1 :: 2", fn ->
        test_module do
          @spec 1 :: 2
        end
      end
    end

    test "undefined type" do
      assert_raise CompileError, ~r"type foo/0 undefined", fn ->
        test_module do
          @type omg :: foo
        end
      end

      assert_raise CompileError, ~r"type foo/2 undefined", fn ->
        test_module do
          @type omg :: foo(atom, integer)
        end
      end

      assert_raise CompileError, ~r"type bar/0 undefined", fn ->
        test_module do
          @spec foo(bar, integer) :: {atom, integer}
          def foo(var1, var2), do: {var1, var2}
        end
      end
    end

    test "redefined type" do
      assert_raise CompileError, ~r"type foo/0 is already defined", fn ->
        test_module do
          @type foo :: atom
          @type foo :: integer
        end
      end

      assert_raise CompileError, ~r"type foo/2 is already defined", fn ->
        test_module do
          @type foo :: atom
          @type foo(var1, var2) :: {var1, var2}
          @type foo(x, y) :: {x, y}
        end
      end

      assert_raise CompileError, ~r"type foo/0 is already defined", fn ->
        test_module do
          @type foo :: atom
          @typep foo :: integer
        end
      end
    end

    test "type variable unused (singleton type variable)" do
      assert_raise CompileError, ~r"type variable x is unused", fn ->
        test_module do
          @type foo(x) :: integer
        end
      end
    end

    test "spec for undefined function" do
      assert_raise CompileError, ~r"spec for undefined function omg/0", fn ->
        test_module do
          @spec omg :: atom
        end
      end
    end

    test "spec variable unused (singleton type variable)" do
      assert_raise CompileError, ~r"type variable x is unused", fn ->
        test_module do
          @spec foo(x, integer) :: integer when x: var
          def foo(x, y), do: x + y
        end
      end
    end

    test "ill defined optional callback" do
      assert_raise CompileError, ~r"invalid optional callback :foo", fn ->
        test_module do
          @optional_callbacks :foo
        end
      end
    end

    test "unknown optional callback" do
      assert_raise CompileError, ~r"unknown callback foo/1 given as optional callback", fn ->
        test_module do
          @optional_callbacks foo: 1
        end
      end
    end

    test "repeated optional callback" do
      message = ~r"foo/1 has been specified as optional callback more than once"

      assert_raise CompileError, message, fn ->
        test_module do
          @callback foo(:ok) :: :ok
          @optional_callbacks foo: 1, foo: 1
        end
      end
    end

    test "behaviour_info/1 explicitly defined alongside @callback/@macrocallback" do
      message = ~r"cannot define @callback attribute for foo/1 when behaviour_info/1"

      assert_raise CompileError, message, fn ->
        test_module do
          @callback foo(:ok) :: :ok
          def behaviour_info(_), do: []
        end
      end

      message = ~r"cannot define @macrocallback attribute for foo/1 when behaviour_info/1"

      assert_raise CompileError, message, fn ->
        test_module do
          @macrocallback foo(:ok) :: :ok
          def behaviour_info(_), do: []
        end
      end
    end

    test "default is not supported" do
      assert_raise ArgumentError, fn ->
        test_module do
          @callback hello(num \\ 0 :: integer) :: integer
        end
      end

      assert_raise ArgumentError, fn ->
        test_module do
          @callback hello(num :: integer \\ 0) :: integer
        end
      end

      assert_raise ArgumentError, fn ->
        test_module do
          @macrocallback hello(num \\ 0 :: integer) :: Macro.t()
        end
      end

      assert_raise ArgumentError, fn ->
        test_module do
          @macrocallback hello(num :: integer \\ 0) :: Macro.t()
        end
      end

      assert_raise ArgumentError, fn ->
        test_module do
          @spec hello(num \\ 0 :: integer) :: integer
        end
      end

      assert_raise ArgumentError, fn ->
        test_module do
          @spec hello(num :: integer \\ 0) :: integer
        end
      end
    end

    test "@spec shows readable error message when return type is missing" do
      message = ~r"type specification missing return type: my_fun\(integer\)"

      assert_raise CompileError, message, fn ->
        test_module do
          @spec my_fun(integer)
        end
      end
    end
  end

  describe "Kernel.Typespec definitions" do
    test "typespec declarations return :ok" do
      test_module do
        def foo(), do: nil

        assert @type(foo :: any()) == :ok
        assert @typep(foop :: any()) == :ok
        assert @spec(foo() :: nil) == :ok
        assert @opaque(my_type :: atom) == :ok
        assert @callback(foo(foop) :: integer) == :ok
        assert @macrocallback(foo(integer) :: integer) == :ok
      end
    end

    test "@type with a single type" do
      bytecode =
        test_module do
          @type my_type :: term
        end

      assert [type: {:my_type, {:type, _, :term, []}, []}] = types(bytecode)
    end

    test "@type with an atom" do
      bytecode =
        test_module do
          @type my_type :: :foo
        end

      assert [type: {:my_type, {:atom, _, :foo}, []}] = types(bytecode)
    end

    test "@type with an atom alias" do
      bytecode =
        test_module do
          @type my_type :: Atom
        end

      assert [type: {:my_type, {:atom, _, Atom}, []}] = types(bytecode)
    end

    test "@type with an integer" do
      bytecode =
        test_module do
          @type my_type :: 10
        end

      assert [type: {:my_type, {:integer, _, 10}, []}] = types(bytecode)
    end

    test "@type with a negative integer" do
      bytecode =
        test_module do
          @type my_type :: -10
        end

      assert [type: {:my_type, {:op, _, :-, {:integer, _, 10}}, []}] = types(bytecode)
    end

    test "@type with a remote type" do
      bytecode =
        test_module do
          @type my_type :: Remote.Some.type()
          @type my_type_arg :: Remote.type(integer)
        end

      assert [type: my_type, type: my_type_arg] = types(bytecode)

      assert {:my_type, type, []} = my_type
      assert {:remote_type, _, [{:atom, _, Remote.Some}, {:atom, _, :type}, []]} = type

      assert {:my_type_arg, type, []} = my_type_arg
      assert {:remote_type, _, args} = type
      assert [{:atom, _, Remote}, {:atom, _, :type}, [{:type, _, :integer, []}]] = args
    end

    test "@type with a binary" do
      bytecode =
        test_module do
          @type my_type :: binary
        end

      assert [type: {:my_type, {:type, _, :binary, []}, []}] = types(bytecode)
    end

    test "@type with an empty binary" do
      bytecode =
        test_module do
          @type my_type :: <<>>
        end

      assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 0}]}, []}] =
               types(bytecode)
    end

    test "@type with a binary with a base size" do
      bytecode =
        test_module do
          @type my_type :: <<_::3>>
        end

      assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 3}, {:integer, _, 0}]}, []}] =
               types(bytecode)
    end

    test "@type with a binary with a unit size" do
      bytecode =
        test_module do
          @type my_type :: <<_::_*8>>
        end

      assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 8}]}, []}] =
               types(bytecode)
    end

    test "@type with a binary with a size and unit size" do
      bytecode =
        test_module do
          @type my_type :: <<_::3, _::_*8>>
        end

      assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 3}, {:integer, _, 8}]}, []}] =
               types(bytecode)
    end

    test "@type with invalid binary spec" do
      assert_raise CompileError, ~r"invalid binary specification", fn ->
        test_module do
          @type my_type :: <<_::3*8>>
        end
      end

      assert_raise CompileError, ~r"invalid binary specification", fn ->
        test_module do
          @type my_type :: <<_::atom>>
        end
      end

      assert_raise CompileError, ~r"invalid binary specification", fn ->
        test_module do
          @type my_type :: <<_::(-4)>>
        end
      end

      assert_raise CompileError, ~r"invalid binary specification", fn ->
        test_module do
          @type my_type :: <<_::3, _::_*atom>>
        end
      end

      assert_raise CompileError, ~r"invalid binary specification", fn ->
        test_module do
          @type my_type :: <<_::3, _::_*(-8)>>
        end
      end
    end

    test "@type with a range op" do
      bytecode =
        test_module do
          @type range1 :: 1..10
          @type range2 :: -1..1
        end

      assert [
               {:type, {:range1, {:type, _, :range, range1_args}, []}},
               {:type, {:range2, {:type, _, :range, range2_args}, []}}
             ] = types(bytecode)

      assert [{:integer, _, 1}, {:integer, _, 10}] = range1_args
      assert [{:op, _, :-, {:integer, _, 1}}, {:integer, _, 1}] = range2_args
    end

    test "@type with invalid range" do
      assert_raise CompileError, ~r"invalid range specification", fn ->
        test_module do
          @type my_type :: atom..10
        end
      end
    end

    test "@type with a keyword map" do
      bytecode =
        test_module do
          @type my_type :: %{hello: :world}
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :map, [arg]} = type
      assert {:type, _, :map_field_exact, [{:atom, _, :hello}, {:atom, _, :world}]} = arg
    end

    test "@type with a map" do
      bytecode =
        test_module do
          @type my_type :: %{required(:a) => :b, optional(:c) => :d}
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :map, [arg1, arg2]} = type
      assert {:type, _, :map_field_exact, [{:atom, _, :a}, {:atom, _, :b}]} = arg1
      assert {:type, _, :map_field_assoc, [{:atom, _, :c}, {:atom, _, :d}]} = arg2
    end

    test "@type with a struct" do
      bytecode =
        test_module do
          defstruct hello: nil, other: nil
          @type my_type :: %TypespecSample{hello: :world}
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :map, [struct, arg1, arg2]} = type
      assert {:type, _, :map_field_exact, struct_args} = struct
      assert [{:atom, _, :__struct__}, {:atom, _, TypespecSample}] = struct_args
      assert {:type, _, :map_field_exact, [{:atom, _, :hello}, {:atom, _, :world}]} = arg1
      assert {:type, _, :map_field_exact, [{:atom, _, :other}, {:type, _, :term, []}]} = arg2
    end

    test "@type with struct does not @enforce_keys" do
      bytecode =
        test_module do
          @enforce_keys [:other]
          defstruct hello: nil, other: nil
          @type my_type :: %TypespecSample{hello: :world}
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
    end

    test "@type with undefined struct" do
      assert_raise CompileError, ~r"ThisModuleDoesNotExist.__struct__/0 is undefined", fn ->
        test_module do
          @type my_type :: %ThisModuleDoesNotExist{}
        end
      end

      assert_raise CompileError, ~r"cannot access struct TypespecTest.TypespecSample", fn ->
        test_module do
          @type my_type :: %TypespecSample{}
        end
      end
    end

    test "@type with a struct with undefined field" do
      assert_raise CompileError, ~r"undefined field :no_field on struct TypespecSample", fn ->
        test_module do
          defstruct [:hello, :eric]
          @type my_type :: %TypespecSample{no_field: :world}
        end
      end
    end

    test "@type when overriding Elixir built-in" do
      assert_raise CompileError, ~r"type struct/0 is a built-in type", fn ->
        test_module do
          @type struct :: :oops
        end
      end
    end

    test "@type when overriding Erlang built-in" do
      assert_raise CompileError, ~r"type list/0 is a built-in type", fn ->
        test_module do
          @type list :: :oops
        end
      end
    end

    test "@type with public record" do
      bytecode =
        test_module do
          require Record
          Record.defrecord(:timestamp, date: 1, time: 2)
          @type my_type :: record(:timestamp, time: :foo)
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :tuple, [timestamp, term, foo]} = type
      assert {:atom, 0, :timestamp} = timestamp
      assert {:type, 0, :term, []} = term
      assert {:atom, 0, :foo} = foo
    end

    test "@type with private record" do
      bytecode =
        test_module do
          require Record
          Record.defrecordp(:timestamp, date: 1, time: 2)
          @type my_type :: record(:timestamp, time: :foo)
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :tuple, args} = type
      assert [{:atom, 0, :timestamp}, {:type, 0, :term, []}, {:atom, 0, :foo}] = args
    end

    test "@type with named record" do
      bytecode =
        test_module do
          require Record
          Record.defrecord(:timestamp, :my_timestamp, date: 1, time: 2)
          @type my_type :: record(:timestamp, time: :foo)
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :tuple, [my_timestamp, term, foo]} = type
      assert {:atom, 0, :my_timestamp} = my_timestamp
      assert {:type, 0, :term, []} = term
      assert {:atom, 0, :foo} = foo
    end

    test "@type with undefined record" do
      assert_raise CompileError, ~r"unknown record :this_record_does_not_exist", fn ->
        test_module do
          @type my_type :: record(:this_record_does_not_exist, [])
        end
      end
    end

    test "@type with a record with undefined field" do
      assert_raise CompileError, ~r"undefined field no_field on record :timestamp", fn ->
        test_module do
          require Record
          Record.defrecord(:timestamp, date: 1, time: 2)
          @type my_type :: record(:timestamp, no_field: :foo)
        end
      end
    end

    test "@type with a record which declares the name as the type `atom` rather than an atom literal" do
      assert_raise CompileError, ~r"expected the record name to be an atom literal", fn ->
        test_module do
          @type my_type :: record(atom, field: :foo)
        end
      end
    end

    test "@type can be named record" do
      bytecode =
        test_module do
          @type record :: binary
          @spec foo?(record) :: boolean
          def foo?(_), do: true
        end

      assert [type: {:record, {:type, _, :binary, []}, []}] = types(bytecode)
    end

    test "@type with an invalid map notation" do
      assert_raise CompileError, ~r"invalid map specification", fn ->
        test_module do
          @type content :: %{atom | String.t() => term}
        end
      end
    end

    test "@type with list shortcuts" do
      bytecode =
        test_module do
          @type my_type :: []
          @type my_type1 :: [integer]
          @type my_type2 :: [integer, ...]
        end

      assert [
               type: {:my_type, {:type, _, nil, []}, []},
               type: {:my_type1, {:type, _, :list, [{:type, _, :integer, []}]}, []},
               type: {:my_type2, {:type, _, :nonempty_list, [{:type, _, :integer, []}]}, []}
             ] = types(bytecode)
    end

    test "@type with a fun" do
      bytecode =
        test_module do
          @type my_type :: (... -> any)
        end

      assert [type: {:my_type, {:type, _, :fun, []}, []}] = types(bytecode)
    end

    test "@type with a fun with multiple arguments and return type" do
      bytecode =
        test_module do
          @type my_type :: (integer, integer -> integer)
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :fun, [args, return_type]} = type
      assert {:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]} = args
      assert {:type, _, :integer, []} = return_type
    end

    test "@type with a fun with no arguments and return type" do
      bytecode =
        test_module do
          @type my_type :: (() -> integer)
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :fun, [{:type, _, :product, []}, {:type, _, :integer, []}]} = type
    end

    test "@type with a fun with any arity and return type" do
      bytecode =
        test_module do
          @type my_type :: (... -> integer)
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :fun, [{:type, _, :any}, {:type, _, :integer, []}]} = type
    end

    test "@type with a union" do
      bytecode =
        test_module do
          @type my_type :: integer | charlist | atom
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :union, [integer, charlist, atom]} = type
      assert {:type, _, :integer, []} = integer
      assert {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :charlist}, []]} = charlist
      assert {:type, _, :atom, []} = atom
    end

    test "@type with keywords" do
      bytecode =
        test_module do
          @type my_type :: [first: integer, step: integer, last: integer]
        end

      assert [type: {:my_type, type, []}] = types(bytecode)
      assert {:type, _, :list, [{:type, _, :union, union_types}]} = type

      assert [
               {:type, _, :tuple, [{:atom, _, :first}, {:type, _, :integer, []}]},
               {:type, _, :tuple, [{:atom, _, :step}, {:type, _, :integer, []}]},
               {:type, _, :tuple, [{:atom, _, :last}, {:type, _, :integer, []}]}
             ] = union_types
    end

    test "@type with parameters" do
      bytecode =
        test_module do
          @type my_type(x) :: x
          @type my_type1(x) :: list(x)
          @type my_type2(x, y) :: {x, y}
        end

      assert [
               type: {:my_type, {:var, _, :x}, [{:var, _, :x}]},
               type: {:my_type1, {:type, _, :list, [{:var, _, :x}]}, [{:var, _, :x}]},
               type: {:my_type2, my_type2, [{:var, _, :x}, {:var, _, :y}]}
             ] = types(bytecode)

      assert {:type, _, :tuple, [{:var, _, :x}, {:var, _, :y}]} = my_type2
    end

    test "@type with annotations" do
      bytecode =
        test_module do
          @type my_type :: named :: integer
          @type my_type1 :: (a :: integer -> integer)
        end

      assert [type: {:my_type, my_type, []}, type: {:my_type1, my_type1, []}] = types(bytecode)

      assert {:ann_type, _, [{:var, _, :named}, {:type, _, :integer, []}]} = my_type

      assert {:type, _, :fun, [fun_args, fun_return]} = my_type1
      assert {:type, _, :product, [{:ann_type, _, [a, {:type, _, :integer, []}]}]} = fun_args
      assert {:var, _, :a} = a
      assert {:type, _, :integer, []} = fun_return
    end

    test "@opaque(type)" do
      bytecode =
        test_module do
          @opaque my_type(x) :: x
        end

      assert [opaque: {:my_type, {:var, _, :x}, [{:var, _, :x}]}] = types(bytecode)
    end

    test "@type + opaque" do
      bytecode =
        test_module do
          @type my_type :: tuple
          @opaque my_type1 :: {}
        end

      assert [opaque: {:my_type1, _, []}, type: {:my_type, _, []}] = types(bytecode)
    end

    test "@type unquote fragment" do
      quoted =
        quote unquote: false do
          name = :my_type
          type = :foo
          @type unquote(name)() :: unquote(type)
        end

      bytecode =
        test_module do
          Module.eval_quoted(__MODULE__, quoted)
        end

      assert [type: {:my_type, {:atom, _, :foo}, []}] = types(bytecode)
    end

    test "@type with module attributes" do
      bytecode =
        test_module do
          @keyword Keyword
          @type kw :: @keyword.t
          @type kw(value) :: @keyword.t(value)
        end

      assert [type: {:kw, kw, _}, type: {:kw, kw_with_value, [{:var, _, :value}]}] =
               types(bytecode)

      assert {:remote_type, _, [{:atom, _, Keyword}, {:atom, _, :t}, []]} = kw
      assert {:remote_type, _, kw_with_value_args} = kw_with_value
      assert [{:atom, _, Keyword}, {:atom, _, :t}, [{:var, _, :value}]] = kw_with_value_args
    end

    test "invalid remote @type with module attribute that does not evaluate to a module" do
      assert_raise CompileError, ~r/\(@foo is "bar"\)/, fn ->
        test_module do
          @foo "bar"
          @type t :: @foo.t
        end
      end
    end

    test "defines_type?" do
      test_module do
        @type my_type :: tuple
        @type my_type(a) :: [a]
        assert Kernel.Typespec.defines_type?(__MODULE__, {:my_type, 0})
        assert Kernel.Typespec.defines_type?(__MODULE__, {:my_type, 1})
        refute Kernel.Typespec.defines_type?(__MODULE__, {:my_type, 2})
      end
    end

    test "spec_to_callback/2" do
      bytecode =
        test_module do
          @spec foo() :: term()
          def foo(), do: :ok
          Kernel.Typespec.spec_to_callback(__MODULE__, {:foo, 0})
        end

      assert specs(bytecode) == callbacks(bytecode)
    end

    test "@spec(spec)" do
      bytecode =
        test_module do
          def my_fun1(x), do: x
          def my_fun2(), do: :ok
          def my_fun3(x, y), do: {x, y}
          def my_fun4(x), do: x
          @spec my_fun1(integer) :: integer
          @spec my_fun2() :: integer
          @spec my_fun3(integer, integer) :: {integer, integer}
          @spec my_fun4(x :: integer) :: integer
        end

      assert [my_fun1, my_fun2, my_fun3, my_fun4] = specs(bytecode)

      assert {{:my_fun1, 1}, [{:type, _, :fun, args}]} = my_fun1
      assert [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}] = args

      assert {{:my_fun2, 0}, [{:type, _, :fun, args}]} = my_fun2
      assert [{:type, _, :product, []}, {:type, _, :integer, []}] = args

      assert {{:my_fun3, 2}, [{:type, _, :fun, [arg1, arg2]}]} = my_fun3
      assert {:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]} = arg1
      assert {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]} = arg2

      assert {{:my_fun4, 1}, [{:type, _, :fun, args}]} = my_fun4
      assert [x, {:type, _, :integer, []}] = args
      assert {:type, _, :product, [{:ann_type, _, [{:var, _, :x}, {:type, _, :integer, []}]}]} = x
    end

    test "@spec(spec) with tuples and tuple vars" do
      bytecode =
        test_module do
          def my_fun1(x), do: x
          def my_fun2(x), do: x
          @spec my_fun1(tuple) :: tuple
          @spec my_fun2(tuple) :: tuple when tuple: {integer, integer}
        end

      assert [my_fun1, my_fun2] = specs(bytecode)

      assert {{:my_fun1, 1}, [{:type, _, :fun, args}]} = my_fun1
      assert [{:type, _, :product, [{:type, _, :tuple, :any}]}, {:type, _, :tuple, :any}] = args

      assert {{:my_fun2, 1}, [{:type, _, :bounded_fun, args}]} = my_fun2

      assert [type, _] = args

      assert {:type, _, :fun, [{:type, _, :product, [{:var, _, :tuple}]}, {:var, _, :tuple}]} =
               type
    end

    test "@spec(spec) with guards" do
      bytecode =
        test_module do
          def my_fun1(x), do: x
          @spec my_fun1(x) :: boolean when x: integer

          def my_fun2(x), do: x
          @spec my_fun2(x) :: x when x: var

          def my_fun3(_x, y), do: y
          @spec my_fun3(x, y) :: y when y: x, x: var
        end

      assert [my_fun1, my_fun2, my_fun3] = specs(bytecode)

      assert {{:my_fun1, 1}, [{:type, _, :bounded_fun, args}]} = my_fun1
      assert [{:type, _, :fun, [product, {:type, _, :boolean, []}]}, constraints] = args
      assert {:type, _, :product, [{:var, _, :x}]} = product
      assert [{:type, _, :constraint, subtype}] = constraints
      assert [{:atom, _, :is_subtype}, [{:var, _, :x}, {:type, _, :integer, []}]] = subtype

      assert {{:my_fun2, 1}, [{:type, _, :fun, args}]} = my_fun2
      assert [{:type, _, :product, [{:var, _, :x}]}, {:var, _, :x}] = args

      assert {{:my_fun3, 2}, [{:type, _, :bounded_fun, args}]} = my_fun3
      assert [{:type, _, :fun, fun_type}, [{:type, _, :constraint, constraint_type}]] = args
      assert [{:type, _, :product, [{:var, _, :x}, {:var, _, :y}]}, {:var, _, :y}] = fun_type
      assert [{:atom, _, :is_subtype}, [{:var, _, :y}, {:var, _, :x}]] = constraint_type
    end

    test "@type, @opaque, and @typep as module attributes" do
      defmodule TypeModuleAttributes do
        @type type1 :: boolean
        @opaque opaque1 :: boolean
        @typep typep1 :: boolean

        def type1, do: @type
        def opaque1, do: @opaque
        def typep1, do: @typep

        @type type2 :: atom
        @type type3 :: pid
        @opaque opaque2 :: atom
        @opaque opaque3 :: pid
        @typep typep2 :: atom

        def type2, do: @type
        def opaque2, do: @opaque
        def typep2, do: @typep

        # Avoid unused warnings
        @spec foo(typep1) :: typep2
        def foo(_x), do: :ok
      end

      assert [
               {:type, {:::, _, [{:type1, _, _}, {:boolean, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}}
             ] = TypeModuleAttributes.type1()

      assert [
               {:type, {:::, _, [{:type3, _, _}, {:pid, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}},
               {:type, {:::, _, [{:type2, _, _}, {:atom, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}},
               {:type, {:::, _, [{:type1, _, _}, {:boolean, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}}
             ] = TypeModuleAttributes.type2()

      assert [
               {:opaque, {:::, _, [{:opaque1, _, _}, {:boolean, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}}
             ] = TypeModuleAttributes.opaque1()

      assert [
               {:opaque, {:::, _, [{:opaque3, _, _}, {:pid, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}},
               {:opaque, {:::, _, [{:opaque2, _, _}, {:atom, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}},
               {:opaque, {:::, _, [{:opaque1, _, _}, {:boolean, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}}
             ] = TypeModuleAttributes.opaque2()

      assert [
               {:typep, {:::, _, [{:typep1, _, _}, {:boolean, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}}
             ] = TypeModuleAttributes.typep1()

      assert [
               {:typep, {:::, _, [{:typep2, _, _}, {:atom, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}},
               {:typep, {:::, _, [{:typep1, _, _}, {:boolean, _, _}]},
                {TypespecTest.TypeModuleAttributes, _}}
             ] = TypeModuleAttributes.typep2()
    after
      :code.delete(TypeModuleAttributes)
      :code.purge(TypeModuleAttributes)
    end

    test "@spec, @callback, and @macrocallback as module attributes" do
      defmodule SpecModuleAttributes do
        @callback callback1 :: integer
        @macrocallback macrocallback1 :: integer

        @spec spec1 :: boolean
        def spec1, do: @spec

        @callback callback2 :: boolean
        @macrocallback macrocallback2 :: boolean

        @spec spec2 :: atom
        def spec2, do: @spec

        @spec spec3 :: pid
        def spec3, do: :ok
        def spec4, do: @spec

        def callback, do: @callback
        def macrocallback, do: @macrocallback
      end

      assert [
               {:spec, {:::, _, [{:spec1, _, _}, {:boolean, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}}
             ] = SpecModuleAttributes.spec1()

      assert [
               {:spec, {:::, _, [{:spec2, _, _}, {:atom, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}},
               {:spec, {:::, _, [{:spec1, _, _}, {:boolean, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}}
             ] = SpecModuleAttributes.spec2()

      assert [
               {:spec, {:::, _, [{:spec3, _, _}, {:pid, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}},
               {:spec, {:::, _, [{:spec2, _, _}, {:atom, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}},
               {:spec, {:::, _, [{:spec1, _, _}, {:boolean, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}}
             ] = SpecModuleAttributes.spec4()

      assert [
               {:callback, {:::, _, [{:callback2, _, _}, {:boolean, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}},
               {:callback, {:::, _, [{:callback1, _, _}, {:integer, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}}
             ] = SpecModuleAttributes.callback()

      assert [
               {:macrocallback, {:::, _, [{:macrocallback2, _, _}, {:boolean, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}},
               {:macrocallback, {:::, _, [{:macrocallback1, _, _}, {:integer, _, _}]},
                {TypespecTest.SpecModuleAttributes, _}}
             ] = SpecModuleAttributes.macrocallback()
    after
      :code.delete(SpecModuleAttributes)
      :code.purge(SpecModuleAttributes)
    end

    test "@callback(callback)" do
      bytecode =
        test_module do
          @callback my_fun(integer) :: integer
          @callback my_fun(list) :: list
          @callback my_fun() :: integer
          @callback my_fun(integer, integer) :: {integer, integer}
        end

      assert [my_fun_0, my_fun_1, my_fun_2] = callbacks(bytecode)

      assert {{:my_fun, 0}, [{:type, _, :fun, args}]} = my_fun_0
      assert [{:type, _, :product, []}, {:type, _, :integer, []}] = args

      assert {{:my_fun, 1}, [clause1, clause2]} = my_fun_1
      assert {:type, _, :fun, args1} = clause1
      assert [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}] = args1
      assert {:type, _, :fun, args2} = clause2
      assert [{:type, _, :product, [{:type, _, :list, []}]}, {:type, _, :list, []}] = args2

      assert {{:my_fun, 2}, [{:type, _, :fun, [args_type, return_type]}]} = my_fun_2

      assert {:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]} =
               args_type

      assert {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]} =
               return_type
    end

    test "@spec + @callback" do
      bytecode =
        test_module do
          def my_fun(x), do: x
          @spec my_fun(integer) :: integer
          @spec my_fun(charlist) :: charlist
          @callback cb(integer) :: integer
        end

      assert [{{:cb, 1}, [{:type, _, :fun, args}]}] = callbacks(bytecode)
      assert [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}] = args

      assert [{{:my_fun, 1}, [integer_clause, charlist_clause]}] = specs(bytecode)

      assert {:type, _, :fun, [{:type, _, :product, [arg]}, return]} = integer_clause
      assert {:type, _, :integer, []} = arg
      assert {:type, _, :integer, []} = return

      assert {:type, _, :fun, [{:type, _, :product, [arg]}, return]} = charlist_clause
      assert {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :charlist}, []]} = arg
      assert {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :charlist}, []]} = return
    end

    test "block handling" do
      bytecode =
        test_module do
          @spec foo((() -> [integer])) :: integer
          def foo(_), do: 1
        end

      assert [{{:foo, 1}, [{:type, _, :fun, [args, return]}]}] = specs(bytecode)
      assert {:type, _, :product, [{:type, _, :fun, fun_args}]} = args
      assert [{:type, _, :product, []}, {:type, _, :list, [{:type, _, :integer, []}]}] = fun_args
      assert {:type, _, :integer, []} = return
    end
  end

  describe "Code.Typespec" do
    test "type_to_quoted" do
      quoted =
        Enum.sort([
          quote(do: @type(with_ann() :: t :: atom())),
          quote(do: @type(a_tuple() :: tuple())),
          quote(do: @type(empty_tuple() :: {})),
          quote(do: @type(one_tuple() :: {:foo})),
          quote(do: @type(two_tuple() :: {:foo, :bar})),
          quote(do: @type(imm_type_1() :: 1)),
          quote(do: @type(imm_type_2() :: :foo)),
          quote(do: @type(simple_type() :: integer())),
          quote(do: @type(param_type(p) :: [p])),
          quote(do: @type(union_type() :: integer() | binary() | boolean())),
          quote(do: @type(binary_type1() :: <<_::_*8>>)),
          quote(do: @type(binary_type2() :: <<_::3>>)),
          quote(do: @type(binary_type3() :: <<_::3, _::_*8>>)),
          quote(do: @type(tuple_type() :: {integer()})),
          quote(
            do: @type(ftype() :: (() -> any()) | (() -> integer()) | (integer() -> integer()))
          ),
          quote(do: @type(cl() :: charlist())),
          quote(do: @type(st() :: struct())),
          quote(do: @type(ab() :: as_boolean(term()))),
          quote(do: @type(kw() :: keyword())),
          quote(do: @type(kwt() :: keyword(term()))),
          quote(do: @type(vaf() :: (... -> any()))),
          quote(do: @type(rng() :: 1..10)),
          quote(do: @type(opts() :: [first: integer(), step: integer(), last: integer()])),
          quote(do: @type(ops() :: {+1, -1})),
          quote(do: @type(a_map() :: map())),
          quote(do: @type(empty_map() :: %{})),
          quote(do: @type(my_map() :: %{hello: :world})),
          quote(do: @type(my_req_map() :: %{required(0) => :foo})),
          quote(do: @type(my_opt_map() :: %{optional(0) => :foo})),
          quote(do: @type(my_struct() :: %TypespecTest{hello: :world})),
          quote(do: @type(list1() :: list())),
          quote(do: @type(list2() :: [0])),
          quote(do: @type(list3() :: [...])),
          quote(do: @type(list4() :: [0, ...])),
          quote(do: @type(nil_list() :: []))
        ])

      bytecode =
        test_module do
          Module.eval_quoted(__MODULE__, quoted)
        end

      types = types(bytecode)

      Enum.each(Enum.zip(types, quoted), fn {{:type, type}, definition} ->
        ast = Code.Typespec.type_to_quoted(type)
        assert Macro.to_string(quote(do: @type(unquote(ast)))) == Macro.to_string(definition)
      end)
    end

    test "type_to_quoted for paren_type" do
      type = {:my_type, {:paren_type, 0, [{:type, 0, :integer, []}]}, []}

      assert Code.Typespec.type_to_quoted(type) ==
               {:::, [], [{:my_type, [], []}, {:integer, [line: 0], []}]}
    end

    test "spec_to_quoted" do
      quoted =
        Enum.sort([
          quote(do: @spec(foo() :: integer())),
          quote(do: @spec(foo(atom()) :: integer() | [{}])),
          quote(do: @spec(foo(arg) :: integer() when [arg: integer()])),
          quote(do: @spec(foo(arg) :: arg when [arg: var])),
          quote(do: @spec(foo(arg :: atom()) :: atom()))
        ])

      bytecode =
        test_module do
          def foo(), do: 1
          def foo(arg), do: arg
          Module.eval_quoted(__MODULE__, quote(do: (unquote_splicing(quoted))))
        end

      specs =
        Enum.flat_map(specs(bytecode), fn {{_, _}, specs} ->
          Enum.map(specs, fn spec ->
            quote(do: @spec(unquote(Code.Typespec.spec_to_quoted(:foo, spec))))
          end)
        end)

      specs_with_quoted = specs |> Enum.sort() |> Enum.zip(quoted)

      Enum.each(specs_with_quoted, fn {spec, definition} ->
        assert Macro.to_string(spec) == Macro.to_string(definition)
      end)
    end

    test "non-variables are given as arguments" do
      msg = ~r/The type one_bad_variable\/1 has an invalid argument\(s\): String.t\(\)/

      assert_raise CompileError, msg, fn ->
        test_module do
          @type one_bad_variable(String.t()) :: String.t()
        end
      end

      msg = ~r/The type two_bad_variables\/2 has an invalid argument\(s\): :ok, Enum.t\(\)/

      assert_raise CompileError, msg, fn ->
        test_module do
          @type two_bad_variables(:ok, Enum.t()) :: {:ok, []}
        end
      end

      msg = ~r/The type one_bad_one_good\/2 has an invalid argument\(s\): \"\"/

      assert_raise CompileError, msg, fn ->
        test_module do
          @type one_bad_one_good(input1, "") :: {:ok, input1}
        end
      end
    end

    test "retrieval invalid data" do
      assert Code.Typespec.fetch_types(Unknown) == :error
      assert Code.Typespec.fetch_specs(Unknown) == :error
    end

    # This is a test that implements all types specified in lib/elixir/pages/Typespecs.md
    test "documented types and their AST" do
      defmodule SomeStruct do
        defstruct [:key]
      end

      quoted =
        Enum.sort([
          ##  Basic types
          quote(do: @type(basic_any() :: any())),
          quote(do: @type(basic_none() :: none())),
          quote(do: @type(basic_atom() :: atom())),
          quote(do: @type(basic_map() :: map())),
          quote(do: @type(basic_pid() :: pid())),
          quote(do: @type(basic_port() :: port())),
          quote(do: @type(basic_reference() :: reference())),
          quote(do: @type(basic_struct() :: struct())),
          quote(do: @type(basic_tuple() :: tuple())),

          # Numbers
          quote(do: @type(basic_float() :: float())),
          quote(do: @type(basic_integer() :: integer())),
          quote(do: @type(basic_neg_integer() :: neg_integer())),
          quote(do: @type(basic_non_neg_integer() :: non_neg_integer())),
          quote(do: @type(basic_pos_integer() :: pos_integer())),

          # Lists
          quote(do: @type(basic_list_type() :: list(integer()))),
          quote(do: @type(basic_nonempty_list_type() :: nonempty_list(integer()))),
          quote do
            @type basic_maybe_improper_list_type() :: maybe_improper_list(integer(), atom())
          end,
          quote do
            @type basic_nonempty_improper_list_type() :: nonempty_improper_list(integer(), atom())
          end,
          quote do
            @type basic_nonempty_maybe_improper_list_type() ::
                    nonempty_maybe_improper_list(integer(), atom())
          end,

          ## Literals
          quote(do: @type(literal_atom() :: :atom)),
          quote(do: @type(literal_integer() :: 1)),
          quote(do: @type(literal_integers() :: 1..10)),
          quote(do: @type(literal_empty_bitstring() :: <<>>)),
          quote(do: @type(literal_size_0() :: <<_::0>>)),
          quote(do: @type(literal_unit_1() :: <<_::_*1>>)),
          quote(do: @type(literal_size_1_unit_8() :: <<_::100, _::_*256>>)),
          quote(do: @type(literal_function_arity_any() :: (... -> integer()))),
          quote(do: @type(literal_function_arity_0() :: (() -> integer()))),
          quote(do: @type(literal_function_arity_2() :: (integer(), atom() -> integer()))),
          quote(do: @type(literal_list_type() :: [integer()])),
          quote(do: @type(literal_empty_list() :: [])),
          quote(do: @type(literal_list_nonempty() :: [...])),
          quote(do: @type(literal_nonempty_list_type() :: [atom(), ...])),
          quote(do: @type(literal_keyword_list_fixed_key() :: [key: integer()])),
          quote(do: @type(literal_keyword_list_fixed_key2() :: [{:key, integer()}])),
          quote(do: @type(literal_keyword_list_type_key() :: [{binary(), integer()}])),
          quote(do: @type(literal_empty_map() :: %{})),
          quote(do: @type(literal_map_with_key() :: %{key: integer()})),
          quote(
            do: @type(literal_map_with_required_key() :: %{required(bitstring()) => integer()})
          ),
          quote(
            do: @type(literal_map_with_optional_key() :: %{optional(bitstring()) => integer()})
          ),
          quote(do: @type(literal_struct_all_fields_any_type() :: %SomeStruct{})),
          quote(do: @type(literal_struct_all_fields_key_type() :: %SomeStruct{key: integer()})),
          quote(do: @type(literal_empty_tuple() :: {})),
          quote(do: @type(literal_2_element_tuple() :: {1, 2})),

          ## Built-in types
          quote(do: @type(built_in_term() :: term())),
          quote(do: @type(built_in_arity() :: arity())),
          quote(do: @type(built_in_as_boolean() :: as_boolean(:t))),
          quote(do: @type(built_in_binary() :: binary())),
          quote(do: @type(built_in_bitstring() :: bitstring())),
          quote(do: @type(built_in_boolean() :: boolean())),
          quote(do: @type(built_in_byte() :: byte())),
          quote(do: @type(built_in_char() :: char())),
          quote(do: @type(built_in_charlist() :: charlist())),
          quote(do: @type(built_in_nonempty_charlist() :: nonempty_charlist())),
          quote(do: @type(built_in_fun() :: fun())),
          quote(do: @type(built_in_function() :: function())),
          quote(do: @type(built_in_identifier() :: identifier())),
          quote(do: @type(built_in_iodata() :: iodata())),
          quote(do: @type(built_in_iolist() :: iolist())),
          quote(do: @type(built_in_keyword() :: keyword())),
          quote(do: @type(built_in_keyword_value_type() :: keyword(:t))),
          quote(do: @type(built_in_list() :: list())),
          quote(do: @type(built_in_nonempty_list() :: nonempty_list())),
          quote(do: @type(built_in_maybe_improper_list() :: maybe_improper_list())),
          quote(
            do: @type(built_in_nonempty_maybe_improper_list() :: nonempty_maybe_improper_list())
          ),
          quote(do: @type(built_in_mfa() :: mfa())),
          quote(do: @type(built_in_module() :: module())),
          quote(do: @type(built_in_no_return() :: no_return())),
          quote(do: @type(built_in_node() :: node())),
          quote(do: @type(built_in_number() :: number())),
          quote(do: @type(built_in_struct() :: struct())),
          quote(do: @type(built_in_timeout() :: timeout())),

          ## Remote types
          quote(do: @type(remote_enum_t0() :: Enum.t())),
          quote(do: @type(remote_keyword_t1() :: Keyword.t(integer())))
        ])

      bytecode =
        test_module do
          Module.eval_quoted(__MODULE__, quoted)
        end

      types = types(bytecode)

      Enum.each(Enum.zip(types, quoted), fn {{:type, type}, definition} ->
        ast = Code.Typespec.type_to_quoted(type)
        ast_string = Macro.to_string(quote(do: @type(unquote(ast))))

        case type do
          # These cases do not translate directly to their own string version.
          {:basic_list_type, _, _} ->
            assert ast_string == "@type(basic_list_type() :: [integer()])"

          {:basic_nonempty_list_type, _, _} ->
            assert ast_string == "@type(basic_nonempty_list_type() :: [integer(), ...])"

          {:literal_empty_bitstring, _, _} ->
            assert ast_string == "@type(literal_empty_bitstring() :: <<_::0>>)"

          {:literal_keyword_list_fixed_key, _, _} ->
            assert ast_string == "@type(literal_keyword_list_fixed_key() :: [{:key, integer()}])"

          {:literal_keyword_list_fixed_key2, _, _} ->
            assert ast_string == "@type(literal_keyword_list_fixed_key2() :: [{:key, integer()}])"

          {:literal_struct_all_fields_any_type, _, _} ->
            assert ast_string ==
                     "@type(literal_struct_all_fields_any_type() :: %TypespecTest.SomeStruct{key: term()})"

          {:literal_struct_all_fields_key_type, _, _} ->
            assert ast_string ==
                     "@type(literal_struct_all_fields_key_type() :: %TypespecTest.SomeStruct{key: integer()})"

          {:built_in_fun, _, _} ->
            assert ast_string == "@type(built_in_fun() :: (... -> any()))"

          {:built_in_nonempty_list, _, _} ->
            assert ast_string == "@type(built_in_nonempty_list() :: [...])"

          _ ->
            assert ast_string == Macro.to_string(definition)
        end
      end)
    end
  end

  describe "behaviour_info" do
    defmodule SampleCallbacks do
      @callback first(integer) :: integer
      @callback foo(atom(), binary) :: binary
      @callback bar(External.hello(), my_var :: binary) :: binary
      @callback guarded(my_var) :: my_var when my_var: binary
      @callback orr(atom | integer) :: atom
      @callback literal(123, {atom}, :foo, [integer], true) :: atom
      @macrocallback last(integer) :: Macro.t()
      @macrocallback last() :: atom
      @optional_callbacks bar: 2, last: 0
      @optional_callbacks first: 1
    end

    test "defines callbacks" do
      expected_callbacks = [
        "MACRO-last": 1,
        "MACRO-last": 2,
        bar: 2,
        first: 1,
        foo: 2,
        guarded: 1,
        literal: 5,
        orr: 1
      ]

      assert Enum.sort(SampleCallbacks.behaviour_info(:callbacks)) == expected_callbacks
    end

    test "defines optional callbacks" do
      assert Enum.sort(SampleCallbacks.behaviour_info(:optional_callbacks)) ==
               ["MACRO-last": 1, bar: 2, first: 1]
    end
  end
end
