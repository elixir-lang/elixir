Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.TypespecTest do
  use ExUnit.Case

  alias Kernel.TypespecTest.TestTypespec

  defstruct [:hello]

  # This macro allows us to focus on the result of the
  # definition and not on the hassles of handling test
  # module.
  defmacrop test_module(do: block) do
    quote do
      {:module, _, bytecode, _} = defmodule TestTypespec do
        unquote(block)
      end
      :code.delete(TestTypespec)
      :code.purge(TestTypespec)
      bytecode
    end
  end

  defp types(bytecode) do
    Kernel.Typespec.beam_types(bytecode)
    |> Enum.sort
  end

  @skip_specs [__info__: 1]

  defp specs(bytecode) do
    Kernel.Typespec.beam_specs(bytecode)
    |> Enum.reject(fn {sign, _} -> sign in @skip_specs end)
    |> Enum.sort()
  end

  defp callbacks(bytecode) do
    Kernel.Typespec.beam_callbacks(bytecode)
    |> Enum.sort
  end

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

  test "@type with a single type" do
    bytecode = test_module do
      @type my_type :: term
    end

    assert [type: {:my_type, {:type, _, :term, []}, []}] =
           types(bytecode)
  end

  test "@type with an atom" do
    bytecode = test_module do
      @type my_type :: :foo
    end

    assert [type: {:my_type, {:atom, _, :foo}, []}] =
           types(bytecode)
  end

  test "@type with an atom alias" do
    bytecode = test_module do
      @type my_type :: Atom
    end

    assert [type: {:my_type, {:atom, _, Atom}, []}] =
           types(bytecode)
  end

  test "@type with an integer" do
    bytecode = test_module do
      @type my_type :: 10
    end
    assert [type: {:my_type, {:integer, _, 10}, []}] =
           types(bytecode)
  end

  test "@type with a negative integer" do
    bytecode = test_module do
      @type my_type :: -10
    end

    assert [type: {:my_type, {:op, _, :-, {:integer, _, 10}}, []}] =
           types(bytecode)
  end

  test "@type with a remote type" do
    bytecode = test_module do
      @type my_type :: Remote.Some.type
      @type my_type_arg :: Remote.type(integer)
    end

    assert [type: {:my_type, {:remote_type, _, [{:atom, _, Remote.Some}, {:atom, _, :type}, []]}, []},
            type: {:my_type_arg, {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :type}, [{:type, _, :integer, []}]]}, []}] =
           types(bytecode)
  end

  test "@type with a binary" do
    bytecode = test_module do
      @type my_type :: binary
    end

    assert [type: {:my_type, {:type, _, :binary, []}, []}] =
           types(bytecode)
  end

  test "@type with an empty binary" do
    bytecode = test_module do
      @type my_type :: <<>>
    end

    assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 0}]}, []}] =
           types(bytecode)
  end

  test "@type with a binary with a base size" do
    bytecode = test_module do
      @type my_type :: <<_::3>>
    end

    assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 3}, {:integer, _, 0}]}, []}] =
           types(bytecode)
  end

  test "@type with a binary with a unit size" do
    bytecode = test_module do
      @type my_type :: <<_::_*8>>
    end

    assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 8}]}, []}] =
           types(bytecode)
  end

  test "@type with a binary with a size and unit size" do
    bytecode = test_module do
      @type my_type :: <<_::3, _::_*8>>
    end

    assert [type: {:my_type, {:type, _, :binary, [{:integer, _, 3}, {:integer, _, 8}]}, []}] =
           types(bytecode)
  end

  test "@type with invalid binary spec" do
    assert_raise CompileError, fn ->
      test_module do
        @type my_type :: <<_::3*8>>
      end
    end
  end

  test "@type with a range op" do
    bytecode = test_module do
      @type my_type :: 1..10
    end

    assert [type: {:my_type, {:type, _, :range, [{:integer, _, 1}, {:integer, _, 10}]}, []}] =
           types(bytecode)
  end

  test "@type with a keyword map" do
    bytecode = test_module do
      @type my_type :: %{hello: :world}
    end

    assert [type: {:my_type,
             {:type, _, :map, [
               {:type, _, :map_field_exact, [{:atom, _, :hello}, {:atom, _, :world}]}
             ]},
            []}] = types(bytecode)
  end

  test "@type with a map" do
    bytecode = test_module do
      @type my_type :: %{required(:a) => :b, optional(:c) => :d}
    end

    assert [type: {:my_type,
             {:type, _, :map, [
               {:type, _, :map_field_exact, [{:atom, _, :a}, {:atom, _, :b}]},
               {:type, _, :map_field_assoc, [{:atom, _, :c}, {:atom, _, :d}]}
             ]},
            []}] = types(bytecode)
  end

  test "@type with a struct" do
    bytecode = test_module do
      defstruct [hello: nil, other: nil]
      @type my_type :: %TestTypespec{hello: :world}
    end

    assert [type: {:my_type,
             {:type, _, :map, [
               {:type, _, :map_field_exact, [{:atom, _, :__struct__}, {:atom, _, TestTypespec}]},
               {:type, _, :map_field_exact, [{:atom, _, :hello}, {:atom, _, :world}]},
               {:type, _, :map_field_exact, [{:atom, _, :other}, {:type, _, :term, []}]}
             ]},
            []}] = types(bytecode)
  end

  test "@type with undefined struct" do
    assert_raise UndefinedFunctionError, fn ->
      test_module do
        @type my_type :: %ThisModuleDoesNotExist{}
      end
    end

    assert_raise CompileError, ~r"struct is not defined for TestTypespec", fn ->
      test_module do
        @type my_type :: %TestTypespec{}
      end
    end
  end

  test "@type with a struct with undefined field" do
    assert_raise CompileError, ~r"undefined field no_field on struct TestTypespec", fn ->
      test_module do
        defstruct [:hello, :eric]
        @type my_type :: %TestTypespec{no_field: :world}
      end
    end
  end

  test "@type when overriding Elixir built-in" do
    assert_raise CompileError, ~r"type struct/0 is a builtin type", fn ->
      test_module do
        @type struct :: :oops
      end
    end
  end

  test "@type when overriding Erlang built-in" do
    assert_raise CompileError, ~r"type list/0 is a builtin type", fn ->
      test_module do
        @type list :: :oops
      end
    end
  end

  test "@type with public record" do
    bytecode = test_module do
      require Record
      Record.defrecord :timestamp, [date: 1, time: 2]
      @type my_type :: record(:timestamp, time: :foo)
    end

    assert [type: {:my_type,
             {:type, _, :tuple, [
               {:atom, 0, :timestamp}, {:type, 0, :term, []}, {:atom, 0, :foo}
             ]},
            []}] = types(bytecode)
  end

  test "@type with private record" do
    bytecode = test_module do
      require Record
      Record.defrecordp :timestamp, [date: 1, time: 2]
      @type my_type :: record(:timestamp, time: :foo)
    end

    assert [type: {:my_type,
             {:type, _, :tuple, [
               {:atom, 0, :timestamp}, {:type, 0, :term, []}, {:atom, 0, :foo}
             ]},
            []}] = types(bytecode)
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
        Record.defrecord :timestamp, [date: 1, time: 2]
        @type my_type :: record(:timestamp, no_field: :foo)
      end
    end
  end

  test "@type with an invalid map notation" do
    assert_raise CompileError, ~r"invalid map specification", fn ->
      test_module do
        @type content :: %{atom | String.t => term}
      end
    end
  end

  test "@type with list shortcuts" do
    bytecode = test_module do
      @type my_type :: []
      @type my_type1 :: [integer]
      @type my_type2 :: [integer, ...]
    end

    assert [type: {:my_type, {:type, _, nil, []}, []},
            type: {:my_type1, {:type, _, :list, [{:type, _, :integer, []}]}, []},
            type: {:my_type2, {:type, _, :nonempty_list, [{:type, _, :integer, []}]}, []}] =
           types(bytecode)
  end

  test "@type with a fun" do
    bytecode = test_module do
      @type my_type :: (... -> any)
    end

    assert [type: {:my_type, {:type, _, :fun, []}, []}] =
           types(bytecode)
  end

  test "@type with a fun with multiple arguments and return type" do
    bytecode = test_module do
      @type my_type :: (integer, integer -> integer)
    end

    assert [type: {:my_type, {:type, _, :fun, [{:type, _, :product,
             [{:type, _, :integer, []}, {:type, _, :integer, []}]},
             {:type, _, :integer, []}]}, []}] =
           types(bytecode)
  end

  test "@type with a fun with no arguments and return type" do
    bytecode = test_module do
      @type my_type :: (() -> integer)
    end

    assert [type: {:my_type, {:type, _, :fun, [{:type, _, :product, []},
             {:type, _, :integer, []}]}, []}] =
           types(bytecode)
  end

  test "@type with a fun with any arity and return type" do
    bytecode = test_module do
      @type my_type :: (... -> integer)
    end

    assert [type: {:my_type, {:type, _, :fun, [{:type, _, :any},
             {:type, _, :integer, []}]}, []}] =
           types(bytecode)
  end

  test "@type with a union" do
    bytecode = test_module do
      @type my_type :: integer | charlist | atom
    end

    assert [type: {:my_type, {:type, _, :union, [{:type, _, :integer, []},
             {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :charlist}, []]},
             {:type, _, :atom, []}]}, []}] =
           types(bytecode)
  end

  test "@type with keywords" do
    bytecode = test_module do
      @type my_type :: [first: integer, step: integer, last: integer]
    end

    assert [type: {:my_type, {:type, _, :list, [
      {:type, _, :union, [
        {:type, _, :tuple, [{:atom, _, :first}, {:type, _, :integer, []}]},
        {:type, _, :tuple, [{:atom, _, :step}, {:type, _, :integer, []}]},
        {:type, _, :tuple, [{:atom, _, :last}, {:type, _, :integer, []}]}
      ]}
    ]}, []}] = types(bytecode)
  end

  test "@type with parameters" do
    bytecode = test_module do
      @type my_type(x) :: x
      @type my_type1(x) :: list(x)
      @type my_type2(x, y) :: {x, y}
    end

    assert [type: {:my_type, {:var, _, :x}, [{:var, _, :x}]},
            type: {:my_type1, {:type, _, :list, [{:var, _, :x}]}, [{:var, _, :x}]},
            type: {:my_type2, {:type, _, :tuple, [{:var, _, :x}, {:var, _, :y}]}, [{:var, _, :x}, {:var, _, :y}]}] =
           types(bytecode)
  end

  test "@type with annotations" do
    bytecode = test_module do
      @type my_type :: (named :: integer)
      @type my_type1 :: (a :: integer -> integer)
    end

    assert [type: {:my_type, {:ann_type, _, [{:var, _, :named}, {:type, _, :integer, []}]}, []},
            type: {:my_type1, {:type, _, :fun, [{:type, _, :product, [{:ann_type, _, [{:var, _, :a}, {:type, _, :integer, []}]}]}, {:type, _, :integer, []}]}, []}] =
           types(bytecode)
  end

  test "@opaque(type)" do
    bytecode = test_module do
      @opaque my_type(x) :: x
    end

    assert [opaque: {:my_type, {:var, _, :x}, [{:var, _, :x}]}] =
           types(bytecode)
  end

  test "@type + opaque" do
    bytecode = test_module do
      @type my_type :: tuple
      @opaque my_type1 :: {}
    end

    assert [opaque: {:my_type1, _, []},
            type: {:my_type, _, []}, ] =
           types(bytecode)
  end

  test "@type unquote fragment" do
    quoted = quote unquote: false do
      name = :my_type
      type = :foo
      @type unquote(name)() :: unquote(type)
    end
    bytecode = test_module do
      Module.eval_quoted(__MODULE__, quoted)
    end

    assert [type: {:my_type, {:atom, _, :foo}, []}] =
           types(bytecode)
  end

  test "@type with module attributes" do
    bytecode = test_module do
      @keyword Keyword
      @type kw :: @keyword.t
      @type kw(value) :: @keyword.t(value)
    end

    assert [type: {:kw, {:remote_type, _, [{:atom, _, Keyword}, {:atom, _, :t}, []]}, _},
            type: {:kw, {:remote_type, _, [{:atom, _, Keyword}, {:atom, _, :t}, [{:var, _, :value}]]}, [{:var, _, :value}]}] =
           types(bytecode)
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
      assert Kernel.Typespec.defines_type?(__MODULE__, :my_type, 0)
      assert Kernel.Typespec.defines_type?(__MODULE__, :my_type, 1)
      refute Kernel.Typespec.defines_type?(__MODULE__, :my_type, 2)
    end
  end

  test "@spec(spec)" do
    bytecode = test_module do
      def my_fun1(x), do: x
      def my_fun2(), do: :ok
      def my_fun3(x, y), do: {x, y}
      def my_fun4(x), do: x
      @spec my_fun1(integer) :: integer
      @spec my_fun2() :: integer
      @spec my_fun3(integer, integer) :: {integer, integer}
      @spec my_fun4(x :: integer) :: integer
    end

    assert [{{:my_fun1, 1}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]},
            {{:my_fun2, 0}, [{:type, _, :fun, [{:type, _, :product, []}, {:type, _, :integer, []}]}]},
            {{:my_fun3, 2}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]}, {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]}]}]},
            {{:my_fun4, 1}, [{:type, _, :fun, [{:type, _, :product, [{:ann_type, _, [{:var, _, :x}, {:type, _, :integer, []}]}]}, {:type, _, :integer, []}]}]}] =
           specs(bytecode)
  end

  test "@spec(spec) for unreachable private function" do
    # Run it inside capture_io/2 so that the "my_fun/1 is unused"
    # warning doesn't get printed among the ExUnit test results.
    output = ExUnit.CaptureIO.capture_io :stderr, fn ->
      bytecode = test_module do
        defp my_fun(x), do: x
        @spec my_fun(integer) :: integer
      end

      assert [] == specs(bytecode)
    end

    assert output != ""
  end

  test "@spec(spec) with guards" do
    bytecode = test_module do
      def my_fun1(x), do: x
      @spec my_fun1(x) :: boolean when [x: integer]

      def my_fun2(x), do: x
      @spec my_fun2(x) :: x when [x: var]

      def my_fun3(_x, y), do: y
      @spec my_fun3(x, y) :: y when [y: x, x: var]
    end

    assert [{{:my_fun1, 1}, [{:type, _, :bounded_fun, [{:type, _, :fun, [{:type, _, :product, [{:var, _, :x}]}, {:type, _, :boolean, []}]}, [{:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, :x}, {:type, _, :integer, []}]]}]]}]},
            {{:my_fun2, 1}, [{:type, _, :fun, [{:type, _, :product, [{:var, _, :x}]}, {:var, _, :x}]}]},
            {{:my_fun3, 2}, [{:type, _, :bounded_fun, [{:type, _, :fun, [{:type, _, :product, [{:var, _, :x}, {:var, _, :y}]}, {:var, _, :y}]}, [{:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, :y}, {:var, _, :x}]]}]]}]}] =
           specs(bytecode)
  end

  test "@callback(callback)" do
    bytecode = test_module do
      @callback my_fun(integer) :: integer
      @callback my_fun() :: integer
      @callback my_fun(integer, integer) :: {integer, integer}
    end

    assert [{{:my_fun, 0}, [{:type, _, :fun, [{:type, _, :product, []}, {:type, _, :integer, []}]}]},
            {{:my_fun, 1}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]},
            {{:my_fun, 2}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]}, {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]}]}]}] =
           callbacks(bytecode)
  end

  test "@spec + @callback" do
    bytecode = test_module do
      def my_fun(x), do: x
      @spec my_fun(integer) :: integer
      @spec my_fun(charlist) :: charlist
      @callback cb(integer) :: integer
    end

    assert [{{:cb, 1}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]}] =
           callbacks(bytecode)

    assert [{{:my_fun, 1}, [
             {:type, _, :fun, [{:type, _, :product, [
               {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :charlist}, []]}]},
               {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :charlist}, []]}]},
             {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]}] =
           specs(bytecode)
  end

  test "block handling" do
    bytecode = test_module do
      @spec foo((() -> [ integer ])) :: integer
      def foo(_), do: 1
    end
    assert [{{:foo, 1},
            [{:type, _, :fun, [{:type, _, :product, [
                     {:type, _, :fun, [{:type, _, :product, []}, {:type, _, :list, [{:type, _, :integer, []}]}]}]},
                     {:type, _, :integer, []}]}]}] =
           specs(bytecode)
  end

  # Conversion to AST

  test "type_to_ast" do
    quoted = [
      quote(do: @type with_ann() :: (t :: atom())),
      quote(do: @type a_tuple() :: tuple()),
      quote(do: @type empty_tuple() :: {}),
      quote(do: @type one_tuple() :: {:foo}),
      quote(do: @type two_tuple() :: {:foo, :bar}),
      quote(do: @type imm_type_1() :: 1),
      quote(do: @type imm_type_2() :: :foo),
      quote(do: @type simple_type() :: integer()),
      quote(do: @type param_type(p) :: [p]),
      quote(do: @type union_type() :: integer() | binary() | boolean()),
      quote(do: @type binary_type1() :: <<_::_*8>>),
      quote(do: @type binary_type2() :: <<_::3>>),
      quote(do: @type binary_type3() :: <<_::3, _::_*8>>),
      quote(do: @type tuple_type() :: {integer()}),
      quote(do: @type ftype() :: (() -> any()) | (() -> integer()) | ((integer() -> integer()))),
      quote(do: @type cl() :: charlist()),
      quote(do: @type st() :: struct()),
      quote(do: @type ab() :: as_boolean(term())),
      quote(do: @type kw() :: keyword()),
      quote(do: @type kwt() :: keyword(term())),
      quote(do: @type vaf() :: (... -> any())),
      quote(do: @type rng() :: 1..10),
      quote(do: @type opts() :: [first: integer(), step: integer(), last: integer()]),
      quote(do: @type ops() :: {+1, -1}),
      quote(do: @type a_map() :: map()),
      quote(do: @type empty_map() :: %{}),
      quote(do: @type my_map() :: %{hello: :world}),
      quote(do: @type my_req_map() :: %{required(0) => :foo}),
      quote(do: @type my_opt_map() :: %{optional(0) => :foo}),
      quote(do: @type my_struct() :: %Kernel.TypespecTest{hello: :world}),
      quote(do: @type list1() :: list()),
      quote(do: @type list2() :: [0]),
      quote(do: @type list3() :: [...]),
      quote(do: @type list4() :: [0, ...]),
      quote(do: @type nil_list() :: []),
    ]
    |> Enum.sort

    bytecode = test_module do
      Module.eval_quoted __MODULE__, quoted
    end

    types = types(bytecode)

    Enum.each(Enum.zip(types, quoted), fn {{:type, type}, definition} ->
      ast = Kernel.Typespec.type_to_ast(type)
      assert Macro.to_string(quote(do: @type unquote(ast))) == Macro.to_string(definition)
    end)
  end

  # This is a test that implements all types specified in lib/elixir/pages/Typespecs.md
  test "test documented types and their AST" do
    defmodule SomeStruct do
      defstruct [:key]
    end

    quoted = [
      ##  Basic types
      quote(do: @type basic_any() :: any()),
      quote(do: @type basic_none() :: none()),
      quote(do: @type basic_atom() :: atom()),
      quote(do: @type basic_map() :: map()),
      quote(do: @type basic_pid() :: pid()),
      quote(do: @type basic_port() :: port()),
      quote(do: @type basic_reference() :: reference()),
      quote(do: @type basic_struct() :: struct()),
      quote(do: @type basic_tuple() :: tuple()),
      # Numbers
      quote(do: @type basic_float() :: float()),
      quote(do: @type basic_integer() :: integer()),
      quote(do: @type basic_neg_integer() :: neg_integer()),
      quote(do: @type basic_non_neg_integer() :: non_neg_integer()),
      quote(do: @type basic_pos_integer() :: pos_integer()),
      # Lists
      quote(do: @type basic_list_type() :: list(integer())),
      quote(do: @type basic_nonempty_list_type() :: nonempty_list(integer())),
      quote(do: @type basic_maybe_improper_list_type() :: maybe_improper_list(integer(), atom())),
      quote(do: @type basic_nonempty_improper_list_type() :: nonempty_improper_list(integer(), atom())),
      quote(do: @type basic_nonempty_maybe_improper_list_type() :: nonempty_maybe_improper_list(integer(), atom())),

      ## Literals
      quote(do: @type literal_atom() :: :atom),
      quote(do: @type literal_integer() :: 1),
      quote(do: @type literal_integers() :: 1..10),
      quote(do: @type literal_empty_bitstring() :: <<>>),
      quote(do: @type literal_size_0() :: <<_::0>>),
      quote(do: @type literal_unit_1() :: <<_::_*1>>),
      quote(do: @type literal_size_1_unit_8() :: <<_::100, _::_*256>>),
      quote(do: @type literal_function_arity_any() :: (... -> integer())),
      quote(do: @type literal_function_arity_0() :: (() -> integer())),
      quote(do: @type literal_function_arity_2() :: (integer(), atom() -> integer())),
      quote(do: @type literal_list_type() :: [integer()]),
      quote(do: @type literal_empty_list() :: []),
      quote(do: @type literal_list_nonempty() :: [...]),
      quote(do: @type literal_nonempty_list_type() :: [atom(), ...]),
      quote(do: @type literal_keyword_list_fixed_key() :: [key: integer()]),
      quote(do: @type literal_keyword_list_fixed_key2() :: [{:key, integer()}]),
      quote(do: @type literal_keyword_list_type_key() :: [{binary(), integer()}]),
      quote(do: @type literal_empty_map() :: %{}),
      quote(do: @type literal_map_with_key() :: %{key: integer()}),
      quote(do: @type literal_map_with_required_key() :: %{required(bitstring()) => integer()}),
      quote(do: @type literal_map_with_optional_key() :: %{optional(bitstring()) => integer()}),
      # TODO: Remove by 1.5, when the following line with give a warning
      quote(do: @type literal_map_with_arrow() :: %{any() => any()}),
      quote(do: @type literal_struct_all_fields_any_type() :: %SomeStruct{}),
      quote(do: @type literal_struct_all_fields_key_type() :: %SomeStruct{key: integer()}),
      quote(do: @type literal_empty_tuple() :: {}),
      quote(do: @type literal_2_element_tuple() :: {1, 2}),

      ## Built-in types
      quote(do: @type builtin_term() :: term()),
      quote(do: @type builtin_arity() :: arity()),
      quote(do: @type builtin_as_boolean() :: as_boolean(:t)),
      quote(do: @type builtin_binary() :: binary()),
      quote(do: @type builtin_bitstring() :: bitstring()),
      quote(do: @type builtin_boolean() :: boolean()),
      quote(do: @type builtin_byte() :: byte()),
      quote(do: @type builtin_char() :: char()),
      quote(do: @type builtin_charlist() :: charlist()),
      quote(do: @type builtin_fun() :: fun()),
      quote(do: @type builtin_function() :: function()),
      quote(do: @type builtin_identifier() :: identifier()),
      quote(do: @type builtin_iodata() :: iodata()),
      quote(do: @type builtin_iolist() :: iolist()),
      quote(do: @type builtin_keyword() :: keyword()),
      quote(do: @type builtin_keyword_value_type() :: keyword(:t)),
      quote(do: @type builtin_list() :: list()),
      quote(do: @type builtin_nonempty_list() :: nonempty_list()),
      quote(do: @type builtin_maybe_improper_list() :: maybe_improper_list()),
      quote(do: @type builtin_nonempty_maybe_improper_list() :: nonempty_maybe_improper_list()),
      quote(do: @type builtin_mfa() :: mfa()),
      quote(do: @type builtin_module() :: module()),
      quote(do: @type builtin_no_return() :: no_return()),
      quote(do: @type builtin_node() :: node()),
      quote(do: @type builtin_number() :: number()),
      quote(do: @type builtin_struct() :: struct()),
      quote(do: @type builtin_timeout() :: timeout()),

      ## Remote types
      quote(do: @type remote_enum_t0() :: Enum.t()),
      quote(do: @type remote_keyword_t1() :: Keyword.t(integer())),
    ]
    |> Enum.sort

    bytecode = test_module do
      Module.eval_quoted __MODULE__, quoted
    end

    types = types(bytecode)

    Enum.each(Enum.zip(types, quoted), fn {{:type, type}, definition} ->
      ast = Kernel.Typespec.type_to_ast(type)
      ast_string = Macro.to_string(quote(do: @type unquote(ast)))

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

        # TODO: Remove by 1.5
        {:literal_map_with_arrow, _, _} ->
          assert ast_string == "@type(literal_map_with_arrow() :: %{optional(any()) => any()})"

        {:literal_struct_all_fields_any_type, _, _} ->
          assert ast_string == "@type(literal_struct_all_fields_any_type() :: %Kernel.TypespecTest.SomeStruct{key: term()})"

        {:literal_struct_all_fields_key_type, _, _} ->
          assert ast_string == "@type(literal_struct_all_fields_key_type() :: %Kernel.TypespecTest.SomeStruct{key: integer()})"

        {:builtin_fun, _, _} ->
          assert ast_string == "@type(builtin_fun() :: (... -> any()))"

        {:builtin_function, _, _} ->
          assert ast_string == "@type(builtin_function() :: (... -> any()))"

        {:builtin_nonempty_list, _, _} ->
          assert ast_string == "@type(builtin_nonempty_list() :: [...])"

        _ ->
          assert ast_string == Macro.to_string(definition)
      end
    end)
  end

  test "type_to_ast for paren_type" do
    type = {:my_type, {:paren_type, 0, [{:type, 0, :integer, []}]}, []}
    assert Kernel.Typespec.type_to_ast(type) ==
      {:::, [], [{:my_type, [], []}, {:integer, [line: 0], []}]}
  end

  test "spec_to_ast" do
    quoted = [
      quote(do: @spec foo() :: integer()),
      quote(do: @spec foo(atom()) :: integer() | [{}]),
      quote(do: @spec foo(arg) :: integer() when [arg: integer()]),
      quote(do: @spec foo(arg) :: arg when [arg: var]),
      quote(do: @spec foo(arg :: atom()) :: atom()),
    ]
    |> Enum.sort

    bytecode = test_module do
      def foo(), do: 1
      def foo(arg), do: arg
      Module.eval_quoted __MODULE__, quote(do: (unquote_splicing(quoted)))
    end

    specs = Enum.flat_map(specs(bytecode), fn {{_, _}, specs} ->
       Enum.map(specs, fn spec ->
         quote(do: @spec unquote(Kernel.Typespec.spec_to_ast(:foo, spec)))
       end)
    end)
    |> Enum.sort

    Enum.each(Enum.zip(specs, quoted), fn {spec, definition} ->
      assert Macro.to_string(spec) == Macro.to_string(definition)
    end)
  end

  test "retrieval invalid data" do
    assert Kernel.Typespec.beam_types(Unknown) == nil
    assert Kernel.Typespec.beam_specs(Unknown) == nil
  end

  defmodule SampleCallbacks do
    @callback first(integer) :: integer
    @callback foo(atom(), binary) :: binary
    @callback bar(External.hello, my_var :: binary) :: binary
    @callback guarded(my_var) :: my_var when my_var: binary
    @callback orr(atom | integer) :: atom
    @callback literal(123, {atom}, :foo, [integer], true) :: atom
    @macrocallback last(integer) :: Macro.t
    @macrocallback last() :: atom
    @optional_callbacks bar: 2, last: 0
  end

  test "callbacks" do
    assert SampleCallbacks.behaviour_info(:callbacks) |> Enum.sort() ==
           ["MACRO-last": 1, "MACRO-last": 2, bar: 2, first: 1, foo: 2, guarded: 1, literal: 5, orr: 1]
  end

  test "optional callbacks" do
    assert SampleCallbacks.behaviour_info(:optional_callbacks) |> Enum.sort() ==
           ["MACRO-last": 1, bar: 2]
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
        @macrocallback hello(num \\ 0 :: integer) :: Macro.t
      end
    end

    assert_raise ArgumentError, fn ->
      test_module do
        @macrocallback hello(num :: integer \\ 0) :: Macro.t
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
    assert_raise CompileError, ~r"type specification missing return type: my_fun\(integer\)", fn ->
      test_module do
        @spec my_fun(integer)
      end
    end
  end
end
