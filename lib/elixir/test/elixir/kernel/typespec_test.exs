Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.TypespecTest do
  use ExUnit.Case, async: true

  alias Kernel.TypespecTest.TestTypespec

  defstruct [:hello]

  # This macro allows us to focus on the result of the
  # definition and not on the hassles of handling test
  # module
  defmacrop test_module([{:do, block}]) do
    quote do
      {:module, _, binary, _} = defmodule TestTypespec do
        unquote(block)
      end
      :code.delete(TestTypespec)
      :code.purge(TestTypespec)
      binary
    end
  end

  defp types(module) do
    Kernel.Typespec.beam_types(module)
    |> Enum.sort
  end

  @skip_specs [__info__: 1]

  defp specs(module) do
    Kernel.Typespec.beam_specs(module)
    |> Enum.reject(fn {sign, _} -> sign in @skip_specs end)
    |> Enum.sort()
  end

  defp callbacks(module) do
    Kernel.Typespec.beam_callbacks(module)
    |> Enum.sort
  end

  test "invalid type specification" do
    assert_raise CompileError, ~r"invalid type specification: mytype = 1", fn ->
      test_module do
        @type mytype = 1
      end
    end
  end

  test "unexpected expression in typespec" do
    assert_raise CompileError, ~r"unexpected expression in typespec: \"foobar\"", fn ->
      test_module do
        @type mytype :: "foobar"
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
    module = test_module do
      @type mytype :: term
    end

    assert [type: {:mytype, {:type, _, :term, []}, []}] =
           types(module)
  end

  test "@type with an atom" do
    module = test_module do
      @type mytype :: :atom
    end

    assert [type: {:mytype, {:atom, _, :atom}, []}] =
           types(module)
  end

  test "@type with an atom alias" do
    module = test_module do
      @type mytype :: Atom
    end

    assert [type: {:mytype, {:atom, _, Atom}, []}] =
           types(module)
  end

  test "@type with an integer" do
    module = test_module do
      @type mytype :: 10
    end
    assert [type: {:mytype, {:integer, _, 10}, []}] =
           types(module)
  end

  test "@type with a negative integer" do
    module = test_module do
      @type mytype :: -10
    end

    assert [type: {:mytype, {:op, _, :-, {:integer, _, 10}}, []}] =
           types(module)
  end

  test "@type with a remote type" do
    module = test_module do
      @type mytype :: Remote.Some.type
      @type mytype_arg :: Remote.type(integer)
    end

    assert [type: {:mytype, {:remote_type, _, [{:atom, _, Remote.Some}, {:atom, _, :type}, []]}, []},
            type: {:mytype_arg, {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :type}, [{:type, _, :integer, []}]]}, []}] =
           types(module)
  end

  test "@type with a binary" do
    module = test_module do
      @type mytype :: binary
    end

    assert [type: {:mytype, {:type, _, :binary, []}, []}] =
           types(module)
  end

  test "@type with an empty binary" do
    module = test_module do
      @type mytype :: <<>>
    end

    assert [type: {:mytype, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 0}]}, []}] =
           types(module)
  end

  test "@type with a binary with a base size" do
    module = test_module do
      @type mytype :: <<_ :: 3>>
    end

    assert [type: {:mytype, {:type, _, :binary, [{:integer, _, 3}, {:integer, _, 0}]}, []}] =
           types(module)
  end

  test "@type with a binary with a unit size" do
    module = test_module do
      @type mytype :: <<_ :: _ * 8>>
    end

    assert [type: {:mytype, {:type, _, :binary, [{:integer, _, 0}, {:integer, _, 8}]}, []}] =
           types(module)
  end

  test "@type with a range op" do
    module = test_module do
      @type mytype :: 1..10
    end

    assert [type: {:mytype, {:type, _, :range, [{:integer, _, 1}, {:integer, _, 10}]}, []}] =
           types(module)
  end

  test "@type with a map" do
    module = test_module do
      @type mytype :: %{hello: :world}
    end

    assert [type: {:mytype,
             {:type, _, :map, [
               {:type, _, :map_field_assoc, [{:atom, _, :hello}, {:atom, _, :world}]}
             ]},
            []}] = types(module)
  end

  test "@type with a struct" do
    module = test_module do
      defstruct [hello: nil, other: nil]
      @type mytype :: %TestTypespec{hello: :world}
    end

    assert [type: {:mytype,
             {:type, _, :map, [
               {:type, _, :map_field_assoc, [{:atom, _, :__struct__}, {:atom, _, TestTypespec}]},
               {:type, _, :map_field_assoc, [{:atom, _, :hello}, {:atom, _, :world}]},
               {:type, _, :map_field_assoc, [{:atom, _, :other}, {:type, _, :term, []}]}
             ]},
            []}] = types(module)
  end

  test "@type with undefined struct" do
    assert_raise UndefinedFunctionError, fn ->
      test_module do
        @type mytype :: %ThisModuleDoesNotExist{}
      end
    end

    assert_raise CompileError, ~r"struct is not defined for TestTypespec", fn ->
      test_module do
        @type mytype :: %TestTypespec{}
      end
    end
  end

  test "@type with a struct with undefined field" do
    assert_raise CompileError, ~r"undefined field no_field on struct TestTypespec", fn ->
      test_module do
        defstruct [hello: nil, eric: nil]
        @type mytype :: %TestTypespec{no_field: :world}
      end
    end
  end

  test "@type when overriding elixir builtin" do
    assert_raise CompileError, ~r"type struct\(\) is a builtin type; it cannot be redefined", fn ->
      test_module do
        @type struct :: :oops
      end
    end
  end

  test "@type when overriding erlang builtin" do
    assert_raise CompileError, ~r"type list\(\) is a builtin type; it cannot be redefined", fn ->
      test_module do
        @type list :: :oops
      end
    end
  end

  test "@type with public record" do
    module = test_module do
      require Record
      Record.defrecord :timestamp, [date: 1, time: 2]
      @type mytype :: record(:timestamp, time: :foo)
    end

    assert [type: {:mytype,
             {:type, _, :tuple, [
               {:atom, 0, :timestamp}, {:type, 0, :term, []}, {:atom, 0, :foo}
             ]},
            []}] = types(module)
  end

  test "@type with private record" do
    module = test_module do
      require Record
      Record.defrecordp :timestamp, [date: 1, time: 2]
      @type mytype :: record(:timestamp, time: :foo)
    end

    assert [type: {:mytype,
             {:type, _, :tuple, [
               {:atom, 0, :timestamp}, {:type, 0, :term, []}, {:atom, 0, :foo}
             ]},
            []}] = types(module)
  end

  test "@type with undefined record" do
    assert_raise CompileError, ~r"unknown record :this_record_does_not_exist", fn ->
      test_module do
        @type mytype :: record(:this_record_does_not_exist, [])
      end
    end
  end

  test "@type with a record with undefined field" do
    assert_raise CompileError, ~r"undefined field no_field on record :timestamp", fn ->
      test_module do
        require Record
        Record.defrecord :timestamp, [date: 1, time: 2]
        @type mytype :: record(:timestamp, no_field: :foo)
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
    module = test_module do
      @type mytype :: []
      @type mytype1 :: [integer]
      @type mytype2 :: [integer, ...]
    end

    assert [type: {:mytype, {:type, _, :nil, []}, []},
            type: {:mytype1, {:type, _, :list, [{:type, _, :integer, []}]}, []},
            type: {:mytype2, {:type, _, :nonempty_list, [{:type, _, :integer, []}]}, []}] =
           types(module)
  end

  test "@type with a fun" do
    module = test_module do
      @type mytype :: (... -> any)
    end

    assert [type: {:mytype, {:type, _, :fun, []}, []}] =
           types(module)
  end

  test "@type with a fun with multiple arguments and return type" do
    module = test_module do
      @type mytype :: (integer, integer -> integer)
    end

    assert [type: {:mytype, {:type, _, :fun, [{:type, _, :product,
             [{:type, _, :integer, []}, {:type, _, :integer, []}]},
             {:type, _, :integer, []}]}, []}] =
           types(module)
  end

  test "@type with a fun with no arguments and return type" do
    module = test_module do
      @type mytype :: (() -> integer)
    end

    assert [type: {:mytype, {:type, _, :fun, [{:type, _, :product, []},
             {:type, _, :integer, []}]}, []}] =
           types(module)
  end

  test "@type with a fun with any arity and return type" do
    module = test_module do
      @type mytype :: (... -> integer)
    end

    assert [type: {:mytype, {:type, _, :fun, [{:type, _, :any},
             {:type, _, :integer, []}]}, []}] =
           types(module)
  end

  test "@type with a union" do
    module = test_module do
      @type mytype :: integer | char_list | atom
    end

    assert [type: {:mytype, {:type, _, :union, [{:type, _, :integer, []},
             {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :char_list}, []]},
             {:type, _, :atom, []}]}, []}] =
           types(module)
  end

  test "@type with keywords" do
    module = test_module do
      @type mytype :: [first: integer, step: integer, last: integer]
    end

    assert [type: {:mytype, {:type, _, :list, [
      {:type, _, :union, [
        {:type, _, :tuple, [{:atom, _, :first}, {:type, _, :integer, []}]},
        {:type, _, :tuple, [{:atom, _, :step}, {:type, _, :integer, []}]},
        {:type, _, :tuple, [{:atom, _, :last}, {:type, _, :integer, []}]}
      ]}
    ]}, []}] = types(module)
  end

  test "@type with parameters" do
    module = test_module do
      @type mytype(x) :: x
      @type mytype1(x) :: list(x)
      @type mytype2(x, y) :: {x, y}
    end

    assert [type: {:mytype, {:var, _, :x}, [{:var, _, :x}]},
            type: {:mytype1, {:type, _, :list, [{:var, _, :x}]}, [{:var, _, :x}]},
            type: {:mytype2, {:type, _, :tuple, [{:var, _, :x}, {:var, _, :y}]}, [{:var, _, :x}, {:var, _, :y}]}] =
           types(module)
  end

  test "@type with annotations" do
    module = test_module do
      @type mytype :: (named :: integer)
      @type mytype1 :: (a :: integer -> integer)
    end

    assert [type: {:mytype, {:ann_type, _, [{:var, _, :named}, {:type, _, :integer, []}]}, []},
            type: {:mytype1, {:type, _, :fun, [{:type, _, :product, [{:ann_type, _, [{:var, _, :a}, {:type, _, :integer, []}]}]}, {:type, _, :integer, []}]}, []}] =
           types(module)
  end

  test "@opaque(type)" do
    module = test_module do
      @opaque mytype(x) :: x
    end

    assert [opaque: {:mytype, {:var, _, :x}, [{:var, _, :x}]}] =
           types(module)
  end

  test "@type + opaque" do
    module = test_module do
      @type mytype :: tuple
      @opaque mytype1 :: {}
    end

    assert [opaque: {:mytype1, _, []},
            type: {:mytype, _, []}, ] =
           types(module)
  end

  test "@type unquote fragment" do
    module = test_module do
      quoted = quote unquote: false do
        name = :mytype
        type = :atom
        @type unquote(name)() :: unquote(type)
      end
      Module.eval_quoted(__MODULE__, quoted) |> elem(0)
    end

    assert [type: {:mytype, {:atom, _, :atom}, []}] =
           types(module)
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
    module = test_module do
      def myfun1(x), do: x
      def myfun2(), do: :ok
      def myfun3(x, y), do: {x, y}
      def myfun4(x), do: x
      @spec myfun1(integer) :: integer
      @spec myfun2() :: integer
      @spec myfun3(integer, integer) :: {integer, integer}
      @spec myfun4(x :: integer) :: integer
    end

    assert [{{:myfun1, 1}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]},
            {{:myfun2, 0}, [{:type, _, :fun, [{:type, _, :product, []}, {:type, _, :integer, []}]}]},
            {{:myfun3, 2}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]}, {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]}]}]},
            {{:myfun4, 1}, [{:type, _, :fun, [{:type, _, :product, [{:ann_type, _, [{:var, _, :x}, {:type, _, :integer, []}]}]}, {:type, _, :integer, []}]}]}] =
           specs(module)
  end

  test "@spec(spec) for unreachable private function" do
    # Run it inside capture_io/2 so that the "myfun/1 is unused"
    # warning doesn't get printed among the ExUnit test results.
    output = ExUnit.CaptureIO.capture_io :stderr, fn ->
      module = test_module do
        defp myfun(x), do: x
        @spec myfun(integer) :: integer
      end

      assert [] == specs(module)
    end

    assert output =~ "warning: function myfun/1 is unused"
  end

  test "@spec(spec) with guards" do
    module = test_module do
      def myfun1(x), do: x
      @spec myfun1(x) :: boolean when [x: integer]

      def myfun2(x), do: x
      @spec myfun2(x) :: x when [x: var]

      def myfun3(_x, y), do: y
      @spec myfun3(x, y) :: y when [y: x, x: var]
    end

    assert [{{:myfun1, 1}, [{:type, _, :bounded_fun, [{:type, _, :fun, [{:type, _, :product, [{:var, _, :x}]}, {:type, _, :boolean, []}]}, [{:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, :x}, {:type, _, :integer, []}]]}]]}]},
            {{:myfun2, 1}, [{:type, _, :fun, [{:type, _, :product, [{:var, _, :x}]}, {:var, _, :x}]}]},
            {{:myfun3, 2}, [{:type, _, :bounded_fun, [{:type, _, :fun, [{:type, _, :product, [{:var, _, :x}, {:var, _, :y}]}, {:var, _, :y}]}, [{:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, :y}, {:var, _, :x}]]}]]}]}] =
           specs(module)
  end

  test "@callback(callback)" do
    module = test_module do
      @callback myfun(integer) :: integer
      @callback myfun() :: integer
      @callback myfun(integer, integer) :: {integer, integer}
    end

    assert [{{:myfun, 0}, [{:type, _, :fun, [{:type, _, :product, []}, {:type, _, :integer, []}]}]},
            {{:myfun, 1}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]},
            {{:myfun, 2}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}, {:type, _, :integer, []}]}, {:type, _, :tuple, [{:type, _, :integer, []}, {:type, _, :integer, []}]}]}]}] =
           callbacks(module)
  end

  test "@spec + @callback" do
    module = test_module do
      def myfun(x), do: x
      @spec myfun(integer)   :: integer
      @spec myfun(char_list) :: char_list
      @callback cb(integer)  :: integer
    end

    assert [{{:cb, 1}, [{:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]}] =
           callbacks(module)

    assert [{{:myfun, 1}, [
             {:type, _, :fun, [{:type, _, :product, [
               {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :char_list}, []]}]},
               {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :char_list}, []]}]},
             {:type, _, :fun, [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}]}] =
           specs(module)
  end

  test "block handling" do
    module = test_module do
      @spec foo((() -> [ integer ])) :: integer
      def foo(_), do: 1
    end
    assert [{{:foo, 1},
            [{:type, _, :fun, [{:type, _, :product, [
                     {:type, _, :fun, [{:type, _, :product, []}, {:type, _, :list, [{:type, _, :integer, []}]}]}]},
                     {:type, _, :integer, []}]}]}] =
           specs(module)
  end

  # Conversion to AST

  test "type_to_ast" do
    quoted = [
      (quote do: @type with_ann() :: (t :: atom())),
      (quote do: @type a_tuple() :: tuple()),
      (quote do: @type empty_tuple() :: {}),
      (quote do: @type one_tuple() :: {:foo}),
      (quote do: @type two_tuple() :: {:foo, :bar}),
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
      (quote do: @type st() :: struct()),
      (quote do: @type ab() :: as_boolean(term())),
      (quote do: @type kw() :: keyword()),
      (quote do: @type kwt() :: keyword(term())),
      (quote do: @type vaf() :: (... -> any())),
      (quote do: @type rng() :: 1..10),
      (quote do: @type opts() :: [first: integer(), step: integer(), last: integer()]),
      (quote do: @type ops() :: {+1, -1}),
      (quote do: @type a_map() :: map()),
      (quote do: @type empty_map() :: %{}),
      (quote do: @type my_map() :: %{hello: :world}),
      (quote do: @type my_struct() :: %Kernel.TypespecTest{hello: :world}),
      (quote do: @type list1() :: list()),
      (quote do: @type list2() :: [0]),
      (quote do: @type list3() :: [...]),
      (quote do: @type list4() :: [0, ...]),
      (quote do: @type nil_list() :: []),
    ] |> Enum.sort

    module = test_module do
      Module.eval_quoted __MODULE__, quoted
    end

    types = types(module)

    Enum.each(Enum.zip(types, quoted), fn {{:type, type}, definition} ->
      ast = Kernel.Typespec.type_to_ast(type)
      assert Macro.to_string(quote do: @type unquote(ast)) == Macro.to_string(definition)
    end)
  end

  test "type_to_ast for paren_type" do
    type = {:my_type, {:paren_type, 0, [{:type, 0, :integer, []}]}, []}
    assert Kernel.Typespec.type_to_ast(type) ==
      {:::, [], [{:my_type, [], []}, {:integer, [line: 0], []}]}
  end

  test "spec_to_ast" do
    quoted = [
      (quote do: @spec a() :: integer()),
      (quote do: @spec a(atom()) :: integer() | [{}]),
      (quote do: @spec a(b) :: integer() when [b: integer()]),
      (quote do: @spec a(b) :: b when [b: var]),
      (quote do: @spec a(c :: atom()) :: atom()),
    ] |> Enum.sort

    module = test_module do
      def a, do: 1
      def a(a), do: a
      Module.eval_quoted __MODULE__, quote do: (unquote_splicing(quoted))
    end

    specs = Enum.flat_map(specs(module), fn {{_, _}, specs} ->
       Enum.map(specs, fn spec ->
         quote do: @spec unquote(Kernel.Typespec.spec_to_ast(:a, spec))
       end)
     end) |> Enum.sort

    Enum.each(Enum.zip(specs, quoted), fn {spec, definition} ->
      assert Macro.to_string(spec) == Macro.to_string(definition)
    end)
  end

  test "retrieval invalid data" do
    assert Kernel.Typespec.beam_types(Unknown) == nil
    assert Kernel.Typespec.beam_specs(Unknown) == nil
  end

  defmodule Sample do
    @callback first(integer) :: integer
    @callback foo(atom(), binary) :: binary
    @callback bar(External.hello, my_var :: binary) :: binary
    @callback guarded(my_var) :: my_var when my_var: binary
    @callback orr(atom | integer) :: atom
    @callback literal(123, {atom}, :atom, [integer], true) :: atom
    @macrocallback last(integer) :: Macro.t
  end

  test "callbacks" do
    assert Sample.behaviour_info(:callbacks) ==
           [first: 1, guarded: 1, "MACRO-last": 2, literal: 5, orr: 1, foo: 2, bar: 2]
  end

  test "default is not supported" do
    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        @callback hello(num \\ 0 :: integer) :: integer
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        @callback hello(num :: integer \\ 0) :: integer
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        @macrocallback hello(num \\ 0 :: integer) :: Macro.t
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        @macrocallback hello(num :: integer \\ 0) :: Macro.t
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        @spec hello(num \\ 0 :: integer) :: integer
      end
    end

    assert_raise ArgumentError, fn ->
      defmodule WithDefault do
        @spec hello(num :: integer \\ 0) :: integer
      end
    end
  end

  test "@spec shows readable error message when return type is missing" do
    assert_raise CompileError, ~r"type specification missing return type: myfun\(integer\)", fn ->
      test_module do
        @spec myfun(integer)
      end
    end
  end
end
