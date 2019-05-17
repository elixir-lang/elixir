Code.require_file("test_helper.exs", __DIR__)

defmodule ModuleTest.ToBeUsed do
  def value, do: 1

  defmacro __using__(_) do
    target = __CALLER__.module
    Module.put_attribute(target, :has_callback, true)
    Module.put_attribute(target, :before_compile, __MODULE__)
    Module.put_attribute(target, :after_compile, __MODULE__)
    Module.put_attribute(target, :before_compile, {__MODULE__, :callback})
    quote(do: def(line, do: __ENV__.line))
  end

  defmacro __before_compile__(env) do
    quote(do: def(before_compile, do: unquote(Macro.Env.vars(env))))
  end

  defmacro __after_compile__(%Macro.Env{module: ModuleTest.ToUse, vars: []}, bin)
           when is_binary(bin) do
    # IO.puts "HELLO"
  end

  defmacro callback(env) do
    value = Module.get_attribute(env.module, :has_callback)

    quote do
      def callback_value(true), do: unquote(value)
    end
  end
end

defmodule ModuleTest.ToUse do
  # Moving the next line around can make tests fail
  35 = __ENV__.line
  var = 1
  # Not available in callbacks
  _ = var
  def callback_value(false), do: false
  use ModuleTest.ToBeUsed
end

defmodule ModuleTest do
  use ExUnit.Case, async: true

  doctest Module

  Module.register_attribute(__MODULE__, :register_example, accumulate: true, persist: true)
  @register_example :it_works
  @register_example :still_works

  contents =
    quote do
      def eval_quoted_info, do: {__MODULE__, __ENV__.file, __ENV__.line}
    end

  Module.eval_quoted(__MODULE__, contents, [], file: "sample.ex", line: 13)

  defp purge(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defmacrop in_module(block) do
    quote do
      defmodule(Temp, unquote(block))
      purge(Temp)
    end
  end

  test "module attributes returns value" do
    in_module do
      assert @return([:foo, :bar]) == :ok
      _ = @return
    end
  end

  test "in memory modules are tagged as so" do
    assert :code.which(__MODULE__) == ''
  end

  ## Eval

  test "executes eval_quoted definitions" do
    assert eval_quoted_info() == {ModuleTest, "sample.ex", 13}
  end

  test "resets last definition information on eval" do
    # This should not emit any warning
    defmodule LastDefinition do
      def foo(0), do: 0

      Module.eval_quoted(
        __ENV__,
        quote do
          def bar, do: :ok
        end
      )

      def foo(1), do: 1
    end
  end

  test "retrieves line from use callsite" do
    assert ModuleTest.ToUse.line() == 40
  end

  ## Callbacks

  test "executes custom before_compile callback" do
    assert ModuleTest.ToUse.callback_value(true) == true
    assert ModuleTest.ToUse.callback_value(false) == false
  end

  test "executes default before_compile callback" do
    assert ModuleTest.ToUse.before_compile() == []
  end

  def __on_definition__(env, kind, name, args, guards, expr) do
    Process.put(env.module, :called)
    assert env.module == ModuleTest.OnDefinition
    assert kind == :def
    assert name == :hello
    assert [{:foo, _, _}, {:bar, _, _}] = args
    assert [] = guards
    assert [do: {:+, _, [{:foo, _, nil}, {:bar, _, nil}]}] = expr
  end

  test "executes on definition callback" do
    defmodule OnDefinition do
      @on_definition ModuleTest

      def hello(foo, bar) do
        foo + bar
      end
    end

    assert Process.get(ModuleTest.OnDefinition) == :called
  end

  defmacro __before_compile__(_) do
    quote do
      def constant, do: 1
      defoverridable constant: 0
    end
  end

  test "may set overridable inside before_compile callback" do
    defmodule OverridableWithBeforeCompile do
      @before_compile ModuleTest
    end

    assert OverridableWithBeforeCompile.constant() == 1
  end

  ## Attributes

  test "reserved attributes" do
    assert List.keyfind(ExUnit.Server.__info__(:attributes), :behaviour, 0) ==
             {:behaviour, [GenServer]}
  end

  test "registered attributes" do
    assert Enum.filter(__MODULE__.__info__(:attributes), &match?({:register_example, _}, &1)) ==
             [{:register_example, [:it_works]}, {:register_example, [:still_works]}]
  end

  @some_attribute [1]
  @other_attribute [3, 2, 1]

  test "inside function attributes" do
    assert @some_attribute == [1]
    assert @other_attribute == [3, 2, 1]
  end

  test "@compile autoload attribute" do
    defmodule NoAutoload do
      @compile {:autoload, false}
    end

    refute :code.is_loaded(NoAutoload)
  end

  ## Naming

  test "concat" do
    assert Module.concat(Foo, Bar) == Foo.Bar
    assert Module.concat(Foo, :Bar) == Foo.Bar
    assert Module.concat(Foo, "Bar") == Foo.Bar
    assert Module.concat(Foo, Bar.Baz) == Foo.Bar.Baz
    assert Module.concat(Foo, "Bar.Baz") == Foo.Bar.Baz
    assert Module.concat(Bar, nil) == Elixir.Bar
  end

  test "safe concat" do
    assert Module.safe_concat(Foo, :Bar) == Foo.Bar

    assert_raise ArgumentError, fn ->
      Module.safe_concat(SafeConcat, Doesnt.Exist)
    end
  end

  test "split" do
    module = Very.Long.Module.Name.And.Even.Longer
    assert Module.split(module) == ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]
    assert Module.split("Elixir.Very.Long") == ["Very", "Long"]

    assert_raise ArgumentError, "expected an Elixir module, got: :just_an_atom", fn ->
      Module.split(:just_an_atom)
    end

    assert_raise ArgumentError, "expected an Elixir module, got: \"Foo\"", fn ->
      Module.split("Foo")
    end

    assert Module.concat(Module.split(module)) == module
  end

  test "__MODULE__" do
    assert Code.eval_string("__MODULE__.Foo") |> elem(0) == Foo
  end

  test "__ENV__.file" do
    assert Path.basename(__ENV__.file) == "module_test.exs"
  end

  @file "sample.ex"
  test "@file sets __ENV__.file" do
    assert __ENV__.file == "sample.ex"
  end

  test "@file raises when invalid" do
    assert_raise ArgumentError, ~r"@file is a built-in module attribute", fn ->
      defmodule BadFile do
        @file :oops
        def my_fun, do: :ok
      end
    end
  end

  ## Creation

  test "defmodule" do
    result =
      defmodule Defmodule do
        1 + 2
      end

    assert {:module, Defmodule, binary, 3} = result
    assert is_binary(binary)
  end

  test "defmodule with atom" do
    result =
      defmodule :root_defmodule do
        :ok
      end

    assert {:module, :root_defmodule, _, _} = result
  end

  test "does not leak alias from atom" do
    defmodule :"Elixir.ModuleTest.RawModule" do
      def hello, do: :world
    end

    refute __ENV__.aliases[Elixir.ModuleTest]
    refute __ENV__.aliases[Elixir.RawModule]
    assert ModuleTest.RawModule.hello() == :world
  end

  test "does not leak alias from non-atom alias" do
    defmodule __MODULE__.NonAtomAlias do
      def hello, do: :world
    end

    refute __ENV__.aliases[Elixir.ModuleTest]
    refute __ENV__.aliases[Elixir.NonAtomAlias]
    assert Elixir.ModuleTest.NonAtomAlias.hello() == :world
  end

  test "create" do
    contents =
      quote do
        def world, do: true
      end

    {:module, ModuleCreateSample, _, _} = Module.create(ModuleCreateSample, contents, __ENV__)
    assert ModuleCreateSample.world()
  end

  test "create with a reserved module name" do
    contents =
      quote do
        def world, do: true
      end

    assert_raise CompileError, fn ->
      {:module, Elixir, _, _} = Module.create(Elixir, contents, __ENV__)
    end
  end

  test "create with aliases/var hygiene" do
    contents =
      quote do
        alias List, as: L

        def test do
          L.flatten([1, [2], 3])
        end
      end

    Module.create(ModuleHygiene, contents, __ENV__)
    assert ModuleHygiene.test() == [1, 2, 3]
  end

  test "ensure function clauses are sorted (to avoid non-determinism in module vsn)" do
    {_, _, binary, _} =
      defmodule Ordered do
        def foo(:foo), do: :bar
        def baz(:baz), do: :bat
      end

    {:ok, {ModuleTest.Ordered, [abstract_code: {:raw_abstract_v1, abstract_code}]}} =
      :beam_lib.chunks(binary, [:abstract_code])

    # We need to traverse functions instead of using :exports as exports are sorted
    funs = for {:function, _, name, arity, _} <- abstract_code, do: {name, arity}
    assert funs == [__info__: 1, baz: 1, foo: 1]
  end

  test "create with generated true does not emit warnings" do
    contents =
      quote generated: true do
        def world, do: true
        def world, do: false
      end

    {:module, ModuleCreateGenerated, _, _} =
      Module.create(ModuleCreateGenerated, contents, __ENV__)

    assert ModuleCreateGenerated.world()
  end

  test "uses the debug_info chunk" do
    {:module, ModuleCreateDebugInfo, binary, _} =
      Module.create(ModuleCreateDebugInfo, :ok, __ENV__)

    {:ok, {_, [debug_info: {:debug_info_v1, backend, data}]}} =
      :beam_lib.chunks(binary, [:debug_info])

    {:ok, map} = backend.debug_info(:elixir_v1, ModuleCreateDebugInfo, data, [])
    assert map.module == ModuleCreateDebugInfo
  end

  test "uses the debug_info chunk even if debug_info is set to false" do
    {:module, ModuleCreateNoDebugInfo, binary, _} =
      Module.create(ModuleCreateNoDebugInfo, quote(do: @compile({:debug_info, false})), __ENV__)

    {:ok, {_, [debug_info: {:debug_info_v1, backend, data}]}} =
      :beam_lib.chunks(binary, [:debug_info])

    assert backend.debug_info(:elixir_v1, ModuleCreateNoDebugInfo, data, []) == {:error, :missing}
  end

  test "no function in module body" do
    in_module do
      assert __ENV__.function == nil
    end
  end

  test "does not use ETS tables named after the module" do
    in_module do
      assert :ets.info(__MODULE__) == :undefined
    end
  end

  ## Definitions

  test "defines?" do
    in_module do
      refute Module.defines?(__MODULE__, {:foo, 0})
      def foo(), do: bar()
      assert Module.defines?(__MODULE__, {:foo, 0})
      assert Module.defines?(__MODULE__, {:foo, 0}, :def)

      refute Module.defines?(__MODULE__, {:bar, 0}, :defp)
      defp bar(), do: :ok
      assert Module.defines?(__MODULE__, {:bar, 0}, :defp)

      refute Module.defines?(__MODULE__, {:baz, 0}, :defmacro)
      defmacro baz(), do: :ok
      assert Module.defines?(__MODULE__, {:baz, 0}, :defmacro)
    end
  end

  test "definitions in" do
    in_module do
      def foo(1, 2, 3), do: 4

      assert Module.definitions_in(__MODULE__) == [foo: 3]
      assert Module.definitions_in(__MODULE__, :def) == [foo: 3]
      assert Module.definitions_in(__MODULE__, :defp) == []

      defoverridable foo: 3

      assert Module.definitions_in(__MODULE__) == []
      assert Module.definitions_in(__MODULE__, :def) == []
      assert Module.definitions_in(__MODULE__, :defp) == []
    end
  end

  test "make_overridable/2 with invalid arguments" do
    contents =
      quote do
        Module.make_overridable(__MODULE__, [{:foo, 256}])
      end

    message =
      "each element in tuple list has to be a {function_name :: atom, arity :: 0..255} " <>
        "tuple, got: {:foo, 256}"

    assert_raise ArgumentError, message, fn ->
      Module.create(Foo, contents, __ENV__)
    end
  after
    purge(Foo)
  end

  test "raise when called with already compiled module" do
    message =
      "could not call Module.get_attribute/2 because the module Enum is already compiled. " <>
        "Use the Module.__info__/1 callback or Code.fetch_docs/1 instead"

    assert_raise ArgumentError, message, fn ->
      Module.get_attribute(Enum, :moduledoc)
    end
  end

  describe "get_attribute/3" do
    test "returns a list when the attribute is marked as `accummulate: true`" do
      in_module do
        Module.register_attribute(__MODULE__, :value, accumulate: true)
        Module.put_attribute(__MODULE__, :value, 1)
        assert Module.get_attribute(__MODULE__, :value) == [1]
        Module.put_attribute(__MODULE__, :value, 2)
        assert Module.get_attribute(__MODULE__, :value) == [2, 1]
      end
    end

    test "returns the value of the attribute if it exists" do
      in_module do
        Module.put_attribute(__MODULE__, :attribute, 1)
        assert Module.get_attribute(__MODULE__, :attribute) == 1
        assert Module.get_attribute(__MODULE__, :attribute, :default) == 1
      end
    end

    test "returns the passed default if the attribute does not exist" do
      in_module do
        assert Module.get_attribute(__MODULE__, :attribute, :default) == :default
      end
    end
  end
end
