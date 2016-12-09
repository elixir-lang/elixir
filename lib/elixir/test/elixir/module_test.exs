Code.require_file "test_helper.exs", __DIR__

defmodule ModuleTest.ToBeUsed do
  def value, do: 1

  defmacro __using__(_) do
    target = __CALLER__.module
    Module.put_attribute(target, :has_callback, true)
    Module.put_attribute(target, :before_compile, __MODULE__)
    Module.put_attribute(target, :after_compile, __MODULE__)
    Module.put_attribute(target, :before_compile, {__MODULE__, :callback})
    quote do: (def line, do: __ENV__.line)
  end

  defmacro __before_compile__(env) do
    quote do: (def before_compile, do: unquote(env.vars))
  end

  defmacro __after_compile__(%Macro.Env{module: ModuleTest.ToUse, vars: []}, bin) when is_binary(bin) do
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
  32 = __ENV__.line # Moving the next line around can make tests fail
  var = 1
  _ = var # Not available in callbacks
  def callback_value(false), do: false
  use ModuleTest.ToBeUsed
end

defmodule ModuleTest do
  use ExUnit.Case, async: true

  doctest Module

  Module.register_attribute __MODULE__, :register_example, accumulate: true, persist: true
  @register_example :it_works
  @register_example :still_works

  contents = quote do
    def eval_quoted_info, do: {__MODULE__, __ENV__.file, __ENV__.line}
  end
  Module.eval_quoted __MODULE__, contents, [], file: "sample.ex", line: 13

  defmacrop in_module(block) do
    quote do
      defmodule Temp, unquote(block)
      :code.purge(Temp)
      :code.delete(Temp)
    end
  end

  test "module attributes returns value" do
    in_module do
      assert (@return [:foo, :bar]) == :ok
      _ = @return
    end
  end

  test "in memory modules are tagged as so" do
    assert :code.which(__MODULE__) == :in_memory
  end

  ## Eval

  test "executes eval_quoted definitions" do
    assert eval_quoted_info() == {ModuleTest, "sample.ex", 13}
  end

  test "retrieves line from macros" do
    assert ModuleTest.ToUse.line == 36
  end

  ## Callbacks

  test "executes custom before_compile callback" do
    assert ModuleTest.ToUse.callback_value(true) == true
    assert ModuleTest.ToUse.callback_value(false) == false
  end

  test "executes default before_compile callback" do
    assert ModuleTest.ToUse.before_compile == []
  end

  def __on_definition__(env, kind, name, args, guards, expr) do
    Process.put(env.module, :called)
    assert env.module == ModuleTest.OnDefinition
    assert kind == :def
    assert name == :hello
    assert [{:foo, _, _}, {:bar, _, _}] = args
    assert [] = guards
    assert {:+, _, [{:foo, _, nil}, {:bar, _, nil}]} = expr
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
    assert OverridableWithBeforeCompile.constant == 1
  end

  ## Attributes

  test "reserved attributes" do
    assert List.keyfind(ExUnit.Server.__info__(:attributes), :behaviour, 0) == {:behaviour, [GenServer]}
  end

  test "registered attributes" do
    assert [{:register_example, [:it_works]}, {:register_example, [:still_works]}] ==
      Enum.filter __MODULE__.__info__(:attributes), &match?({:register_example, _}, &1)
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
    assert Module.concat(Foo, Bar)  == Foo.Bar
    assert Module.concat(Foo, :Bar) == Foo.Bar
    assert Module.concat(Foo, "Bar") == Foo.Bar
    assert Module.concat(Foo, Bar.Baz) == Foo.Bar.Baz
    assert Module.concat(Foo, "Bar.Baz") == Foo.Bar.Baz
    assert Module.concat(Bar, nil) == Elixir.Bar
  end

  test "safe concat" do
    assert Module.safe_concat(Foo, :Bar) == Foo.Bar
    assert_raise ArgumentError, fn ->
      Module.safe_concat SafeConcat, Doesnt.Exist
    end
  end

  test "split" do
    module = Very.Long.Module.Name.And.Even.Longer
    assert Module.split(module) == ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]
    assert Module.split("Elixir.Very.Long") == ["Very", "Long"]
    assert_raise FunctionClauseError, fn ->
      Module.split(:just_an_atom)
    end
    assert_raise FunctionClauseError, fn ->
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
  test "__ENV__.file with module attribute" do
    assert __ENV__.file == "sample.ex"
  end

  ## Creation

  test "defmodule" do
    assert match?({:module, Defmodule, binary, 3} when is_binary(binary), defmodule Defmodule do
      1 + 2
    end)
  end

  test "defmodule with atom" do
    assert match?({:module, :root_defmodule, _, _}, defmodule :root_defmodule do
      :ok
    end)
  end

  test "defmodule with alias as atom" do
    defmodule :"Elixir.ModuleTest.RawModule" do
      def hello, do: :world
    end

    assert RawModule.hello == :world
  end

  test "create" do
    contents =
      quote do
        def world, do: true
      end
    {:module, ModuleCreateSample, _, _} =
      Module.create(ModuleCreateSample, contents, __ENV__)
    assert ModuleCreateSample.world
  end

  test "create with elixir as a name" do
    contents =
      quote do
        def world, do: true
      end
    assert_raise CompileError, fn ->
      {:module, Elixir, _, _} =
        Module.create(Elixir, contents, __ENV__)
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

    Module.create ModuleHygiene, contents, __ENV__
    assert ModuleHygiene.test == [1, 2, 3]
  end

  test "ensure function clauses are ordered" do
    {_, _, binary, _} =
      defmodule Ordered do
        def foo(:foo), do: :bar
        def baz(:baz), do: :bat
      end
    atoms = :beam_lib.chunks(binary, [:atoms])
    assert :erlang.phash2(atoms) == 53987778
  end

  # TODO: Remove this check once we depend only on 19
  if :erlang.system_info(:otp_release) >= '19' do
    test "create with generated true does not emit warnings" do
      contents =
        quote generated: true do
          def world, do: true
          def world, do: false
        end
      {:module, ModuleCreateGenerated, _, _} =
        Module.create(ModuleCreateGenerated, contents, __ENV__)
      assert ModuleCreateGenerated.world
    end
  end

  test "no function in module body" do
    in_module do
      assert __ENV__.function == nil
    end
  end

  ## Definitions

  test "defines?" do
    in_module do
      refute Module.defines? __MODULE__, {:foo, 0}
      def foo(), do: bar()
      assert Module.defines? __MODULE__, {:foo, 0}
      assert Module.defines? __MODULE__, {:foo, 0}, :def

      refute Module.defines? __MODULE__, {:bar, 0}, :defp
      defp bar(), do: :ok
      assert Module.defines? __MODULE__, {:bar, 0}, :defp

      refute Module.defines? __MODULE__, {:baz, 0}, :defmacro
      defmacro baz(), do: :ok
      assert Module.defines? __MODULE__, {:baz, 0}, :defmacro
    end
  end

  test "definitions in" do
    in_module do
      def foo(1, 2, 3), do: 4

      assert Module.definitions_in(__MODULE__)        == [foo: 3]
      assert Module.definitions_in(__MODULE__, :def)  == [foo: 3]
      assert Module.definitions_in(__MODULE__, :defp) == []
    end
  end
end
