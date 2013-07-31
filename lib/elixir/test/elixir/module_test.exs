Code.require_file "test_helper.exs", __DIR__

defmodule ModuleTest.ToBeUsed do
  def value, do: 1

  defmacro __using__(_) do
    target = __CALLER__.module
    Module.put_attribute(target, :has_callback, false)
    Module.put_attribute(target, :before_compile, __MODULE__)
    Module.put_attribute(target, :after_compile, __MODULE__)
    Module.put_attribute(target, :before_compile, { __MODULE__, :callback })
    quote do: (def line, do: __ENV__.line)
  end

  defmacro __before_compile__(_) do
    quote do: (def before_compile, do: true)
  end

  defmacro __after_compile__(Macro.Env[module: ModuleTest.ToUse], bin) when is_binary(bin) do
    # IO.puts "HELLO"
  end

  defmacro callback(env) do
    value = Module.get_attribute(env.module, :has_callback)
    quote do
      name  = :original_value
      args  = [1]
      guard = []
      def name, args, guard, do: unquote(value)
    end
  end
end

defmodule ModuleTest.ToUse do
  35 = __ENV__.line # Moving the next line around can make tests fail
  def original_value(2), do: true
  use ModuleTest.ToBeUsed
end

defmodule ModuleTest do
  use ExUnit.Case, async: true

  Module.register_attribute __MODULE__, :register_example, accumulate: true, persist: true
  @register_example :it_works
  @register_example :still_works

  contents = quote do: (def eval_quoted_info, do: { __MODULE__, __FILE__, __ENV__.line })
  Module.eval_quoted __MODULE__, contents, [], file: "sample.ex", line: 13

  defmacrop in_module(block) do
    quote do
      defmodule Temp, unquote(block)
      :code.purge(Temp)
      :code.delete(Temp)
    end
  end

  ## Eval

  test :eval_quoted do
    assert eval_quoted_info() == { ModuleTest, "sample.ex", 13 }
  end

  test :line_from_macro do
    assert ModuleTest.ToUse.line == 37
  end

  ## Callbacks

  test :compile_callback_hook do
    refute ModuleTest.ToUse.original_value(1)
    assert ModuleTest.ToUse.original_value(2)
  end

  test :before_compile_callback_hook do
    assert ModuleTest.ToUse.before_compile
  end

  test :on_definition do
    defmodule OnDefinition do
      @on_definition ModuleTest

      def hello(foo, bar) do
        foo + bar
      end
    end

    assert Process.get(ModuleTest.OnDefinition) == :called
  end

  def __on_definition__(env, kind, name, args, guards, expr) do
    Process.put(env.module, :called)
    assert env.module == ModuleTest.OnDefinition
    assert kind == :def
    assert name == :hello
    assert [{ :foo, _, _ }, { :bar, _ , _ }] = args
    assert [] = guards
    assert [do: { :+, _, [{ :foo, _, _ }, { :bar, _, _ }] }] = expr
  end

  test :overridable_inside_before_compile do
    defmodule OverridableWithBeforeCompile do
      @before_compile ModuleTest
    end
    assert OverridableWithBeforeCompile.constant == 1
  end

  test :alias_with_raw_atom do
    defmodule :"Elixir.ModuleTest.RawModule" do
      def hello, do: :world
    end

    assert RawModule.hello == :world
  end

  defmacro __before_compile__(_) do
    quote do
      def constant, do: 1
      defoverridable constant: 0
    end
  end

  ## Attributes

  test :reserved_attributes do
    assert List.keyfind(ExUnit.Server.__info__(:attributes), :behavior, 0) == {:behavior, [:gen_server]}
  end

  test :registered_attributes do
    assert [{:register_example, [:it_works]}, {:register_example, [:still_works]}] ==
      Enum.filter __MODULE__.__info__(:attributes), match?({ :register_example, _ }, &1)
  end

  @some_attribute  [1]
  @other_attribute [3, 2, 1]

  test :inside_function_attributes do
    assert [1] = @some_attribute
    assert [3, 2, 1] = @other_attribute
  end

  ## Naming

  test :concat do
    assert Module.concat(Foo, Bar)  == Foo.Bar
    assert Module.concat(Foo, :Bar) == Foo.Bar
    assert Module.concat(Foo, "Bar") == Foo.Bar
    assert Module.concat(Foo, 'Bar') == Foo.Bar
    assert Module.concat(Foo, Bar.Baz) == Foo.Bar.Baz
    assert Module.concat(Foo, "Bar.Baz") == Foo.Bar.Baz
    assert Module.concat(Bar, :nil) == :"Elixir.Bar.nil"
  end

  test :safe_concat do
    assert Module.safe_concat(Foo, :Bar) == Foo.Bar
    assert_raise ArgumentError, fn ->
      Module.safe_concat SafeConcat, Doesnt.Exist
    end
  end

  test :split do
    module = Very.Long.Module.Name.And.Even.Longer
    assert Module.split(module) == ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]
    assert Module.split("Elixir.Very.Long") == ["Very", "Long"]
    assert Module.concat(Module.split(module)) == module
  end

  test :__MODULE__ do
    assert Code.eval_string("__MODULE__.Foo") |> elem(0) == Foo
  end

  ## Creation

  test :defmodule do
    assert match?({ :module, Defmodule, binary, 3 } when is_binary(binary), defmodule Defmodule do
      1 + 2
    end)
  end

  test :defmodule_with_atom do
    assert match?({ :module, :root_defmodule, _, _ }, defmodule :root_defmodule do
      :ok
    end)
  end

  test :create do
    contents =
      quote do
        def world, do: true
      end
    { :module, ModuleCreateSample, _, _ } =
      Module.create(ModuleCreateSample, contents, __ENV__)
    assert ModuleCreateSample.world
  end

  test :no_function_in_module_body do
    in_module do
      assert __ENV__.function == nil
    end
  end

  ## Definitions

  test :defines? do
    in_module do
      refute Module.defines? __MODULE__, { :foo, 0 }
      def foo(), do: bar()
      assert Module.defines? __MODULE__, { :foo, 0 }
      assert Module.defines? __MODULE__, { :foo, 0 }, :def

      refute Module.defines? __MODULE__, { :bar, 0 }, :defp
      defp bar(), do: :ok
      assert Module.defines? __MODULE__, { :bar, 0 }, :defp

      refute Module.defines? __MODULE__, { :baz, 0 }, :defmacro
      defmacro baz(), do: :ok
      assert Module.defines? __MODULE__, { :baz, 0 }, :defmacro
    end
  end

  test :definitions_in do
    in_module do
      def foo(1, 2, 3), do: 4

      assert Module.definitions_in(__MODULE__)        == [foo: 3]
      assert Module.definitions_in(__MODULE__, :def)  == [foo: 3]
      assert Module.definitions_in(__MODULE__, :defp) == []
    end
  end

  test :function do
    assert Module.function(:erlang, :atom_to_list, 1).(:hello) == 'hello'
    assert is_function Module.function(This, :also_works, 0)
  end
end
