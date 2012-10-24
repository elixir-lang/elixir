Code.require_file "../test_helper.exs", __FILE__

defmodule ModuleTest.ToBeUsed do
  def value, do: 1

  defmacro __using__(_) do
    target = __CALLER__.module
    Module.put_attribute target, :has_callback, false
    Module.put_attribute(target, :before_compile, __MODULE__)
    Module.put_attribute(target, :after_compile, __MODULE__)
    Module.put_attribute(target, :before_compile, { __MODULE__, :callback })
    quote do: (def line, do: __ENV__.line)
  end

  defmacro __before_compile__(_) do
    quote do: (def before_compile, do: true)
  end

  defmacro __after_compile__(ModuleTest.ToUse, bin) when is_binary(bin) do
    # IO.puts "HELLO"
  end

  defmacro callback(target) do
    value = Module.get_attribute(target, :has_callback)
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

defmodule ModuleTest.DuplicateAttribute do
  Module.register_attribute __MODULE__, :foo
  @foo 1
  def first_foo, do: @foo
  @foo 2
  @foo 3
  def third_foo, do: @foo
end

defmodule ModuleTest.DefinedFunctions do
  def foo(1,2,3), do: 4

  Module.register_attribute __MODULE__, :definitions_in
  Module.register_attribute __MODULE__, :defined_def
  Module.register_attribute __MODULE__, :defined_defp

  @definitions_in Module.definitions_in __MODULE__
  @defined_def  Module.definitions_in __MODULE__, :def
  @defined_defp Module.definitions_in __MODULE__, :defp
end

defmodule ModuleTest do
  use ExUnit.Case, async: true

  Module.register_attribute __MODULE__, :register_example
  @register_example :it_works
  @register_example :still_works

  false = Module.defines? __MODULE__, { :eval_quoted_info, 0 }
  false = Module.defines? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.defines? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.defines? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  contents = quote do: (def eval_quoted_info, do: { __MODULE__, __FILE__, __ENV__.line })
  Module.eval_quoted __MODULE__, contents, [], file: "sample.ex", line: 13

  true  = Module.defines? __MODULE__, { :eval_quoted_info, 0 }
  true  = Module.defines? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.defines? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.defines? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  Module.put_attribute __MODULE__, :value, 1
  Module.put_attribute __MODULE__, :other_value, 1
  Module.put_attribute __MODULE__, :other_value, 2

  nil = __ENV__.function

  test :eval_quoted do
    assert eval_quoted_info() == { ModuleTest, "sample.ex", 13 }
  end

  test :line_from_macro do
    assert ModuleTest.ToUse.line == 37
  end

  test :__MODULE__ do
    assert __MODULE__ == :"Elixir-ModuleTest"
  end

  test :compile_callback_hook do
    refute ModuleTest.ToUse.original_value(1)
    assert ModuleTest.ToUse.original_value(2)
  end

  test :before_compile_callback_hook do
    assert ModuleTest.ToUse.before_compile
  end

  test :reserved_attributes do
    assert List.keyfind(ExUnit.Server.__info__(:attributes), :behavior, 0) == {:behavior,[:gen_server]}
  end

  test :persisted_attributes do
    assert [{:register_example,[:it_works]},{:register_example,[:still_works]}] ==
      Enum.filter __MODULE__.__info__(:attributes), match?({ :register_example, _ }, &1)
  end

  test :duplicated_attributes do
    assert [{:vsn,_},{:foo,[1]},{:foo,[2]},{:foo,[3]}] = ModuleTest.DuplicateAttribute.__info__(:attributes)
  end

  test :inside_function_attributes do
    assert [1] = ModuleTest.DuplicateAttribute.first_foo
    assert [3,2,1] = ModuleTest.DuplicateAttribute.third_foo
  end

  test :function_from___ENV__ do
    assert __ENV__.function == { :test_function_from___ENV__, 0 }
  end

  test :apply do
    assert apply(Enum, :reverse, [[1|[2,3]]]) == [3,2,1]
    assert apply(fn x -> x * 2 end, [2]) == 4
  end

  test :concat do
    assert Module.concat(Foo, Bar)  == Foo.Bar
    assert Module.concat(Foo, :Bar) == Foo.Bar
    assert Module.concat(Foo, "Bar") == Foo.Bar
    assert Module.concat(Foo, 'Bar') == Foo.Bar
    assert Module.concat(Foo, Bar.Baz) == Foo.Bar.Baz
    assert Module.concat(Foo, "Bar.Baz") == Foo.Bar.Baz
  end

  test :safe_concat do
    assert Module.safe_concat(Foo, :Bar) == Foo.Bar
    assert_raise ArgumentError, fn ->
      Module.safe_concat SafeConcat, Doesnt.Exist
    end
  end

  test :definitions_in do
    attrs  = ModuleTest.DefinedFunctions.__info__(:attributes)
    finder = List.keyfind(attrs, &1, 0)

    assert finder.(:definitions_in) == {:definitions_in,[{:foo, 3}]}
    assert finder.(:defined_def) == {:defined_def,[{:foo, 3}]}
    assert finder.(:defined_defp) == {:defined_defp,[]}
  end

  test :split do
    module = Very.Long.Module.Name.And.Even.Longer
    assert Module.split(module) == ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]
    assert Module.split("Elixir-Very-Long") == ["Very", "Long"]
    assert Module.concat(Module.split(module)) == module
  end

  test :defmodule do
    assert match?({ :module, LOL, binary, 3 } when is_binary(binary), defmodule LOL do
      1 + 2
    end)
  end

  def __on_definition__(env, kind, name, args, guards, expr) do
    Process.put(env.module, :called)
    assert env.module == ModuleTest.OnDefinition
    assert kind == :def
    assert name == :hello
    assert [{ :foo, _, _ }, { :bar, _ ,_ }] = args
    assert [] = guards
    assert [do: { :+, _, [{ :foo, _, _ }, { :bar, _, _ }] }] = expr
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

  test :create do
    contents =
      quote do
        def world, do: true
      end
    { :module, ModuleCreateSample, _, _ } =
      Module.create(ModuleCreateSample, contents, __ENV__)
    assert ModuleCreateSample.world
  end

  defmacro __before_compile__(_) do
    quote do
      def constant, do: 1
      defoverridable constant: 0
    end
  end

  test :overridable_inside_before_compile do
    defmodule OverridableWithBeforeCompile do
      @before_compile ModuleTest
    end
    assert OverridableWithBeforeCompile.constant == 1
  end
end
