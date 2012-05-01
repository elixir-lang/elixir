Code.require_file "../test_helper", __FILE__

defmodule ModuleTest.ToBeUsed do
  def value, do: 1

  defmacro __using__(target, _) do
    Module.merge_data target, has_callback: false
    Module.add_compile_callback(target, __MODULE__)
    Module.add_compile_callback(target, __MODULE__, :callback)
    quote do: (def line, do: __LINE__)
  end

  def __compiling__(target) do
    Module.merge_data target, compiling: true
  end

  defmacro callback(target) do
    value = Module.read_data(target, :has_callback)
    quote do
      @has_callback true
      name  = :original_value
      args  = [1]
      guard = []
      def name, args, guard, do: unquote(value)
    end
  end
end

defmodule ModuleTest.ToUse do
  30 = __LINE__ # Moving the next line around can make tests fail
  def original_value(2), do: true
  use ModuleTest.ToBeUsed
end

defmodule ModuleTest.DuplicateAttribute do
  Module.add_attribute __MODULE__, :foo, 1
  Module.add_attribute __MODULE__, :foo, 2
  Module.add_attribute __MODULE__, :foo, 3
end

defmodule ModuleTest.DefinedFunctions do
  def foo(1,2,3), do: 4
  @defined_functions Module.defined_functions __MODULE__
  @defined_def  Module.defined_functions __MODULE__, :def
  @defined_defp Module.defined_functions __MODULE__, :defp
end

defmodule ModuleTest do
  use ExUnit.Case

  Module.register_attribute __MODULE__, :register_example
  @register_example :it_works
  @register_example :still_works

  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  contents = quote do: (def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ })
  Module.eval_quoted __MODULE__, contents, [], file: "sample.ex", line: 13

  true  = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }
  true  = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  Module.merge_data __MODULE__, value: 1
  Module.merge_data __MODULE__, other_value: 1
  Module.merge_data __MODULE__, other_value: 2

  nil = __FUNCTION__

  test :eval_quoted do
    assert eval_quoted_info() == { ModuleTest, "sample.ex", 13 }
  end

  test :line_from_macro do
    assert ModuleTest.ToUse.line == 32
  end

  test :__MODULE__ do
    assert __MODULE__ == :"__MAIN__.ModuleTest"
  end

  test :merge_data do
    assert __MODULE__.__info__(:data) == [other_value: 2, value: 1]
  end

  test :compile_callback_hook do
    refute ModuleTest.ToUse.original_value(1)
    assert ModuleTest.ToUse.original_value(2)
    assert Keyword.get ModuleTest.ToUse.__info__(:data), :has_callback, false
  end

  test :default_compile_callback_hook do
    assert Keyword.get ModuleTest.ToUse.__info__(:data), :compiling, false
  end

  test :reserved_attributes do
    assert List.keyfind(ExUnit.Server.__info__(:attributes), :behavior, 1) == {:behavior,[:gen_server]}
  end

  test :registered_attributes do
    assert [{:register_example,[:it_works]},{:register_example,[:still_works]}] ==
      Enum.filter __MODULE__.__info__(:attributes), match?({ :register_example, _ }, &1)
  end

  test :duplicated_attributes do
    assert_match [{:vsn,_},{:foo,[1]},{:foo,[2]},{:foo,[3]}], ModuleTest.DuplicateAttribute.__info__(:attributes)
  end

  test :__FUNCTION__ do
    assert __FUNCTION__ == { :test___FUNCTION__, 0 }
  end

  test :apply do
    assert apply(List, :reverse, [[1|[2,3]]]) == [3,2,1]
    assert apply(fn(x) -> x * 2 end, [2]) == 4
  end

  test :concat do
    assert Module.concat(Foo, Bar)  == Foo.Bar
    assert Module.concat(Foo, :Bar) == Foo.Bar
    assert Module.concat(Foo, "Bar") == Foo.Bar
    assert Module.concat(Foo, 'Bar') == Foo.Bar
  end

  test :safe_concat do
    assert Module.safe_concat(Foo, :Bar) == Foo.Bar
    assert_raise ArgumentError, fn ->
      Module.safe_concat SafeConcat, Doesnt.Exist
    end
  end

  test :defined_functions do
    assert Keyword.get(ModuleTest.DefinedFunctions.__info__(:data), :defined_functions) == [{:foo, 3}]
    assert Keyword.get(ModuleTest.DefinedFunctions.__info__(:data), :defined_def) == [{:foo, 3}]
    assert Keyword.get(ModuleTest.DefinedFunctions.__info__(:data), :defined_defp) == []
  end
end
