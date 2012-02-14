Code.require_file "../test_helper", __FILE__

defmodule ModuleTest::ToBeUsed do
  def value, do: 1

  defmacro __using__(target) do
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
      args  = []
      guard = true
      def name, args, guard, do: unquote(value)
    end
  end
end

defmodule ModuleTest::ToUse do
  30 = __LINE__ # Moving the next line around can make tests fail
  use ModuleTest::ToBeUsed
end

defmodule ModuleTest::DuplicateAttribute do
  Module.add_attribute __MODULE__, :foo, 1
  Module.add_attribute __MODULE__, :foo, 2
  Module.add_attribute __MODULE__, :foo, 3
end

defmodule ModuleTest do
  use ExUnit::Case

  Module.register_attribute __MODULE__, :register_example
  @register_example :it_works

  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  contents = quote do: (def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ })
  Module.eval_quoted __MODULE__, contents, [], "sample.ex", 13

  true  = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }
  true  = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  Module.merge_data __MODULE__, value: 1
  Module.merge_data __MODULE__, other_value: 1
  Module.merge_data __MODULE__, other_value: 2

  nil = __FUNCTION__

  test :eval_quoted do
    assert_equal { ::ModuleTest, "sample.ex", 13 }, eval_quoted_info()
  end

  test :line_from_macro do
    assert_equal 31, ModuleTest::ToUse.line
  end

  test :__MODULE__ do
    assert_equal :"::ModuleTest", __MODULE__
  end

  test :merge_data do
    assert_equal [other_value: 2, value: 1], __MODULE__.__info__(:data)
  end

  test :compile_callback_hook do
    refute ModuleTest::ToUse.original_value
    assert Orddict.get ModuleTest::ToUse.__info__(:data), :has_callback, false
  end

  test :default_compile_callback_hook do
    assert Orddict.get ModuleTest::ToUse.__info__(:data), :compiling, false
  end

  test :reserved_attributes do
    assert_equal {:behavior,[:gen_server]}, :lists.keyfind(:behavior, 1, Elixir::Server.__info__(:attributes))
  end

  test :registered_attributes do
    assert_equal {:register_example,[:it_works]}, :lists.keyfind(:register_example, 1, __MODULE__.__info__(:attributes))
  end

  test :duplicated_attributes do
    [{:vsn,_},{:foo,[1]},{:foo,[2]},{:foo,[3]}] = ModuleTest::DuplicateAttribute.__info__(:attributes)
  end

  test :__FUNCTION__ do
    assert_equal { :test___FUNCTION__, 0 }, __FUNCTION__
  end

  test :apply do
    assert_equal [3,2,1], apply(List, :reverse, [[1|[2,3]]])
  end
end
