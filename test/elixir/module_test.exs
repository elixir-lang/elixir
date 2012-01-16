Code.require_file "../test_helper", __FILE__

defmodule ModuleTest::ToBeUsed do
  def value, do: 1

  defmacro __using__(target) do
    Module.merge_data target, callback: false
    Module.add_compile_callback(target, __MODULE__)
    Module.add_compile_callback(target, __MODULE__, :callback)
    quote { def line, do: __LINE__ }
  end

  def __compiling__(target) do
    Module.merge_data target, compiling: true
  end

  defmacro callback(target) do
    value = Module.read_data(target, :callback)
    Module.merge_data target, callback: true
    quote do
      name = :original_value
      def name, [], [], do: unquote(value)
    end
  end
end

defmodule ModuleTest::ToUse do
  28 = __LINE__ # Moving the next line around can make tests fail
  use ModuleTest::ToBeUsed
end

defmodule ModuleTest do
  use ExUnit::Case

  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  contents = quote { def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ } }
  Module.eval_quoted __MODULE__, contents, [], "sample.ex", 13

  true  = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }
  true  = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :def
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defp
  false = Module.function_defined? __MODULE__, { :eval_quoted_info, 0 }, :defmacro

  Module.merge_data __MODULE__, value: 1
  Module.merge_data __MODULE__, other_value: 1
  Module.merge_data __MODULE__, other_value: 2

  def test_eval_quoted do
    { ::ModuleTest, "sample.ex", 13 } = eval_quoted_info()
  end

  def test_line_from_macro do
    29 = ModuleTest::ToUse.line
  end

  def test_refer_with_one_arg do
    refer ModuleTest::ToBeUsed
    1 = ToBeUsed.value
  end

  def test___MODULE__ do
    __MODULE__ = :"::ModuleTest"
    :"::ModuleTest" = __MODULE__
  end

  def test_merge_data do
    [other_value: 2, value: 1] == __MODULE__.__info__(:data)
  end

  def test_compile_callback_hook do
    false = ModuleTest::ToUse.original_value
    true  = Orddict.get ModuleTest::ToUse.__info__(:data), :callback, false
  end

  def test_default_compile_callback_hook do
    true  = Orddict.get ModuleTest::ToUse.__info__(:data), :compiling, false
  end
end