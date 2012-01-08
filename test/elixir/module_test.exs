Code.require_file "../test_helper", __FILE__

defmodule ModuleTest::ToBeUsed do
  def value, do: 1

  def __using__(target) do
    Module.merge_data target, callback: false
    Module.add_compile_callback(target, __MODULE__, :callback)
  end

  defmacro callback(target) do
    value = Orddict.fetch(Module.read_data(target), :callback, nil)
    Module.merge_data target, callback: true
    quote { def original_value, do: unquote(value) }
  end
end

defmodule ModuleTest::ToUse do
  use ModuleTest::ToBeUsed
end

defmodule ModuleTest do
  use ExUnit::Case

  contents = quote { def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ } }
  Module.eval_quoted __MODULE__, contents, [], __FILE__, __LINE__

  Module.merge_data __MODULE__, value: 1
  Module.merge_data __MODULE__, other_value: 1
  Module.merge_data __MODULE__, other_value: 2

  def test_eval_quoted do
    # We do not assert on the line here because macros
    # always ignore the line numbers. We need to revaluate
    # the situation on Erlang R15.
    { ::ModuleTest, __FILE__, _ } = eval_quoted_info()
  end

  def test_refer_with_one_arg do
    refer ModuleTest::ToBeUsed
    1 = ToBeUsed.value
  end

  def test_merge_data do
    [other_value: 2, value: 1] == __MODULE__.__data__
  end

  def test_compile_callback_hook do
    false = ModuleTest::ToUse.original_value
    [callback: true] = ModuleTest::ToUse.__data__
  end
end