Code.require_file "../test_helper", __FILE__

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

  def test_merge_data do
    [other_value: 2, value: 1] == __MODULE__.__data__
  end
end