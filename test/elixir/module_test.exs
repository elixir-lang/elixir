defmodule ModuleTest do
  use ExUnit::Case

  contents = quote { def eval_info, do: { __MODULE__, __FILE__, __LINE__ } }
  Module.eval __MODULE__, __FILE__, __LINE__ - 1, contents

  def test_eval do
    # We do not assert on the line here because macros
    # always ignore the line numbers. We need to revaluated
    # the situation on Erlang R15.
    { ::ModuleTest, __FILE__, _ } = eval_info()
  end
end