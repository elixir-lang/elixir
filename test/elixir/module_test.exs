module ModuleTest do
  use ExUnit::Case

  contents = "def eval_info, do: { __MODULE__, __FILE__, __LINE__ }"
  Module.eval __MODULE__, contents, __FILE__, __LINE__ - 1

  def test_eval do
    { ::ModuleTest, __FILE__, 4 } = eval_info()
  end
end