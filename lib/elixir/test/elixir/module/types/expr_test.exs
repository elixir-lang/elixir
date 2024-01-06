Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.ExprTest do
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr

  test "literal" do
    assert typecheck!(true) == atom(true)
    assert typecheck!(false) == atom(false)
    assert typecheck!(:foo) == atom(:foo)
    assert typecheck!(0) == integer()
    assert typecheck!(0.0) == float()
    assert typecheck!("foo") == binary()
    assert typecheck!([]) == empty_list()
    assert typecheck!([1, 2]) == non_empty_list()
    assert typecheck!({1, 2}) == tuple()
    assert typecheck!(%{}) == map()
  end
end
