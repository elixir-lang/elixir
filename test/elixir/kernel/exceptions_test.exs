Code.require_file "../../test_helper", __FILE__

defmodule Kernel::ExceptionTest do
  use ExUnit::Case

  def test_is_exception do
    true  = is_exception(RuntimeError.new)
    false = is_exception({ :foo, :bar })
  end
end