Code.require_file "../../test_helper", __FILE__

defmodule Kernel::Abstract do
  @abstract true
  def sample do
    1
  end
end

defmodule Kernel::AbstractTest do
  use ExUnit::Case

  test "abstract function is made concrete if no other is defined" do
    assert_equal 1, Abstract.sample
  end
end