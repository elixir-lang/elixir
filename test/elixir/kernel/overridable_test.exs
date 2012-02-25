Code.require_file "../../test_helper", __FILE__

defmodule Kernel::Overridable do
  @overridable true
  def sample do
    1
  end
end

defmodule Kernel::OverridableTest do
  use ExUnit::Case

  test "overridable function is made concrete if no other is defined" do
    assert_equal 1, Overridable.sample
  end
end