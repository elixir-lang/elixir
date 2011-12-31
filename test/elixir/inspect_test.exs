module InspectTest do
  use ExUnit::Case

  def test_atom do
    ":foo" = inspect(:foo)
  end
end