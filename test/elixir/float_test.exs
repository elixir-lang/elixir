Code.require_file "../test_helper", __FILE__

module FloatTest
  mixin ExUnit::Case

  def to_s_test
    "1.00000000000000000000e+00" = 1.0.to_s
  end
end
