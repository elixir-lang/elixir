object RegexpTest
  proto ExUnit::Case

  def constructor_test
    true = ~r(foo)i == Regexp.new("foo", "i")
    true = ~r(foo)iu == Regexp.new("foo", "iu")
  end
end
