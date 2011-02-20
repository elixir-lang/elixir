object RegexpTest
  proto ExUnit::Case

  def constructor_test
    true = ~r(foo)i == Regexp.new("foo", "i")
    true = ~r(foo)iu == Regexp.new("foo", "iu")
  end

  def match_test
    true = ~r(foo).match?("foo")
    false = ~r(foo).match?("bar")

    true = ~r(foo).match?("afooa")

    false = ~r(^foo).match?("afooa")
    true = ~r(^foo).match?("fooa")

    false = ~r(foo$).match?("afooa")
    true = ~r(foo$).match?("afoo")
  end
end
