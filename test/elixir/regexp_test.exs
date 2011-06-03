Code.require_file "../test_helper", __FILE__

module RegexpTest
  mixin ExUnit::Case

  module Yes
    def yes?
      "YES"
    end
  end

  def mixin_and_proto_test
    regexp = Regexp.new("foo", "i").mixin(RegexpTest::Yes).proto(RegexpTest::Yes)
    "YES"  = regexp.yes?
    regexp = regexp.new("foo", "i")
    "YES"  = regexp.yes?
  end

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

  def caseless_match_test
    false = ~r(foo).match?("fOo")
    true = ~r(foo)i.match?("fOo")
  end

  def run_test
    ["cd", "d"] = ~r"c(d)".run("abcd")
    nil = ~r"e".run("abcd")
    [{3,3}] = ~r"bar".run("foobarbaz", 'all, 0, 'index)
  end

  def replace_test
    "abc"   = ~r(d).replace("abc", "d")
    "adc"   = ~r(b).replace("abc", "d")
    "a[b]c" = ~r(b).replace("abc", "[&]")
    "a[&]c" = ~r(b).replace("abc", "[\\&]")
    "a[b]c" = ~r[(b)].replace("abc", "[\\1]")
  end

  def replace_all_test
    "abcbe"     = ~r(d).replace("abcbe", "d")
    "adcde"     = ~r(b).replace_all("abcbe", "d")
    "a[b]c[b]e" = ~r(b).replace_all("abcbe", "[&]")
    "a[&]c[&]e" = ~r(b).replace_all("abcbe", "[\\&]")
    "a[b]c[b]e" = ~r[(b)].replace_all("abcbe", "[\\1]")
  end
end
