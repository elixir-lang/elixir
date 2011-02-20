object BinaryTest
  proto ExUnit::Case

  def inspect_test
    "<<1,2,3>>" = << 1,2,3 >>.inspect
  end

  def bin_comprehension_test
    <<2,3,4>> = << x+1 for <<x>> in <<1,2,3>> >>
    <<2,3,4>> = << x+1|integer for <<x>> in <<1,2,3>> >>
    <<>> = << x * 2 for <<x>> in <<1,2,3>>, falsy >>

    <<2,4,6>> = << x * 2 for x in [1,2,3] >>
    <<2,4,6>> = << x * 2 for x in [1,2,3], true >>
    <<2,4,6>> = << x * 2 for x in [1,2,3], x.abs > 0 >>
    <<3,2,6,4,9,6>> = << x * y for x in [1,2,3], y in [3,2]>>
  end

  def variables_in_bin_comprehensions_do_not_leak_test
    <<2,4,6>> = << falsy * 2 for <<falsy>> in <<1,2,3>> >>
    false = falsy
  end

  private

  def falsy
    false
  end
end
