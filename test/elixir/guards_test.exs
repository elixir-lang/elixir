Code.require_file "../test_helper", __FILE__

module GuardsTest
  mixin ExUnit::Case

  def guards_on_method_test
    2 = guards(1)
    2 = guards(-1)
    'zero = guards(0)
  end

  def guards_on_method_with_defaults_test
    2 = defaults
    2 = defaults(1)
    2 = defaults(-1)
  end

  def guards_on_case_test
    x = 10

    3 = case 1
    match ~x
      10
    match x when x < 0
      x * 2
    match x
      x * 3
    end

    x
  end

  def guards_on_catch_test
    2 = try
      throw 1
    catch 'throw: x when x < 0
      x * -2
    catch 'throw: x
      x * 2
    end

    -20 = try
      throw(-2)
    catch 'throw: x when x < 0
      x * 10
    catch 'throw: x
      x * 2
    end

    -20 = try
      throw(-2)
    catch 'throw: x, 'error: x when x < 0
      x * 10
    catch 'throw: x
      x * 2
    end
  end

  private

  def guards(x) when x > 0
    x * 2
  end

  def guards x when x < 0
    x * - 2
  end

  def guards(_)
    'zero
  end

  def defaults(x := 1) when x > 0
    x * 2
  end

  def defaults(x)
    x * - 2
  end
end