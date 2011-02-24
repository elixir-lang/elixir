object ExceptionsTest
  proto ExUnit::Case

  def begin_only_test
    begin
      foo = 13
      foo + 1
    end
    11 = foo
  end

  def begin_with_after_test
    assert_executed true, -> begin
    after
      put! true
    end
  end

  def begin_with_throw_rescue_test
    assert_executed {1,2}, -> begin
      self.throw({1,2})
    rescue value
      put! value
    end
  end

  def begin_with_error_rescue_test
    assert_executed {1,2}, -> begin
      self.error({1,2})
    rescue 'error: value
      put! value
    end
  end

  def begin_with_exit_rescue_test
    assert_executed {1,2}, -> begin
      self.exit({1,2})
    rescue 'exit: value
      put! value
    end
  end

  def begin_with_rescue_and_after_test
    assert_executed true, -> begin
      self.throw({1,2})
    rescue value
      put! value
    after
      put! true
    end

    {1,2} = self.catch do
      assert_executed true, -> begin
        self.throw({1,2})
      rescue {3,4}
        put! false
      after
        put! true
      end
    end
  end

  def begin_with_several_rescue_test
    assert_executed true, -> begin
      self.throw({1,2})
    rescue {3,4} 
      put! false
    rescue {1,2}
      put! true
    end

    assert_executed true, -> begin
      self.throw({1,2})
    rescue {3,4}, {1,2}
      put! true
    end

    assert_executed true, -> begin
      self.error({1,2})
    rescue {3,4}, 'error: {1,2}
      put! true
    end
  end

  private

  def assert_executed(value, function)
    put!(false)
    function()
    value = Erlang.get("assert_executed")
  end

  def put!(value)
    Erlang.put("assert_executed", value)
  end

  def foo
    11
  end
end