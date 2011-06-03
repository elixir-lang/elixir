Code.require_file "../test_helper", __FILE__

module ExceptionsTest
  mixin ExUnit::Case

  def try_only_test
    try
      foo = 13
      foo + 1
    end
    11 = foo
  end

  def try_with_after_test
    assert_executed true, -> try
    after
      put! true
    end
  end

  def try_with_throw_catch_test
    nil = try
      self.throw({1,2})
    catch _
    end

    assert_executed {1,2}, -> try
      self.throw({1,2})
    catch value
      put! value
    end
  end

  def try_with_error_catch_test
    assert_executed {1,2}, -> try
      self.error({1,2})
    catch 'error: value
      put! value
    end
  end

  def try_with_exit_catch_test
    assert_executed {1,2}, -> try
      self.exit({1,2})
    catch 'exit: value
      put! value
    end
  end

  def try_with_catch_and_after_test
    assert_executed true, -> try
      self.throw({1,2})
    catch value
      put! value
    after
      put! true
    end

    {1,2} = try
      assert_executed true, -> try
        self.throw({1,2})
      catch {3,4}
        put! false
      after
        put! true
      end
    catch val
      val
    end
  end

  def try_with_several_catch_test
    assert_executed true, -> try
      self.throw({1,2})
    catch {3,4}
      put! false
    catch {1,2}
      put! true
    end

    assert_executed true, -> try
      self.throw({1,2})
    catch {3,4}, {1,2}
      put! true
    end

    assert_executed true, -> try
      self.error({1,2})
    catch {3,4}, 'error: {1,2}
      put! true
    end
  end

  def implicit_test
    assert_executed true, -> implicit_catch
    assert_executed true, -> implicit_after
    assert_executed true, -> implicit_catch_after
    assert_executed true, -> implicit_useless_catch_after
  end

  private

  def implicit_catch
    self.throw({1,2})
  catch {3,4}, {1,2}
    put! true
  end

  def implicit_after
    []
  after
    put! true
  end

  def implicit_catch_after
    self.throw({1,2})
  catch {3,4}, {1,2}
    put! true
  after
    []
  end

  def implicit_useless_catch_after
    []
  catch {1,2}
    []
  after
    put! true
  end

  def assert_executed(value, function)
    put!(false)
    function.()
    ~value = Erlang.get("assert_executed")
  end

  def put!(value)
    Erlang.put("assert_executed", value)
  end

  def foo
    11
  end
end