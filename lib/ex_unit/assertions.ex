module ExUnit::Assertions
  def assert_included(base, container)
    unless container.include?(base)
      self.error { 'badassertion, "Expected #{container.inspect} to include #{base.inspect}" } 
    end
  end

  def assert_not_included(base, container)
    if container.include?(base)
      self.error { 'badassertion, "Expected #{container.inspect} to not include #{base.inspect}" } 
    end
  end
 
  def assert_error(value, function)
    assert_try('error, value, function)
  end 

  def assert_throw(value, function)
    assert_try('throw, value, function)
  end 

  def assert_exit(value, function)
    assert_try('exit, value, function)
  end 

  private

  def assert_try(kind, value, function)
    result = try
      function.()
      false
    catch ~kind: ~value
      true
    catch real_kind: real_value
      bad_assertion "Expected #{kind} with #{value.inspect}, got #{real_kind} with #{real_value.inspect}"
    end

    unless result
      bad_assertion "Expected #{kind} with #{value.inspect}, got nothing"
    end
  end 

  def bad_assertion(message)
    self.error { 'badassertion, message }
  end
end