module ExUnit::Case
  mixin ExUnit::Assertions

  def __added_as_mixin__(base)
    ExUnit::Server.add_case(base.__name__)
    base
  end

  def synchronous!
    @('synchronous, true)
  end

  def synchronous?
    @synchronous || false
  end

  def __tests__
    regexp = ~r(_test$)
    [name for {name, _} in __mixin_methods__, regexp.match?(name.to_s)]
  end

  def setup(_)
    self
  end

  def teardown(_)
  end
end