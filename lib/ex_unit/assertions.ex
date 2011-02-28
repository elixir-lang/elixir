module ExUnit::Assertions
  def assert_raise(rescued, function)
    { 'EXIT, { rescued, _ } } = self.catch! do
      function()
    end
    % Return true to avoid nested self.catch failures
    true
  end 
end