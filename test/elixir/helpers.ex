module Helpers

  def assert_raise(error_type, function)
    { 'EXIT, { { error_type, true }, _ } } = self.catch do
      function()
    end
    % Return true to avoid nested self.catch failures
    true
  end

end