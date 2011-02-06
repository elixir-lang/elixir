object ListTest < ExUnit::Case
  def list_test
    true = [1].include?(1)
    false = [1].include?(1)
  end
end