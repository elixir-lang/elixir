object ListTest < ExUnit::Case
  def include_and_member_test
    list = [1,2,3]
    true  = list.include?(1)
    false = list.include?(4)
    true  = list.member?(1)
    false = list.member?(4)
  end
end