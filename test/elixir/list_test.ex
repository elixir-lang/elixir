object ListTest < ExUnit::Case

  def include_and_member_test
    list = [1,2,3]
    true  = list.include?(1)
    false = list.include?(4)
    true  = list.member?(1)
    false = list.member?(4)
  end

  def add_two_lists_together_test
    list_one = [1,2,3]
    list_two = [4,5,6]
    [1,2,3,4,5,6] = list_one + list_two
    [1,2,3,1,2,3] = list_one + list_one
  end

  def head_test
    1  = [1,2,3].head
    1  = [1].head
    [] = [].head
  end

  def tail_test
    [2,3] = [1,2,3].tail
    [2] = [1,2].tail
    [] = [1].tail
    [] = [].tail
  end
end