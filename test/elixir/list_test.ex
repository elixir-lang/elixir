object ListTest
  proto ExUnit::Case

  def length_size_test
    3 = [1,2,3].length
    0 = [].size
  end

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

  def each_test
    list = [1,2,4]
    list.each do (x)
      true = list.include?(x)
    end

    self.assert_raise { 'badmatch, true }, do
      list.each -> (x) false = list.include?(x)
    end
  end

  def each_returns_self_test
    list = [1,2,3]
    each_returns = list.each -> (x) x
    list = each_returns
  end

  def map_and_collect_test
    map_result = [1,2,3].map -> (x) x + 1
    [2,3,4] = map_result

    collect_result = [1,2,3].collect -> (x) x + 1
    [2,3,4] = collect_result
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

  def delete_test
    x = [6,8,4,9]
    [6,8,4] = x.delete(9)
    [6,8,9] = x.delete(4)
  end

  def delete_with_duplicates_test
    x = [6,8,6,4,9]
    [8,6,4,9] = x.delete(6)
  end

  def brackets_test
    1 = [1,2,3][0]
    2 = [1,2,3][1]
    3 = [1,2,3][2]
    self.assert_raise 'function_clause, do
      [1,2,3][3]
    end
  end

  def brackets_negative_index_test
    1 = [1,2,3][-3]
    2 = [1,2,3][-2]
    3 = [1,2,3][-1]
    self.assert_raise 'function_clause, do
      [1,2,3][-4]
    end
  end

  def brackets_syntax_test
    2 = [1,2,3][1]
    2 = [1,2,3] [1]
    2 = ([1] + [2,3])[1]

    % Test ambiguity in method calls
    2 = a_list()[1]
    2 = self.a_list()[1]
    2 = a_list[1]
    2 = self.a_list[1]
    [1] = a_list [1]
    [1] = self.a_list [1]
    2 = 6 - 1 - self.a_list[1] - 1

    % Test ambiguity in variables
    a_list = [4,5,6]
    5 = a_list[1]
    [1] = a_list [1]

    % Test ambiguity in method calls with punctuation
    2 = a_list!()[1]
    2 = self.a_list!()[1]
    2 = a_list![1]
    2 = self.a_list![1]
    [1] = a_list! [1]
    [1] = self.a_list! [1]
    2 = 6 - 1 - self.a_list![1] - 1
  end

  protected

  def a_list
    [1,2,3]
  end

  def a_list(list)
    list
  end

  def a_list!
    [1,2,3]
  end

  def a_list!(list)
    list
  end
end