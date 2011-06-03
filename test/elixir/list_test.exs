Code.require_file "../test_helper", __FILE__

module ListTest
  mixin ExUnit::Case

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

  def inspect_test
    "[]" = [].inspect    
    "[1,2,3]" = [1,2,3].inspect
    "[1,2|3]" = [1,2|3].inspect
  end

  def add_two_lists_together_test
    list_one = [1,2,3]
    list_two = [4,5,6]
    [1,2,3,4,5,6] = list_one + list_two
    [1,2,3,1,2,3] = list_one + list_one
  end

  def fold_test
    addfun = -> (e, acc) e + acc
    "bazbarfoo" = ["foo", "bar", "baz"].foldl("", addfun)
    "foobarbaz" = ["foo", "bar", "baz"].foldr("", addfun)
  end

  def flatten_test
    [1,2,3] = [1,2,3].flatten
    [1,2,3] = [[1,2,3]].flatten
    [1,2,3] = [[[1,2,3]]].flatten
    [1,2,3] = [[1],[2],[3]].flatten
  end

  def flatten_lists_test
    [1,2,3] = [[1,2,3]].flatten_lists
    [1,2,3] = [[1],[2],[3]].flatten_lists
    assert_error 'badarg, -> [1,2,3].flatten_lists
  end

  def each_test
    list = [1,2,4]
    list.each do (x)
      true = list.include?(x)
    end

    assert_error { 'badmatch, true }, do
      list.each -> (x) false = list.include?(x)
    end
  end

  def each_returns_self_test
    list = [1,2,3]
    each_returns = list.each -> (x) x
    ~list = each_returns
  end

  def map_and_collect_test
    map_result = [1,2,3].map -> (x) x + 1
    [2,3,4] = map_result

    collect_result = [1,2,3].collect -> (x) x + 1
    [2,3,4] = collect_result
  end

  def filter_and_select_test
    filter_result = [1,2,3].filter -> (x) x / 2 == 1
    [2] = filter_result

    select_result = [1,2,3,4].select -> (x) [3,4].include?(x)
    [3,4] = select_result
  end

  def proper_test
    true  = [1,2].proper?
    true  = [1|[2]].proper?
    false = [1|2].proper?
  end

  def all_test
    true  = [].all? -> (_) false
    false = [1,2,3].all? -> (i) i rem 2 == 0
    true  = [2,4,6].all? -> (i) i rem 2 == 0
  end

  def keyfind_test
    {'foo, 1} = ['foo/1, 'bar/2].keyfind('foo, 0)
    {'bar, 2} = ['foo/1, 'bar/2].keyfind('bar, 0)
    false = ['foo/1, 'bar/2].keyfind('foo, 1)
    false = ['foo/1, 'bar/2].keyfind('baz, 0)
  end

  def zip_test
    [{'foo, 1}, {'bar, 2}] = ['foo, 'bar].zip([1, 2])
    assert_error 'function_clause, -> ['foo].zip([1, 2])
  end

  def unzip_test
    {['foo, 'bar], [1,2]} = [{'foo, 1}, {'bar, 2}].unzip
    assert_error 'function_clause, -> [{'foo, 0}, 'bar].unzip
  end

  def zipwith_test
    [5,7,9] = [1,2,3].zipwith([4,5,6], -> (x,y) x + y)
    assert_error 'function_clause, -> [1,2,3].zipwith([1,2], -> (_,_) 0)
  end

  def head_test
    1  = [1,2,3].head
    1  = [1].head
    assert_error 'badarg, -> [].head
  end

  def tail_test
    [2,3] = [1,2,3].tail
    [2] = [1,2].tail
    [] = [1].tail
    assert_error 'badarg, -> [].tail
  end

  def uniq_test
    [1,2,4] = [1,2,1,4,1,2].uniq
  end

  def delete_test
    x = [6,8,4,9]
    [6,8,4] = x.delete(9)
    [6,8,9] = x.delete(4)
  end

  def arithmetic_syntax_test
    [1,2,3] = [1,2,3].delete -1
    2 = [1,2,3].length - 1
    2 = [1,2,3].length-1
  end

  def delete_with_duplicates_test
    x = [6,8,6,4,9]
    [8,6,4,9] = x.delete(6)
  end

  def delete_all_test
    x = [1,2,1,3]
    [3,2] = x.delete_all(1)
  end

  def join_test
    "1,2,3" = [1,2,3].join(",")
    "foo" = ["foo"].join("_")
    "foo_bar" = ['foo, 'bar].join("_")
  end

  def sort_test
    [1,2,3,4,4] = [4,1,3,2,4].sort
    ["bar", "baz", "foo"] = ["foo", "bar", "baz"].sort
  end

  def split_test
    {[1,2,3], [4,5]} = [1,2,3,4,5].split(3)
    assert_error 'badarg, -> [1,2,3,4,5].split(10)
  end

  def insert_test
    [1,2,0,3,4,5] = [1,2,3,4,5].insert(0, 2)
    assert_error 'badarg, -> [1,2,3].insert(0, 10)
  end

  def brackets_test
    1 = [1,2,3][0]
    2 = [1,2,3][1]
    3 = [1,2,3][2]
    nil = [1,2,3][3]
  end

  def brackets_negative_index_test
    1 = [1,2,3][-3]
    2 = [1,2,3][-2]
    3 = [1,2,3][-1]
    nil = [1,2,3][-4]
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

  def list_comprehension_test
    [2,4,6] = [x * 2 for x inlist [1,2,3]]
    [2,4,6] = [x * 2 for x in [1,2,3], true]
    [2,4,6] = [x * 2 for x in [1,2,3], x.abs > 0]
    [2,6] = [x * 2 for x in [0,1,-2,3], x > 0]
    [3,2,6,4,9,6] = [x * y for x in [1,2,3], y in [3,2]]
    [] = [x * 2 for x in [1,2,3], falsy]
    [] = [x * 2 for x in [1,2,3], nilly]
  end

  def list_comprehension_with_binary_generator_test
    [2,4,6] = [x * 2 for <<x>> in <<1,2,3>>]
    [2,4,6] = [x * 2 for <<x>> inlist [<<1>>,<<2>>,<<3>>]]

    pixels = << 213,45,132,64,76,32,76,0,0,234,32,15 >>
    result = [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]
    ~result = [{r,g,b} for <<r:8,g:8,b:8>> in pixels]
  end

  def variables_in_list_comprehensions_do_not_leak_test
    [2,4,6] = [falsy * 2 for falsy in [1,2,3]]
    false = falsy
  end

  def empty_test
    false = ["abc"].empty?
    true  = [].empty?
  end

  % Helper methods

  def a_list
    [1,2,3]
  end

  def a_list(list)
    list
  end

  def falsy
    false
  end

  def nilly
    nil
  end

  def a_list!
    [1,2,3]
  end

  def a_list!(list)
    list
  end
end
