Code.require_file "../test_helper", __FILE__

module OrderedDictTest
  mixin ExUnit::Case

  def setup(_)
    @('dict, { 'a: 1 })
  end

  def instance_variable_bracket_test
    1 = @dict['a]
  end

  def fold_test
    [2, 1] = a_dict.fold [], -> (key, _, acc) [key|acc]
    33 = a_dict.fold 0, -> (_, value, acc) acc + value
  end

  def map_test
    { 1: 11, 2: 44 } == a_dict.map -> (key, value) key * value
  end

  def from_list_test
    { 'a: 1, 'b: 2 } = OrderedDict.from_list(['a/1, 'b/2])
  end

  def merge_test
    { 'a: 3, 'b: 2 } = { 'a: 1 }.merge 'b: 2, 'a: 3
  end

  def merge_function_test
    { 'a: 1, 'b: 2 } = { 'a: 1 }.merge { 'b: 2, 'a: 3 }, -> (_, v1, _) v1
  end

  def to_list_test
    [{'a, 1}, {'b, 2}] = { 'b: 2, 'a: 1 }.to_list
  end

  def match_test
    { 1: 2, 2: 4 } = { 2: 4, 1: 2 }

    { 1: 2, x: y } = { 2: 4, 1: 2 }
    2 = x
    4 = y

    { 1,2 } = kwargs 'a: 1, 'b: 2
    { 1,2 } = kwargs 'b: 2, 'a: 1
    { 1,2 } = kwargs 'b: 2, 'a: 1, 'c: 3

    self.assert_error {'badmatch, {1: 2, 2: 4}}, do
      { 2: 4, 1: 2 } = { 1: 2, 2: 4 }
    end
  end

  def delete_test
    { 'a: 1 } = { 'a: 1, 'b: 2 }.delete('b)
    { 'a: 1 } = { 'a: 1 }.delete('b)
  end

  def update_test
    [1] = {}.update('foo, [1], -> (_) self.error "Never called")['foo]

    dict = { 'list: [] }.update('list, _.push(1)).update('list, _.push(2))
    [1,2] = dict['list]

    assert_error 'function_clause, do
      {}.update('foo, 2)
    end
  end

  def brackets_test
    4 = { 2: 4, 1: 2 }[2]
    2 = { 2: 4, 1: 2 }[1]
    nil = { 2: 4, 1: 2 }[0]
  end

  def get_test
    4 = { 2: 4, 1: 2 }.get 2
    2 = { 2: 4, 1: 2 }.get 1
    nil = { 2: 4, 1: 2 }.get 0
  end

  def key_question_mark_test
    true  = { 1: 2 }.key?(1)
    false = { 1: 2 }.key?(2)
  end

  def set_new_test
    { 1: 2 } = { 1: 2 }.set_new(1, 3)
    { 1: 3 } = {}.set_new(1, 3)
  end

  def empty_test
    false = { 'a: "abc" }.empty?
    true  = {}.empty?
  end

  def inspect
    "{}" = {}.inspect
    "{'a: 'b}" = {'a: 'b}.inspect
  end

  private

  def a_dict
    { 1:11, 2:22 }
  end

  def kwargs('a: x, 'b: y)
    { x, y }
  end

  def kwargs(dict)
    kwargs(dict.delete('c))
  end
end
