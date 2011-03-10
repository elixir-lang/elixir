Code.require File.expand_path("../test_helper", __FILE__)

object OrderedDictTest
  proto ExUnit::Case

  def fold_test
    [2, 1] = a_dict.fold [], -> (key, _, acc) [key|acc]
    33 = a_dict.fold 0, -> (_, value, acc) acc + value
  end

  def map_test
    { 1: 11, 2: 44 } == a_dict.map -> (key, value) key * value
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
    [1] = {:}.update('foo, [1], -> (_) self.error "Never called")['foo]

    dict = { 'list: [] }.update('list, _.push(1)).update('list, _.push(2))
    [1,2] = dict['list]

    self.assert_error 'function_clause, do
      {:}.update('foo, 2)
    end
  end

  def brackets_test
    4 = { 2: 4, 1: 2 }[2]
    2 = { 2: 4, 1: 2 }[1]
    [] = { 2: 4, 1: 2 }[0]
  end

  protected

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
