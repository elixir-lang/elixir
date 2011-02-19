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

    self.assert_raise {'badmatch, {1: 2, 2: 4}}, do
      { 2: 4, 1: 2 } = { 1: 2, 2: 4 }
    end
  end

  protected

  def a_dict
    { 1:11, 2:22 }
  end
end
