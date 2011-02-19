object OrderedDictTest
  proto ExUnit::Case

  def fold_test
    [2, 1] = a_dict.fold [], -> (key, _, acc) [key|acc]
    33 = a_dict.fold 0, -> (_, value, acc) acc + value
  end

  def map_test
    true = { 1:11, 2:44 } == a_dict.map -> (key, value) key * value
  end

  protected

  def a_dict
    { 1:11, 2:22 }
  end
end
