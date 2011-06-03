Code.require_file "../test_helper", __FILE__

module SetTest
  mixin ExUnit::Case
  
  def is_set_test
    true = Set.new().set?    
  end
  
  def from_list_test
    set = Set.from_list([1,2,3])
    true = set.include?(1)
    true = set.include?(2)
    true = set.include?(3)
    3 = set.size
  end
  
  def size_test
    0 = Set.new().size
    1 = Set.new().add(2).size
  end
  
  def to_list_test
    [] = Set.new.to_list
    [1] = Set.new.add(1).to_list
  end
  
  def add_test
    [1] = Set.new().add(1).to_list
  end
  
  def include_test
    false = Set.new().include?(1)
    true = Set.new().add(1).include?(1)
  end
  
  def delete_test
    false = Set.new().add(1).delete(1).include?(1)
  end
  
  def equality_test
    true = Set.new == Set.new
    false = Set.new == Set.new.add(1)
    true = Set.new.add(1) == Set.new.add(1)
  end
  
  def union_test
    true = Set.from_list([1,2,3]) == Set.new.add(1).add(2).union(Set.new.add(2).add(3))
    true = Set.from_list([1,2,3,4,5]) == Set.from_list([1,2,3]).union(Set.from_list([2,4,5]))
  end
  
  def intersection_test
    [2] = Set.new.add(1).add(2).intersection(Set.new.add(2).add(3)).to_list
  end
  
  def disjoint_test
    true = Set.new.add(1).disjoint?(Set.new.add(2))
    false = Set.new.add(1).disjoint?(Set.new.add(1))
  end
  
  def subtract_test
    [1] = Set.new.add(1).add(2).subtract(Set.new.add(2)).to_list
    [1] = (Set.from_list([1,2,3]) - Set.from_list([2,3,4])).to_list
  end
  
  def subset_test
    true = Set.new.add(1).subset?(Set.new.add(1).add(2))
    false = Set.new.add(3).subset?(Set.new.add(1).add(2))
  end
  
  def superset_test
    false = Set.new.add(1).superset?(Set.new.add(1).add(2))
    true = Set.new.add(1).add(2).superset?(Set.new.add(1))
  end
  
  def fold_test
    set = Set.new.add(1).add(2)
    3 = set.fold(-> (x,y) x + y, 0)
  end
  
  def filter_test
    set = Set.from_list([1,2,3,4])
    true = Set.from_list([3,4]) == set.filter(-> (x) x > 2)
  end
end
