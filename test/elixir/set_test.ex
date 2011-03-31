Code.require File.expand_path("../test_helper", __FILE__)

object SetTest
  proto ExUnit::Case
  
  def is_set_test
    true = Set.new().set?    
  end
  
  def from_list_test
    [3,2,1] = Set.from_list([1,2,3]).to_list  % Fix up order depenance  
  end
  
  def size_test
    0 = Set.new().size
    1 = Set.new().add(2).size
  end
  
  def to_list_test
    [] = Set.new().to_list
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
    [3,2,1] = Set.new.add(1).add(2).union(Set.new.add(2).add(3)).to_list % Fix up order depenance  
    [3,2,5,1,4] = Set.from_list([1,2,3]).union(Set.from_list([2,4,5])).to_list
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
  end
  
  def subset_test
    true = Set.new.add(1).subset?(Set.new.add(1).add(2))
    false = Set.new.add(3).subset?(Set.new.add(1).add(2))
  end
  
  def fold_test
    set = Set.new.add(1).add(2)
    3 = set.fold(-> (x,y) x + y, 0)
  end
  
  def filter_test
    set = Set.new.add(1).add(2).add(3).add(4)
    [3,4] = set.filter(-> (x) x > 2).to_list
  end
end
