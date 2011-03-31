object Set
  module Mixin
    def from_list(list)
      self.new(Erlang.sets.from_list(list))
    end
  end
  
  def initialize
    @('set: Erlang.sets.new())
  end
  
  def initialize set
    @('set: set)
  end
  
  % is set
  def set?
    Erlang.sets.is_set(@set)
  end
  
  % size
  def size
    Erlang.sets.size(@set)
  end
  
  % to_list
  def to_list
    Erlang.sets.to_list(@set)
  end
  
  % from_list
  
  % add_element
  def add element
    Set.new(Erlang.sets.add_element(element, @set))
  end
  
  def include? element
    Erlang.sets.is_element(element, @set)
  end
  
  def delete(element)
    Set.new(Erlang.sets.del_element(element, @set))
  end
  
  % union(Set1, Set2)
  def union set
    Set.new(Erlang.sets.union(@set, set.set))
  end
  
  def set
    @set
  end
  
  def intersection set
    Set.new(Erlang.sets.intersection(@set, set.set))
  end
  
  def disjoint? set
    Erlang.sets.is_disjoint(@set, set.set)
  end
  
  def subtract set
    Set.new(Erlang.sets.subtract(@set, set.set))
  end

  def subset?(set)
    Erlang.sets.is_subset(@set, set.set)
  end
  
  %fold(Function, Acc0, Set) -> Acc1
  
  %filter(Pred, Set1) -> Set2
end