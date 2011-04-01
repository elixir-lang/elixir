% elixir: cache

object Set
  module Mixin
    def from_list(list)
      self.new(Erlang.sets.from_list(list))
    end
  end
  
  def initialize
    @('set: Erlang.sets.new)
  end
  
  def initialize set
    @('set: set)
  end
  
  def set?
    Erlang.sets.is_set(set)
  end

  def size
    Erlang.sets.size(set)
  end
  
  def to_list
    Erlang.sets.to_list(set)
  end
  
  def add element
    Set.new(Erlang.sets.add_element(element, set))
  end
  
  def include? element
    Erlang.sets.is_element(element, set)
  end
  
  def delete(element)
    Set.new(Erlang.sets.del_element(element, set))
  end
  
  def union other
    Set.new(Erlang.sets.union(set, other.set))
  end
  
  def set
    @set
  end
  
  def intersection other
    Set.new(Erlang.sets.intersection(set, other.set))
  end
  
  def disjoint? other
    Erlang.sets.is_disjoint(set, other.set)
  end
  
  def subtract other
    Set.new(Erlang.sets.subtract(set, other.set))
  end
  alias_local 'subtract, '-, 1

  def subset?(other)
    Erlang.sets.is_subset(set, other.set)
  end
  
  def superset?(other)
    other.subset?(self)
  end
  
  def fold(function, accumulator)
    Erlang.sets.fold(function, accumulator, set)
  end
  
  def filter(function)
    Set.new(Erlang.sets.filter(function, set))
  end
end