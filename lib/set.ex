module Set
  def new([])
    #Set::Instance(Erlang.sets.new)
  end

  def from_list(list)
    #Set::Instance(Erlang.sets.from_list(list))
  end

  module Instance
    attr_reader ['set]

    def __bound__(set)
      @('set: set)
    end
  
    def set?
      Erlang.sets.is_set(@set)
    end

    def size
      Erlang.sets.size(@set)
    end
  
    def to_list
      Erlang.sets.to_list(@set)
    end
  
    def add element
      @('set, Erlang.sets.add_element(element, @set))
    end
  
    def include? element
      Erlang.sets.is_element(element, @set)
    end
  
    def delete(element)
      @('set, Erlang.sets.del_element(element, @set))
    end
  
    def union other
      @('set, Erlang.sets.union(@set, other.set))
    end
  
    def intersection other
      @('set, Erlang.sets.intersection(@set, other.set))
    end
  
    def disjoint? other
      Erlang.sets.is_disjoint(@set, other.set)
    end
  
    def subtract other
      @('set, Erlang.sets.subtract(@set, other.set))
    end
    alias_local 'subtract, '-, 1

    def subset?(other)
      Erlang.sets.is_subset(@set, other.set)
    end
  
    def superset?(other)
      other.subset?(self)
    end
  
    def fold(function, accumulator)
      Erlang.sets.fold(function, accumulator, @set)
    end
  
    def filter(function)
      @('set, Erlang.sets.filter(function, @set))
    end
  end
end