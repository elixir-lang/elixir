object Tuple
  def inspect
    strings = to_list.map -> (x) x.inspect
    "{#{strings.join([$,, $\s])}}"
  end

  def to_s
    inspect
  end

  def to_list
    Erlang.tuple_to_list(self)
  end
end
