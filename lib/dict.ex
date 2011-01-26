object Dict
  def constructor
    { 'dict: Erlang.dict.new }
  end

  def fold(acc, function)
    Erlang.dict.fold(function, acc, @dict)
  end

  % TODO Use append here is a bad idea. Fix it once we have [H|T]
  def to_s
    transformer = -> (key, value, acc) acc.append "#{key}: #{value}"
    "{" + fold([], transformer).join(", ") + "}"
  end
end