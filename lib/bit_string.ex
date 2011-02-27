object BitString
  def to_bin
    self
  end

  def to_char_list
    Erlang.binary_to_list(self)
  end
  
  def to_list
    Erlang.binary_to_list(self)
  end

  def inspect
    String.new Erlang.io_lib.format($"~p", [self]).flatten
  end

  def to_s
    inspect
  end
end