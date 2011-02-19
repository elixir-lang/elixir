object Binary
  def to_bin
    self
  end

  def to_char_list
    Erlang.binary_to_list(self)
  end
  
  def to_list
    Erlang.binary_to_list(self)
  end
end