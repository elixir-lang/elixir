object Binary
  def to_s
    String.new self
  end

  def to_bin
    self
  end

  def to_char_list
    Erlang.binary_to_list @bin
  end
  
  def to_list
    Erlang.binary_to_list @bin
  end
end