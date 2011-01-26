object String
  def constructor(list)
    { 'list: list }
  end

  def +(another)
    String.new(@list + another.to_list)
  end

  def length
    Erlang.length(@list)
  end

  def to_list
    @list
  end

  def to_s
    self
  end
end