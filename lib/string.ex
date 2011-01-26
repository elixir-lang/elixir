object String
  def contructor(list)
    { 'list: list }
  end

  def length
    Erlang.length(@list)
  end
end