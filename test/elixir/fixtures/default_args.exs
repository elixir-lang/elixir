module DefaultArgs
  def arity0(foo := 1)
    foo
  end

  def arity1(foo, bar := 2)
    { foo, bar }
  end

  def many0(foo := 1, bar:= 2)
    { foo, bar }
  end

  def many1(foo, bar := 1, baz := 2)
    { foo, bar, baz }
  end

  def clash([])
    [1,2,3]
  end

  def clash(foo := 1, bar := 2)
    { foo, bar }
  end

  def atom('foo, baz := 1)
    baz
  end

  def atom('bar, baz := 2)
    baz
  end
end