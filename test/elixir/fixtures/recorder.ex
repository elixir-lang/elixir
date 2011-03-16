object Recorder
  attr_reader ['calls]

  def constructor
    {'calls : []}
  end

  def method_missing(method, args)
    @('calls: [{method, args}|@calls])
  end

  def play(receiver)
    @calls.foldr receiver, do ({method, args}, acc)
      acc.__send__(method, args)
    end
  end
end
