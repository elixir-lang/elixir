object Recorder
  attr_reader ['calls]

  def initialize
    @('calls : [])
  end

  def method_missing(method, args)
    @('calls: [{method, args}|@calls])
  end

  def play(receiver)
    @calls.foldr receiver, do ({method, args}, acc)
      acc.send(method, args)
    end
  end
end
