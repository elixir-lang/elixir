% Example taken from http://learnyousomeerlang.com/more-on-multiprocessing#state-your-state
module Kitchen
  def store(pid, food)
    pid <- {Pid.self, {'store, food}}
    receive {pid, msg}
      msg
    end
  end

  def take(pid, food)
    pid <- {Pid.self, {'take, food}}
    receive {pid, msg}
      msg
    after 10000
      IO.puts "Timed out"
    end
  end

  def fridge(foodlist)
    receive
    match {from, {'store, food}}
      from <- {Pid.self, 'ok}
      fridge([food|foodlist])
    match {from, {'take, food}}
      if foodlist.include?(food)
        from <- {Pid.self, {'ok, food}}
        fridge(foodlist.delete(food))
      else
        from <- {Pid.self, 'not_found}
        fridge(foodlist)
      end
    match 'terminate
      'ok
    after 10000
      IO.puts "Timed out"
    end
  end
end