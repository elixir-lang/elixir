% Example taken from http://learnyousomeerlang.com/more-on-multiprocessing#state-your-state

% This example is very very erlangish. See pid_test.ex.
% It would require you to pass the PID around the whole time,
% a better example is the MyFridge object below.
module Kitchen
  def store(pid, food)
    pid <- {Process.current, {'store, food}}
    receive {pid, msg}
      msg
    end
  end

  def take(pid, food)
    pid <- {Process.self, {'take, food}}
    receive {pid, msg}
      msg
    after 10000
      IO.puts "Timed out"
    end
  end

  def see(pid)
    pid <- {Process.self, 'see}
    receive {pid, msg}
      msg
    end
  end

  def fridge(foodlist)
    receive
    match {from, {'store, food}}
      from <- { Process.self, 'ok }
      fridge([food|foodlist])
    match {from, {'take, food}}
      if foodlist.include?(food)
        from <- { Process.self, {'ok, food} }
        fridge(foodlist.delete(food))
      else
        from <- { Process.self, 'not_found }
        fridge(foodlist)
      end
    match {from,'see}
      from <- { Process.self, foodlist }
      fridge(foodlist)
    match 'terminate
      'ok
    after 10000
      IO.puts "Timed out"
    end
  end
end

object MyKitchen
  proto Kitchen
end

% This example is more object oriented. The PID is internal
% to the object and you actually don't pass it around.
object MyFridge
  def constructor(list)
    pid = Process.spawn Kitchen, 'fridge, [list]
    { 'pid: pid }
  end

  def store(food)
    Kitchen.store(@pid, food)
  end
  
  def take(food)
    Kitchen.take(@pid, food)
  end

  def see
    Kitchen.see(@pid)
  end

  def terminate
    @pid <- 'terminate
  end
end