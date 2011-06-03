% Example taken from http://learnyousomeerlang.com/more-on-multiprocessing#state-your-state

% This example is very very erlangish. See process_test.exs.
% It would require you to pass the PID around the whole time,
% a better example is the MyFridge object below.
module Fridge
  def store(pid, food)
    pid <- {Process.current, {'store, food}}
    receive {~pid, msg}
      msg
    end
  end

  def take(pid, food)
    pid <- {Process.current, {'take, food}}
    receive {~pid, msg}
      msg
    after 10000
      IO.puts "Timed out"
    end
  end

  def see(pid)
    pid <- {Process.current, 'see}
    receive {~pid, msg}
      msg
    end
  end

  def loop(foodlist)
    receive
    match {from, {'store, food}}
      from <- { Process.current, 'ok }
      loop([food|foodlist])
    match {from, {'take, food}}
      if foodlist.include?(food)
        from <- { Process.current, {'ok, food} }
        loop(foodlist.delete(food))
      else
        from <- { Process.current, 'not_found }
        loop(foodlist)
      end
    match {from,'see}
      from <- { Process.current, foodlist }
      loop(foodlist)
    match 'terminate
      'ok
    after 10000
      IO.puts "Timed out"
    end
  end
end

% This example is more object oriented. The PID is internal
% to the object and you actually don't pass it around.
module BestFridge
  def __bound__(list)
    pid = Process.spawn -> Fridge.loop list
    @('pid: pid)
  end

  def store(food)
    Fridge.store(@pid, food)
  end
  
  def take(food)
    Fridge.take(@pid, food)
  end

  def see
    Fridge.see(@pid)
  end

  def terminate
    @pid <- 'terminate
  end
end