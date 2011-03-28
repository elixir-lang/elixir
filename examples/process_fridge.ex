% Example from http://learnyousomeerlang.com/more-on-multiprocessing#state-your-state
% and converted to Elixir.
%
% This example showcases Elixir communication between process, in a similar fashion to Erlang's.
% This example builds a small fridge where you can store, take and see items.

object Fridge
  % Creates a new fridge by receiving a list.
  def initialize(list)
    % Spawn a new process by invoking the 'loop method defined below.
    % Notice that all methods defined in Fridge are actually defined
    % in Fridge::Proto, this is why we pass Fridge::Proto below.
    pid = Process.spawn Fridge::Proto, 'loop, [list]

    % Returns the object with the pid as instance variable.
    @('pid: pid)
  end

  % Store the given food in the fridge.
  def store(food)
    pid = @pid

    % Send a message to the spawned @pid passing the current
    % pid plus a message telling to store the given food.
    % This is the same as Erlang's ! operator
    pid <- { Process.self, { 'store, food } }

    % The spawn process will respond back with its own
    % pid and an ok message
    receive {~pid, msg}
      msg
    end
  end

  % Removes the given food from the fridge
  def take(food)
    pid = @pid

    % Send a message to the spawned @pid passing the current
    % pid plus a message telling to take the given food.
    %
    % This is exactly the same as above, but the <<- syntax
    % automatically adds Process.self for us.
    pid <- { Process.self, {'take, food} }

    % Retrieve the message again. Also, let's set a timeout
    % now for 10 seconds.
    receive {~pid, msg}
      msg
    after 10000
      'timedout
    end
  end

  % Returns a list with all the items in the fridge.
  def see
    pid = @pid

    % Send a see message
    pid <- { Process.self, 'see }

    % And get a message back
    receive {~pid, msg}
      msg
    end
  end

  % Send a terminate message
  def destroy
    @pid <- 'terminate
  end

  % Finally, the loop method. It receives all messages from above
  % and is responsible to keep the fridge updated. As the last
  % function called in loop is itself, it is tail call optimized.
  def loop(foodlist)
    % Receive a message, notice that when you are expecting more
    % than one type of message, you have to use receive/match.
    receive
    match {from, {'store, food}}
      from <- { Process.self, 'ok }
      loop([food|foodlist])
    match {from, {'take, food}}
      if foodlist.include?(food)
        from <- { Process.self, {'ok, food} }
        loop(foodlist.delete(food))
      else
        from <- { Process.self, 'not_found }
        loop(foodlist)
      end
    match {from,'see}
      from <- { Process.self, foodlist }
      loop(foodlist)
    match 'terminate
      'ok
    end
  end
end

fridge = Fridge.new(['beer,'water])
fridge.store('apples)
IO.puts "We have #{fridge.see.join(", ")} in the fridge"
fridge.take('beer)
IO.puts "We have #{fridge.see.join(", ")} in the fridge"
fridge.destroy