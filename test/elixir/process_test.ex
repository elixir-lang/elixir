Code.require "fixtures/fridge"

object ProcessTest
  proto ExUnit::Case

  def spawn_and_messages_with_module_test
    pid = Process.spawn(Fridge, 'loop, [['iogurt, 'bread]])
    {'ok, 'bread} = Fridge.take(pid, 'bread)
    ['iogurt] = Fridge.see(pid)
    'ok = Fridge.store(pid, 'soda)
    ['soda, 'iogurt] = Fridge.see(pid)
    pid <- 'terminate
  end

  def spawn_and_messages_with_object_test
    fridge = MyFridge.new
    pid = Process.spawn(fridge, 'loop, [['iogurt, 'bread]])
    {'ok, 'bread} = fridge.take(pid, 'bread)
    ['iogurt] = fridge.see(pid)
    'ok = fridge.store(pid, 'soda)
    ['soda, 'iogurt] = fridge.see(pid)
    pid <- 'terminate
  end

  def spawn_and_messages_with_object_orientation_test
    fridge = BestFridge.new ['iogurt, 'bread]
    {'ok, 'bread} = fridge.take('bread)
    ['iogurt] = fridge.see
    'ok = fridge.store('soda)
    ['soda, 'iogurt] = fridge.see
    fridge.terminate
  end

  def inspect_test
    pid = Erlang.list_to_pid($"<0.4.1>")
    "<Process 0.4.1>" = pid.inspect
  end
end