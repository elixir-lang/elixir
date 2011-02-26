Code.require "fixtures/kitchen"

object ProcessTest
  proto ExUnit::Case

  def spawn_and_messages_with_module_test
    pid = Process.spawn(Kitchen, 'fridge, [['iogurt, 'bread]])
    {'ok, 'bread} = Kitchen.take(pid, 'bread)
    ['iogurt] = Kitchen.see(pid)
    'ok = Kitchen.store(pid, 'soda)
    ['soda, 'iogurt] = Kitchen.see(pid)
    pid <- 'terminate
  end

  def spawn_and_messages_with_object_test
    kitchen = MyKitchen.new
    pid = Process.spawn(kitchen, 'fridge, [['iogurt, 'bread]])
    {'ok, 'bread} = kitchen.take(pid, 'bread)
    ['iogurt] = kitchen.see(pid)
    'ok = kitchen.store(pid, 'soda)
    ['soda, 'iogurt] = kitchen.see(pid)
    pid <- 'terminate
  end

  def spawn_and_messages_with_object_orientation_test
    fridge = MyFridge.new ['iogurt, 'bread]
    {'ok, 'bread} = fridge.take('bread)
    ['iogurt] = fridge.see
    'ok = fridge.store('soda)
    ['soda, 'iogurt] = fridge.see
    fridge.terminate
  end
end