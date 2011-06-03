Code.require_file "../test_helper", __FILE__
Code.require_file "../fixtures/fridge", __FILE__

module ProcessTest
  mixin ExUnit::Case

  def spawn_and_messages_with_module_test
    pid = Process.spawn -> Fridge.loop ['iogurt, 'bread]
    {'ok, 'bread} = Fridge.take(pid, 'bread)
    ['iogurt] = Fridge.see(pid)
    'ok = Fridge.store(pid, 'soda)
    ['soda, 'iogurt] = Fridge.see(pid)
    pid <- 'terminate
  end

  def spawn_and_messages_with_object_test
    fridge = MyFridge.new
    pid = Process.spawn -> fridge.loop(['iogurt, 'bread])
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

  def register_and_unregister_test
    process = Process.current
    process.register('elixir_test_process)
    self.assert_include 'elixir_test_process, Process.registered

    'elixir_test_process <- { 'test, 'message }
    receive { 'test, 'message }
    after 1000
      self.error 'timedout
    end

    true = Process.unregister('elixir_test_process)
    self.assert_error 'badarg, -> 'elixir_test_process <- { 'test, 'message }
  end

  def process_flag_and_link_test
    Process.flag 'trap_exit, true
    pid = Process.spawn -> internal_loop
    Process.link pid
    pid.exit('i_give_up)
    receive { 'EXIT, ~pid, 'i_give_up }
    after 1000
      self.error 'timedout
    end
  after
    Process.flag 'trap_exit, false
  end

  def inspect_test
    pid = Erlang.list_to_pid($"<0.4.1>")
    "<Process 0.4.1>" = pid.inspect
  end

  private

  def internal_loop
    receive _
      internal_loop
    end
  end
end