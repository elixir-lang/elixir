module ExUnit::Server
  def start
    { 'ok, _ } = GenServer.start_link({'local, 'exunit_server}, #ExUnit::Server::Instance(), [])
  end

  def add_case(name)
    check -> GenServer.call('exunit_server, { 'add_case, name })
  end

  def merge_options(options)
    check -> GenServer.call('exunit_server, { 'merge_options, options })
  end

  def cases
    GenServer.call('exunit_server, 'cases)
  end

  def options
    GenServer.call('exunit_server, 'options)
  end

  private

  def check(function)
    try
      function.()
    catch 'exit: { 'noproc, _ }
      self.exit "ExUnit::Server is not running. Are you sure you used exunit from command line?"
    end
  end

  module Instance
    def __bound__
      @('options: {}, 'cases: [])
    end

    def init()
      { 'ok, self }
    end

    def handle_call({'add_case, name}, _from)
      { 'reply, 'ok, self.update_ivar('cases, _.push(name)) }
    end

    def handle_call({'merge_options, options}, _from)
      { 'reply, 'ok, self.update_ivar('options, _.merge(options)) }
    end

    def handle_call('cases, _from)
      { 'reply, @cases, self }
    end

    def handle_call('options, _from)
      { 'reply, @options, self }
    end

    def handle_call(_request, _from)
      { 'reply, 'undef, self }
    end

    def handle_info(_msg)
      { 'noreply, self }
    end

    def handle_cast(_msg)
      { 'noreply, self }
    end

    def terminate(reason)
      IO.puts "[FATAL] ExUnit::Server crashed:\n#{reason}"
      IO.puts "[FATAL] ExUnit::Server snapshot:\n#{self}"
      'ok
    end

    def code_change(_old, _extra)
      { 'ok, self }
    end
  end
end