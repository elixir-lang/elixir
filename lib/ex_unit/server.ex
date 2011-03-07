% elixir: cache

object ExUnit::Server
  module Mixin
    def start(options)
      { 'ok, _ } = GenServer.start_link({'local, 'exunit_server}, self.new(options), [])
    end

    def add_case(name)
      try
        GenServer.call('exunit_server, { 'add_case, name })
      catch 'exit: { 'noproc, _ }
        self.exit "ExUnit::Server is not running. Are you sure you invoked ExUnit.configure() ?"
      end
    end

    def cases
      GenServer.call('exunit_server, 'cases)
    end
  end

  def constructor(options)
    { 'options: options, 'cases: [] }
  end

  protected

  def init()
    { 'ok, self }
  end

  def handle_call({'add_case, name}, _from)
    { 'reply, 'ok, self.update_ivar('cases, _.push(name)) }
  end

  def handle_call('cases, _from)
    { 'reply, @cases, self }
  end

  def handle_call(_request, _from)
    { 'reply, 'undef, self }
  end

  def handle_info(_msg)
    { 'no_reply, self }
  end

  def handle_cast(_msg)
    { 'no_reply, self }
  end

  def terminate(reason)
    IO.puts "[FATAL] ExUnit::Server crashed:\n#{reason}"
    IO.puts "[FATAL] ExUnit::Server snapshot:\n#{self}"
    Erlang.init.stop(1) % Shut everything
  end

  def code_change(_old, _extra)
    { 'ok, self }
  end
end