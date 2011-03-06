% elixir: cache

object Code::Server
  module Mixin
    def start(basepath, basefiles)
      { 'ok, _ } = GenServer.start({'local, 'elixir_code_server}, self.new(basepath, basefiles), [])
    end
  end

  def constructor(basepath, basefiles)
    paths  = [File.expand_path(basepath)]
    loaded = [File.expand_path(file) for file in basefiles]
    { 'paths: paths, 'loaded: loaded, 'argv: [] }
  end

  protected

  def init
    { 'ok, self }
  end

  def handle_call({'push_path, path}, _from)
    unless_included path, -> @paths + [path]
  end

  def handle_call({'unshift_path, path}, _from)
    unless_included path, -> [path|@paths]
  end

  def handle_call({'delete_path, path}, _from)
    { 'reply, 'ok, self.set_ivar('paths, @paths.delete(path)) }
  end

  def handle_call({'loaded, path}, _from)
    { 'reply, 'ok, self.set_ivar('loaded, [path|@loaded]) }
  end

  def handle_call({'argv, argv}, _from)
    { 'reply, 'ok, self.set_ivar('argv, argv) }
  end

  % Read values from state
  ['paths, 'loaded, 'argv].each do (arg)
    module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
def handle_call('#{arg}, _from)
  { 'reply, @#{arg}, self }
end
~~
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
    IO.puts "[FATAL] Code::Server crashed:\n#{reason}"
    IO.puts "[FATAL] Code::Server snapshot:\n#{self}"
    Erlang.init.stop(1) % Shut everything
  end

  def code_change(_old, _extra)
    { 'ok, self }
  end

  private

  def unless_included(path, function)
    if @paths.include?(path)
      { 'reply, 'ok, self }
    else
      { 'reply, 'ok, self.set_ivar('paths, function()) }
    end
  end
end