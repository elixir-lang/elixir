module Bookshelf
  proto GenServer

  def start!(books)
    self.start_link(books)
  end

  def put(ref, title)
    GenServer.cast(ref, {'put, title})
  end

  def take(ref, title)
    GenServer.call(ref, {'take, title})
  end

  def see(ref)
    GenServer.call(ref, 'see)
  end

  def terminate(ref)
    GenServer.call(ref, 'terminate)
  end

  callbacks

  def init(books)
    { 'ok, books }
  end

  % Async message
  def handle_cast({'put, title}, books)
    { 'noreply, [title|books] }
  end

  % Sync message
  def handle_call({'take, title}, _from, books)
    if books.include?(title)
      { 'reply, 'ok, books.delete(title) }
    else
      { 'reply, 'not_found, books }
    end
  end

  def handle_call('see, _from, books)
    { 'reply, books, books }
  end

  % Terminate sync message
  def handle_call('terminate, _from, books)
    { 'stop, 'normal, 'ok, books }
  end

  def handle_info(msg, books)
    IO.puts("Unexpected message: #{msg}\n")
    { 'no_reply, books }
  end

  def terminate('normal, _books)
    'ok
  end

  % Just do the code reloading
  def code_change(_old, books, _extra)
    { 'ok, books }
  end
end

% Just to ensure callbacks won't propagate
object MyBookshelf
  proto Bookshelf

  def empty
  end
end

object BestBookshelf
  proto GenServer

  def constructor(books)
    { 'ok, ref } = self.start_link(books)
    { 'ref: ref }
  end

  def put(title)
    GenServer.cast(@ref, {'put, title})
  end

  def take(title)
    GenServer.call(@ref, {'take, title})
  end

  def see
    GenServer.call(@ref, 'see)
  end

  def terminate
    GenServer.call(@ref, 'terminate)
  end

  callbacks

  def init(books)
    { 'ok, books }
  end

  % Async message
  def handle_cast({'put, title}, books)
    { 'noreply, [title|books] }
  end

  % Sync message
  def handle_call({'take, title}, _from, books)
    if books.include?(title)
      { 'reply, 'ok, books.delete(title) }
    else
      { 'reply, 'not_found, books }
    end
  end

  def handle_call('see, _from, books)
    { 'reply, books, books }
  end

  % Terminate sync message
  def handle_call('terminate, _from, books)
    { 'stop, 'normal, 'ok, books }
  end

  def handle_info(msg, books)
    IO.puts("Unexpected message: #{msg}\n")
    { 'no_reply, books }
  end

  def terminate('normal, _books)
    'ok
  end

  % Just do the code reloading
  def code_change(_old, books, _extra)
    { 'ok, books }
  end
end

object AwesomeBookshelf
  module Mixin
    def start
      GenServer::Delegator.start_link({'local, 'delegator}, self.new, [])
    end

    def set(key, value)
      GenServer.call('delegator, { 'set, key, value })
    end
  end

  def constructor
    {:}
  end

  def init
    { 'ok, self }
  end

  % Async message
  def handle_cast(_)
    { 'noreply, self }
  end

  % Sync message
  def handle_call({'set, key, value}, _from)
    object = self.set_ivar(key, value)
    { 'reply, object, object }
  end

  % Terminate sync message
  def handle_call('terminate, _from)
    { 'stop, 'normal, 'ok, self }
  end

  def handle_info(msg)
    IO.puts("Unexpected message: #{msg}\n")
    { 'no_reply, self }
  end

  def terminate('normal)
    'ok
  end

  % Just do the code reloading
  def code_change(_old, _extra)
    { 'ok, self }
  end
end