module Bookshelf
  proto GenServer

  def start!(books)
    self.start_link(books)
  end

  def put(ref, title)
    self.cast(ref, {'put, title})
  end

  def take(ref, title)
    self.call(ref, {'take, title})
  end

  def see(ref)
    self.call(ref, 'see)
  end

  def terminate(ref)
    self.call(ref, 'terminate)
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
    self.cast(@ref, {'put, title})
  end

  def take(title)
    self.call(@ref, {'take, title})
  end

  def see
    self.call(@ref, 'see)
  end

  def terminate
    self.call(@ref, 'terminate)
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