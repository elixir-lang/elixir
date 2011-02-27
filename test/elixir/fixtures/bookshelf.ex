module Bookshelf
  proto GenServer

  def start!(books)
    self.start_link(books)
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

  % Terminate sync message
  def handle_call('terminate, _from, books)
    { 'stop, 'normal, 'ok, books }
  end

  def handle_info(msg, books)
    IO.puts("Unexpected message: #{msg}\n")
    { 'no_reply, books }
  end

  def terminate('normal, books)
    books.each -> (b) IO.puts("Oh no! Setting \"#{b}\" on fire!\n")
    'ok
  end

  % Just do the code reloading
  def code_change(_old, books, _extra)
    { 'ok, books }
  end
end