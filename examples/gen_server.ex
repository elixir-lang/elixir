Code.require "gen_server"

object Bookshelf
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

  def burn
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

  def terminate('normal, books)
    books.each -> (b) IO.puts("Oh no! \"#{b}\" is burning!")
    'ok
  end

  % Just do the code reloading
  def code_change(_old, books, _extra)
    { 'ok, books }
  end
end

bookshelf = Bookshelf.new(["Crafting Rails Apps","Programming Erlang"])

bookshelf.put("Programming Elixir")
IO.puts "In the bookshelf: "
bookshelf.see.each -> (b) IO.puts "* #{b}"
IO.puts

bookshelf.take("Programming Erlang")
IO.puts "In the bookshelf: "
bookshelf.see.each -> (b) IO.puts "* #{b}"
IO.puts

bookshelf.burn