module Bookshelf
  def __bound__(books)
    { 'ok, ref } = GenServer.start_link(#Bookshelf::Server(books))
    @('ref: ref)
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

  module Server
    def __bound__(books)
      @('books: books)
    end

    def init
      { 'ok, self }
    end

    % Async message
    def handle_cast({'put, title})
      { 'noreply, self.set_ivar('books, [title|@books]) }
    end

    % Sync message
    def handle_call({'take, title}, _from)
      if @books.include?(title)
        { 'reply, 'ok, self.set_ivar('books, @books.delete(title)) }
      else
        { 'reply, 'not_found, self }
      end
    end

    def handle_call('see, _from)
      { 'reply, @books, self }
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
end