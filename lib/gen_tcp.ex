object GenTCP
  module Mixin
    def listen(port, options)
      case Erlang.gen_tcp.listen(port, options)
      match { 'ok, sock }
        { 'ok, GenTCP.new(sock) }
      match other
        other
      end
    end

    def listen(port, options, timeout)
      case Erlang.gen_tcp.listen(port, options, timeout)
      match { 'ok, sock }
        { 'ok, GenTCP.new(sock) }
      match other
        other
      end
    end

    def connect(address, port, options)
      connect(address, port, options, 0)
    end 

    def connect(address, port, options, timeout)
      case Erlang.gen_tcp.connect(address.to_char_list, port, options, timeout)
      match { 'ok, sock }
        { 'ok, GenTCP.new(sock) }
      match other
        other
      end
    end 

  end
  
  def constructor(sock)
    {'sock: sock}
  end

  def controlling_process(pid)
    Erlang.gen_tcp.controlling_process(@sock, pid)
  end

  def recv(sock, length)
    Erlang.gen_tcp.recv(sock, length)
  end

  def recv(sock, length, timeout)
    Erlang.gen_tcp.recv(sock, length, timeout)
  end

  def accept
    Erlang.gen_tcp.accept(@sock)
  end

  def accept(timeout)
    Erlang.gen_tcp.accept(@sock, timeout)
  end

  def send(message)
    Erlang.gen_tcp.send(@sock, message.to_bin) 
  end

  def close
    Erlang.gen_tcp.close(@sock)
  end  

  def shutdown
    Erlang.gen_tcp.shutdown(@sock)
  end
end

