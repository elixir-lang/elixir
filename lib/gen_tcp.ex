% Interface to Erlang's gen_tcp. Here is a simple example of client/server:
%
%     Code.require "gen_tcp"
%
%     module Server
%       def start
%         {'ok, tcp} = GenTCP.listen(5678, ['binary, {'packet, 0}, {'active, false}])
%         {'ok, sock} = tcp.accept
%         {'ok, bin} = recv(tcp, sock, [])
%         'ok = tcp.close
%         IO.puts bin.to_s
%       end
%
%       private
%
%       def recv(tcp, sock, bs)
%         case tcp.recv(sock,0)
%         match {'ok, b}
%           recv(tcp, sock, [bs, b])
%         match {'error, 'closed}
%           {'ok, bs.to_bin}
%         end
%       end
%     end
%
%     Server.start
%
% After starting the server file above, add the client below to a file (or run it
% directly from iex):
%
%     Code.require "gen_tcp"
%     {'ok, tcp} = GenTCP.connect("localhost", 5678, ['binary, {'packet, 0}])
%     'ok = tcp.send("Some Data")
%     'ok = tcp.close
%
% You will see the server will print <<"Some Data">> (as binary) and then exit.
module GenTCP
  def listen(port, options)
    init_socket Erlang.gen_tcp.listen(port, options)
  end

  def listen(port, options, timeout)
    init_socket Erlang.gen_tcp.listen(port, options, timeout)
  end

  def connect(address, port, options)
    init_socket Erlang.gen_tcp.connect(address.to_char_list, port, options)
  end

  def connect(address, port, options, timeout)
    init_socket Erlang.gen_tcp.connect(address.to_char_list, port, options, timeout)
  end

  private

  def init_socket(result)
    case result
    match { 'ok, socket }
      { 'ok, #GenTCP::Instance(socket) }
    match other
      other
    end
  end

  module Instance
    def __bound__(socket)
      @('socket: socket)
    end

    def controlling_process(pid)
      Erlang.gen_tcp.controlling_process(@socket, pid)
    end

    def recv(socket, length)
      Erlang.gen_tcp.recv(socket, length)
    end

    def recv(socket, length, timeout)
      Erlang.gen_tcp.recv(socket, length, timeout)
    end

    def accept
      Erlang.gen_tcp.accept(@socket)
    end

    def accept(timeout)
      Erlang.gen_tcp.accept(@socket, timeout)
    end

    def send(message)
      Erlang.gen_tcp.send(@socket, message)
    end

    def close
      Erlang.gen_tcp.close(@socket)
    end

    def shutdown
      Erlang.gen_tcp.shutdown(@socket)
    end
  end
end