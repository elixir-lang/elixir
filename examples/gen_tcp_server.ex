Code.require "gen_tcp"

module Server

  def do_recv(tcp, sock, bs)    
    case tcp.recv(sock,0)
    match {'ok, b}
      do_recv(tcp, sock, [bs, b])
    match {'error, 'closed}
      {'ok, Erlang.list_to_binary(bs)}
    end
  end

  def start
    {'ok, tcp} = GenTCP.listen(5678, ['binary, {'packet, 0}, {'active, false}])

    {'ok, sock} = tcp.accept
    {'ok, bin} = do_recv(tcp, sock, [])
    'ok = tcp.close
    IO.puts bin.to_s
  end
end

Server.start
